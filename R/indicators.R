# =============================================================================
# Polio surveillance indicators (denominator layer)
#
# polished produces analytic case/ES/SIA outputs but, by design, leaves the
# computation of *rates* to downstream consumers. This module fills that gap
# with a registry-driven indicator engine that mirrors how WHO POLIS computes
# its `FACT_IndicatorValues`: annualised NPAFP rate per 100,000 under-15
# population, stool-adequacy %, investigation timeliness %, and the share of
# districts meeting the NPAFP >= 2 benchmark -- each at country / province /
# district level, with confidence flags and red/orange/green categories.
#
# Design notes
#   * Each indicator is a self-contained spec in `.polio_indicator_registry()`
#     (label, formula, kind, thresholds, compute fn). Adding an indicator means
#     adding one list entry -- the orchestrator, output assembly and CLI summary
#     all derive from the registry.
#   * Column names are fully parameterised; defaults match the AFP output of
#     `clean_afp()` (`classification_all`, `age_months`, `year_onset`,
#     `adm{0,1,2}_guid`, `adequate_stool`, `notify_to_invest`).
#   * Annualisation reproduces `ufn_AnnualizeIndicatorValues`:
#     `365 / (1 + days_in_period)` with the period end capped at `reference_date`
#     so year-to-date periods scale up to a full-year rate.
# =============================================================================

utils::globalVariables(c(
  "guid",
  "name",
  "year",
  "value",
  "numerator",
  "denominator",
  "pop",
  "ann_factor",
  "text_code",
  "confidence",
  "category",
  "indicator",
  "g0",
  "n0",
  "g1",
  "n1",
  "g2",
  "n2",
  "age",
  "class",
  "adequate",
  "nottoinvest",
  "is_afp",
  "is_npafp",
  "level",
  ".n",
  ".parent_guid",
  ".parent_name",
  "rate"
))

# ---- public API -------------------------------------------------------------

#' Calculate polio surveillance indicators (NPAFP rate, stool adequacy, ...)
#'
#' Computes the headline AFP surveillance indicators from a cleaned case table
#' (e.g. the output of [clean_afp()]) at country (`adm0`), province
#' (`adm1`) and district (`adm2`) level, by year. Rates that need a population
#' denominator (NPAFP rate, % districts NPAFP >= 2) require a `population`
#' table of *under-15* population per admin unit per year.
#'
#' Indicators currently implemented:
#' \describe{
#'   \item{`npafp_rate`}{Annualised non-polio AFP cases per 100,000 children
#'     under 15: `annualise(npafp_cases / under15_pop * 100000)`. Target >= 2
#'     (warn) / >= 3 (good).}
#'   \item{`stool_adequacy_pct`}{% of AFP cases with two adequate stool
#'     specimens. Target >= 80% (good) / >= 60% (warn).}
#'   \item{`inv_timeliness_pct`}{% of AFP cases investigated within
#'     `invest_timely_days` days of notification.}
#'   \item{`pct_districts_npafp_ge2`}{% of districts (adm2) whose annualised
#'     NPAFP rate is >= `npafp_warn`; reported at adm0 and adm1.}
#' }
#'
#' @param cases Cleaned AFP case data, one row per case (data.frame/tibble).
#' @param population Optional under-15 population denominators, one row per
#'   admin unit per year, with columns named by `pop_guid_var`, `pop_year_var`,
#'   `pop_var`. Required for `npafp_rate` and `pct_districts_npafp_ge2`; if
#'   absent those indicators are skipped with a message.
#' @param indicators Character vector of indicators to compute (default: all).
#' @param levels Admin levels to report at: any of `"adm0"`, `"adm1"`, `"adm2"`.
#' @param class_var,age_var,year_var,onset_date_var Case columns: final
#'   classification, age in months, onset year, onset date.
#' @param adm0_guid_var,adm1_guid_var,adm2_guid_var Case GUID columns per level.
#' @param adm0_name_var,adm1_name_var,adm2_name_var Case admin-name columns.
#' @param adequacy_var Case column flagging adequate stool (logical / 0-1 /
#'   "Yes"-"No").
#' @param invest_interval_var Case column with notification->investigation days
#'   (default `"notify_to_invest"`, as produced by [clean_afp()]).
#' @param npafp_classes Values of `class_var` that count as non-polio AFP
#'   (default `"NPAFP"`). Matched case-insensitively.
#' @param pending_classes Values treated as pending (default `c("PENDING",
#'   "LAB PENDING")`).
#' @param include_pending If `TRUE` (default) pending cases are included in the
#'   NPAFP numerator, matching `ufn_Indicator_NPAFP_RATE`. Set `FALSE` for the
#'   `_NOPENDING` variant.
#' @param afp_exclude_classes Classifications excluded from the AFP denominator
#'   for percentage indicators (default `c("NOT-AFP")`).
#' @param pop_guid_var,pop_year_var,pop_var Column names in `population`.
#' @param rate_multiplier Population scale for the rate (default `1e5`).
#' @param npafp_target,npafp_warn Good / warning thresholds for NPAFP rate
#'   (default `3` / `2` per 100,000).
#' @param adequacy_target,adequacy_warn Thresholds for stool adequacy % and
#'   timeliness % (default `80` / `60`).
#' @param invest_timely_days Max notification->investigation days considered
#'   timely (default `2`).
#' @param min_pop Population below which a rate is flagged low-confidence
#'   (default `1e5`).
#' @param min_cases Case count below which a percentage is flagged
#'   low-confidence (default `10`).
#' @param reference_date Date used to cap the annualisation period (default
#'   today). Pass a fixed date for reproducible runs.
#' @param verbose Emit a cli progress + summary report (default `TRUE`).
#'
#' @return A named list:
#'   \describe{
#'     \item{`adm0`,`adm1`,`adm2`}{Wide tibbles (one row per admin-year) with a
#'       value column per indicator plus `afp_cases`, `npafp_cases`,
#'       `under15_pop`.}
#'     \item{`long`}{Tidy tibble: one row per level x admin x year x indicator
#'       with `value`, `numerator`, `denominator`, `confidence`, `category`,
#'       `text_code`.}
#'     \item{`meta`}{Run metadata (indicators, levels, thresholds,
#'       `reference_date`).}
#'   }
#' @export
calc_polio_indicators <- function(
  cases,
  population = NULL,
  indicators = c(
    "npafp_rate",
    "stool_adequacy_pct",
    "inv_timeliness_pct",
    "pct_districts_npafp_ge2"
  ),
  levels = c("adm0", "adm1", "adm2"),
  class_var = "classification_all",
  age_var = "age_months",
  year_var = "year_onset",
  onset_date_var = "paralysis_onset_date",
  adm0_guid_var = "adm0_guid",
  adm1_guid_var = "adm1_guid",
  adm2_guid_var = "adm2_guid",
  adm0_name_var = "adm0",
  adm1_name_var = "adm1",
  adm2_name_var = "adm2",
  adequacy_var = "adequate_stool",
  invest_interval_var = "notify_to_invest",
  npafp_classes = "NPAFP",
  pending_classes = c("PENDING", "LAB PENDING"),
  include_pending = TRUE,
  afp_exclude_classes = "NOT-AFP",
  pop_guid_var = "guid",
  pop_year_var = "year",
  pop_var = "pop",
  rate_multiplier = 1e5,
  npafp_target = 3,
  npafp_warn = 2,
  adequacy_target = 80,
  adequacy_warn = 60,
  invest_timely_days = 2,
  min_pop = 1e5,
  min_cases = 10,
  reference_date = Sys.Date(),
  verbose = TRUE
) {
  registry <- .polio_indicator_registry()

  # ---- validate ------------------------------------------------------------
  if (!is.data.frame(cases)) {
    cli::cli_abort("{.arg cases} must be a data.frame or tibble.")
  }
  if (nrow(cases) == 0) {
    cli::cli_abort("{.arg cases} is empty.")
  }
  bad_ind <- setdiff(indicators, names(registry))
  if (length(bad_ind) > 0) {
    cli::cli_abort(c(
      "Unknown indicator{?s}: {.val {bad_ind}}",
      "i" = "Available: {.val {names(registry)}}"
    ))
  }
  bad_levels <- setdiff(levels, c("adm0", "adm1", "adm2"))
  if (length(bad_levels) > 0) {
    cli::cli_abort("Invalid {.arg levels}: {.val {bad_levels}}")
  }

  # required case columns for the requested indicators
  level_cols <- list(
    adm0 = c(adm0_guid_var, adm0_name_var),
    adm1 = c(adm1_guid_var, adm1_name_var),
    adm2 = c(adm2_guid_var, adm2_name_var)
  )
  needed <- c(class_var, age_var, year_var, unlist(level_cols[levels]))
  if (any(c("stool_adequacy_pct") %in% indicators))
    needed <- c(needed, adequacy_var)
  if ("inv_timeliness_pct" %in% indicators)
    needed <- c(needed, invest_interval_var)
  missing_cols <- setdiff(unique(needed), names(cases))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required column{?s} in {.arg cases}:",
      "x" = "{.var {missing_cols}}",
      "i" = "Override the matching `*_var` argument(s) if your columns differ."
    ))
  }

  # population-dependent indicators
  pop_indicators <- names(registry)[vapply(
    registry,
    `[[`,
    logical(1),
    "requires_pop"
  )]
  want_pop <- intersect(indicators, pop_indicators)
  pop_std <- NULL
  if (length(want_pop) > 0) {
    if (is.null(population)) {
      if (verbose) {
        cli::cli_alert_warning(
          "No {.arg population} supplied -- skipping {.val {want_pop}}."
        )
      }
      indicators <- setdiff(indicators, want_pop)
    } else {
      pop_missing <- setdiff(
        c(pop_guid_var, pop_year_var, pop_var),
        names(population)
      )
      if (length(pop_missing) > 0) {
        cli::cli_abort(c(
          "Missing column{?s} in {.arg population}:",
          "x" = "{.var {pop_missing}}"
        ))
      }
      pop_std <- population |>
        dplyr::transmute(
          guid = as.character(.data[[pop_guid_var]]),
          year = as.integer(.data[[pop_year_var]]),
          pop = as.numeric(.data[[pop_var]])
        ) |>
        dplyr::filter(!is.na(guid), !is.na(year), !is.na(pop), pop > 0) |>
        dplyr::distinct(guid, year, .keep_all = TRUE)
    }
  }
  if (length(indicators) == 0) {
    cli::cli_abort(
      "No indicators left to compute (population-only request without population)."
    )
  }

  if (!is.numeric(rate_multiplier) || rate_multiplier <= 0) {
    cli::cli_abort("{.arg rate_multiplier} must be a positive number.")
  }
  reference_date <- as.Date(reference_date)

  if (verbose) {
    cli::cli_alert_info(
      "Computing {length(indicators)} indicator(s) for {nrow(cases)} cases at level(s) {.val {levels}}."
    )
  }

  # ---- standardise the case table -----------------------------------------
  std <- cases |>
    dplyr::transmute(
      g0 = as.character(.data[[adm0_guid_var]]),
      n0 = as.character(.data[[adm0_name_var]]),
      g1 = if (adm1_guid_var %in% names(cases))
        as.character(.data[[adm1_guid_var]]) else NA_character_,
      n1 = if (adm1_name_var %in% names(cases))
        as.character(.data[[adm1_name_var]]) else NA_character_,
      g2 = if (adm2_guid_var %in% names(cases))
        as.character(.data[[adm2_guid_var]]) else NA_character_,
      n2 = if (adm2_name_var %in% names(cases))
        as.character(.data[[adm2_name_var]]) else NA_character_,
      year = suppressWarnings(as.integer(.data[[year_var]])),
      age = suppressWarnings(as.numeric(.data[[age_var]])),
      class = toupper(trimws(as.character(.data[[class_var]]))),
      adequate = if (adequacy_var %in% names(cases))
        .polis_as_logical(.data[[adequacy_var]]) else NA,
      nottoinvest = if (invest_interval_var %in% names(cases)) {
        suppressWarnings(as.numeric(.data[[invest_interval_var]]))
      } else {
        NA_real_
      }
    )

  n_no_year <- sum(is.na(std$year))
  if (n_no_year > 0 && verbose) {
    cli::cli_alert_warning(
      "{n_no_year} case(s) have no onset year and are excluded from indicators."
    )
  }
  std <- std |> dplyr::filter(!is.na(year))

  # derived case-level flags (definitions shared across indicators)
  std <- std |>
    dplyr::mutate(
      is_afp = .polis_afp_flag(class, age, afp_exclude_classes),
      is_npafp = .polis_npafp_flag(
        class,
        age,
        npafp_classes,
        pending_classes,
        include_pending
      )
    )

  cfg <- list(
    rate_multiplier = rate_multiplier,
    npafp_target = npafp_target,
    npafp_warn = npafp_warn,
    adequacy_target = adequacy_target,
    adequacy_warn = adequacy_warn,
    invest_timely_days = invest_timely_days,
    min_pop = min_pop,
    min_cases = min_cases,
    reference_date = reference_date,
    pop = pop_std
  )

  # ---- compute each indicator at each level --------------------------------
  long_rows <- list()
  for (ind in indicators) {
    spec <- registry[[ind]]
    ind_levels <- intersect(levels, spec$levels)
    if (length(ind_levels) == 0) next
    if (verbose) cli::cli_alert_info("{spec$label} ({.field {ind}}) ...")

    for (lv in ind_levels) {
      lf <- .polio_level_frame(std, lv)
      res <- spec$compute(lf, cfg, lv)
      if (is.null(res) || nrow(res) == 0) next
      res <- res |>
        dplyr::mutate(
          confidence = spec$confidence(numerator, denominator, cfg),
          category = spec$categorise(value, cfg),
          indicator = ind,
          level = lv
        )
      long_rows[[paste(ind, lv, sep = "@")]] <- res
    }
    if (verbose) {
      done <- long_rows[grepl(paste0("^", ind, "@"), names(long_rows))]
      n_val <- sum(vapply(done, function(d) sum(!is.na(d$value)), integer(1)))
      cli::cli_alert_success("{spec$label}: {n_val} value(s) computed.")
    }
  }

  long <- dplyr::bind_rows(long_rows)
  if (nrow(long) == 0) {
    cli::cli_abort(
      "No indicator values could be computed from the supplied data."
    )
  }
  long <- long |>
    dplyr::select(
      level,
      guid,
      name,
      year,
      indicator,
      value,
      numerator,
      denominator,
      confidence,
      category,
      text_code
    ) |>
    dplyr::arrange(level, indicator, name, year)

  # ---- assemble wide per-level tibbles -------------------------------------
  out <- list()
  for (lv in levels) {
    out[[lv]] <- .polio_wide_level(std, long, lv, registry)
  }
  out$long <- long
  out$meta <- list(
    indicators = indicators,
    levels = levels,
    reference_date = reference_date,
    thresholds = list(
      npafp = c(warn = npafp_warn, target = npafp_target),
      adequacy = c(warn = adequacy_warn, target = adequacy_target)
    ),
    rate_multiplier = rate_multiplier,
    include_pending = include_pending
  )

  # ---- CLI summary ---------------------------------------------------------
  if (verbose) .polio_print_summary(out, indicators, cfg, registry)

  out
}

# ---- indicator registry -----------------------------------------------------

#' Indicator registry (internal)
#'
#' One entry per indicator. Each spec carries display metadata, a `compute`
#' function `(level_frame, cfg, level) -> tibble(guid,name,year,value,
#' numerator,denominator,text_code)`, a `confidence` function and a
#' `categorise` (red/orange/green) function.
#' @keywords internal
#' @noRd
.polio_indicator_registry <- function() {
  conf_rate <- function(num, den, cfg) {
    dplyr::if_else(!is.na(den) & den >= cfg$min_pop, 1, 0)
  }
  conf_pct <- function(num, den, cfg) {
    dplyr::if_else(!is.na(den) & den >= cfg$min_cases, 1, 0)
  }
  cat_higher <- function(target, warn) {
    function(value, cfg) {
      dplyr::case_when(
        is.na(value) ~ NA_character_,
        value >= target ~ "green",
        value >= warn ~ "orange",
        TRUE ~ "red"
      )
    }
  }

  list(
    npafp_rate = list(
      label = "NPAFP rate (per 100k under-15)",
      formula = "annualise(npafp_cases / under15_pop * 100000)",
      kind = "rate",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = TRUE,
      compute = .calc_npafp_rate,
      confidence = conf_rate,
      categorise = function(value, cfg) {
        cat_higher(cfg$npafp_target, cfg$npafp_warn)(value, cfg)
      }
    ),
    stool_adequacy_pct = list(
      label = "Stool adequacy (%)",
      formula = "100 * adequate_afp_cases / afp_cases",
      kind = "percent",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = FALSE,
      compute = .calc_stool_adequacy,
      confidence = conf_pct,
      categorise = function(value, cfg) {
        cat_higher(cfg$adequacy_target, cfg$adequacy_warn)(value, cfg)
      }
    ),
    inv_timeliness_pct = list(
      label = "Investigation timeliness (%)",
      formula = "100 * cases(notif->invest <= k days) / afp_cases",
      kind = "percent",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = FALSE,
      compute = .calc_inv_timeliness,
      confidence = conf_pct,
      categorise = function(value, cfg) {
        cat_higher(cfg$adequacy_target, cfg$adequacy_warn)(value, cfg)
      }
    ),
    pct_districts_npafp_ge2 = list(
      label = "Districts NPAFP >= 2 (%)",
      formula = "100 * districts(npafp_rate >= warn) / districts_with_rate",
      kind = "percent",
      levels = c("adm0", "adm1"),
      requires_pop = TRUE,
      compute = .calc_pct_districts_npafp_ge2,
      confidence = function(num, den, cfg) {
        dplyr::if_else(!is.na(den) & den > 0, 1, 0)
      },
      categorise = function(value, cfg) NA_character_
    )
  )
}

# ---- per-indicator compute functions ---------------------------------------

#' @keywords internal
#' @noRd
.calc_npafp_rate <- function(lf, cfg, level) {
  universe <- lf |> dplyr::distinct(guid, name, year)
  num <- lf |>
    dplyr::filter(is_npafp) |>
    dplyr::count(guid, name, year, name = "numerator")

  universe |>
    dplyr::left_join(num, by = dplyr::join_by(guid, name, year)) |>
    dplyr::mutate(numerator = dplyr::coalesce(numerator, 0L)) |>
    dplyr::left_join(cfg$pop, by = dplyr::join_by(guid, year)) |>
    dplyr::mutate(
      ann_factor = .polio_annualise_factor(year, cfg$reference_date),
      denominator = pop,
      value = dplyr::if_else(
        !is.na(pop) & pop > 0,
        numerator / pop * cfg$rate_multiplier * ann_factor,
        NA_real_
      ),
      text_code = dplyr::if_else(is.na(pop), "NOPOP", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

#' @keywords internal
#' @noRd
.calc_stool_adequacy <- function(lf, cfg, level) {
  lf |>
    dplyr::filter(is_afp) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      denominator = dplyr::n(),
      numerator = sum(adequate %in% TRUE, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        denominator > 0,
        100 * numerator / denominator,
        NA_real_
      ),
      text_code = dplyr::if_else(denominator == 0, "NOCASE", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

#' @keywords internal
#' @noRd
.calc_inv_timeliness <- function(lf, cfg, level) {
  lf |>
    dplyr::filter(is_afp) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      denominator = dplyr::n(),
      numerator = sum(
        !is.na(nottoinvest) &
          nottoinvest >= 0 &
          nottoinvest <= cfg$invest_timely_days
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        denominator > 0,
        100 * numerator / denominator,
        NA_real_
      ),
      text_code = dplyr::if_else(denominator == 0, "NOCASE", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

#' @keywords internal
#' @noRd
.calc_pct_districts_npafp_ge2 <- function(lf, cfg, level) {
  # district-level NPAFP rate, retaining the parent admin for roll-up
  parent_guid <- if (level == "adm0") "g0" else "g1"
  parent_name <- if (level == "adm0") "n0" else "n1"

  d_universe <- lf |>
    dplyr::distinct(
      .parent_guid = .data[[parent_guid]],
      .parent_name = .data[[parent_name]],
      g2,
      n2,
      year
    ) |>
    dplyr::filter(!is.na(g2))
  d_num <- lf |>
    dplyr::filter(is_npafp, !is.na(g2)) |>
    dplyr::count(g2, year, name = "numerator")

  district_rate <- d_universe |>
    dplyr::left_join(d_num, by = dplyr::join_by(g2, year)) |>
    dplyr::mutate(numerator = dplyr::coalesce(numerator, 0L)) |>
    dplyr::left_join(cfg$pop, by = dplyr::join_by(g2 == guid, year)) |>
    dplyr::mutate(
      ann_factor = .polio_annualise_factor(year, cfg$reference_date),
      rate = dplyr::if_else(
        !is.na(pop) & pop > 0,
        numerator / pop * cfg$rate_multiplier * ann_factor,
        NA_real_
      )
    )

  district_rate |>
    dplyr::group_by(guid = .parent_guid, name = .parent_name, year) |>
    dplyr::summarise(
      denominator = sum(!is.na(rate)),
      numerator = sum(rate >= cfg$npafp_warn, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        denominator > 0,
        100 * numerator / denominator,
        NA_real_
      ),
      text_code = dplyr::if_else(denominator == 0, "NOPOP", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

# ---- internal helpers -------------------------------------------------------

#' Project the standardised case frame onto one admin level.
#' Returns columns: guid, name, year, age, class, adequate, nottoinvest,
#' is_afp, is_npafp, plus parent guids/names (g0,n0,g1,n1,g2,n2) for roll-ups.
#' @keywords internal
#' @noRd
.polio_level_frame <- function(std, level) {
  gv <- switch(level, adm0 = "g0", adm1 = "g1", adm2 = "g2")
  nv <- switch(level, adm0 = "n0", adm1 = "n1", adm2 = "n2")
  std |>
    dplyr::mutate(guid = .data[[gv]], name = .data[[nv]]) |>
    dplyr::filter(!is.na(guid))
}

#' Annualisation factor reproducing `ufn_AnnualizeIndicatorValues`.
#' For a full elapsed year returns ~1; for a year-to-date period (end capped
#' at `reference_date`) returns 365 / days-elapsed, scaling the rate up.
#' @keywords internal
#' @noRd
.polio_annualise_factor <- function(year, reference_date) {
  start <- as.Date(sprintf("%d-01-01", year))
  end <- as.Date(sprintf("%d-12-31", year))
  eff_end <- pmin(end, reference_date)
  days <- as.integer(eff_end - start)
  dplyr::if_else(days < 0, NA_real_, 365 / (1 + days))
}

#' AFP denominator flag: under-15 and not an excluded (non-AFP) classification.
#' @keywords internal
#' @noRd
.polis_afp_flag <- function(class, age, exclude_classes) {
  excl <- toupper(trimws(exclude_classes))
  !is.na(age) & age >= 0 & age < 180 & !(class %in% excl)
}

#' NPAFP numerator flag: under-15 non-polio AFP (optionally incl. pending).
#' @keywords internal
#' @noRd
.polis_npafp_flag <- function(
  class,
  age,
  npafp_classes,
  pending_classes,
  include_pending
) {
  keep <- toupper(trimws(npafp_classes))
  if (include_pending) keep <- union(keep, toupper(trimws(pending_classes)))
  !is.na(age) & age >= 0 & age < 180 & class %in% keep
}

#' Coerce a heterogeneous yes/no column to logical.
#' Accepts logical, 0/1 numeric, and common string spellings.
#' @keywords internal
#' @noRd
.polis_as_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x)) {
    return(x == 1)
  }
  s <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    s %in% c("1", "yes", "y", "true", "t", "adequate") ~ TRUE,
    s %in% c("0", "no", "n", "false", "f", "inadequate") ~ FALSE,
    TRUE ~ NA
  )
}

#' Build the wide per-level tibble (one row per admin-year, value per indicator).
#' @keywords internal
#' @noRd
.polio_wide_level <- function(std, long, level, registry) {
  gv <- switch(level, adm0 = "g0", adm1 = "g1", adm2 = "g2")
  nv <- switch(level, adm0 = "n0", adm1 = "n1", adm2 = "n2")

  base <- std |>
    dplyr::mutate(guid = .data[[gv]], name = .data[[nv]]) |>
    dplyr::filter(!is.na(guid)) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      afp_cases = sum(is_afp, na.rm = TRUE),
      npafp_cases = sum(is_npafp, na.rm = TRUE),
      .groups = "drop"
    )

  vals <- long |>
    dplyr::filter(level == !!level) |>
    dplyr::select(guid, name, year, indicator, value) |>
    tidyr::pivot_wider(names_from = indicator, values_from = value)

  # under-15 pop, if any rate indicator carried it
  pop_col <- long |>
    dplyr::filter(level == !!level, indicator == "npafp_rate") |>
    dplyr::select(guid, year, under15_pop = denominator)

  base |>
    dplyr::left_join(vals, by = dplyr::join_by(guid, name, year)) |>
    dplyr::left_join(pop_col, by = dplyr::join_by(guid, year)) |>
    dplyr::arrange(name, year)
}

# ---- CLI summary ------------------------------------------------------------

#' @keywords internal
#' @noRd
.polio_print_summary <- function(out, indicators, cfg, registry) {
  cli::cli_rule()
  cli::cli_h2("Polio Indicator Summary")

  cli::cli_h3("Formulas used")
  for (ind in indicators) {
    spec <- registry[[ind]]
    cli::cli_text("{.strong {spec$label}}: {spec$formula}")
  }

  cli::cli_h3("Thresholds")
  cli::cli_bullets(c(
    "*" = "NPAFP rate: green >= {cfg$npafp_target}, orange >= {cfg$npafp_warn}, else red (per 100k)",
    "*" = "Adequacy / timeliness: green >= {cfg$adequacy_target}%, orange >= {cfg$adequacy_warn}%",
    "*" = "Annualisation reference date: {format(cfg$reference_date)}"
  ))

  long <- out$long
  latest <- suppressWarnings(max(long$year, na.rm = TRUE))

  cli::cli_h3("Coverage ({latest})")
  cov <- long |>
    dplyr::filter(year == latest) |>
    dplyr::group_by(indicator) |>
    dplyr::summarise(
      units = dplyr::n(),
      valued = sum(!is.na(value)),
      low_conf = sum(confidence == 0, na.rm = TRUE),
      .groups = "drop"
    )
  for (i in seq_len(nrow(cov))) {
    r <- cov[i, ]
    cli::cli_text(
      "{.field {r$indicator}}: {r$valued}/{r$units} unit-values ({r$low_conf} low-confidence)"
    )
  }

  # National results by year (adm0), value-only, for the headline indicators
  if (!is.null(out$adm0) && nrow(out$adm0) > 0) {
    cli::cli_h3("National results by year (adm0)")
    show_cols <- intersect(
      c("name", "year", "afp_cases", "npafp_cases", "under15_pop", indicators),
      names(out$adm0)
    )
    round_cols <- intersect(indicators, names(out$adm0))
    nat <- out$adm0 |>
      dplyr::select(dplyr::all_of(show_cols)) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(round_cols), ~ round(.x, 2))) |>
      dplyr::arrange(name, year)
    cli::cat_line(format(nat, n = Inf))
  }
  cli::cli_rule()
  invisible(out)
}

# ---- indicator dictionary ---------------------------------------------------

#' Dictionary of available polio surveillance indicators
#'
#' Returns a tidy description of every indicator [calc_polio_indicators()] can
#' compute -- its code, label, formula, numerator/denominator definition, the
#' admin levels it is reported at, whether it needs a population denominator,
#' its WHO target/warning thresholds and the data source. Call it to discover
#' what is available without reading the source.
#'
#' @param as_tibble If `TRUE` (default) return a tibble; if `FALSE` return the
#'   underlying named list of specs.
#'
#' @return A tibble (one row per indicator) with columns: `code`, `label`,
#'   `kind`, `formula`, `numerator`, `denominator`, `levels`, `requires_pop`,
#'   `target`, `warn`, `unit`, `source`, `notes`.
#'
#' @examples
#' available_indicators()
#' available_indicators()[, c("code", "label", "formula")]
#' @export
available_indicators <- function(as_tibble = TRUE) {
  dict <- .polio_indicator_dictionary()
  if (!as_tibble) {
    return(dict)
  }
  rows <- lapply(dict, function(s) {
    data.frame(
      code = s$code,
      label = s$label,
      kind = s$kind,
      formula = s$formula,
      numerator = s$numerator,
      denominator = s$denominator,
      levels = paste(s$levels, collapse = ", "),
      requires_pop = s$requires_pop,
      target = s$target,
      warn = s$warn,
      unit = s$unit,
      source = s$source,
      notes = s$notes,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  rownames(out) <- NULL
  dplyr::as_tibble(out)
}

#' Static dictionary metadata for indicators (internal).
#'
#' Kept separate from the compute registry so documentation can be richer than
#' the runtime spec and surfaced via [available_indicators()].
#' @keywords internal
#' @noRd
.polio_indicator_dictionary <- function() {
  list(
    npafp_rate = list(
      code = "npafp_rate",
      label = "NPAFP rate (per 100k under-15)",
      kind = "rate",
      formula = "annualise(npafp_cases / under15_pop * 100000)",
      numerator = "Non-polio AFP cases (age <15y), onset in period; pending included by default",
      denominator = "Under-15 population (REF_Populations analogue)",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = TRUE,
      target = 3,
      warn = 2,
      unit = "per 100,000",
      source = "cases + population",
      notes = paste(
        "Mirrors ufn_Indicator_NPAFP_RATE. Annualised via 365/(1+days),",
        "end capped at reference_date. Low confidence when pop < min_pop."
      )
    ),
    stool_adequacy_pct = list(
      code = "stool_adequacy_pct",
      label = "Stool adequacy (%)",
      kind = "percent",
      formula = "100 * adequate_afp_cases / afp_cases",
      numerator = "AFP cases (age <15y, not Not-AFP) with two adequate stools",
      denominator = "AFP cases (age <15y, not Not-AFP)",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = FALSE,
      target = 80,
      warn = 60,
      unit = "%",
      source = "cases",
      notes = paste(
        "Mirrors ufn_Indicator_NPAFP_SA. Uses the adequacy flag produced by",
        "clean_afp(); low confidence when AFP cases < min_cases."
      )
    ),
    inv_timeliness_pct = list(
      code = "inv_timeliness_pct",
      label = "Investigation timeliness (%)",
      kind = "percent",
      formula = "100 * cases(notif->invest <= k days) / afp_cases",
      numerator = "AFP cases investigated within invest_timely_days of notification",
      denominator = "AFP cases (age <15y, not Not-AFP)",
      levels = c("adm0", "adm1", "adm2"),
      requires_pop = FALSE,
      target = 80,
      warn = 60,
      unit = "%",
      source = "cases",
      notes = paste(
        "Timeliness family analogue (ST_NOTIF_INVEST_*). Uses the precomputed",
        "nottoinvest interval; k defaults to 2 days."
      )
    ),
    pct_districts_npafp_ge2 = list(
      code = "pct_districts_npafp_ge2",
      label = "Districts NPAFP >= 2 (%)",
      kind = "percent",
      formula = "100 * districts(npafp_rate >= warn) / districts_with_rate",
      numerator = "Child districts whose annualised NPAFP rate >= npafp_warn",
      denominator = "Child districts with a computable NPAFP rate",
      levels = c("adm0", "adm1"),
      requires_pop = TRUE,
      target = NA_real_,
      warn = NA_real_,
      unit = "%",
      source = "cases + population",
      notes = "Mirrors ufn_Indicator_NB_DIST_NPAFP_RATE_GE2 (sensitivity of surveillance)."
    )
  )
}
