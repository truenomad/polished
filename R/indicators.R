# =============================================================================
# Polio surveillance indicators (denominator / indicator layer)
#
# polished produces analytic case / ES / virus / SIA / lab outputs but, by
# design, leaves the computation of *rates* to downstream consumers. This module
# fills that gap with a registry-driven indicator engine that mirrors how WHO
# POLIS computes its `FACT_IndicatorValues` family (`ufn_Indicator_*`): annualised
# NPAFP rate, condition-aware stool adequacy, timeliness buckets, dose-history
# bands, environmental EV positivity, virus / SIA counts and the composite map
# classes -- each at country / province / district level, with confidence flags,
# explicit numerators / denominators and `NA` / `NOCASE` / `NOPOP` text codes.
#
# Design
#   * One unified spec per indicator in `.polio_indicator_registry()` carries
#     *both* the runtime fields (compute / confidence / categorise / source /
#     period_basis / requires / family / levels) and the documentation fields
#     (formula / numerator / denominator / target / warn / unit / polis_fn).
#     `available_indicators()` derives from the same registry, so the catalogue
#     and the engine can never drift (one source of truth).
#   * Indicator *families* are built by DRY generators (`.make_count_indicator`,
#     `.make_percent_indicator`, `.make_rate_indicator`, `.make_bucket_family`,
#     `.make_dose_band_family`, `.make_sia_count`) -- ~60 indicators from a few
#     dozen lines of config, never 60 hand-written near-duplicates.
#   * Each source table is standardised once into a canonical, level-projectable
#     frame; column names are fully parameterised (`cols = list(...)`).
#   * Skip-if-unavailable: a missing source table, a missing required column, an
#     absent population or admin universe **skips that indicator with a
#     `cli_alert_warning`** -- it never errors the whole run.
#   * Annualisation reproduces `ufn_AnnualizeIndicatorValues`:
#     `365 / (1 + days_in_period)` with the period end capped at `reference_date`;
#     applied to rates, never to year-to-date counts.
# =============================================================================

utils::globalVariables(c(
  "surveillance_type",
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
  "region",
  "report_year",
  "age",
  "class",
  "adequate",
  "rate",
  "level",
  ".hit",
  ".num",
  ".den",
  ".grp_year",
  ".parent_guid",
  ".parent_name",
  "is_afp",
  "is_afp_count",
  "is_npafp",
  "is_npafp_strict",
  "is_unclass",
  "is_wpv",
  "is_cvdpv",
  "is_vdpv",
  "ev_pos",
  "adq_code",
  "dose_total",
  "followup_present",
  "vaccine_type",
  "round_id",
  "dosages",
  "npafp_rate",
  "stool_adequacy_cond_pct",
  "family",
  "parent",
  "silent",
  ".seen",
  "site",
  "site_rate",
  "adq",
  ".is_case",
  ".is_contact",
  "pg",
  "pn",
  ".rate_ok",
  ".adq_ok",
  ".assessable"
))

# ---- public API -------------------------------------------------------------

#' Calculate polio surveillance indicators (the POLIS indicator catalogue)
#'
#' Computes the WHO POLIS surveillance-quality indicator catalogue from cleaned
#' analytic tables (the outputs of [clean_afp()], [clean_es()], [clean_virus()],
#' [clean_sia()] and [clean_human_spec()]) at country (`adm0`), province (`adm1`)
#' and district (`adm2`) level, by year. Each indicator is a registry spec; the
#' full catalogue is discoverable with [available_indicators()].
#'
#' Indicators whose source table or required columns are absent are **skipped
#' with a warning** rather than erroring, so a partial schema (e.g. `cases` +
#' `population` only) still computes every applicable indicator. Rates that need
#' a population denominator require a `population` table of *under-15* population
#' per admin unit per year.
#'
#' @param cases Cleaned AFP case data, one row per case (data.frame/tibble).
#' @param population Optional under-15 population denominators, one row per
#'   admin unit per year (`pop_guid_var`, `pop_year_var`, `pop_var`). Required
#'   for the rate / district indicators; absent -> those are skipped.
#' @param virus,es,sia,lab Optional cleaned analytic tables for the virus,
#'   environmental-surveillance, SIA and human-specimen (lab) indicator families.
#'   Absent -> those families are skipped with a warning.
#' @param admin_units Optional universe of expected district admin units (columns
#'   `adm2_guid`, `adm0_guid`, optionally `adm1_guid`, `year`). Required for the
#'   silent-districts indicator; absent -> it is skipped.
#' @param indicators Character vector of indicator codes to compute (default:
#'   the full registered catalogue, skipping any that are unavailable).
#' @param levels Admin levels to report at: any of `"adm0"`, `"adm1"`, `"adm2"`.
#' @param cols Named list overriding default source-column mappings, e.g.
#'   `list(cases = list(class = "my_class"))`. See
#'   `polished:::.polio_default_cols()` for the full default map.
#' @param class_var,age_var,year_var,onset_date_var Case columns (back-compat
#'   shortcuts that override `cols$cases`).
#' @param adm0_guid_var,adm1_guid_var,adm2_guid_var Case GUID columns per level.
#' @param adm0_name_var,adm1_name_var,adm2_name_var Case admin-name columns.
#' @param adequacy_var Case column flagging adequate stool (timing-based).
#' @param invest_interval_var Case column with notification->investigation days.
#' @param npafp_classes,pending_classes Classification values counted as NPAFP /
#'   pending (matched case-insensitively).
#' @param include_pending If `TRUE` (default) pending cases count in the NPAFP
#'   numerator (`ufn_Indicator_NPAFP_RATE`); the `_nopending` variant is separate.
#' @param afp_exclude_classes Classifications excluded from the AFP denominator
#'   for percentage indicators (default `"NOT-AFP"`).
#' @param pop_guid_var,pop_year_var,pop_var Column names in `population`.
#' @param rate_multiplier Population scale for rates (default `1e5`).
#' @param npafp_target,npafp_warn Good / warn thresholds for NPAFP rate.
#' @param adequacy_target,adequacy_warn Thresholds for percentage indicators.
#' @param invest_timely_days Max notification->investigation days that count as
#'   timely (default `2`).
#' @param survindcat_rate_cutoff Policy NPAFP-rate cutoff used by `survindcat`
#'   (default `2`; WHO uses region/endemic-specific cutoffs -- override per run).
#' @param min_pop Population below which a rate is flagged low-confidence.
#' @param min_cases Case count below which a percentage is flagged low-confidence.
#' @param reference_date Date capping the annualisation period (default today).
#' @param verbose Emit a cli progress + summary report (default `TRUE`).
#'
#' @return A named list with per-level wide tibbles (`adm0`, `adm1`, `adm2`), a
#'   tidy `long` tibble (one row per level x admin x year x indicator with
#'   `value`, `numerator`, `denominator`, `confidence`, `category`, `text_code`,
#'   `family`) and a `meta` list (indicators, skipped indicators with reasons,
#'   levels, thresholds, `reference_date`).
#' @examples
#' \donttest{
#' # available_indicators() lists the full catalogue without running anything.
#' available_indicators()
#' }
#' @export
calc_polio_indicators <- function(
  cases,
  population = NULL,
  virus = NULL,
  es = NULL,
  sia = NULL,
  lab = NULL,
  admin_units = NULL,
  indicators = NULL,
  levels = c("adm0", "adm1", "adm2"),
  cols = list(),
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
  survindcat_rate_cutoff = 2,
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
  if (is.null(indicators)) {
    indicators <- names(registry)
  }
  bad_ind <- setdiff(indicators, names(registry))
  if (length(bad_ind) > 0) {
    cli::cli_abort(c(
      "Unknown indicator{?s}: {.val {bad_ind}}",
      "i" = "See {.code available_indicators()} for the catalogue."
    ))
  }
  bad_levels <- setdiff(levels, c("adm0", "adm1", "adm2"))
  if (length(bad_levels) > 0) {
    cli::cli_abort("Invalid {.arg levels}: {.val {bad_levels}}")
  }
  if (!is.numeric(rate_multiplier) || rate_multiplier <= 0) {
    cli::cli_abort("{.arg rate_multiplier} must be a positive number.")
  }
  reference_date <- as.Date(reference_date)

  # ---- column maps (defaults <- back-compat shortcuts <- user cols) --------
  col_map <- .polio_merge_cols(
    cols,
    cases_overrides = list(
      class = class_var,
      age = age_var,
      year = year_var,
      onset_date = onset_date_var,
      adm0_guid = adm0_guid_var,
      adm1_guid = adm1_guid_var,
      adm2_guid = adm2_guid_var,
      adm0 = adm0_name_var,
      adm1 = adm1_name_var,
      adm2 = adm2_name_var,
      adequate_stool = adequacy_var,
      notify_to_invest = invest_interval_var
    )
  )

  # ---- standardise each available source -----------------------------------
  flags_cfg <- list(
    afp_exclude_classes = afp_exclude_classes,
    afp_count_exclude = unique(c(afp_exclude_classes, "NPEV")),
    npafp_classes = npafp_classes,
    pending_classes = pending_classes,
    include_pending = include_pending
  )
  std <- list(
    cases = .std_cases(cases, col_map$cases, flags_cfg),
    es = if (!is.null(es)) .std_es(es, col_map$es) else NULL,
    virus = if (!is.null(virus)) .std_virus(virus, col_map$virus) else NULL,
    sia = if (!is.null(sia)) .std_sia(sia, col_map$sia) else NULL,
    lab = if (!is.null(lab)) .std_lab(lab, col_map$lab) else NULL
  )

  n_no_year <- sum(is.na(std$cases$year))
  if (n_no_year > 0 && verbose) {
    cli::cli_alert_warning(
      "{n_no_year} case(s) have no onset year and are excluded from indicators."
    )
  }
  std$cases <- dplyr::filter(std$cases, !is.na(year))

  # ---- population & admin universe -----------------------------------------
  pop_std <- .polio_std_population(
    population,
    pop_guid_var,
    pop_year_var,
    pop_var
  )
  admin_std <- .polio_std_admin_units(admin_units, col_map$cases)
  parent_map <- .polio_parent_map(std$cases)

  cfg <- list(
    afp_exclude_classes = afp_exclude_classes,
    npafp_warn = npafp_warn,
    npafp_target = npafp_target,
    adequacy_target = adequacy_target,
    adequacy_warn = adequacy_warn,
    invest_timely_days = invest_timely_days,
    survindcat_rate_cutoff = survindcat_rate_cutoff,
    rate_multiplier = rate_multiplier,
    min_pop = min_pop,
    min_cases = min_cases,
    reference_date = reference_date,
    ref_year = as.integer(format(reference_date, "%Y")),
    pop = pop_std,
    admin_units = admin_std,
    parent_map = parent_map
  )

  # ---- decide which indicators run vs skip ---------------------------------
  plan <- .polio_plan_indicators(indicators, registry, std, cfg, levels)
  if (verbose) {
    for (s in plan$skipped) {
      cli::cli_alert_warning("Skipping {.val {s$code}}: {s$reason}.")
    }
  }
  if (length(plan$run) == 0) {
    cli::cli_abort("No indicators left to compute (all skipped).")
  }
  if (verbose) {
    cli::cli_alert_info(
      "Computing {length(plan$run)} indicator(s) for {nrow(std$cases)} case(s) at level(s) {.val {levels}}."
    )
  }

  # ---- compute base indicators (pass 1) ------------------------------------
  base_codes <- plan$run[vapply(
    plan$run,
    function(i) registry[[i]]$source != "derived",
    logical(1)
  )]
  long_rows <- list()
  for (ind in base_codes) {
    spec <- registry[[ind]]
    src <- std[[spec$source]]
    for (lv in intersect(levels, spec$levels)) {
      lf <- .polio_level_frame(src, lv)
      if (nrow(lf) == 0) next
      res <- spec$compute(lf, cfg, lv)
      if (is.null(res) || nrow(res) == 0) next
      long_rows[[paste(ind, lv, sep = "@")]] <- .polio_finalise_rows(
        res,
        spec,
        ind,
        lv,
        cfg
      )
    }
  }
  long_base <- dplyr::bind_rows(long_rows)

  # ---- compute derived indicators (pass 2, off the base long table) --------
  derived_codes <- setdiff(plan$run, base_codes)
  for (ind in derived_codes) {
    spec <- registry[[ind]]
    for (lv in intersect(levels, spec$levels)) {
      res <- spec$compute(long_base, cfg, lv)
      if (is.null(res) || nrow(res) == 0) next
      long_rows[[paste(ind, lv, sep = "@")]] <- .polio_finalise_rows(
        res,
        spec,
        ind,
        lv,
        cfg
      )
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
      family,
      value,
      numerator,
      denominator,
      confidence,
      category,
      text_code
    ) |>
    dplyr::arrange(level, family, indicator, name, year)

  # ---- assemble wide per-level tibbles -------------------------------------
  out <- list()
  for (lv in levels) {
    out[[lv]] <- .polio_wide_level(std$cases, long, lv)
  }
  out$long <- long
  out$meta <- list(
    indicators = plan$run,
    skipped = plan$skipped,
    levels = levels,
    reference_date = reference_date,
    thresholds = list(
      npafp = c(warn = npafp_warn, target = npafp_target),
      adequacy = c(warn = adequacy_warn, target = adequacy_target)
    ),
    rate_multiplier = rate_multiplier,
    include_pending = include_pending
  )

  if (verbose) .polio_print_summary(out, plan, cfg, registry)
  out
}

# ---- planning / skip-if-unavailable -----------------------------------------

#' Decide which requested indicators run and which skip (and why).
#' @keywords internal
#' @noRd
.polio_plan_indicators <- function(indicators, registry, std, cfg, levels) {
  run <- character(0)
  skipped <- list()
  skip <- function(code, reason) {
    skipped[[length(skipped) + 1]] <<- list(code = code, reason = reason)
  }
  for (ind in indicators) {
    spec <- registry[[ind]]
    if (length(intersect(levels, spec$levels)) == 0) {
      skip(
        ind,
        paste0("not reported at level(s) ", paste(levels, collapse = "/"))
      )
      next
    }
    if (spec$source == "derived") {
      if (!all(spec$requires_ind %in% indicators)) {
        skip(
          ind,
          paste0(
            "needs indicator(s) ",
            paste(spec$requires_ind, collapse = ", ")
          )
        )
        next
      }
      if (isTRUE(spec$requires_pop) && is.null(cfg$pop)) {
        skip(ind, "no population supplied")
        next
      }
      if (isTRUE(spec$requires_admin) && is.null(cfg$admin_units)) {
        skip(ind, "no admin_units universe supplied")
        next
      }
      run <- c(run, ind)
      next
    }
    src <- std[[spec$source]]
    if (is.null(src)) {
      skip(ind, paste0("no {", spec$source, "} table supplied"))
      next
    }
    miss <- setdiff(spec$requires, names(src))
    if (length(miss) > 0) {
      skip(
        ind,
        paste0(
          spec$source,
          " missing column(s): ",
          paste(miss, collapse = ", ")
        )
      )
      next
    }
    if (isTRUE(spec$requires_pop) && is.null(cfg$pop)) {
      skip(ind, "no population supplied")
      next
    }
    if (isTRUE(spec$requires_admin) && is.null(cfg$admin_units)) {
      skip(ind, "no admin_units universe supplied")
      next
    }
    run <- c(run, ind)
  }
  list(run = run, skipped = skipped)
}

#' Attach confidence / category / indicator / level / family to a result tibble.
#' @keywords internal
#' @noRd
.polio_finalise_rows <- function(res, spec, ind, lv, cfg) {
  res |>
    dplyr::mutate(
      confidence = spec$confidence(numerator, denominator, cfg),
      category = spec$categorise(value, cfg),
      indicator = ind,
      family = spec$family,
      level = lv
    )
}

# ---- column maps ------------------------------------------------------------

#' Default source-column map (canonical name -> cleaned-output column name).
#' @keywords internal
#' @noRd
.polio_default_cols <- function() {
  list(
    cases = list(
      class = "classification_all",
      age = "age_months",
      year = "year_onset",
      onset_date = "paralysis_onset_date",
      adm0_guid = "adm0_guid",
      adm1_guid = "adm1_guid",
      adm2_guid = "adm2_guid",
      adm0 = "adm0",
      adm1 = "adm1",
      adm2 = "adm2",
      region = "who_region",
      adequate_stool = "adequate_stool",
      stool1_condition = "stool1condition",
      stool2_condition = "stool2condition",
      onset_to_stool1 = "onset_to_stool1",
      onset_to_stool2 = "onset_to_stool2",
      stool1_to_stool2 = "stool1_to_stool2",
      notify_to_invest = "notify_to_invest",
      onset_date_quality = "onset_date_quality",
      onset_to_followup = "onset_to_followup",
      followup_date = "followup_date",
      investigation_date = "investigation_date",
      stool2collection_date = "stool2collection_date",
      stool_date_sent_to_lab = "stool_date_sent_to_lab",
      spec_date_received_by_nat_lab = "spec_date_received_by_nat_lab",
      surveillance_type = "surveillance_type_name",
      doses_total = "doses_total",
      doses_opv_routine = "doses_opv_routine",
      doses_opvsia = "doses_opvsia",
      doses_ipv_routine = "doses_ipv_routine",
      doses_ipvsia = "doses_ipvsia",
      doses_ipv_number = "doses_ipv_number"
    ),
    es = list(
      adm0_guid = "adm0_guid",
      adm1_guid = "adm1_guid",
      adm2_guid = "adm2_guid",
      adm0 = "adm0",
      adm1 = "adm1",
      adm2 = "adm2",
      region = "who_region",
      site = "site_name",
      class = "classification_all",
      collection_date = "collection_date",
      year = "year_collection",
      ev_positive = "ev_detect",
      result_date = "date_final_combined_result"
    ),
    virus = list(
      adm0_guid = "adm0_guid",
      adm1_guid = "adm1_guid",
      adm2_guid = "adm2_guid",
      adm0 = "adm0",
      adm1 = "adm1",
      adm2 = "adm2",
      region = "who_region",
      class = "classification_all",
      surveillance_type = "surveillance_type",
      virus_date = "virus_date",
      year = "year_onset",
      report_date = "report_date"
    ),
    sia = list(
      adm0_guid = "adm0_guid",
      adm1_guid = "adm1_guid",
      adm2_guid = "adm2_guid",
      adm0 = "adm0",
      adm1 = "adm1",
      adm2 = "adm2",
      region = "who_region",
      vaccine_type = "vaccine_type",
      round_id = "sia_sub_activity_code",
      year = "year_start",
      dosages = "calculated_dosages"
    ),
    lab = list(
      adm0_guid = "adm0_guid",
      adm1_guid = "adm1_guid",
      adm2_guid = "adm2_guid",
      adm0 = "adm0",
      adm1 = "adm1",
      adm2 = "adm2",
      region = "who_region",
      year = "year_collection",
      collect_to_lab = "collect_to_lab",
      lab_to_culture = "lab_to_culture",
      adequate = "adequate"
    )
  )
}

#' Merge default cols, back-compat `*_var` overrides and user `cols`.
#' @keywords internal
#' @noRd
.polio_merge_cols <- function(user_cols, cases_overrides) {
  base <- .polio_default_cols()
  base$cases <- utils::modifyList(base$cases, cases_overrides)
  for (src in names(user_cols)) {
    base[[src]] <- utils::modifyList(base[[src]] %||% list(), user_cols[[src]])
  }
  base
}

# ---- source standardisers ---------------------------------------------------

#' Pull admin g0/n0..g2/n2 + region from a source using its column map.
#' @keywords internal
#' @noRd
.polio_admin_frame <- function(df, m) {
  pull_chr <- function(key) {
    nm <- m[[key]]
    if (!is.null(nm) && nm %in% names(df)) as.character(df[[nm]]) else
      NA_character_
  }
  tibble::tibble(
    g0 = pull_chr("adm0_guid"),
    n0 = pull_chr("adm0"),
    g1 = pull_chr("adm1_guid"),
    n1 = pull_chr("adm1"),
    g2 = pull_chr("adm2_guid"),
    n2 = pull_chr("adm2"),
    region = toupper(trimws(pull_chr("region")))
  )
}

#' Add a coerced canonical column to `out` iff its source column exists.
#' @keywords internal
#' @noRd
.polio_add <- function(out, df, m, key, coerce) {
  nm <- m[[key]]
  if (!is.null(nm) && nm %in% names(df)) {
    out[[key]] <- coerce(df[[nm]])
  }
  out
}

.as_num <- function(x) suppressWarnings(as.numeric(x))
.as_int <- function(x) suppressWarnings(as.integer(x))
.as_date <- function(x) suppressWarnings(as.Date(x))
.as_upper <- function(x) toupper(trimws(as.character(x)))

#' Standardise cleaned AFP cases into a level-projectable analytic frame.
#' @keywords internal
#' @noRd
.std_cases <- function(df, m, flags) {
  out <- .polio_admin_frame(df, m)
  out$year <- .as_int(df[[m$year]])
  out$report_year <- out$year
  out$class <- .as_upper(df[[m$class]])
  out$age <- .as_num(df[[m$age]])

  out <- out |>
    .polio_add(df, m, "onset_date", .as_date) |>
    .polio_add(df, m, "adequate_stool", identity) |>
    .polio_add(
      df,
      m,
      "stool1_condition",
      function(x) trimws(as.character(x))
    ) |>
    .polio_add(
      df,
      m,
      "stool2_condition",
      function(x) trimws(as.character(x))
    ) |>
    .polio_add(df, m, "onset_to_stool1", .as_num) |>
    .polio_add(df, m, "onset_to_stool2", .as_num) |>
    .polio_add(df, m, "stool1_to_stool2", .as_num) |>
    .polio_add(df, m, "notify_to_invest", .as_num) |>
    .polio_add(
      df,
      m,
      "onset_date_quality",
      function(x) trimws(as.character(x))
    ) |>
    .polio_add(df, m, "onset_to_followup", .as_num) |>
    .polio_add(df, m, "surveillance_type", function(x) trimws(as.character(x)))

  # derived timeliness intervals from raw dates (only when both dates present)
  out <- .polio_interval(
    out,
    df,
    m,
    "invest_to_stool2",
    "stool2collection_date",
    "investigation_date"
  )
  out <- .polio_interval(
    out,
    df,
    m,
    "stool2_to_sentlab",
    "stool_date_sent_to_lab",
    "stool2collection_date"
  )
  out <- .polio_interval(
    out,
    df,
    m,
    "recinlab_to_stool2",
    "spec_date_received_by_nat_lab",
    "stool2collection_date"
  )

  # case-level flags shared across indicators
  out$is_afp <- .polis_afp_flag(out$class, out$age, flags$afp_exclude_classes)
  out$is_afp_count <- .polis_afp_flag(
    out$class,
    out$age,
    flags$afp_count_exclude
  )
  out$is_npafp <- .polis_npafp_flag(
    out$class,
    out$age,
    flags$npafp_classes,
    flags$pending_classes,
    TRUE
  )
  out$is_npafp_strict <- .polis_npafp_flag(
    out$class,
    out$age,
    flags$npafp_classes,
    flags$pending_classes,
    FALSE
  )
  unclass_set <- .as_upper(c(flags$pending_classes, "UNKNOWN"))
  out$is_unclass <- out$is_afp & out$class %in% unclass_set
  out$adequate <- if ("adequate_stool" %in% names(out)) {
    .polis_as_logical(out$adequate_stool)
  } else {
    NA
  }

  # condition-aware adequacy code {1,0,99,77}
  if (
    all(
      c("stool1_condition", "stool2_condition", "onset_date_quality") %in%
        names(out)
    )
  ) {
    out$adq_code <- .afp_adequacy_code(out)
  }

  # follow-up presence and dose total
  if ("onset_to_followup" %in% names(out)) {
    out$followup_present <- !is.na(out$onset_to_followup)
  }
  dose_keys <- c(
    "doses_opv_routine",
    "doses_opvsia",
    "doses_ipv_routine",
    "doses_ipvsia",
    "doses_ipv_number"
  )
  have_doses <- vapply(
    dose_keys,
    function(k) {
      !is.null(m[[k]]) && m[[k]] %in% names(df)
    },
    logical(1)
  )
  if (!is.null(m$doses_total) && m$doses_total %in% names(df)) {
    out$dose_total <- .as_num(df[[m$doses_total]])
  } else if (any(have_doses)) {
    mat <- vapply(
      dose_keys[have_doses],
      function(k) .as_num(df[[m[[k]]]]),
      numeric(nrow(df))
    )
    out$dose_total <- .afp_dose_total(mat)
  }
  out
}

#' Add a derived day-interval column `key = end - start` when both dates exist.
#' @keywords internal
#' @noRd
.polio_interval <- function(out, df, m, key, end_key, start_key) {
  en <- m[[end_key]]
  st <- m[[start_key]]
  if (!is.null(en) && !is.null(st) && en %in% names(df) && st %in% names(df)) {
    out[[key]] <- as.numeric(.as_date(df[[en]]) - .as_date(df[[st]]))
  }
  out
}

#' Standardise cleaned ES samples.
#' @keywords internal
#' @noRd
.std_es <- function(df, m) {
  out <- .polio_admin_frame(df, m)
  out$year <- .as_int(df[[m$year]])
  out$report_year <- out$year
  out$class <- .as_upper(df[[m$class]])
  out <- out |>
    .polio_add(df, m, "site", function(x) trimws(as.character(x))) |>
    .polio_add(df, m, "collection_date", .as_date)
  if (!is.null(m$ev_positive) && m$ev_positive %in% names(df)) {
    out$ev_pos <- .polis_as_logical(df[[m$ev_positive]])
  }
  out$is_wpv <- grepl("^WPV", out$class)
  out$is_cvdpv <- grepl("CVDPV", out$class)
  out$is_vdpv <- grepl("VDPV", out$class)
  # Reporting basis keys on when the lab result was reported, not when the
  # sample was collected; fall back to the collection year when the result
  # date is absent so the `_rep` indicators still resolve.
  if (!is.null(m$result_date) && m$result_date %in% names(df)) {
    result_year <- .as_int(format(.as_date(df[[m$result_date]]), "%Y"))
    out$report_year <- dplyr::coalesce(result_year, out$year)
  }
  if (
    !is.null(m$result_date) &&
      m$result_date %in% names(df) &&
      !is.null(m$collection_date) &&
      m$collection_date %in% names(df)
  ) {
    out$collect_to_result <- as.numeric(
      .as_date(df[[m$result_date]]) - .as_date(df[[m$collection_date]])
    )
  }
  out
}

#' Standardise the combined virus table.
#' @keywords internal
#' @noRd
.std_virus <- function(df, m) {
  out <- .polio_admin_frame(df, m)
  out$year <- .as_int(df[[m$year]])
  out$class <- .as_upper(df[[m$class]])
  out <- .polio_add(
    out,
    df,
    m,
    "surveillance_type",
    function(x) trimws(as.character(x))
  )
  if (!is.null(m$report_date) && m$report_date %in% names(df)) {
    out$report_year <- .as_int(format(.as_date(df[[m$report_date]]), "%Y"))
  }
  out$is_wpv <- grepl("^WPV", out$class)
  out$is_cvdpv <- grepl("CVDPV", out$class)
  out$is_vdpv <- grepl("VDPV", out$class)
  out
}

#' Standardise cleaned SIA sub-activities.
#' @keywords internal
#' @noRd
.std_sia <- function(df, m) {
  out <- .polio_admin_frame(df, m)
  out$year <- .as_int(df[[m$year]])
  out$report_year <- out$year
  out <- out |>
    .polio_add(df, m, "vaccine_type", .as_upper) |>
    .polio_add(df, m, "round_id", as.character) |>
    .polio_add(df, m, "dosages", .as_num)
  out
}

#' Standardise cleaned human specimens (lab turnaround).
#' @keywords internal
#' @noRd
.std_lab <- function(df, m) {
  out <- .polio_admin_frame(df, m)
  out$year <- .as_int(df[[m$year]])
  out$report_year <- out$year
  out <- out |>
    .polio_add(df, m, "collect_to_lab", .as_num) |>
    .polio_add(df, m, "lab_to_culture", .as_num) |>
    .polio_add(df, m, "adequate", .polis_as_logical)
  out
}

#' Standardise the population denominator table.
#' @keywords internal
#' @noRd
.polio_std_population <- function(population, guid_var, year_var, var) {
  if (is.null(population)) {
    return(NULL)
  }
  miss <- setdiff(c(guid_var, year_var, var), names(population))
  if (length(miss) > 0) {
    cli::cli_abort("Missing column{?s} in {.arg population}: {.var {miss}}")
  }
  population |>
    dplyr::transmute(
      guid = as.character(.data[[guid_var]]),
      year = as.integer(.data[[year_var]]),
      pop = as.numeric(.data[[var]])
    ) |>
    dplyr::filter(!is.na(guid), !is.na(year), !is.na(pop), pop > 0) |>
    dplyr::distinct(guid, year, .keep_all = TRUE)
}

#' Standardise the expected-districts universe for silent-district detection.
#' @keywords internal
#' @noRd
.polio_std_admin_units <- function(admin_units, m) {
  if (is.null(admin_units)) {
    return(NULL)
  }
  g2 <- m$adm2_guid
  g1 <- m$adm1_guid
  g0 <- m$adm0_guid
  if (is.null(g2) || !g2 %in% names(admin_units)) {
    cli::cli_abort(
      "{.arg admin_units} must carry an {.field adm2_guid} column."
    )
  }
  out <- tibble::tibble(g2 = as.character(admin_units[[g2]]))
  out$g0 <- if (!is.null(g0) && g0 %in% names(admin_units)) {
    as.character(admin_units[[g0]])
  } else {
    NA_character_
  }
  out$g1 <- if (!is.null(g1) && g1 %in% names(admin_units)) {
    as.character(admin_units[[g1]])
  } else {
    NA_character_
  }
  out$year <- if ("year" %in% names(admin_units)) {
    as.integer(admin_units[["year"]])
  } else {
    NA_integer_
  }
  dplyr::filter(out, !is.na(g2))
}

#' adm2 -> parent (adm0/adm1) name/guid lookup built from the case frame.
#' @keywords internal
#' @noRd
.polio_parent_map <- function(std_cases) {
  std_cases |>
    dplyr::filter(!is.na(g2)) |>
    dplyr::distinct(g2, g0, n0, g1, n1)
}

# ---- DRY indicator generators ----------------------------------------------

#' Count rows matching a predicate per admin-year (optionally summing a column).
#'
#' Powers AFP/NPAFP/virus/ES/SIA/dose count indicators. `basis = "reporting"`
#' groups on the reporting year; `"ytd"` keeps only the reference year. When
#' `value_col` is supplied the count becomes a sum of that column (e.g. dosages).
#' @keywords internal
#' @noRd
.make_count_indicator <- function(
  predicate,
  basis = "onset",
  value_col = NULL
) {
  force(predicate)
  force(basis)
  force(value_col)
  function(lf, cfg, level) {
    yr <- if (basis == "reporting") "report_year" else "year"
    lf <- dplyr::mutate(
      lf,
      .grp_year = .data[[yr]],
      .hit = predicate(dplyr::pick(dplyr::everything()), cfg)
    )
    if (basis == "ytd") lf <- dplyr::filter(lf, .grp_year == cfg$ref_year)
    lf <- dplyr::filter(lf, !is.na(.grp_year))
    summ <- if (is.null(value_col)) {
      dplyr::summarise(
        dplyr::group_by(lf, guid, name, year = .grp_year),
        numerator = sum(.hit, na.rm = TRUE),
        .groups = "drop"
      )
    } else {
      dplyr::summarise(
        dplyr::group_by(lf, guid, name, year = .grp_year),
        numerator = sum(.data[[value_col]] * .hit, na.rm = TRUE),
        .groups = "drop"
      )
    }
    summ |>
      dplyr::mutate(
        denominator = NA_real_,
        value = numerator,
        text_code = NA_character_
      ) |>
      dplyr::select(guid, name, year, value, numerator, denominator, text_code)
  }
}

#' `100 * numer / denom` per admin-year with NOCASE for an empty denominator.
#' @keywords internal
#' @noRd
.make_percent_indicator <- function(numer, denom, basis = "onset") {
  force(numer)
  force(denom)
  force(basis)
  function(lf, cfg, level) {
    yr <- if (basis == "reporting") "report_year" else "year"
    lf |>
      dplyr::mutate(
        .grp_year = .data[[yr]],
        .den = denom(dplyr::pick(dplyr::everything()), cfg),
        .num = numer(dplyr::pick(dplyr::everything()), cfg) &
          denom(dplyr::pick(dplyr::everything()), cfg)
      ) |>
      dplyr::filter(!is.na(.grp_year)) |>
      dplyr::group_by(guid, name, year = .grp_year) |>
      dplyr::summarise(
        denominator = sum(.den, na.rm = TRUE),
        numerator = sum(.num, na.rm = TRUE),
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
}

#' Annualised population rate from a numerator predicate (NPAFP-rate family).
#' @keywords internal
#' @noRd
.make_rate_indicator <- function(numer) {
  force(numer)
  function(lf, cfg, level) {
    universe <- dplyr::distinct(lf, guid, name, year)
    num <- lf |>
      dplyr::mutate(.hit = numer(dplyr::pick(dplyr::everything()), cfg)) |>
      dplyr::filter(.hit) |>
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
}

#' Build a mutually-exclusive timeliness bucket family (+ a NEG_MISS band).
#'
#' Returns a named list of percent compute closures: one per band plus a
#' negative/missing band, sharing the AFP-case denominator, so the bands sum to
#' 100% of the denominator.
#' @keywords internal
#' @noRd
.make_bucket_family <- function(prefix, interval_col, bands) {
  denom <- function(d, cfg) d$is_afp
  out <- list()
  for (b in bands) {
    lo <- b$lo
    hi <- b$hi
    numer <- local({
      lo <- lo
      hi <- hi
      function(d, cfg) {
        v <- d[[interval_col]]
        !is.na(v) & v >= lo & v <= hi
      }
    })
    out[[paste0(prefix, "_", b$suffix)]] <- .make_percent_indicator(
      numer,
      denom
    )
  }
  neg <- function(d, cfg) {
    v <- d[[interval_col]]
    is.na(v) | v < 0
  }
  out[[paste0(prefix, "_neg_miss")]] <- .make_percent_indicator(neg, denom)
  out
}

#' Build the AFP/NPAFP dose-band % family (age 6-59m, exclude unknown >=99).
#' @keywords internal
#' @noRd
.make_dose_band_family <- function(prefix, cohort_flag, bands) {
  denom <- local({
    flag <- cohort_flag
    function(d, cfg) {
      d[[flag]] &
        !is.na(d$age) &
        d$age >= 6 &
        d$age < 60 &
        !is.na(d$dose_total) &
        d$dose_total < 99
    }
  })
  out <- list()
  for (b in bands) {
    numer <- local({
      lo <- b$lo
      hi <- b$hi
      function(d, cfg)
        !is.na(d$dose_total) & d$dose_total >= lo & d$dose_total <= hi
    })
    out[[paste0(prefix, "_", b$suffix)]] <- .make_percent_indicator(
      numer,
      denom
    )
  }
  out
}

#' Count distinct SIA rounds whose vaccine type matches `vaccine_regex`.
#' @keywords internal
#' @noRd
.make_sia_count <- function(vaccine_regex) {
  force(vaccine_regex)
  function(lf, cfg, level) {
    lf |>
      dplyr::filter(grepl(vaccine_regex, vaccine_type)) |>
      dplyr::filter(!is.na(year)) |>
      dplyr::group_by(guid, name, year) |>
      dplyr::summarise(
        numerator = dplyr::n_distinct(round_id),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        denominator = NA_real_,
        value = numerator,
        text_code = NA_character_
      ) |>
      dplyr::select(guid, name, year, value, numerator, denominator, text_code)
  }
}

# ---- bespoke compute functions (the genuine one-offs) -----------------------

#' Districts (adm2) whose annualised NPAFP rate >= warn, rolled to the parent.
#' @keywords internal
#' @noRd
.calc_pct_districts_npafp_ge2 <- function(lf, cfg, level) {
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

#' Average contacts sampled per AFP case (CASE_CONTACTS analogue).
#' @keywords internal
#' @noRd
.calc_case_contacts <- function(lf, cfg, level) {
  lf |>
    dplyr::filter(!is.na(year)) |>
    dplyr::mutate(
      .is_case = .as_upper(surveillance_type) %in% c("AFP", "CASE", "1"),
      .is_contact = .as_upper(surveillance_type) %in% c("CONTACT", "7")
    ) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      numerator = sum(.is_contact, na.rm = TRUE),
      denominator = sum(.is_case, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      value = dplyr::if_else(
        denominator > 0,
        numerator / denominator,
        NA_real_
      ),
      text_code = dplyr::if_else(denominator == 0, "NOCASE", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

#' 60-day follow-up of inadequate-stool cases (FUP_INSA_CASES_PERCENT).
#' @keywords internal
#' @noRd
.calc_fup_insa <- function(lf, cfg, level) {
  lf |>
    dplyr::filter(is_afp, !is.na(year), adq_code == 0L) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      denominator = dplyr::n(),
      numerator = sum(followup_present, na.rm = TRUE),
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

#' Share of ES sites with > 49% EV positivity (SITES_WITH_ENTERO_PERCENT).
#' @keywords internal
#' @noRd
.calc_sites_with_entero <- function(lf, cfg, level) {
  lf |>
    dplyr::filter(!is.na(year), !is.na(site)) |>
    dplyr::group_by(guid, name, year, site) |>
    dplyr::summarise(
      site_rate = 100 * sum(ev_pos, na.rm = TRUE) / dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      denominator = dplyr::n(),
      numerator = sum(site_rate > 49, na.rm = TRUE),
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

#' Silent districts: expected districts with zero AFP cases, per parent.
#' @keywords internal
#' @noRd
.calc_silent_districts <- function(lf, cfg, level) {
  parent_col <- if (level == "adm0") "g0" else "g1"
  name_col <- switch(level, adm0 = "n0", "n1")
  years <- sort(unique(lf$year[!is.na(lf$year)]))
  universe <- cfg$admin_units |>
    dplyr::filter(!is.na(.data[[parent_col]])) |>
    dplyr::transmute(g2, parent = .data[[parent_col]])
  if (nrow(universe) == 0 || length(years) == 0) {
    return(NULL)
  }
  # districts with at least one AFP case, per year (the "reporting" set)
  reported <- lf |>
    dplyr::filter(is_afp, !is.na(g2), !is.na(year)) |>
    dplyr::distinct(g2, year)
  name_lookup <- lf |>
    dplyr::distinct(parent = .data[[parent_col]], name = .data[[name_col]])

  # expected districts crossed with each case-year, flagged silent if unreported
  tidyr::expand_grid(universe, year = years) |>
    dplyr::left_join(
      dplyr::mutate(reported, .seen = TRUE),
      by = dplyr::join_by(g2, year)
    ) |>
    dplyr::mutate(silent = is.na(.seen)) |>
    dplyr::group_by(guid = parent, year) |>
    dplyr::summarise(
      denominator = dplyr::n_distinct(g2),
      numerator = sum(silent, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(name_lookup, by = dplyr::join_by(guid == parent)) |>
    dplyr::mutate(
      name = dplyr::coalesce(name, guid),
      value = dplyr::if_else(
        denominator > 0,
        100 * numerator / denominator,
        NA_real_
      ),
      text_code = dplyr::if_else(denominator == 0, "NOCASE", NA_character_)
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

#' Combined surveillance standard: child districts meeting NPAFP>=warn AND
#' stool adequacy>=target, rolled to the parent level (derived).
#' @keywords internal
#' @noRd
.calc_combined_standard <- function(long, cfg, level) {
  d_rate <- long |>
    dplyr::filter(level == "adm2", indicator == "npafp_rate") |>
    dplyr::select(g2 = guid, year, npafp_rate = value)
  d_adq <- long |>
    dplyr::filter(level == "adm2", indicator == "stool_adequacy_cond_pct") |>
    dplyr::select(g2 = guid, year, adq = value)
  if (nrow(d_rate) == 0 || nrow(d_adq) == 0) {
    return(NULL)
  }
  parent_guid <- if (level == "adm0") "g0" else "g1"
  parent_name <- if (level == "adm0") "n0" else "n1"
  pm <- cfg$parent_map |>
    dplyr::transmute(g2, pg = .data[[parent_guid]], pn = .data[[parent_name]])

  d_rate |>
    dplyr::inner_join(d_adq, by = dplyr::join_by(g2, year)) |>
    dplyr::inner_join(pm, by = dplyr::join_by(g2)) |>
    dplyr::filter(!is.na(pg)) |>
    dplyr::group_by(guid = pg, name = pn, year) |>
    dplyr::summarise(
      denominator = sum(!is.na(npafp_rate) & !is.na(adq)),
      numerator = sum(
        npafp_rate >= cfg$npafp_warn & adq >= cfg$adequacy_target,
        na.rm = TRUE
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

#' SURVINDCAT composite map class (0/1/2/4) from NPAFP rate + cond. adequacy.
#'
#' Policy-overridable: `cfg$survindcat_rate_cutoff` is the NPAFP-rate threshold
#' (WHO uses region / endemic-specific cutoffs). Classes: 4 = both met,
#' 2 = exactly one, 1 = neither (but assessable), 0 = not assessable.
#' @keywords internal
#' @noRd
.calc_survindcat <- function(long, cfg, level) {
  d_rate <- long |>
    dplyr::filter(level == !!level, indicator == "npafp_rate") |>
    dplyr::select(guid, name, year, npafp_rate = value)
  d_adq <- long |>
    dplyr::filter(level == !!level, indicator == "stool_adequacy_cond_pct") |>
    dplyr::select(guid, name, year, adq = value)
  if (nrow(d_rate) == 0) {
    return(NULL)
  }
  d_rate |>
    dplyr::full_join(d_adq, by = dplyr::join_by(guid, name, year)) |>
    dplyr::mutate(
      .rate_ok = !is.na(npafp_rate) & npafp_rate >= cfg$survindcat_rate_cutoff,
      .adq_ok = !is.na(adq) & adq >= cfg$adequacy_target,
      .assessable = !is.na(npafp_rate) | !is.na(adq),
      value = dplyr::case_when(
        !.assessable ~ 0,
        .rate_ok & .adq_ok ~ 4,
        .rate_ok | .adq_ok ~ 2,
        TRUE ~ 1
      ),
      numerator = NA_real_,
      denominator = NA_real_,
      text_code = dplyr::if_else(.assessable, NA_character_, "NA")
    ) |>
    dplyr::select(guid, name, year, value, numerator, denominator, text_code)
}

# ---- condition-aware stool adequacy -----------------------------------------

#' Port of generate_ad_final_col(): per-case adequacy code in {1,0,99,77}.
#' @keywords internal
#' @noRd
.afp_adequacy_code <- function(d) {
  s1 <- d$stool1_condition
  s2 <- d$stool2_condition
  ots1 <- d$onset_to_stool1
  ots2 <- d$onset_to_stool2
  s1s2 <- d$stool1_to_stool2
  dplyr::case_when(
    !is.na(d$onset_date_quality) & d$onset_date_quality != "Good" ~ 77L,
    (!is.na(ots1) & (ots1 > 13 | ots1 < 0)) |
      is.na(s1s2) |
      (!is.na(ots2) & (ots2 > 14 | ots2 < 1)) |
      (!is.na(s1s2) & s1s2 < 1) |
      (!is.na(s1) & s1 == "Poor") |
      (!is.na(s2) & s2 == "Poor") ~
      0L,
    !is.na(ots1) &
      ots1 <= 13 &
      ots1 >= 0 &
      !is.na(ots2) &
      ots2 <= 14 &
      ots2 >= 1 &
      !is.na(s1s2) &
      s1s2 >= 1 &
      !is.na(s1) &
      s1 == "Good" &
      !is.na(s2) &
      s2 == "Good" ~
      1L,
    is.na(s1) | is.na(s2) | s1 == "Unknown" | s2 == "Unknown" ~ 99L,
    TRUE ~ NA_integer_
  )
}

#' Sum dose components, counting only values < 99; all-99 -> 999, all-NA -> NA.
#' @keywords internal
#' @noRd
.afp_dose_total <- function(mat) {
  mat <- as.matrix(mat)
  total <- rowSums(mat * (mat < 99), na.rm = TRUE)
  total[rowSums(mat == 99, na.rm = TRUE) == ncol(mat)] <- 999
  total[rowSums(is.na(mat)) == ncol(mat)] <- NA_real_
  total
}

# ---- internal engine helpers -----------------------------------------------

#' Project a standardised source frame onto one admin level.
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
#' @keywords internal
#' @noRd
.polio_annualise_factor <- function(year, reference_date) {
  start <- as.Date(sprintf("%d-01-01", year))
  end <- as.Date(sprintf("%d-12-31", year))
  eff_end <- pmin(end, reference_date)
  days <- as.integer(eff_end - start)
  dplyr::if_else(days < 0, NA_real_, 365 / (1 + days))
}

#' AFP denominator flag: under-15 and not an excluded classification.
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
    s %in% c("1", "yes", "y", "true", "t", "adequate", "good") ~ TRUE,
    s %in% c("0", "no", "n", "false", "f", "inadequate", "poor") ~ FALSE,
    TRUE ~ as.logical(NA)
  )
}

#' Build the wide per-level tibble (one row per admin-year, value per indicator).
#' @keywords internal
#' @noRd
.polio_wide_level <- function(std_cases, long, level) {
  gv <- switch(level, adm0 = "g0", adm1 = "g1", adm2 = "g2")
  nv <- switch(level, adm0 = "n0", adm1 = "n1", adm2 = "n2")

  counts <- std_cases |>
    dplyr::mutate(guid = .data[[gv]], name = .data[[nv]]) |>
    dplyr::filter(!is.na(guid)) |>
    dplyr::group_by(guid, name, year) |>
    dplyr::summarise(
      afp_cases = sum(is_afp, na.rm = TRUE),
      npafp_cases = sum(is_npafp, na.rm = TRUE),
      .groups = "drop"
    )

  lvl_long <- dplyr::filter(long, level == !!level)
  # universe is the AFP-case admin-years plus any extra units a non-case source
  # (ES / virus / SIA) contributed at this level, so every computed value lands.
  extra <- lvl_long |>
    dplyr::distinct(guid, name, year) |>
    dplyr::anti_join(counts, by = dplyr::join_by(guid, name, year))
  base <- dplyr::bind_rows(counts, extra)

  vals <- lvl_long |>
    dplyr::select(guid, name, year, indicator, value) |>
    tidyr::pivot_wider(names_from = indicator, values_from = value)

  pop_col <- lvl_long |>
    dplyr::filter(indicator == "npafp_rate") |>
    dplyr::select(guid, year, under15_pop = denominator)

  base |>
    dplyr::left_join(vals, by = dplyr::join_by(guid, name, year)) |>
    dplyr::left_join(pop_col, by = dplyr::join_by(guid, year)) |>
    dplyr::arrange(name, year)
}

# ---- CLI summary ------------------------------------------------------------

#' @keywords internal
#' @noRd
.polio_print_summary <- function(out, plan, cfg, registry) {
  cli::cli_rule()
  cli::cli_h2("Polio Indicator Summary")

  long <- out$long
  latest <- suppressWarnings(max(long$year, na.rm = TRUE))
  cli::cli_h3("Coverage by family ({latest})")
  cov <- long |>
    dplyr::filter(year == latest) |>
    dplyr::group_by(family, indicator) |>
    dplyr::summarise(
      units = dplyr::n(),
      valued = sum(!is.na(value)),
      low_conf = sum(confidence == 0, na.rm = TRUE),
      .groups = "drop"
    )
  for (fam in unique(cov$family)) {
    cli::cli_text("{.strong {fam}}")
    fc <- dplyr::filter(cov, family == fam)
    for (i in seq_len(nrow(fc))) {
      r <- fc[i, ]
      cli::cli_text(
        "  {.field {r$indicator}}: {r$valued}/{r$units} valued ({r$low_conf} low-confidence)"
      )
    }
  }

  if (length(plan$skipped) > 0) {
    cli::cli_h3("Skipped (missing source / columns)")
    for (s in plan$skipped) {
      cli::cli_text("{.field {s$code}}: {s$reason}")
    }
  }
  cli::cli_rule()
  invisible(out)
}

# ---- indicator registry (single source of truth) ---------------------------

#' Indicator registry: one unified spec per indicator (internal).
#'
#' Each spec carries the runtime fields (`compute`, `confidence`, `categorise`,
#' `source`, `period_basis`, `requires`, `requires_pop`, `requires_admin`,
#' `requires_ind`, `family`, `levels`, `kind`) **and** the documentation fields
#' surfaced by [available_indicators()] (`label`, `formula`, `numerator`,
#' `denominator`, `target`, `warn`, `unit`, `polis_fn`, `notes`). The dictionary
#' and the engine therefore share one definition and can never drift.
#' @keywords internal
#' @noRd
.polio_indicator_registry <- function() {
  all_lv <- c("adm0", "adm1", "adm2")
  conf_rate <- function(num, den, cfg)
    dplyr::if_else(!is.na(den) & den >= cfg$min_pop, 1, 0)
  conf_pct <- function(num, den, cfg)
    dplyr::if_else(!is.na(den) & den >= cfg$min_cases, 1, 0)
  conf_d10 <- function(num, den, cfg)
    dplyr::if_else(!is.na(den) & den >= 10, 1, 0)
  conf_one <- function(num, den, cfg) rep(1, length(num))
  cat_none <- function(value, cfg) rep(NA_character_, length(value))
  cat_higher <- function(target_key, warn_key) {
    function(value, cfg) {
      dplyr::case_when(
        is.na(value) ~ NA_character_,
        value >= cfg[[target_key]] ~ "green",
        value >= cfg[[warn_key]] ~ "orange",
        TRUE ~ "red"
      )
    }
  }
  cat_adequacy <- cat_higher("adequacy_target", "adequacy_warn")

  # predicates (functions of a picked data frame + cfg)
  p_afp_count <- function(d, cfg) d$is_afp_count
  p_npafp <- function(d, cfg) d$is_npafp
  p_npafp_strict <- function(d, cfg) d$is_npafp_strict
  p_afp_denom <- function(d, cfg) d$is_afp
  p_unclass <- function(d, cfg) d$is_unclass
  p_invest_timely <- function(d, cfg) {
    !is.na(d$notify_to_invest) &
      d$notify_to_invest >= 0 &
      d$notify_to_invest <= cfg$invest_timely_days
  }
  p_adequate_timing <- function(d, cfg) d$adequate %in% TRUE
  p_adq_cond <- function(d, cfg)
    d$is_afp & !is.na(d$adq_code) & d$adq_code == 1L
  p_adq_assessable <- function(d, cfg)
    d$is_afp & !is.na(d$adq_code) & d$adq_code %in% c(0L, 1L)
  p_es_all <- function(d, cfg) rep(TRUE, nrow(d))
  p_es_wpv <- function(d, cfg) d$is_wpv
  p_es_cvdpv <- function(d, cfg) d$is_cvdpv
  p_ev_pos <- function(d, cfg) d$ev_pos %in% TRUE
  p_es35 <- function(d, cfg)
    !is.na(d$collect_to_result) & d$collect_to_result <= 35
  p_vir_wpv <- function(d, cfg) d$is_wpv
  p_vir_cvdpv <- function(d, cfg) d$is_cvdpv
  p_vir_vdpv <- function(d, cfg) d$is_vdpv
  p_lab_cellc <- function(d, cfg)
    !is.na(d$lab_to_culture) & d$lab_to_culture <= 14
  p_lab_denom <- function(d, cfg) !is.na(d$lab_to_culture)
  p_dose_all <- function(d, cfg) rep(TRUE, nrow(d))
  p_dose_unknown <- function(d, cfg) {
    d$is_afp &
      !is.na(d$age) &
      d$age >= 6 &
      d$age < 60 &
      (!is.na(d$dose_total) & d$dose_total >= 99)
  }

  spec <- function(
    label,
    family,
    kind,
    source,
    compute,
    confidence,
    categorise,
    levels = all_lv,
    requires = character(0),
    requires_pop = FALSE,
    requires_admin = FALSE,
    requires_ind = character(0),
    period_basis = "onset",
    formula = "",
    numerator = "",
    denominator = "",
    target = NA_real_,
    warn = NA_real_,
    unit = "",
    polis_fn = "",
    notes = ""
  ) {
    # force all args: specs are built in loops, so unforced promises would
    # otherwise collapse to the final loop iteration's value (lazy eval).
    force(label)
    force(family)
    force(kind)
    force(source)
    force(compute)
    force(confidence)
    force(categorise)
    force(levels)
    force(requires)
    force(requires_pop)
    force(requires_admin)
    force(requires_ind)
    force(period_basis)
    force(formula)
    force(numerator)
    force(denominator)
    force(target)
    force(warn)
    force(unit)
    force(polis_fn)
    force(notes)
    list(
      label = label,
      family = family,
      kind = kind,
      source = source,
      compute = compute,
      confidence = confidence,
      categorise = categorise,
      levels = levels,
      requires = requires,
      requires_pop = requires_pop,
      requires_admin = requires_admin,
      requires_ind = requires_ind,
      period_basis = period_basis,
      formula = formula,
      numerator = numerator,
      denominator = denominator,
      target = target,
      warn = warn,
      unit = unit,
      polis_fn = polis_fn,
      notes = notes
    )
  }

  reg <- list()
  add <- function(code, s) reg[[code]] <<- s

  # ---- Family A: AFP / NPAFP counts & rates -------------------------------
  add(
    "afp_count",
    spec(
      "AFP count",
      "AFP",
      "count",
      "cases",
      .make_count_indicator(p_afp_count),
      conf_one,
      cat_none,
      formula = "COUNT(age 0-179m, class not in {Not-AFP, NPEV})",
      numerator = "AFP cases",
      denominator = "(count)",
      unit = "cases",
      polis_fn = "ufn_Indicator_AFP_COUNT"
    )
  )
  add(
    "npafp_count",
    spec(
      "NPAFP count",
      "AFP",
      "count",
      "cases",
      .make_count_indicator(p_npafp_strict),
      conf_one,
      cat_none,
      levels = c("adm0", "adm1", "adm2"),
      formula = "COUNT(NPAFP, excl. pending)",
      numerator = "NPAFP cases",
      denominator = "(count)",
      unit = "cases",
      polis_fn = "ufn_Indicator_NPAFP_COUNT"
    )
  )
  add(
    "npafp_rate",
    spec(
      "NPAFP rate (per 100k under-15)",
      "AFP",
      "rate",
      "cases",
      .make_rate_indicator(p_npafp),
      conf_rate,
      cat_higher("npafp_target", "npafp_warn"),
      requires_pop = TRUE,
      formula = "annualise(npafp_cases / under15_pop * 100000)",
      numerator = "Non-polio AFP cases (pending included)",
      denominator = "Under-15 population",
      target = 3,
      warn = 2,
      unit = "per 100,000",
      polis_fn = "ufn_Indicator_NPAFP_RATE",
      notes = "Annualised via 365/(1+days), end capped at reference_date."
    )
  )
  add(
    "npafp_rate_nopending",
    spec(
      "NPAFP rate, no pending (per 100k)",
      "AFP",
      "rate",
      "cases",
      .make_rate_indicator(p_npafp_strict),
      conf_rate,
      cat_higher("npafp_target", "npafp_warn"),
      requires_pop = TRUE,
      formula = "annualise(npafp_cases[excl pending] / under15_pop * 100000)",
      numerator = "Non-polio AFP cases (pending excluded)",
      denominator = "Under-15 population",
      target = 3,
      warn = 2,
      unit = "per 100,000",
      polis_fn = "ufn_Indicator_NPAFP_RATE_NOPENDING"
    )
  )

  # ---- Family A2: stool adequacy ------------------------------------------
  add(
    "stool_adequacy_pct",
    spec(
      "Stool adequacy, timing (%)",
      "Stool",
      "percent",
      "cases",
      .make_percent_indicator(p_adequate_timing, p_afp_denom),
      conf_pct,
      cat_adequacy,
      requires = "adequate_stool",
      formula = "100 * adequate(timing) / afp_cases",
      numerator = "AFP cases with two adequate stools (timing flag)",
      denominator = "AFP cases",
      target = 80,
      warn = 60,
      unit = "%",
      polis_fn = "ufn_Indicator_NPAFP_SA"
    )
  )
  add(
    "stool_adequacy_cond_pct",
    spec(
      "Stool adequacy, condition-aware (%)",
      "Stool",
      "percent",
      "cases",
      .make_percent_indicator(p_adq_cond, p_adq_assessable),
      conf_pct,
      cat_adequacy,
      requires = c(
        "stool1_condition",
        "stool2_condition",
        "onset_date_quality"
      ),
      formula = "100 * adequate(timing & not poor) / assessable",
      numerator = "AFP cases adequate by timing and condition (code 1)",
      denominator = "Cases coded adequate or inadequate (1 or 0)",
      target = 80,
      warn = 60,
      unit = "%",
      polis_fn = "ufn_Indicator_NPAFP_SA_WithStoolCond",
      notes = "Condition-aware adequacy used by survindcat."
    )
  )
  add(
    "stool_adequacy_good_pct",
    spec(
      "Stool adequacy, both Good (%)",
      "Stool",
      "percent",
      "cases",
      .make_percent_indicator(p_adq_cond, function(d, cfg) {
        d$is_afp & !is.na(d$adq_code) & d$adq_code %in% c(0L, 1L, 99L)
      }),
      conf_pct,
      cat_adequacy,
      requires = c(
        "stool1_condition",
        "stool2_condition",
        "onset_date_quality"
      ),
      formula = "100 * adequate(both Good) / assessable(incl missing)",
      numerator = "AFP cases adequate with both conditions Good",
      denominator = "Assessable cases including missing condition",
      target = 80,
      warn = 60,
      unit = "%",
      polis_fn = "ufn_Indicator_NPAFP_SA_GoodStoolCond"
    )
  )

  # ---- Family A3: dose history --------------------------------------------
  dose_bands_3 <- list(
    list(suffix = "0", lo = 0, hi = 0),
    list(suffix = "1_2", lo = 1, hi = 2),
    list(suffix = "3plus", lo = 3, hi = 98)
  )
  afp_dose <- .make_dose_band_family("afp_dose", "is_afp", dose_bands_3)
  npafp_dose <- .make_dose_band_family(
    "npafp_dose",
    "is_npafp_strict",
    c(dose_bands_3, list(list(suffix = "0_2", lo = 0, hi = 2)))
  )
  dose_meta <- function(code, lab, num)
    spec(
      lab,
      "Dose",
      "percent",
      "cases",
      afp_dose[[code]] %||% npafp_dose[[code]],
      conf_pct,
      cat_none,
      requires = "dose_total",
      formula = "100 * cohort(dose band) / cohort(6-59m, known doses)",
      numerator = num,
      denominator = "AFP/NPAFP cohort age 6-59m, doses < 99",
      unit = "%",
      polis_fn = "ufn_Indicator_*_DOSE_*"
    )
  add(
    "afp_dose_0",
    dose_meta("afp_dose_0", "AFP zero-dose (%)", "AFP cases with 0 doses")
  )
  add(
    "afp_dose_1_2",
    dose_meta("afp_dose_1_2", "AFP 1-2 doses (%)", "AFP cases with 1-2 doses")
  )
  add(
    "afp_dose_3plus",
    dose_meta("afp_dose_3plus", "AFP 3+ doses (%)", "AFP cases with 3+ doses")
  )
  add(
    "npafp_dose_0",
    dose_meta("npafp_dose_0", "NPAFP zero-dose (%)", "NPAFP cases with 0 doses")
  )
  add(
    "npafp_dose_1_2",
    dose_meta(
      "npafp_dose_1_2",
      "NPAFP 1-2 doses (%)",
      "NPAFP cases with 1-2 doses"
    )
  )
  add(
    "npafp_dose_0_2",
    dose_meta(
      "npafp_dose_0_2",
      "NPAFP 0-2 doses (%)",
      "NPAFP cases with 0-2 doses"
    )
  )
  add(
    "npafp_dose_3plus",
    dose_meta(
      "npafp_dose_3plus",
      "NPAFP 3+ doses (%)",
      "NPAFP cases with 3+ doses"
    )
  )
  add(
    "missing_opv_doses",
    spec(
      "Missing OPV dose history (count)",
      "Dose",
      "count",
      "cases",
      .make_count_indicator(p_dose_unknown),
      conf_one,
      cat_none,
      requires = "dose_total",
      formula = "COUNT(cohort with unknown dose >= 99)",
      numerator = "AFP cases age 6-59m with unknown dose history",
      denominator = "(count)",
      unit = "cases",
      polis_fn = "ufn_Indicator_Missing_OPV_Doses"
    )
  )

  # ---- Family A4: case quality --------------------------------------------
  add(
    "unclass_cases_pct",
    spec(
      "Unclassified cases (%)",
      "AFP",
      "percent",
      "cases",
      .make_percent_indicator(p_unclass, p_afp_denom),
      conf_pct,
      cat_none,
      formula = "100 * unclassified / all_afp",
      numerator = "AFP cases still pending/unknown",
      denominator = "AFP cases",
      unit = "%",
      polis_fn = "ufn_Indicator_UNCLASS_CASES_PERCENT"
    )
  )
  add(
    "fup_insa_cases_pct",
    spec(
      "60-day follow-up of inadequate cases (%)",
      "AFP",
      "percent",
      "cases",
      .calc_fup_insa,
      conf_d10,
      cat_none,
      requires = c("adq_code", "followup_present"),
      formula = "100 * followed-up / inadequate-stool cases",
      numerator = "Inadequate-stool cases with a 60-day follow-up",
      denominator = "Inadequate-stool AFP cases",
      unit = "%",
      polis_fn = "ufn_Indicator_FUP_INSA_CASES_PERCENT",
      notes = "Confidence 0 when denominator < 10."
    )
  )
  add(
    "case_contacts_avg",
    spec(
      "Contacts sampled per case (avg)",
      "AFP",
      "ratio",
      "cases",
      .calc_case_contacts,
      conf_pct,
      cat_none,
      requires = "surveillance_type",
      formula = "SUM(contacts) / SUM(cases)",
      numerator = "Contact specimens",
      denominator = "AFP cases",
      unit = "contacts/case",
      polis_fn = "ufn_Indicator_CASE_CONTACTS"
    )
  )

  # ---- Family B: timeliness buckets ---------------------------------------
  add(
    "inv_timeliness_pct",
    spec(
      "Investigation timeliness (%)",
      "Timeliness",
      "percent",
      "cases",
      .make_percent_indicator(p_invest_timely, p_afp_denom),
      conf_pct,
      cat_adequacy,
      requires = "notify_to_invest",
      formula = "100 * cases(notif->invest <= k) / afp_cases",
      numerator = "AFP cases investigated within invest_timely_days",
      denominator = "AFP cases",
      target = 80,
      warn = 60,
      unit = "%",
      polis_fn = "ufn_Indicator_ST_NOTIF_INVEST"
    )
  )
  bands_2_10 <- list(
    list(suffix = "0_2", lo = 0, hi = 2),
    list(suffix = "3_10", lo = 3, hi = 10),
    list(suffix = "gt10", lo = 11, hi = Inf)
  )
  bands_3_6 <- list(
    list(suffix = "0_3", lo = 0, hi = 3),
    list(suffix = "4_6", lo = 4, hi = 6),
    list(suffix = "gt6", lo = 7, hi = Inf)
  )
  add_bucket_family <- function(prefix, interval_col, bands, req, polis) {
    fns <- .make_bucket_family(prefix, interval_col, bands)
    for (code in names(fns)) {
      add(
        code,
        spec(
          paste0(code, " (%)"),
          "Timeliness",
          "percent",
          "cases",
          fns[[code]],
          conf_pct,
          cat_none,
          requires = req,
          formula = paste0(
            "100 * cases(",
            interval_col,
            " in band) / afp_cases"
          ),
          numerator = "AFP cases in band",
          denominator = "AFP cases",
          unit = "%",
          polis_fn = polis
        )
      )
    }
  }
  add_bucket_family(
    "st_notif_invest",
    "notify_to_invest",
    bands_2_10,
    "notify_to_invest",
    "ufn_Indicator_ST_NOTIF_INVEST_*"
  )
  add_bucket_family(
    "st_invest_stool2",
    "invest_to_stool2",
    bands_2_10,
    "invest_to_stool2",
    "ufn_Indicator_ST_INVEST_STC2_*"
  )
  add_bucket_family(
    "st2_sent_tolab",
    "stool2_to_sentlab",
    bands_3_6,
    "stool2_to_sentlab",
    "ufn_Indicator_ST2_SENT_TOLAB_*"
  )
  add_bucket_family(
    "st2_rec_inlab",
    "recinlab_to_stool2",
    bands_3_6,
    "recinlab_to_stool2",
    "ufn_Indicator_ST2_REC_INLAB_*"
  )
  add(
    "cellc_perf_bylab",
    spec(
      "Cell-culture performance (%)",
      "Lab",
      "percent",
      "lab",
      .make_percent_indicator(p_lab_cellc, p_lab_denom),
      conf_pct,
      cat_adequacy,
      levels = "adm0",
      requires = "lab_to_culture",
      formula = "100 * results within 14d of receipt / specimens with a result",
      numerator = "Specimens with culture result <= 14d of receipt",
      denominator = "Specimens with a culture result",
      target = 80,
      warn = 60,
      unit = "%",
      polis_fn = "ufn_Indicator_CELLC_PERF_BYLAB"
    )
  )

  # ---- Family C: environmental --------------------------------------------
  add(
    "env_count",
    spec(
      "ES sample count",
      "ES",
      "count",
      "es",
      .make_count_indicator(p_es_all),
      conf_one,
      cat_none,
      requires = "collection_date",
      formula = "COUNT(ES samples)",
      numerator = "ES samples",
      denominator = "(count)",
      unit = "samples",
      polis_fn = "ufn_Indicator_ENV_COUNT"
    )
  )
  add(
    "env_wpv_count",
    spec(
      "ES WPV count",
      "ES",
      "count",
      "es",
      .make_count_indicator(p_es_wpv),
      conf_one,
      cat_none,
      formula = "COUNT(ES WPV+)",
      numerator = "WPV-positive ES samples",
      denominator = "(count)",
      unit = "samples",
      polis_fn = "ufn_Indicator_ENV_WPV_COUNT"
    )
  )
  add(
    "env_cvdpv_count",
    spec(
      "ES cVDPV count",
      "ES",
      "count",
      "es",
      .make_count_indicator(p_es_cvdpv),
      conf_one,
      cat_none,
      formula = "COUNT(ES cVDPV+)",
      numerator = "cVDPV-positive ES samples",
      denominator = "(count)",
      unit = "samples",
      polis_fn = "ufn_Indicator_ENV_CVDPV_COUNT"
    )
  )
  add(
    "env_wpv_count_rep",
    spec(
      "ES WPV count (reporting)",
      "ES",
      "count",
      "es",
      .make_count_indicator(p_es_wpv, basis = "reporting"),
      conf_one,
      cat_none,
      period_basis = "reporting",
      requires = "report_year",
      formula = "COUNT(ES WPV+) on reporting date",
      numerator = "WPV-positive ES samples (reporting basis)",
      denominator = "(count)",
      unit = "samples",
      polis_fn = "ufn_Indicator_ENV_WPV_COUNT_REPORTING"
    )
  )
  add(
    "env_cvdpv_count_rep",
    spec(
      "ES cVDPV count (reporting)",
      "ES",
      "count",
      "es",
      .make_count_indicator(p_es_cvdpv, basis = "reporting"),
      conf_one,
      cat_none,
      period_basis = "reporting",
      requires = "report_year",
      formula = "COUNT(ES cVDPV+) on reporting date",
      numerator = "cVDPV-positive ES samples (reporting basis)",
      denominator = "(count)",
      unit = "samples",
      polis_fn = "ufn_Indicator_ENV_CVDPV_COUNT_REPORTING"
    )
  )
  add(
    "ev_rate",
    spec(
      "EV detection rate (%)",
      "ES",
      "percent",
      "es",
      .make_percent_indicator(p_ev_pos, p_es_all),
      conf_pct,
      cat_none,
      requires = "ev_pos",
      formula = "100 * ev_positive / samples",
      numerator = "EV-positive samples",
      denominator = "ES samples",
      target = 50,
      warn = 50,
      unit = "%",
      polis_fn = "ufn_Indicator_SITES_WITH_ENTERO_PERCENT"
    )
  )
  add(
    "sites_with_entero_pct",
    spec(
      "Sites with EV > 49% (%)",
      "ES",
      "percent",
      "es",
      .calc_sites_with_entero,
      conf_pct,
      cat_none,
      levels = "adm0",
      requires = c("site", "ev_pos"),
      formula = "100 * sites(EV rate > 49%) / sites",
      numerator = "Sites with > 49% EV positivity",
      denominator = "ES sites",
      unit = "%",
      polis_fn = "ufn_Indicator_SITES_WITH_ENTERO_PERCENT"
    )
  )
  add(
    "case_es_35days_pct",
    spec(
      "ES result within 35 days (%)",
      "ES",
      "percent",
      "es",
      .make_percent_indicator(p_es35, p_es_all),
      conf_pct,
      cat_none,
      levels = "adm0",
      requires = "collect_to_result",
      formula = "100 * results within 35d of collection / samples",
      numerator = "Samples with result <= 35d",
      denominator = "ES samples",
      unit = "%",
      polis_fn = "ufn_Indicator_CASE_ES_35DAYS_PERCENT"
    )
  )

  # ---- Family D: virus counts ---------------------------------------------
  add(
    "wpv_count",
    spec(
      "WPV count",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_wpv),
      conf_one,
      cat_none,
      formula = "COUNT(WPV)",
      numerator = "WPV detections",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_WPV_COUNT"
    )
  )
  add(
    "wpv_count_rep",
    spec(
      "WPV count (reporting)",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_wpv, basis = "reporting"),
      conf_one,
      cat_none,
      period_basis = "reporting",
      requires = "report_year",
      formula = "COUNT(WPV) on reporting date",
      numerator = "WPV detections (reporting)",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_WPV_COUNT_REPORTING"
    )
  )
  add(
    "wpv_count_ytd",
    spec(
      "WPV count (YTD)",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_wpv, basis = "ytd"),
      conf_one,
      cat_none,
      period_basis = "ytd",
      formula = "COUNT(WPV) Jan->reference_date, no annualisation",
      numerator = "WPV detections year-to-date",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_WPV_YTD_COUNT"
    )
  )
  add(
    "cvdpv_count",
    spec(
      "cVDPV count",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_cvdpv),
      conf_one,
      cat_none,
      formula = "COUNT(cVDPV)",
      numerator = "cVDPV detections",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_CVDPV_COUNT"
    )
  )
  add(
    "cvdpv_count_rep",
    spec(
      "cVDPV count (reporting)",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_cvdpv, basis = "reporting"),
      conf_one,
      cat_none,
      period_basis = "reporting",
      requires = "report_year",
      formula = "COUNT(cVDPV) on reporting date",
      numerator = "cVDPV detections (reporting)",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_CVDPV_COUNT_REPORTING"
    )
  )
  add(
    "cvdpv_count_ytd",
    spec(
      "cVDPV count (YTD)",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_cvdpv, basis = "ytd"),
      conf_one,
      cat_none,
      period_basis = "ytd",
      formula = "COUNT(cVDPV) Jan->reference_date",
      numerator = "cVDPV detections year-to-date",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_CVDPV_YTD_COUNT"
    )
  )
  add(
    "vdpv_count",
    spec(
      "VDPV count",
      "Virus",
      "count",
      "virus",
      .make_count_indicator(p_vir_vdpv),
      conf_one,
      cat_none,
      formula = "COUNT(VDPV)",
      numerator = "VDPV detections",
      denominator = "(count)",
      unit = "viruses",
      polis_fn = "ufn_Indicator_VDPV_COUNT"
    )
  )

  # ---- Family E: SIA ------------------------------------------------------
  sia_meta <- function(code, lab, regex, polis)
    spec(
      lab,
      "SIA",
      "count",
      "sia",
      .make_sia_count(regex),
      conf_one,
      cat_none,
      requires = c("vaccine_type", "round_id"),
      formula = paste0("COUNT(distinct rounds, vaccine ~ ", regex, ")"),
      numerator = "Distinct SIA rounds",
      denominator = "(count)",
      unit = "rounds",
      polis_fn = polis
    )
  add(
    "sia_opvtot",
    sia_meta(
      "sia_opvtot",
      "SIA OPV rounds",
      "OPV",
      "ufn_Indicator_SIA_OPVTOT_COUNT"
    )
  )
  add(
    "sia_bopv",
    sia_meta(
      "sia_bopv",
      "SIA bOPV rounds",
      "BOPV",
      "ufn_Indicator_SIA_BOPV_COUNT"
    )
  )
  add(
    "sia_topv",
    sia_meta(
      "sia_topv",
      "SIA tOPV rounds",
      "TOPV",
      "ufn_Indicator_SIA_TOPV_COUNT"
    )
  )
  add(
    "sia_mopv",
    sia_meta(
      "sia_mopv",
      "SIA mOPV rounds",
      "MOPV",
      "ufn_Indicator_SIA_MOPV_COUNT"
    )
  )
  add(
    "sia_nopv2",
    sia_meta(
      "sia_nopv2",
      "SIA nOPV2 rounds",
      "NOPV",
      "ufn_Indicator_SIA_NOPV2_COUNT"
    )
  )
  add(
    "doses_count",
    spec(
      "SIA doses administered",
      "SIA",
      "count",
      "sia",
      .make_count_indicator(p_dose_all, value_col = "dosages"),
      conf_one,
      cat_none,
      requires = "dosages",
      formula = "SUM(dosages)",
      numerator = "Doses administered",
      denominator = "(count)",
      unit = "doses",
      polis_fn = "ufn_Indicator_DOSES_COUNT"
    )
  )
  add(
    "sia_lastcase_count",
    spec(
      "SIA rounds since last case",
      "SIA",
      "count",
      "sia",
      .make_sia_count("OPV"),
      conf_one,
      cat_none,
      levels = c("adm1", "adm2"),
      requires = c("vaccine_type", "round_id", "last_case_date"),
      formula = "COUNT(rounds since last case)",
      numerator = "SIA rounds",
      denominator = "(count)",
      unit = "rounds",
      polis_fn = "ufn_Indicator_SIA_LASTCASE_COUNT",
      notes = "Requires a last-case date feed; registered but skipped until available."
    )
  )

  # ---- Family F: composite (derived) --------------------------------------
  add(
    "pct_districts_npafp_ge2",
    spec(
      "Districts NPAFP >= 2 (%)",
      "Composite",
      "percent",
      "cases",
      .calc_pct_districts_npafp_ge2,
      function(num, den, cfg) dplyr::if_else(!is.na(den) & den > 0, 1, 0),
      cat_none,
      levels = c("adm0", "adm1"),
      requires_pop = TRUE,
      formula = "100 * districts(npafp_rate >= warn) / districts_with_rate",
      numerator = "Child districts with NPAFP rate >= warn",
      denominator = "Child districts with a computable rate",
      unit = "%",
      polis_fn = "ufn_Indicator_NB_DIST_NPAFP_RATE_GE2"
    )
  )
  add(
    "combined_standard",
    spec(
      "Meeting surveillance standards (%)",
      "Composite",
      "percent",
      "derived",
      .calc_combined_standard,
      conf_d10,
      cat_none,
      levels = c("adm0", "adm1"),
      requires_pop = TRUE,
      requires_ind = c("npafp_rate", "stool_adequacy_cond_pct"),
      formula = "100 * districts(rate >= warn & adequacy >= target) / assessable",
      numerator = "Child districts meeting both core standards",
      denominator = "Child districts assessable on both",
      unit = "%",
      polis_fn = "ufn_Indicator_SURVINDCAT (combined)"
    )
  )
  add(
    "silent_districts",
    spec(
      "Silent districts (%)",
      "Composite",
      "percent",
      "cases",
      .calc_silent_districts,
      function(num, den, cfg) dplyr::if_else(!is.na(den) & den > 0, 1, 0),
      cat_none,
      levels = c("adm0", "adm1"),
      requires_admin = TRUE,
      formula = "100 * districts(zero AFP cases) / expected districts",
      numerator = "Expected districts with zero AFP cases",
      denominator = "Expected districts",
      unit = "%",
      polis_fn = "(build fresh)"
    )
  )
  add(
    "survindcat",
    spec(
      "Surveillance indicator category (0/1/2/4)",
      "Composite",
      "class",
      "derived",
      .calc_survindcat,
      conf_one,
      cat_none,
      requires_ind = c("npafp_rate", "stool_adequacy_cond_pct"),
      formula = "class from npafp_rate + condition-aware adequacy vs policy cutoffs",
      numerator = "(class)",
      denominator = "(class)",
      unit = "class",
      polis_fn = "ufn_Indicator_SURVINDCAT",
      notes = "Policy cutoffs (survindcat_rate_cutoff) are overridable; verify vs current GPEI definitions."
    )
  )

  reg
}

# ---- indicator dictionary ---------------------------------------------------

#' Dictionary of available polio surveillance indicators
#'
#' Returns a tidy description of every indicator [calc_polio_indicators()] knows
#' about -- code, label, family, formula, numerator/denominator, source,
#' period basis, admin levels, whether it needs a population denominator, WHO
#' target/warn thresholds, unit, the POLIS source function and notes. Derived
#' from the same registry the engine runs, so the catalogue and the engine never
#' drift.
#'
#' @param as_tibble If `TRUE` (default) return a tibble; if `FALSE` return the
#'   underlying named list of registry specs.
#' @param family Optional family filter (e.g. `"AFP"`, `"ES"`, `"Virus"`,
#'   `"SIA"`, `"Composite"`, `"Timeliness"`, `"Stool"`, `"Dose"`, `"Lab"`).
#'
#' @return A tibble (one row per indicator) with columns `code`, `label`,
#'   `family`, `kind`, `formula`, `numerator`, `denominator`, `source`,
#'   `period_basis`, `levels`, `requires_pop`, `target`, `warn`, `unit`,
#'   `polis_fn`, `notes`.
#'
#' @examples
#' available_indicators()
#' available_indicators(family = "ES")[, c("code", "label", "formula")]
#' @export
available_indicators <- function(as_tibble = TRUE, family = NULL) {
  reg <- .polio_indicator_registry()
  if (!is.null(family)) {
    keep <- vapply(reg, function(s) s$family %in% family, logical(1))
    reg <- reg[keep]
  }
  if (!as_tibble) {
    return(reg)
  }
  rows <- lapply(names(reg), function(code) {
    s <- reg[[code]]
    data.frame(
      code = code,
      label = s$label,
      family = s$family,
      kind = s$kind,
      formula = s$formula,
      numerator = s$numerator,
      denominator = s$denominator,
      source = s$source,
      period_basis = s$period_basis,
      levels = paste(s$levels, collapse = ", "),
      requires_pop = s$requires_pop,
      target = s$target,
      warn = s$warn,
      unit = s$unit,
      polis_fn = s$polis_fn,
      notes = s$notes,
      stringsAsFactors = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  rownames(out) <- NULL
  dplyr::as_tibble(out)
}
