# =============================================================================
# SIA campaign-quality indicators: LQAS and Independent Monitoring (IM)
#
# polished downloads the `Lqas` and `Im` OData tables but the surveillance
# cleaners do not touch them -- they are campaign-quality monitoring, not case
# data. This module turns the raw tables into analytic district-year roll-ups,
# cleaning the underlying grain the same way clean_afp() / clean_es() /
# clean_sia() do before it aggregates:
#
#   standardise names (crosswalk) -> clean strings -> parse + sanitise dates
#   -> derive year -> fix admin names -> reconcile admin GUIDs against a shape
#   -> dedup by id -> roll up to district-year.
#
# Column naming follows the same rule as every other cleaner: the raw POLIS
# names map to canonical snake_case via the package crosswalk; there are no
# per-function column-name arguments.
#
#   * LQAS: each lot is classified two ways. POLIS already ships its own
#     classification (`lqas2_classification_name` / `lqas3_classification_name`,
#     derived from the unexposed `REF_LQASThresholds` lookup); we roll those up
#     as the faithful answer AND re-derive a transparent coverage-threshold
#     classification alongside it for QA comparison.
#   * IM: the missed-children fraction `1 - sum(marked)/sum(checked)` (falling
#     back to `mean(result)` when no children were checked) is computed
#     separately for in-house (household visit) and out-of-house (market /
#     transit) monitoring, each with its own Valid/Invalid status.
# =============================================================================

utils::globalVariables(c(
  ".district",
  "adm0",
  "adm1",
  "adm2",
  "adm2_guid",
  "year",
  "checked",
  "unvacc",
  "coverage",
  "invalid",
  "lqas2_polis",
  "lqas3_polis",
  "lqas2_derived",
  "lqas3_derived",
  "n_lots",
  "n_pass_polis",
  "n_fail_polis",
  "n_invalid_polis",
  "pass_pct_polis",
  "n_pass_derived",
  "n_fail_derived",
  "n_invalid_derived",
  "pass_pct_derived",
  "checked_in",
  "marked_in",
  "result_in",
  "checked_out",
  "marked_out",
  "result_out",
  "n_checked_inhouse",
  "n_marked_inhouse",
  "missed_frac_inhouse",
  "im_status_inhouse",
  "n_checked_outhouse",
  "n_marked_outhouse",
  "missed_frac_outhouse",
  "im_status_outhouse",
  "n",
  "pct"
))

# ---- orchestrator -----------------------------------------------------------

#' Process the POLIS SIA campaign-quality tables (LQAS + IM)
#'
#' Thin entry point that runs [process_lqas()] and/or [process_im()] over the
#' raw POLIS `Lqas` and `Im` tables and returns their roll-ups together. Either
#' input may be `NULL`; the matching slot is then `NULL`. This is what
#' [run_pipeline()] calls when an `lqas` / `im` table is present in its inputs.
#'
#' @param lqas Optional raw POLIS LQAS table (data.frame). `NULL` (default)
#'   skips LQAS.
#' @param im Optional raw POLIS IM table (data.frame). `NULL` (default) skips IM.
#' @param cfg A [polis_config()] object (default `polis_config()`); its
#'   `crosswalk` standardises the raw column names.
#' @param shape Optional district shape (an `sf` polygon layer or a long ADM2
#'   attribute table) used to reconcile admin names/GUIDs via
#'   [reconcile_admin_guids()], exactly as the other cleaners use it. Default
#'   `NULL` (no shape-based reconciliation).
#' @param verbose Emit cli progress + one-line roll-up results (default `TRUE`).
#' @param summary Emit the full cli summary panel per stream (rules, class
#'   breakdown, mean missed-children). Default `TRUE` for standalone use;
#'   [run_pipeline()] passes `FALSE` so the pipeline stays terse and only the
#'   key steps and roll-up counts are reported.
#' @param ... Extra analytic arguments forwarded to [process_lqas()] (e.g.
#'   `pass_threshold`). Each processor keeps only the arguments it accepts.
#'
#' @return A named list with `lqas` (the [process_lqas()] result, or `NULL`) and
#'   `im` (the [process_im()] result, or `NULL`).
#'
#' @examples
#' lqas <- data.frame(
#'   Id = 1L, Admin0Name = "NIGERIA", Admin1Name = "KANO",
#'   Admin2Name = "NASSARAWA", Admin2GUID = "{A2}", Year = 2024L,
#'   ChildrenChecked = 60L, ChildrenFoundUnvaccinated = 3L,
#'   Lqas2ClassificationName = "Pass", Lqas3ClassificationName = "Pass"
#' )
#' out <- process_sia_quality(lqas = lqas, verbose = FALSE)
#' out$lqas$district
#'
#' @export
process_sia_quality <- function(
  lqas = NULL,
  im = NULL,
  cfg = polis_active_config(),
  shape = NULL,
  verbose = TRUE,
  summary = TRUE,
  ...
) {
  if (is.null(lqas) && is.null(im)) {
    cli::cli_abort("Supply at least one of {.arg lqas} or {.arg im}.")
  }
  lqas_out <- if (!is.null(lqas)) {
    .lqasim_quality_call(process_lqas, lqas, cfg, shape, verbose, summary, ...)
  } else {
    NULL
  }
  im_out <- if (!is.null(im)) {
    .lqasim_quality_call(process_im, im, cfg, shape, verbose, summary, ...)
  } else {
    NULL
  }
  list(lqas = lqas_out, im = im_out)
}

#' Call `fn(data, cfg =, shape =, verbose =, summary =, ...)` keeping only the
#' dots `fn` accepts.
#'
#' Lets [process_sia_quality()] forward a shared `...` (e.g. `pass_threshold`)
#' without passing an LQAS-only argument to the IM processor.
#' @noRd
.lqasim_quality_call <- function(fn, data, cfg, shape, verbose, summary, ...) {
  dots <- list(...)
  keep <- dots[names(dots) %in% names(formals(fn))]
  do.call(
    fn,
    c(
      list(
        data,
        cfg = cfg,
        shape = shape,
        verbose = verbose,
        summary = summary
      ),
      keep
    )
  )
}

# ---- LQAS -------------------------------------------------------------------

#' Process raw LQAS lots into classifications and district pass rates
#'
#' Cleans the raw POLIS `Lqas` table with the same recipe as [clean_afp()] /
#' [clean_sia()] -- standardise names via the crosswalk, clean strings, parse and
#' sanitise dates, fix admin names and (when a `shape` is supplied) reconcile
#' admin GUIDs, then dedup by `id` -- and rolls the cleaned lots up to a
#' per-district-year table, classifying each lot **two ways**:
#' \itemize{
#'   \item *POLIS* -- the classification POLIS already ships in the download
#'     (`lqas2_classification_name` / `lqas3_classification_name`), derived from
#'     its unexposed `REF_LQASThresholds` lookup. This is the faithful answer.
#'   \item *derived* -- a transparent re-derivation from coverage
#'     (`1 - children_found_unvaccinated / children_checked`) against
#'     `pass_threshold` / `warn_threshold`, plus the documented default-60 and
#'     2019 "sample size must be a multiple of 60 or INVALID" rules. Provided for
#'     QA comparison against the POLIS classes.
#' }
#'
#' @param lqas Raw LQAS table (data.frame/tibble), one row per lot.
#' @param cfg A [polis_config()] object (default `polis_config()`); its
#'   `crosswalk` maps the raw column names to canonical snake_case.
#' @param shape Optional district shape (an `sf` polygon layer or a long ADM2
#'   attribute table) for admin GUID reconciliation via
#'   [reconcile_admin_guids()] (keyed on `year`). Default `NULL`.
#' @param default_checked Lot size assumed when the sample size is missing
#'   (default `60`, per POLIS).
#' @param multiple_of Sample-size modulus enforced from `enforce_since`
#'   (default `60`).
#' @param enforce_since Year from which the multiple-of rule makes a lot INVALID
#'   in the *derived* classification (default `2019`).
#' @param pass_threshold Coverage (vaccinated fraction) at/above which a lot is
#'   a Pass in the *derived* classification (default `0.90`).
#' @param warn_threshold Coverage band for the derived 3-level "Intermediate"
#'   class (default `0.80`). Lots in `[warn_threshold, pass_threshold)` are
#'   Intermediate (3-level) / Fail (2-level). Set to `NULL` to disable the
#'   Intermediate band, collapsing the derived classes to strict Pass/Fail.
#' @param verbose Emit cli progress + the one-line roll-up result (default
#'   `TRUE`).
#' @param summary Emit the full cli summary panel (rules + class breakdown).
#'   Default `TRUE`; [run_pipeline()] passes `FALSE` to keep the pipeline terse.
#'
#' @return A list with `lots` (the cleaned lot-level grain carrying both
#'   `lqas2_polis` / `lqas3_polis` and the derived `coverage` / `invalid` /
#'   `lqas2_derived` / `lqas3_derived`), `district` (per-district roll-up with
#'   `n_lots`, the POLIS `n_pass_polis` / `n_fail_polis` / `n_invalid_polis` /
#'   `pass_pct_polis` and the derived `n_pass_derived` / `n_fail_derived` /
#'   `n_invalid_derived` / `pass_pct_derived`), and `meta`.
#'
#' @examples
#' lqas <- data.frame(
#'   Id = 1L, Admin0Name = "NIGERIA", Admin1Name = "KANO",
#'   Admin2Name = "NASSARAWA", Admin2GUID = "{A2}", Year = 2024L,
#'   ChildrenChecked = 60L, ChildrenFoundUnvaccinated = 3L,
#'   Lqas2ClassificationName = "Pass", Lqas3ClassificationName = "Pass"
#' )
#' process_lqas(lqas, verbose = FALSE)$district
#'
#' @export
process_lqas <- function(
  lqas,
  cfg = polis_active_config(),
  shape = NULL,
  default_checked = 60,
  multiple_of = 60,
  enforce_since = 2019,
  pass_threshold = 0.90,
  warn_threshold = 0.80,
  verbose = TRUE,
  summary = TRUE
) {
  step <- .lqasim_stepper(verbose)
  .polis_check_input(lqas, "lqas")

  step("Standardising names", "Standardised names")
  data <- standardise_names(lqas, cfg$crosswalk) |>
    .polis_clean_strings()
  .lqasim_require(data, c("adm2_guid", "children_checked"), "lqas")

  step("Parsing dates and deriving year", "Parsed dates and derived year")
  data <- data |>
    .lqasim_parse_dates(c("start", "end")) |>
    .lqasim_add_year("year", "start")

  data <- .lqasim_clean_geo(data, shape, step)

  step("Classifying lots", "Classified lots")
  data <- .lqas_derive(
    data,
    default_checked,
    multiple_of,
    enforce_since,
    pass_threshold,
    warn_threshold
  )

  step("Deduplicating by id and rolling up", "Deduplicated by id and rolled up")
  lots <- data |>
    polis_upsert(id = "id", date = "updated_date") |>
    .geo_guid_display_cols()
  district <- .lqas_rollup(lots)

  meta <- list(
    n_lots = nrow(lots),
    has_polis_class = any(!is.na(lots$lqas2_polis), na.rm = TRUE),
    default_checked = default_checked,
    multiple_of = multiple_of,
    enforce_since = enforce_since,
    pass_threshold = pass_threshold,
    warn_threshold = warn_threshold
  )

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    if (isTRUE(summary)) {
      .lqas_print_summary(lots, district, meta)
    }
    n_lots_fmt <- .polis_big_num(meta$n_lots)
    n_dist_fmt <- .polis_big_num(nrow(district))
    cli::cli_alert_success(
      "Rolled up {n_lots_fmt} {cli::qty(meta$n_lots)}lot{?s} to \\
      {n_dist_fmt} {cli::qty(nrow(district))}district-year{?s}."
    )
  }
  list(lots = lots, district = district, meta = meta)
}

#' Derive the per-lot coverage, validity and both classifications (in place).
#' @noRd
.lqas_derive <- function(
  data,
  default_checked,
  multiple_of,
  enforce_since,
  pass_threshold,
  warn_threshold
) {
  # guarantee the admin grouping columns exist on the grain (a trimmed input may
  # carry only adm2_guid); absent name levels roll up as NA, never an error.
  for (col in c("adm0", "adm1", "adm2")) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_character_
    }
  }
  checked_raw <- .lqasim_num(data, "children_checked")
  unvacc <- .lqasim_num(data, "children_found_unvaccinated")
  size_missing <- is.na(checked_raw) | checked_raw <= 0
  checked <- dplyr::if_else(
    size_missing,
    as.numeric(default_checked),
    checked_raw
  )
  coverage <- dplyr::if_else(
    !is.na(unvacc) & checked > 0,
    1 - unvacc / checked,
    NA_real_
  )
  year <- if ("year" %in% names(data)) {
    data[["year"]]
  } else {
    rep(NA_integer_, nrow(data))
  }
  invalid <- !is.na(year) &
    year >= enforce_since &
    (size_missing | checked %% multiple_of != 0)

  data$children_checked <- checked
  data$coverage <- coverage
  data$invalid <- invalid
  data$lqas3_derived <- .lqas_classify(
    coverage,
    invalid,
    pass_threshold,
    warn_threshold
  )
  data$lqas2_derived <- dplyr::case_when(
    data$lqas3_derived == "INVALID" ~ "INVALID",
    data$lqas3_derived == "Pass" ~ "Pass",
    is.na(data$lqas3_derived) ~ NA_character_,
    TRUE ~ "Fail"
  )
  data$lqas2_polis <- .lqas_norm_class(
    .lqasim_chr(data, "lqas2_classification_name")
  )
  data$lqas3_polis <- .lqas_norm_class(
    .lqasim_chr(data, "lqas3_classification_name")
  )
  data
}

#' Per-district key: GUID when present (so name spelling variants don't split a
#' district), else the admin-name composite (so GUID-less districts don't merge).
#' @noRd
.lqasim_district_key <- function(adm2_guid, adm0, adm1, adm2) {
  dplyr::if_else(
    !is.na(adm2_guid) & adm2_guid != "",
    adm2_guid,
    paste(adm0, adm1, adm2, sep = "")
  )
}

#' Roll the cleaned lots up to a per-district-year pass-rate table.
#' @noRd
.lqas_rollup <- function(lots) {
  lots |>
    dplyr::mutate(
      .district = .lqasim_district_key(adm2_guid, adm0, adm1, adm2)
    ) |>
    dplyr::group_by(.district, year) |>
    dplyr::summarise(
      adm2_guid = dplyr::first(adm2_guid),
      adm0 = dplyr::first(adm0),
      adm1 = dplyr::first(adm1),
      adm2 = dplyr::first(adm2),
      n_lots = dplyr::n(),
      n_pass_polis = sum(lqas2_polis == "Pass", na.rm = TRUE),
      n_fail_polis = sum(lqas2_polis == "Fail", na.rm = TRUE),
      n_invalid_polis = sum(lqas2_polis == "INVALID", na.rm = TRUE),
      n_pass_derived = sum(lqas2_derived == "Pass", na.rm = TRUE),
      n_fail_derived = sum(lqas2_derived == "Fail", na.rm = TRUE),
      n_invalid_derived = sum(lqas2_derived == "INVALID", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(-.district) |>
    dplyr::mutate(
      pass_pct_polis = .lqasim_pass_pct(n_pass_polis, n_fail_polis),
      pass_pct_derived = .lqasim_pass_pct(n_pass_derived, n_fail_derived)
    ) |>
    dplyr::arrange(adm0, adm1, adm2, year)
}

#' Normalise a shipped LQAS class label to Pass / Fail / INVALID / NA.
#'
#' Tolerant of POLIS wording variants (Accepted/Rejected) and case; anything
#' unrecognised (including blanks) becomes `NA`.
#' @noRd
.lqas_norm_class <- function(x) {
  x <- trimws(as.character(x))
  dplyr::case_when(
    is.na(x) | x == "" ~ NA_character_,
    grepl("invalid", x, ignore.case = TRUE) ~ "INVALID",
    grepl("pass|accept", x, ignore.case = TRUE) ~ "Pass",
    grepl("fail|reject", x, ignore.case = TRUE) ~ "Fail",
    TRUE ~ NA_character_
  )
}

#' Derived LQAS classifier (transparent stand-in for REF_LQASThresholds).
#' @noRd
.lqas_classify <- function(coverage, invalid, pass_threshold, warn_threshold) {
  # hoist the scalar NULL guard out of the vectorised case_when
  has_warn <- !is.null(warn_threshold)
  dplyr::case_when(
    invalid ~ "INVALID",
    is.na(coverage) ~ NA_character_,
    coverage >= pass_threshold ~ "Pass",
    has_warn & coverage >= warn_threshold ~ "Intermediate",
    TRUE ~ "Fail"
  )
}

#' Pass percentage from pass / fail counts; `NA` when neither occurs.
#' @noRd
.lqasim_pass_pct <- function(n_pass, n_fail) {
  dplyr::if_else(
    (n_pass + n_fail) > 0,
    100 * n_pass / (n_pass + n_fail),
    NA_real_
  )
}

#' @noRd
.lqas_print_summary <- function(lots, district, meta) {
  cli::cli_rule()
  cli::cli_h2("LQAS Summary")
  cli::cli_h3("Rules")
  cli::cli_bullets(c(
    "*" = "POLIS classes used when present: {meta$has_polis_class}",
    "*" = "Derived default lot size: {meta$default_checked}",
    "*" = "Derived INVALID if sample not a multiple of {meta$multiple_of} (from {meta$enforce_since})",
    "*" = "Derived Pass if coverage >= {meta$pass_threshold}; Intermediate if >= {meta$warn_threshold}",
    "!" = "The derived cut-off is a transparent stand-in, not REF_LQASThresholds."
  ))
  tab <- lots |>
    dplyr::count(lqas2_polis, name = "n") |>
    dplyr::mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1))
  cli::cli_h3("Lots by POLIS 2-level class")
  for (i in seq_len(nrow(tab))) {
    label <- dplyr::coalesce(tab$lqas2_polis[i], "NA")
    n_fmt <- .polis_big_num(tab$n[i])
    cli::cli_text("{label}: {n_fmt} ({tab$pct[i]}%)")
  }
  cli::cli_rule()
}

# ---- Independent Monitoring (IM) -------------------------------------------

#' Process raw Independent Monitoring (IM) data into missed-children rates
#'
#' Cleans the raw POLIS `Im` table with the same recipe as [clean_afp()] /
#' [clean_sia()] -- standardise names via the crosswalk, clean strings, parse and
#' sanitise dates, fix admin names and (when a `shape` is supplied) reconcile
#' admin GUIDs, then dedup by `id` -- and computes the missed-children fraction
#' per district-year **separately for in-house and out-of-house monitoring**.
#' For each setting `missed = 1 - sum(marked)/sum(checked)`, falling back to
#' `mean(result)` when no children were checked, with a Valid/Invalid status
#' flag (Invalid when the result is missing or negative).
#'
#' @param im Raw IM table (data.frame/tibble).
#' @param cfg A [polis_config()] object (default `polis_config()`); its
#'   `crosswalk` maps the raw column names to canonical snake_case.
#' @param shape Optional district shape (an `sf` polygon layer or a long ADM2
#'   attribute table) for admin GUID reconciliation via
#'   [reconcile_admin_guids()] (keyed on `year`). Default `NULL`.
#' @param verbose Emit cli progress + the one-line roll-up result (default
#'   `TRUE`).
#' @param summary Emit the full cli summary panel (per-setting missed-children).
#'   Default `TRUE`; [run_pipeline()] passes `FALSE` to keep the pipeline terse.
#'
#' @return A list with `district` (per-district-year, carrying both
#'   `missed_frac_inhouse` / `im_status_inhouse` and `missed_frac_outhouse` /
#'   `im_status_outhouse` plus their checked/marked totals) and `meta`.
#'
#' @examples
#' im <- data.frame(
#'   Id = 1L, Admin0 = "NIGERIA", Admin1 = "KANO", Admin2 = "NASSARAWA",
#'   Admin2GUID = "{A2}", ActivityPlannedDateFromYear = 2024L,
#'   HouseholdsNumberChildrenChecked = 20L,
#'   HouseholdsNumberChildrenMarked = 18L, HouseholdsResult = NA_real_,
#'   OutOfHouseNumberChildrenChecked = 10L,
#'   OutOfHouseNumberChildrenMarked = 8L, OutOfHouseResult = NA_real_
#' )
#' process_im(im, verbose = FALSE)$district
#'
#' @export
process_im <- function(
  im,
  cfg = polis_active_config(),
  shape = NULL,
  verbose = TRUE,
  summary = TRUE
) {
  step <- .lqasim_stepper(verbose)
  .polis_check_input(im, "im")

  step("Standardising names", "Standardised names")
  data <- standardise_names(im, cfg$crosswalk) |>
    .polis_clean_strings()
  .lqasim_require(
    data,
    c(
      "adm2_guid",
      "households_number_children_checked",
      "households_number_children_marked"
    ),
    "im"
  )

  step("Parsing dates and deriving year", "Parsed dates and derived year")
  data <- data |>
    .lqasim_parse_dates(c(
      "activity_planned_date_from",
      "activity_planned_date_to",
      "start_sia"
    )) |>
    .lqasim_add_year(
      "activity_planned_date_from_year",
      "activity_planned_date_from"
    )

  data <- .lqasim_clean_geo(data, shape, step)

  step("Deduplicating by id and rolling up", "Deduplicated by id and rolled up")
  data <- data |>
    polis_upsert(id = "id", date = "updated_date") |>
    .geo_guid_display_cols()
  district <- .im_rollup(data)

  meta <- list(n_rows = nrow(data), n_district = nrow(district))

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    if (isTRUE(summary)) {
      .im_print_summary(district, meta)
    }
    n_dist_fmt <- .polis_big_num(meta$n_district)
    cli::cli_alert_success(
      "Rolled up to {n_dist_fmt} {cli::qty(meta$n_district)}district-year{?s}."
    )
  }
  list(district = district, meta = meta)
}

#' Roll the cleaned IM records up to a per-district-year missed-children table,
#' keeping in-house and out-of-house monitoring separate.
#' @noRd
.im_rollup <- function(data) {
  std <- tibble::tibble(
    adm2_guid = .lqasim_chr(data, "adm2_guid"),
    adm2 = .lqasim_chr(data, "adm2"),
    adm1 = .lqasim_chr(data, "adm1"),
    adm0 = .lqasim_chr(data, "adm0"),
    year = if ("year" %in% names(data)) {
      data[["year"]]
    } else {
      rep(NA_integer_, nrow(data))
    },
    checked_in = .lqasim_num(data, "households_number_children_checked"),
    marked_in = .lqasim_num(data, "households_number_children_marked"),
    result_in = .lqasim_num(data, "households_result"),
    checked_out = .lqasim_num(data, "out_of_house_number_children_checked"),
    marked_out = .lqasim_num(data, "out_of_house_number_children_marked"),
    result_out = .lqasim_num(data, "out_of_house_result")
  )

  std |>
    dplyr::mutate(
      .district = .lqasim_district_key(adm2_guid, adm0, adm1, adm2)
    ) |>
    dplyr::group_by(.district, year) |>
    dplyr::summarise(
      adm2_guid = dplyr::first(adm2_guid),
      adm0 = dplyr::first(adm0),
      adm1 = dplyr::first(adm1),
      adm2 = dplyr::first(adm2),
      n_checked_inhouse = sum(checked_in, na.rm = TRUE),
      n_marked_inhouse = sum(marked_in, na.rm = TRUE),
      missed_frac_inhouse = .im_missed(checked_in, marked_in, result_in),
      n_checked_outhouse = sum(checked_out, na.rm = TRUE),
      n_marked_outhouse = sum(marked_out, na.rm = TRUE),
      missed_frac_outhouse = .im_missed(checked_out, marked_out, result_out),
      .groups = "drop"
    ) |>
    dplyr::select(-.district) |>
    dplyr::mutate(
      im_status_inhouse = .im_status(missed_frac_inhouse),
      im_status_outhouse = .im_status(missed_frac_outhouse)
    ) |>
    dplyr::arrange(adm0, adm1, adm2, year)
}

#' Missed-children fraction for one group: `1 - sum(marked)/sum(checked)`,
#' falling back to `mean(result)` when no children were checked; `NaN` -> `NA`.
#' @noRd
.im_missed <- function(checked, marked, result) {
  total_checked <- sum(checked, na.rm = TRUE)
  out <- if (total_checked == 0) {
    mean(result, na.rm = TRUE)
  } else {
    both <- !is.na(checked) & !is.na(marked)
    1 - sum(marked[both]) / sum(checked[both])
  }
  dplyr::if_else(is.nan(out), NA_real_, out)
}

#' Valid/Invalid IM status from a missed-children fraction.
#' @noRd
.im_status <- function(missed_frac) {
  dplyr::if_else(
    is.na(missed_frac) | missed_frac < 0,
    "Invalid",
    "Valid"
  )
}

#' @noRd
.im_print_summary <- function(district, meta) {
  cli::cli_rule()
  cli::cli_h2("Independent Monitoring Summary")
  cli::cli_text(
    "Formula: missed = 1 - sum(marked)/sum(checked) (else mean(result))"
  )
  for (setting in c("inhouse", "outhouse")) {
    status <- district[[paste0("im_status_", setting)]]
    frac <- district[[paste0("missed_frac_", setting)]]
    n_valid <- sum(status == "Valid", na.rm = TRUE)
    n_invalid <- sum(status == "Invalid", na.rm = TRUE)
    cli::cli_h3(if (setting == "inhouse") "In-house" else "Out-of-house")
    n_valid_fmt <- .polis_big_num(n_valid)
    n_invalid_fmt <- .polis_big_num(n_invalid)
    cli::cli_text(
      "{n_valid_fmt} Valid, {n_invalid_fmt} Invalid district-year(s)."
    )
    if (n_valid > 0) {
      mean_missed <- round(
        100 * mean(frac[status == "Valid"], na.rm = TRUE),
        1
      )
      cli::cli_text("Mean missed-children (valid): {mean_missed}%")
    }
  }
  cli::cli_rule()
}

# ---- shared cleaning helpers ------------------------------------------------

#' Build a cli progress stepper (or a no-op when quiet), as the cleaners do.
#' @noRd
.lqasim_stepper <- function(verbose) {
  function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
}

#' Abort when canonical required columns are absent after standardisation.
#' @noRd
.lqasim_require <- function(data, cols, arg) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Missing required column(s) in {.arg {arg}} after standardisation:",
      "x" = "{.var {missing_cols}}",
      "i" = "Check the {.fn polis_crosswalk} mapping for this table."
    ))
  }
  invisible(data)
}

#' Garbage floor for the SIA "sensible date" test (pre-surveillance era).
#' @noRd
.lqasim_min_sensible_year <- 1980L

#' Parse the named date columns to `Date`, NA-ing implausible values.
#'
#' The clean_afp() "sensible date" rule applied to the campaign/planning dates:
#' a value before `min_year` or after `reference_date` is a data-entry error and
#' set to `NA` before a year is derived from it. Audit timestamps
#' (`updated_date`) are deliberately left untouched so they stay sortable ISO
#' strings for the keep-latest dedup.
#' @noRd
.lqasim_parse_dates <- function(
  data,
  cols,
  min_year = .lqasim_min_sensible_year,
  reference_date = Sys.Date()
) {
  cols <- intersect(cols, names(data))
  if (length(cols) == 0L) {
    return(data)
  }
  floor_date <- lubridate::make_date(min_year, 1L, 1L)
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      \(x) {
        parsed <- .lqasim_parse_date(x)
        dplyr::if_else(
          !is.na(parsed) & parsed >= floor_date & parsed <= reference_date,
          parsed,
          lubridate::NA_Date_
        )
      }
    )
  )
}

#' Add an integer `year` column: prefer `year_var`, else derive from `date_var`.
#' @noRd
.lqasim_add_year <- function(data, year_var, date_var) {
  data$year <- .lqasim_resolve_year(data, year_var, date_var)
  data
}

#' Standardise admin names and -- when a shape is supplied -- reconcile admin
#' GUIDs against it (keyed on `year`), exactly as clean_afp() / clean_es() do.
#' @noRd
.lqasim_clean_geo <- function(data, shape, step) {
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data)
  if (is.null(shape)) {
    return(data)
  }
  long_shape <- if (inherits(shape, "sf")) {
    step(
      "Building the long district lookup from the shape",
      "Built the long district lookup from the shape"
    )
    create_long_shape(shape, "adm2")
  } else {
    shape
  }
  step(
    "Reconciling admin GUIDs against the district shape",
    "Reconciled admin GUIDs against the district shape"
  )
  reconcile_admin_guids(data, long_shape, year_var = "year", verbose = FALSE)
}

#' Extract a character column by name, or an all-`NA` column when absent.
#' @noRd
.lqasim_chr <- function(data, var) {
  if (var %in% names(data)) {
    as.character(data[[var]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

#' Extract a numeric column by name, or an all-`NA` column when absent.
#' @noRd
.lqasim_num <- function(data, var) {
  if (var %in% names(data)) {
    suppressWarnings(as.numeric(data[[var]]))
  } else {
    rep(NA_real_, nrow(data))
  }
}

#' Resolve a year vector: prefer the integer `year_var`, else parse `date_var`.
#' @noRd
.lqasim_resolve_year <- function(data, year_var, date_var) {
  if (year_var %in% names(data)) {
    return(suppressWarnings(as.integer(data[[year_var]])))
  }
  if (date_var %in% names(data)) {
    return(lubridate::year(.lqasim_parse_date(data[[date_var]])))
  }
  rep(NA_integer_, nrow(data))
}

#' Parse a heterogeneous date column to Date (tries several formats).
#' @noRd
.lqasim_parse_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  suppressWarnings(lubridate::as_date(lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd", "Y-m-d", "Y-m-d H:M:S", "dmY", "mdY"),
    quiet = TRUE
  )))
}
