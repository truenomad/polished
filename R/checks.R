# =============================================================================
# Data-quality checks
#
# Per-dataset check functions (checks_afp/checks_es/checks_sia/checks_virus/
# checks_hum_spec) that surface data-quality problems by *reading columns the
# cleaners already produced* -- duplicates, blank keys, unreconciled geography,
# out-of-range values, date-ordering violations. Every check is an O(n) filter
# or grouping: nothing here re-derives geography or recomputes anything the
# pipeline did not already compute. Each returns a list whose `summary` tibble
# counts every applicable check and whose remaining named entries hold the
# flagged rows (key columns only) for checks that found problems --
# write_checks_excel() turns that into one workbook, one tab per check.
# =============================================================================

# ---- shared predicates ------------------------------------------------------

# TRUE where a value is NA or (for character) blank after trimming.
.polis_blank <- function(x) {
  if (is.character(x)) {
    is.na(x) | !nzchar(trimws(x))
  } else {
    is.na(x)
  }
}

# TRUE where a coordinate is missing or exactly zero (the null-island sentinel).
.polis_zero_coord <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  is.na(n) | n == 0
}

# TRUE where a yes/no (or adequacy) flag reads as false. The character vocabulary
# mirrors the FALSE side of .polis_as_logical(), so the POLIS adequacy wording
# ("Inadequate"/"Poor") on stool/specimen columns is matched, not just yes/no.
.polis_is_false <- function(x) {
  if (is.logical(x)) {
    !is.na(x) & !x
  } else {
    tolower(trimws(as.character(x))) %in%
      c("no", "false", "f", "0", "n", "inadequate", "poor")
  }
}

# Parse to Date tolerantly; already-Date input passes through.
.polis_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  suppressWarnings(as.Date(x))
}

# Rows that share a composite key with at least one other row. A row whose key is
# missing/blank in any component is not a real business key and is excluded, so
# unrelated blank-key rows are never paste-collapsed (e.g. "NA\rNA") into a
# spurious duplicate group.
.polis_dup_rows <- function(data, keys) {
  keys <- intersect(keys, names(data))
  if (length(keys) == 0L) {
    return(data[0, , drop = FALSE])
  }
  present <- Reduce(`&`, lapply(keys, function(col) !.polis_blank(data[[col]])))
  row_key <- do.call(
    paste,
    c(lapply(keys, function(col) as.character(data[[col]])), sep = "\r")
  )
  present_keys <- row_key[present]
  dup_keys <- unique(present_keys[duplicated(present_keys)])
  data[present & row_key %in% dup_keys, , drop = FALSE]
}

# Rows where any timeliness interval column (`*_to_*`, numeric) is negative -- a
# date-ordering violation. Reads the interval columns the cleaners already
# derived; computes nothing new.
.polis_negative_interval_rows <- function(data) {
  iv <- grep("_to_", names(data), value = TRUE)
  if (length(iv) == 0L) {
    return(data[0, , drop = FALSE])
  }
  hit <- rep(FALSE, nrow(data))
  for (col in iv) {
    vals <- suppressWarnings(as.numeric(data[[col]]))
    hit <- hit | (!is.na(vals) & vals < 0)
  }
  data[hit, , drop = FALSE]
}

# ---- runner -----------------------------------------------------------------

# Run a dataset's check specs. A spec runs when all its `needs` columns are
# present and its optional `applies(data)` predicate holds. Returns
# list(summary = <tibble>, <check> = <flagged rows>...) with detail entries only
# for checks that flagged >= 1 row.
.polis_run_checks <- function(data, specs, key_cols, reference_date) {
  reference_date <- .polis_as_date(reference_date)
  summary_rows <- list()
  details <- list()
  for (spec in specs) {
    if (!all(spec$needs %in% names(data))) {
      next
    }
    if (!is.null(spec$applies) && !isTRUE(spec$applies(data))) {
      next
    }
    flagged <- spec$fn(data, reference_date)
    n_flagged <- nrow(flagged)
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      check = spec$check,
      domain = spec$domain,
      severity = spec$severity,
      n_flagged = n_flagged,
      description = spec$description,
      stringsAsFactors = FALSE
    )
    if (n_flagged > 0L) {
      cols <- intersect(c(key_cols, spec$cols), names(flagged))
      details[[spec$check]] <- dplyr::as_tibble(flagged[, cols, drop = FALSE])
    }
  }

  summary <- if (length(summary_rows) > 0L) {
    summary_unsorted <- dplyr::bind_rows(summary_rows)
    ord <- order(
      match(summary_unsorted$severity, c("error", "warning", "info")),
      -summary_unsorted$n_flagged
    )
    dplyr::as_tibble(summary_unsorted[ord, , drop = FALSE])
  } else {
    dplyr::tibble(
      check = character(),
      domain = character(),
      severity = character(),
      n_flagged = integer(),
      description = character()
    )
  }
  c(list(summary = summary), details)
}

# ---- spec catalogues (one builder per dataset) ------------------------------

.polis_checks_specs_afp <- function() {
  list(
    list(
      check = "afp_duplicates",
      domain = "AFP",
      severity = "warning",
      description = "Duplicate EPID + onset date + admin0",
      needs = c("epid", "paralysis_onset_date", "adm0"),
      cols = character(0),
      fn = function(d, ref) {
        .polis_dup_rows(d, c("epid", "paralysis_onset_date", "adm0"))
      }
    ),
    list(
      check = "afp_no_onset",
      domain = "AFP",
      severity = "warning",
      description = "AFP cases with no paralysis onset date",
      needs = "paralysis_onset_date",
      cols = "paralysis_onset_date",
      fn = function(d, ref) {
        d[.polis_blank(d[["paralysis_onset_date"]]), , drop = FALSE]
      }
    ),
    list(
      check = "afp_no_classification",
      domain = "AFP",
      severity = "warning",
      description = "AFP cases with no usable classification",
      needs = "classification_all",
      cols = "classification_all",
      fn = function(d, ref) {
        d[.polis_blank(d[["classification_all"]]), , drop = FALSE]
      }
    ),
    list(
      check = "afp_missing_guid",
      domain = "AFP",
      severity = "error",
      description = "Cases missing an admin1/admin2 GUID after reconciliation",
      needs = c("adm1_guid", "adm2_guid"),
      cols = c("adm1_guid", "adm2_guid", "geo_source"),
      fn = function(d, ref) {
        d[is.na(d[["adm1_guid"]]) | is.na(d[["adm2_guid"]]), , drop = FALSE]
      }
    ),
    list(
      check = "afp_empty_coords",
      domain = "AFP",
      severity = "info",
      description = "Cases with missing or zero coordinates",
      needs = c("latitude", "longitude"),
      cols = c("latitude", "longitude"),
      fn = function(d, ref) {
        d[
          .polis_zero_coord(d[["latitude"]]) |
            .polis_zero_coord(d[["longitude"]]),
          ,
          drop = FALSE
        ]
      }
    ),
    list(
      check = "afp_future_onset",
      domain = "AFP",
      severity = "warning",
      description = "Onset date later than the run date",
      needs = "paralysis_onset_date",
      cols = "paralysis_onset_date",
      fn = function(d, ref) {
        onset_date <- .polis_as_date(d[["paralysis_onset_date"]])
        d[!is.na(onset_date) & onset_date > ref, , drop = FALSE]
      }
    ),
    list(
      check = "afp_age_out_of_range",
      domain = "AFP",
      severity = "info",
      description = "Age in months negative or implausibly large (> 1800)",
      needs = "age_months",
      cols = "age_months",
      fn = function(d, ref) {
        age_num <- suppressWarnings(as.numeric(d[["age_months"]]))
        d[!is.na(age_num) & (age_num < 0 | age_num > 1800), , drop = FALSE]
      }
    ),
    list(
      check = "afp_negative_intervals",
      domain = "AFP",
      severity = "warning",
      description = "Negative timeliness interval (date-ordering violation)",
      needs = character(0),
      cols = character(0),
      applies = function(d) any(grepl("_to_", names(d))),
      fn = function(d, ref) .polis_negative_interval_rows(d)
    ),
    list(
      check = "afp_inadequate_stool",
      domain = "AFP",
      severity = "info",
      description = "Cases flagged with inadequate stool",
      needs = "adequate_stool",
      cols = "adequate_stool",
      fn = function(d, ref) {
        d[.polis_is_false(d[["adequate_stool"]]), , drop = FALSE]
      }
    )
  )
}

.polis_checks_specs_es <- function() {
  es_id <- function(d) {
    if ("sample_id" %in% names(d)) "sample_id" else "enviro_sample_id"
  }
  list(
    list(
      check = "es_duplicates",
      domain = "ES",
      severity = "warning",
      description = "Duplicate sample ID + admin0",
      needs = "adm0",
      cols = character(0),
      applies = function(d) {
        any(c("sample_id", "enviro_sample_id") %in% names(d))
      },
      fn = function(d, ref) .polis_dup_rows(d, c(es_id(d), "adm0"))
    ),
    list(
      check = "es_no_collection_date",
      domain = "ES",
      severity = "warning",
      description = "ES samples with no collection date",
      needs = "collection_date",
      cols = "collection_date",
      fn = function(d, ref) {
        d[.polis_blank(d[["collection_date"]]), , drop = FALSE]
      }
    ),
    list(
      check = "es_missing_guid",
      domain = "ES",
      severity = "error",
      description = "Samples missing an admin1/admin2 GUID after reconciliation",
      needs = c("adm1_guid", "adm2_guid"),
      cols = c("adm1_guid", "adm2_guid", "geo_source"),
      fn = function(d, ref) {
        d[is.na(d[["adm1_guid"]]) | is.na(d[["adm2_guid"]]), , drop = FALSE]
      }
    ),
    list(
      check = "es_empty_coords",
      domain = "ES",
      severity = "info",
      description = "Samples with missing or zero coordinates",
      needs = c("latitude", "longitude"),
      cols = c("latitude", "longitude"),
      fn = function(d, ref) {
        d[
          .polis_zero_coord(d[["latitude"]]) |
            .polis_zero_coord(d[["longitude"]]),
          ,
          drop = FALSE
        ]
      }
    ),
    list(
      check = "es_future_collection",
      domain = "ES",
      severity = "warning",
      description = "Collection date later than the run date",
      needs = "collection_date",
      cols = "collection_date",
      fn = function(d, ref) {
        collection_dt <- .polis_as_date(d[["collection_date"]])
        d[!is.na(collection_dt) & collection_dt > ref, , drop = FALSE]
      }
    )
  )
}

.polis_checks_specs_sia <- function() {
  list(
    list(
      check = "sia_missing_guid",
      domain = "SIA",
      severity = "warning",
      description = "SIA rows missing an admin2 GUID after reconciliation",
      needs = "adm2_guid",
      cols = c("adm2_guid", "geo_source"),
      fn = function(d, ref) d[is.na(d[["adm2_guid"]]), , drop = FALSE]
    ),
    list(
      check = "sia_no_start_year",
      domain = "SIA",
      severity = "info",
      description = "SIA rows with no start year",
      needs = "year_start",
      cols = "year_start",
      fn = function(d, ref) d[is.na(d[["year_start"]]), , drop = FALSE]
    )
  )
}

.polis_checks_specs_virus <- function() {
  list(
    list(
      # clean_virus() emits no POLIS `id` (positives are constructed from the
      # cleaned case/ES streams), so a duplicate is keyed on the analytic
      # identity: the same EPID/sample reporting the same virus label
      # (classification_all, the always-present canonical label) on the same
      # event date.
      check = "virus_duplicates",
      domain = "Virus",
      severity = "warning",
      description = "Duplicate positives (same EPID + virus label + date)",
      needs = c("epid", "classification_all"),
      cols = c("classification_all", "virus_date"),
      fn = function(d, ref) {
        .polis_dup_rows(d, c("epid", "classification_all", "virus_date"))
      }
    ),
    list(
      check = "virus_missing_guid",
      domain = "Virus",
      severity = "error",
      description = "Positives missing an admin1/admin2 GUID after reconciliation",
      needs = c("adm1_guid", "adm2_guid"),
      cols = c("adm1_guid", "adm2_guid"),
      fn = function(d, ref) {
        d[is.na(d[["adm1_guid"]]) | is.na(d[["adm2_guid"]]), , drop = FALSE]
      }
    ),
    list(
      check = "virus_large_nt",
      domain = "Virus",
      severity = "warning",
      description = "Vaccine viruses with >= 6 nucleotide changes",
      needs = "nt_changes",
      cols = "nt_changes",
      fn = function(d, ref) {
        nt_num <- suppressWarnings(as.numeric(d[["nt_changes"]]))
        d[!is.na(nt_num) & nt_num >= 6, , drop = FALSE]
      }
    ),
    list(
      check = "virus_missing_emergence",
      domain = "Virus",
      severity = "info",
      description = "VDPV positives with no emergence group",
      needs = c("emergence_group", "classification_all"),
      cols = c("emergence_group", "classification_all"),
      fn = function(d, ref) {
        d[
          .polis_blank(d[["emergence_group"]]) &
            grepl("VDPV", d[["classification_all"]], ignore.case = TRUE),
          ,
          drop = FALSE
        ]
      }
    )
  )
}

.polis_checks_specs_hum_spec <- function() {
  list(
    list(
      check = "hum_spec_duplicates",
      domain = "HumSpec",
      severity = "warning",
      description = "Duplicate specimen records (same specimen id)",
      needs = "specimen_id",
      cols = character(0),
      fn = function(d, ref) .polis_dup_rows(d, "specimen_id")
    ),
    list(
      check = "hum_spec_no_collection_date",
      domain = "HumSpec",
      severity = "warning",
      description = "Specimens with no collection date",
      needs = "collection_date",
      cols = "collection_date",
      fn = function(d, ref) {
        d[.polis_blank(d[["collection_date"]]), , drop = FALSE]
      }
    ),
    list(
      check = "hum_spec_missing_guid",
      domain = "HumSpec",
      severity = "error",
      description = "Specimens missing an admin1/admin2 GUID after reconciliation",
      needs = c("adm1_guid", "adm2_guid"),
      cols = c("adm1_guid", "adm2_guid", "geo_source"),
      fn = function(d, ref) {
        d[is.na(d[["adm1_guid"]]) | is.na(d[["adm2_guid"]]), , drop = FALSE]
      }
    ),
    list(
      check = "hum_spec_inadequate",
      domain = "HumSpec",
      severity = "info",
      description = "Specimens flagged inadequate",
      needs = "adequate",
      cols = "adequate",
      fn = function(d, ref) d[.polis_is_false(d[["adequate"]]), , drop = FALSE]
    )
  )
}

# Standard key columns surfaced on every flagged-row tab, per dataset.
.polis_checks_keys <- list(
  afp = c(
    "id",
    "epid",
    "country_actual",
    "adm0",
    "adm1",
    "adm2",
    "paralysis_onset_date",
    "year_onset"
  ),
  es = c(
    "id",
    "sample_id",
    "enviro_sample_id",
    "adm0",
    "adm1",
    "adm2",
    "collection_date",
    "year_collection"
  ),
  sia = c("id", "adm0", "adm1", "adm2", "year_start", "round_num"),
  virus = c(
    "epid",
    "adm0",
    "adm1",
    "adm2",
    "vtype",
    "classification_all",
    "emergence_group"
  ),
  hum_spec = c(
    "id",
    "epid",
    "specimen_id",
    "adm0",
    "adm1",
    "adm2",
    "collection_date"
  )
)

# Validate a checks input is a data frame (empty is allowed -- every check
# simply reports zero flagged rows).
.polis_check_checks_input <- function(data, dataset) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg {dataset}} must be a data.frame.")
  }
  invisible(data)
}

# ---- public per-dataset check functions -------------------------------------

#' Run AFP data-quality checks
#'
#' Surfaces data-quality problems in a cleaned AFP table by reading columns the
#' cleaner already produced (duplicates, blank keys, unreconciled GUIDs,
#' missing/zero coordinates, future onset dates, out-of-range age, negative
#' timeliness intervals, inadequate stool). Checks whose required columns are
#' absent are skipped, so a trimmed input is handled gracefully.
#'
#' @param afp A cleaned AFP tibble (from [clean_afp()]).
#' @param reference_date Date treated as "today" for future-date checks
#'   (default [Sys.Date()]).
#'
#' @return A named list: `summary` (a tibble with one row per applicable check:
#'   `check`, `domain`, `severity`, `n_flagged`, `description`) followed by one
#'   tibble of flagged rows (key columns) per check that found problems. Pass it
#'   to [write_checks_excel()] to export a workbook.
#'
#' @examples
#' afp <- data.frame(
#'   id = c(1, 1), epid = c("A-1", "A-1"),
#'   paralysis_onset_date = c("2024-01-02", "2024-01-02"),
#'   adm0 = "NIGERIA"
#' )
#' checks_afp(afp)$summary
#'
#' @export
checks_afp <- function(afp, reference_date = Sys.Date()) {
  .polis_check_checks_input(afp, "afp")
  .polis_run_checks(
    afp,
    .polis_checks_specs_afp(),
    .polis_checks_keys$afp,
    reference_date
  )
}

#' Run environmental-surveillance data-quality checks
#'
#' Reads columns [clean_es()] already produced to flag duplicate sample IDs,
#' blank/future collection dates, unreconciled GUIDs and missing/zero
#' coordinates. See [checks_afp()] for the return shape.
#'
#' @param es A cleaned ES tibble (from [clean_es()]).
#' @param reference_date Date treated as "today" for future-date checks
#'   (default [Sys.Date()]).
#'
#' @return A named list (`summary` + one tibble per flagged check); see
#'   [checks_afp()].
#'
#' @examples
#' es <- data.frame(
#'   id = 1, sample_id = "E1", adm0 = "CHAD",
#'   collection_date = "2024-02-01"
#' )
#' checks_es(es)$summary
#'
#' @export
checks_es <- function(es, reference_date = Sys.Date()) {
  .polis_check_checks_input(es, "es")
  .polis_run_checks(
    es,
    .polis_checks_specs_es(),
    .polis_checks_keys$es,
    reference_date
  )
}

#' Run SIA data-quality checks
#'
#' Reads columns [clean_sia()] already produced to flag rows missing an admin2
#' GUID or a start year. See [checks_afp()] for the return shape.
#'
#' @param sia A cleaned SIA tibble (from [clean_sia()]).
#' @param reference_date Date treated as "today" for future-date checks
#'   (default [Sys.Date()]).
#'
#' @return A named list (`summary` + one tibble per flagged check); see
#'   [checks_afp()].
#'
#' @examples
#' sia <- data.frame(id = 1, adm0 = "CHAD", year_start = NA_integer_)
#' checks_sia(sia)$summary
#'
#' @export
checks_sia <- function(sia, reference_date = Sys.Date()) {
  .polis_check_checks_input(sia, "sia")
  .polis_run_checks(
    sia,
    .polis_checks_specs_sia(),
    .polis_checks_keys$sia,
    reference_date
  )
}

#' Run virus/positives data-quality checks
#'
#' Reads columns [clean_virus()] already produced to flag duplicate records,
#' vaccine viruses with large nucleotide changes, and VDPV positives with no
#' emergence group. See [checks_afp()] for the return shape.
#'
#' @param virus A cleaned virus tibble (from [clean_virus()]).
#' @param reference_date Date treated as "today" for future-date checks
#'   (default [Sys.Date()]).
#'
#' @return A named list (`summary` + one tibble per flagged check); see
#'   [checks_afp()].
#'
#' @examples
#' virus <- data.frame(id = c(1, 1), nt_changes = c(7, 7))
#' checks_virus(virus)$summary
#'
#' @export
checks_virus <- function(virus, reference_date = Sys.Date()) {
  .polis_check_checks_input(virus, "virus")
  .polis_run_checks(
    virus,
    .polis_checks_specs_virus(),
    .polis_checks_keys$virus,
    reference_date
  )
}

#' Run human-specimen data-quality checks
#'
#' Reads columns [clean_human_spec()] already produced to flag duplicate
#' specimens, blank collection dates, unreconciled GUIDs and inadequate
#' specimens. See [checks_afp()] for the return shape.
#'
#' @param hum_spec A cleaned human-specimen tibble (from [clean_human_spec()]).
#' @param reference_date Date treated as "today" for future-date checks
#'   (default [Sys.Date()]).
#'
#' @return A named list (`summary` + one tibble per flagged check); see
#'   [checks_afp()].
#'
#' @examples
#' hs <- data.frame(id = 1, specimen_id = c("S1"), collection_date = NA)
#' checks_hum_spec(hs)$summary
#'
#' @export
checks_hum_spec <- function(hum_spec, reference_date = Sys.Date()) {
  .polis_check_checks_input(hum_spec, "hum_spec")
  .polis_run_checks(
    hum_spec,
    .polis_checks_specs_hum_spec(),
    .polis_checks_keys$hum_spec,
    reference_date
  )
}

#' Run POLIS population data-quality checks
#'
#' Documents every POLIS-vs-WorldPop reconciliation issue [clean_pop()] found and
#' how each was resolved, as a `checks_*` result ready for [write_checks_excel()].
#' Unlike the other `checks_*()` functions it takes the **whole [clean_pop()]
#' list** (it reads `$adm2` and the `$meta` audit), not a single table.
#'
#' @param pop The list returned by [clean_pop()] (`$adm0`/`$adm1`/`$adm2`/`$meta`).
#' @param reference_date Unused; accepted for a uniform `checks_*()` signature.
#'
#' @return A named list: `summary` (one row per issue with `check`, `domain`,
#'   `severity`, `n_flagged`, `description`) followed by the detail tables
#'   (`conflicting_dups`, `age_violations`, `orphan_guids`, `ratio_outliers`,
#'   `coverage_by_country`, `overrides`, `source_mix`). Pass it to
#'   [write_checks_excel()].
#'
#' @seealso [clean_pop()].
#' @examples
#' pop_raw <- data.frame(
#'   PlaceId = "g-1", PlaceDisplayName = "X", Year = 2020,
#'   AgeGroupName = "0 to 15 years", Value = 1000, check.names = FALSE
#' )
#' checks_pop(clean_pop(pop_raw, years = 2020, verbose = FALSE))$summary
#'
#' @export
checks_pop <- function(pop, reference_date = Sys.Date()) {
  if (!is.list(pop) || is.null(pop$adm2) || is.null(pop$meta)) {
    cli::cli_abort(
      "{.arg pop} must be a {.fn clean_pop} result (with {.field adm2}/{.field meta})."
    )
  }
  audit <- pop$meta$audit %||% dplyr::tibble()
  adm2 <- pop$adm2
  empty <- dplyr::tibble()

  # 01: conflicting POLIS duplicates (same place-year, different values)
  conflicting_dups <- pop$meta$dup_conflicts %||% empty

  # 02: age-ordering breaches (u5 <= u15 <= all) reconciled to WorldPop
  age_violations <- if ("age_order_bad" %in% names(adm2)) {
    adm2 |>
      dplyr::filter(age_order_bad) |>
      dplyr::select(dplyr::any_of(c(
        "who_region",
        "country_iso3code",
        "adm0",
        "adm1",
        "adm2",
        "adm2_guid",
        "year",
        "u5_pop",
        "u15_pop",
        "all_pop",
        "u5_pop_wp",
        "u15_pop_wp",
        "all_pop_wp"
      )))
  } else {
    empty
  }

  # 03: orphan POLIS guids absent from the shape, + crosswalk outcome
  orphan_guids <- pop$meta$orphan_xwalk %||% empty

  # 04: POLIS values implausible vs WorldPop (severity = fold difference)
  ratio_outliers <- if ("bad_vs_worldpop" %in% names(audit)) {
    audit |>
      dplyr::filter(bad_vs_worldpop) |>
      dplyr::mutate(fold = round(pmax(wp_ratio, 1 / wp_ratio), 1)) |>
      dplyr::arrange(dplyr::desc(fold)) |>
      dplyr::select(dplyr::any_of(c(
        "age_group",
        "who_region",
        "country_iso3code",
        "adm0",
        "adm1",
        "adm2",
        "adm2_guid",
        "year",
        "pop_polis",
        "pop_wp",
        "wp_ratio",
        "fold",
        "source"
      )))
  } else {
    empty
  }

  # 05: coverage per country (districts: POLIS-sourced vs imputed)
  coverage_by_country <- if (all(c("age_group", "source") %in% names(audit))) {
    audit |>
      dplyr::filter(age_group == "u15") |>
      dplyr::summarise(
        districts = dplyr::n_distinct(adm2_guid),
        with_polis = dplyr::n_distinct(adm2_guid[source %in% "polis"]),
        worldpop = dplyr::n_distinct(adm2_guid[source %in% "worldpop"]),
        imputed_local = dplyr::n_distinct(
          adm2_guid[source %in% c("district_trend", "adm1", "adm0")]
        ),
        .by = dplyr::any_of(c("who_region", "country_iso3code", "adm0"))
      ) |>
      dplyr::arrange(dplyr::desc(districts - with_polis))
  } else {
    empty
  }

  # 06: overrides = POLIS had a value but it was rejected / replaced
  overrides <- if (all(c("source", "pop_polis") %in% names(audit))) {
    audit |>
      dplyr::filter(
        source %in% c("worldpop", "district_trend", "adm1", "adm0"),
        !is.na(pop_polis),
        pop_polis > 0
      ) |>
      dplyr::arrange(age_group, dplyr::desc(n_votes)) |>
      dplyr::select(dplyr::any_of(c(
        "age_group",
        "who_region",
        "country_iso3code",
        "adm0",
        "adm1",
        "adm2",
        "adm2_guid",
        "year",
        "pop",
        "source",
        "pop_polis",
        "pop_wp",
        "wp_ratio",
        "n_votes",
        "bad_vs_worldpop",
        "bad_vs_history",
        "bad_vs_adm1"
      )))
  } else {
    empty
  }

  # 07: chosen-source mix per age band
  source_mix <- if (all(c("age_group", "source") %in% names(audit))) {
    audit |>
      dplyr::count(age_group, source) |>
      dplyr::mutate(pct = round(100 * n / sum(n), 1), .by = age_group) |>
      dplyr::arrange(age_group, dplyr::desc(n))
  } else {
    empty
  }

  summary <- dplyr::bind_rows(
    .pop_check_row(
      "conflicting_dups",
      "warning",
      nrow(conflicting_dups),
      "POLIS place-years with >1 distinct value (collapsed to the median)"
    ),
    .pop_check_row(
      "age_violations",
      "warning",
      nrow(age_violations),
      "District-years where u5 <= u15 <= all was breached (reconciled to WorldPop)"
    ),
    .pop_check_row(
      "orphan_guids",
      "warning",
      if ("xwalk_status" %in% names(orphan_guids)) {
        sum(orphan_guids$xwalk_status != "resolved")
      } else {
        0L
      },
      "POLIS guids absent from the shape and not resolved by name crosswalk"
    ),
    .pop_check_row(
      "ratio_outliers",
      "info",
      nrow(ratio_outliers),
      "POLIS values implausible vs WorldPop (outside the ratio tolerance)"
    ),
    .pop_check_row(
      "overrides",
      "info",
      nrow(overrides),
      "Cells where a present POLIS value was rejected and imputed"
    )
  )

  detail <- list(
    conflicting_dups = conflicting_dups,
    age_violations = age_violations,
    orphan_guids = orphan_guids,
    ratio_outliers = ratio_outliers,
    coverage_by_country = coverage_by_country,
    overrides = overrides,
    source_mix = source_mix
  )
  detail <- detail[vapply(detail, function(x) nrow(x) > 0L, logical(1))]
  c(list(summary = summary), detail)
}

# One population-check summary row in the shared checks summary shape.
#' @noRd
.pop_check_row <- function(check, severity, n_flagged, description) {
  dplyr::tibble(
    check = check,
    domain = "population",
    severity = severity,
    n_flagged = as.integer(n_flagged),
    description = description
  )
}

utils::globalVariables(c(
  "age_order_bad",
  "bad_vs_worldpop",
  "wp_ratio",
  "fold",
  "age_group",
  "source",
  "adm2_guid",
  "pop_polis",
  "n_votes",
  "n"
))

# ---- excel export -----------------------------------------------------------

#' Write a checks result to an Excel workbook
#'
#' Turns the list returned by a `checks_*()` function into a single styled
#' `.xlsx` workbook: a `Summary` tab listing every applicable check and its
#' flagged-row count, followed by one tab of flagged rows per check that found
#' problems. Sheets get a navy header, sized columns and inferred number
#' formats. No versioning -- the file is written straight to `path`.
#'
#' @param checks A list returned by [checks_afp()] (or any `checks_*()`),
#'   carrying a `summary` element and zero or more flagged-row tibbles.
#' @param path Output `.xlsx` path.
#'
#' @return `path`, invisibly.
#'
#' @examples
#' \dontrun{
#' write_checks_excel(checks_afp(afp), "checks_afp.xlsx")
#' }
#'
#' @export
write_checks_excel <- function(checks, path) {
  if (!is.list(checks) || is.null(checks$summary)) {
    cli::cli_abort(
      "{.arg checks} must be a {.fn checks_*} result (with a `summary`)."
    )
  }
  detail <- checks[setdiff(names(checks), "summary")]
  sheets <- c(list(Summary = checks$summary), detail)
  .polis_write_xlsx(sheets, path)
  invisible(path)
}

# Run every applicable dataset's checks and write a checks_<key>.xlsx workbook
# directly into `dir` (the caller resolves the checks sub-directory). Returns the
# basenames written. Degrades to a no-op (with a warning) when the optional
# openxlsx package is absent, so it never aborts the pipeline.
.polis_write_check_workbooks <- function(cleaned, dir, reference_date) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_alert_warning(
      "Install {.pkg openxlsx} to write data-quality workbooks; skipping checks."
    )
    return(invisible(character(0)))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  fns <- list(
    afp = checks_afp,
    es = checks_es,
    hum_spec = checks_hum_spec,
    sia = checks_sia,
    virus = checks_virus,
    pop = checks_pop
  )
  written <- character(0)
  for (key in names(fns)) {
    data <- cleaned[[key]]
    # pop is a list (adm0/adm1/adm2/meta), not a single frame; the rest are
    # frames that must be non-empty to be worth a workbook.
    ok <- if (key == "pop") {
      is.list(data) && !is.null(data$adm2)
    } else {
      is.data.frame(data) && nrow(data) > 0L
    }
    if (!ok) {
      next
    }
    result <- fns[[key]](data, reference_date = reference_date)
    path <- file.path(dir, paste0("checks_", key, ".xlsx"))
    write_checks_excel(result, path)
    written <- c(written, basename(path))
  }
  invisible(written)
}
