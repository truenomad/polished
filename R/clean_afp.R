# =============================================================================
# Clean AFP (acute flaccid paralysis) case data
#
# Reads one POLIS Case table and returns a canonically-named, deduped, ordered
# analytic tibble. Like the other cleaners this is one linear recipe that runs
# standalone -- no archive, no other dataset, no files. On top of naming and
# dedup it derives the case-level analytic variables surveillance work needs
# (onset year, age in months, the standard date intervals, and stool
# timeliness / 60-day follow-up flags) directly from the canonical columns.
# =============================================================================

#' Clean POLIS AFP case data
#'
#' Standardises one raw POLIS case table and derives the case-level analytic
#' variables AFP surveillance relies on:
#' \itemize{
#'   \item canonical snake_case names (via the crosswalk + janitor) and
#'     merged-EPID remapping;
#'   \item parsed epidemiological/laboratory `*_date` columns;
#'   \item `year_onset` / `month_onset` from `paralysis_onset_date` (falling
#'     back to the stool-1 then notification year when onset is missing), and a
#'     numeric `age_months`;
#'   \item the standard onset-relative date intervals (`onset_to_notify`,
#'     `onset_to_invest`, `onset_to_stool1`, `onset_to_stool2`,
#'     `invest_to_stool1`, `stool1_to_stool2`, `notify_to_invest`,
#'     `onset_to_followup`), in days;
#'   \item the `stool1_missing` / `stool2_missing` / `stool_missing` flags;
#'   \item stool `timeliness` and the 60-day follow-up flags
#'     (`needs_60day_followup`, `got_60day_followup`, `followup_on_time`);
#'   \item the fused analytic classification `classification_all` (with its
#'     building blocks `vtype` / `vtype_fixed`) plus the Sabin flags
#'     `sabin1` / `sabin2` / `sabin3` and a recomputed `hot_case`;
#'   \item country-keyed enrichment from [polis_country_lookup()] --
#'     `country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2` -- the
#'     `polio_type` serotype, and the surveillance AFP flags `afp_class`, `afp`,
#'     `npafp` and `pending_results`;
#'   \item normalised admin names and one row per POLIS `id` (latest by
#'     `last_update_date`).
#' }
#' The raw POLIS `classification`, `polio_virus_types`, `vdpv_classifications`,
#' `adequate_stool` and `paralysis_hot_case` fields are kept as-is alongside the
#' derived columns. Records sharing the business key `epid` + `adm0` (the same
#' case re-entered under a new POLIS Id) are collapsed to the latest by
#' `last_update_date`; a tripwire then flags any key still spanning multiple Ids
#' to QA, never dropping it.
#'
#' @param data A raw POLIS case data frame.
#' @param cfg A [polis_config()] object (default `polis_config()`). Supply
#'   `cfg$synonyms` to remap merged EPIDs and `cfg$qa` to route ambiguity flags.
#' @param shape Optional district shape that drives admin recovery. Either form
#'   works and a single input does everything:
#'   \itemize{
#'     \item a polygon layer (`spatial_global_adm2`, an `sf` object) -- its long
#'       form is derived here (as [process_spatial()] does) for the GUID/name
#'       reconcile via [reconcile_admin_guids()], and its geometry drives
#'       coordinate recovery via [impute_geo_from_coords()] for cases still
#'       missing a district but carrying coordinates;
#'     \item a long ADM2 attribute table (`spatial_adm2_long_shape`) -- reconcile
#'       only, since it has no geometry for the coordinate step.
#'   }
#'   Reconciliation adds a `geo_source` column. Default `NULL` (no shape-based
#'   recovery).
#' @param impute_geo If `TRUE` (default) cases still missing `adm1`/`adm2` (and
#'   their GUIDs) after reconciliation have them recovered from the EPID prefix
#'   via [impute_geo_from_epid()] (self-reference + prefix matching, every row
#'   kept). Adds `*_source` provenance columns.
#' @param verbose Emit cli progress messages for each phase. Default `TRUE`.
#'
#' @return A tibble of cleaned AFP records, one row per POLIS `id` (and at most
#'   one per `epid` + `adm0` business key after the duplicate collapse), with
#'   columns ordered id -> location -> time -> other. The canonical and derived
#'   columns
#'   (`year_onset`, `month_onset`, `age_months`, the `*_to_*` intervals,
#'   `onset_date_quality`, `timeliness` and the 60-day follow-up flags) are added
#'   only when their prerequisite source columns are present in `data`, so a
#'   trimmed input yields a correspondingly trimmed output rather than an error.
#'
#' @examples
#' raw <- data.frame(
#'   Id = c(1, 1, 2),
#'   Epid = c("A-1", "A-1", "B-2"),
#'   LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
#'   ParalysisOnsetDate = c("2024-01-02", "2024-01-02", "2024-02-03"),
#'   NotificationDate = c("2024-01-05", "2024-01-05", "2024-02-06"),
#'   Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_afp(raw)
#'
#' @export
clean_afp <- function(
  data,
  cfg = polis_config(),
  shape = NULL,
  impute_geo = TRUE,
  verbose = TRUE
) {
  # each call to step() marks the previous step done (past-tense tick) and starts
  # the next: present-continuous while running, `done` shown on the tick.
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
  n_in <- .polis_big_num(if (is.data.frame(data)) nrow(data) else 0L)

  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(data, "afp")
  step(
    "Standardising names on {n_in} rows",
    "Standardised names on {n_in} rows"
  )
  data <- data |>
    standardise_names(cfg$crosswalk) |>
    remap_synonyms(cfg$synonyms) |>
    .polis_clean_strings()

  # ---- derive analytic variables --------------------------------------------
  # dates -> onset/age -> coherence -> intervals -> stool flags -> timeliness,
  # each tolerant of a partial schema so the cleaner still runs on a trimmed
  # case table. Garbage values are set to NA at every step (never dropped, never
  # clamped) so they cannot propagate into impossible intervals, ages or
  # timeliness verdicts.
  step(
    "Parsing dates and deriving onset/age/intervals/timeliness",
    "Parsed dates and derived onset/age/intervals/timeliness"
  )
  data <- data |>
    .afp_parse_dates() |>
    .afp_add_onset_vars() |>
    .afp_add_onset_quality() |>
    .afp_add_intervals() |>
    .afp_add_stool_flags()
  step(
    "Classifying virus type and case classification",
    "Classified virus type and case classification"
  )
  data <- clean_afp_classification(data) |>
    .afp_add_timeliness()

  # ---- standardise geography ------------------------------------------------
  # `shape` may be a long ADM2 attribute table or a polygon layer. A polygon
  # serves both: its long form (built here, like process_spatial does) drives
  # the GUID reconcile, and its geometry drives coordinate recovery.
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data)
  long_shape <- NULL
  poly_shape <- NULL
  if (!is.null(shape)) {
    if (inherits(shape, "sf")) {
      poly_shape <- shape
      step(
        "Building the long district lookup from the shape",
        "Built the long district lookup from the shape"
      )
      long_shape <- create_long_shape(shape, "adm2")
    } else {
      long_shape <- shape
    }
  }
  if (!is.null(long_shape)) {
    step(
      "Reconciling admin GUIDs against the district shape",
      "Reconciled admin GUIDs against the district shape"
    )
    data <- reconcile_admin_guids(data, long_shape, verbose = FALSE)
  }
  # Coordinates first: the point-in-polygon fill is fast and geometry beats
  # prefix-guessing, so it clears the bulk of the gaps before the (slow) EPID
  # prefix match, which then only has to handle the small remainder.
  if (!is.null(poly_shape) && "adm2_guid" %in% names(data)) {
    nc_before <- .geo_miss_admin(data)
    n_crec <- "0"
    step(
      "Recovering missing admin from coordinates",
      "Recovered admin for {n_crec} cases from coordinates"
    )
    data <- impute_geo_from_coords(data, poly_shape, verbose = FALSE)
    n_crec <- .polis_big_num(max(nc_before - .geo_miss_admin(data), 0L))
  }
  if (isTRUE(impute_geo)) {
    na2_before <- if ("adm2" %in% names(data)) sum(is.na(data$adm2)) else 0L
    # n_rec is filled below and glued into msg_done when the step ticks.
    n_rec <- "0"
    step(
      "Recovering missing admin from the EPID",
      "Recovered admin for {n_rec} cases from the EPID"
    )
    data <- .afp_impute_geo(data, verbose = verbose)
    na2_after <- if ("adm2" %in% names(data)) sum(is.na(data$adm2)) else 0L
    n_rec <- .polis_big_num(max(na2_before - na2_after, 0L))
  }

  # ---- enrich: country groupings, polio type, AFP flags ---------------------
  step(
    "Enriching with country groupings and AFP flags",
    "Enriched with country groupings and AFP flags"
  )
  data <- .afp_enrich(data)

  # ---- finalise: dedup by id, infer types, assert key, order ----------------
  step("Deduplicating by id and finalising", "Deduplicated by id and finalised")
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    collapse_business_key(
      key = c("epid", "adm0"),
      date = "last_update_date",
      verbose = verbose
    ) |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    .geo_guid_display_cols() |>
    flag_ambiguous(key = c("epid", "adm0"), sink = cfg$qa) |>
    order_columns(cfg$column_roles)
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    out_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Cleaned {out_fmt} AFP cases.")
  }
  out
}

#' Recover missing admin names/GUIDs from the EPID prefix (in place)
#'
#' Cases whose district GUID is absent often still carry an EPID and admin
#' names; [impute_geo_from_epid()] fills missing `adm1`/`adm2` (and their GUIDs)
#' from the EPID prefix via self-reference and prefix-matching, keeping every
#' row. A no-op when the EPID or admin columns are absent.
#' @noRd
.afp_impute_geo <- function(data, verbose = FALSE) {
  required <- c("epid", "adm0", "adm1", "adm2", "year_onset")
  if (!all(required %in% names(data))) {
    return(data)
  }
  guid_vars <- c(adm1 = "adm1_guid", adm2 = "adm2_guid")
  guid_vars <- guid_vars[guid_vars %in% names(data)]
  res <- impute_geo_from_epid(
    data,
    year_var = "year_onset",
    guid_vars = if (length(guid_vars) > 0) guid_vars else NULL,
    audit = FALSE,
    verbose = FALSE
  )
  res$data
}

#' POLIS case date columns parsed to `Date`
#'
#' Every genuine date on the case table, so the output is type-consistent. The
#' first block (onset .. spec-received) are the epidemiological/laboratory dates
#' that feed onset, interval and timeliness logic; the rest are peripheral dates
#' normalised for convenience. Audit timestamps (`created_date`,
#' `last_update_date`, `publish_date`) are deliberately excluded -- in
#' particular `last_update_date` stays a sortable ISO string for the keep-latest
#' dedup.
#' @noRd
.afp_date_cols <- function() {
  c(
    # analytic: consumed by the derived onset/interval/timeliness variables
    "paralysis_onset_date",
    "notification_date",
    "investigation_date",
    "stool1collection_date",
    "stool2collection_date",
    "followup_date",
    "clinical_admitted_date",
    "stool_date_sent_to_lab",
    "stool_date_sent_to_ic_lab",
    "spec_date_received_by_nat_lab",
    # peripheral: type-normalised only
    "case_date",
    "epi_date_isolation_results_received",
    "epi_date_itd_results_received",
    "vdpv_classification_change_date",
    "pons_spec_date",
    "pons_receipt_date",
    "pons_on_set_date",
    "positive_contact_stool1collection_date",
    "doses_date_of1st",
    "doses_date_of2nd",
    "doses_date_of3rd",
    "doses_date_of4th",
    "doses_opv_dateof_last",
    "doses_ipv_dateof_last"
  )
}

#' Garbage floor for the "sensible date" test (pre-AFP-surveillance era).
#'
#' A clearly-impossible threshold for catching data-entry errors only. It is
#' deliberately NOT the analytic `start_year`: cleaning preserves every real
#' date, and the year-of-interest filter belongs in the indicator layer.
#' @noRd
.afp_min_sensible_year <- 1980L

#' Parse AFP date columns to `Date`, NA-ing implausible values
#'
#' Parses each date column then nulls any value outside the plausible window
#' `[min_year-01-01, reference_date]` -- the "sensible date" rule: a date before
#' the dawn of AFP surveillance or in the future is a data-entry error, not a
#' real observation, so it is set to `NA` before any interval is derived from it.
#' @noRd
.afp_parse_dates <- function(
  data,
  min_year = .afp_min_sensible_year,
  reference_date = Sys.Date()
) {
  cols <- union(
    intersect(.afp_date_cols(), names(data)),
    grep("^date_", names(data), value = TRUE)
  )
  if (length(cols) == 0) {
    return(data)
  }
  floor_date <- lubridate::make_date(min_year, 1L, 1L)
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      # as_date tolerates both ISO date and datetime strings
      \(x)
        .afp_sensible_date(
          suppressWarnings(lubridate::as_date(x)),
          floor_date,
          reference_date
        )
    )
  )
}

#' NA a parsed date that falls outside the plausible surveillance window
#' @noRd
.afp_sensible_date <- function(x, floor_date, reference_date) {
  dplyr::if_else(x >= floor_date & x <= reference_date, x, lubridate::NA_Date_)
}

#' Derive onset year/month (with fallback) and numeric age in months
#' @noRd
.afp_add_onset_vars <- function(data) {
  if ("paralysis_onset_date" %in% names(data)) {
    # fall back to the nearest available date so a case with any date still gets
    # a year. The first two (stool 1 -> notification) reproduce the upstream
    # cascade exactly; stool 2 and investigation only fire when those are all
    # missing, as a last-resort rescue rather than an NA.
    fallbacks <- intersect(
      c(
        "stool1collection_date",
        "notification_date",
        "stool2collection_date",
        "investigation_date"
      ),
      names(data)
    )
    year_onset <- lubridate::year(data$paralysis_onset_date)
    for (col in fallbacks) {
      year_onset <- dplyr::if_else(
        is.na(year_onset),
        lubridate::year(data[[col]]),
        year_onset
      )
    }
    data <- dplyr::mutate(
      data,
      year_onset = year_onset,
      month_onset = lubridate::month(paralysis_onset_date)
    )
  }
  age_sources <- intersect(
    c("calculated_age_in_month", "person_age_in_months"),
    names(data)
  )
  if (length(age_sources) > 0) {
    # POLIS age arrives as character; non-numeric entries coerce to NA, which is
    # exactly the wanted behaviour, so the coercion warning is suppressed.
    age_months <- suppressWarnings(as.numeric(data[[age_sources[1]]]))
    for (col in age_sources[-1]) {
      age_months <- dplyr::coalesce(
        age_months,
        suppressWarnings(as.numeric(data[[col]]))
      )
    }
    # NA physiologically impossible ages (< 0 or > 110 years). The <15y (180
    # month) surveillance cut stays in the indicator layer so adult AFP cases
    # survive cleaning.
    data$age_months <- dplyr::if_else(
      age_months >= 0 & age_months <= .afp_max_age_months,
      age_months,
      NA_real_
    )
  }
  data
}

#' Physiological upper bound for age in months (110 years = 1320 months).
#' @noRd
.afp_max_age_months <- 1320

#' Add one onset-relative interval (in days), NA outside its plausible window
#'
#' Computes `to - from` in days and nulls any result outside `[lo, hi]`. The
#' window encodes each pair's directionality (forward-from-onset intervals start
#' at 0; all are capped at a year, the gross data-entry-error threshold), so the
#' same mechanism sanitises every interval consistently.
#' @noRd
.afp_days_between <- function(data, name, to, from, lo, hi) {
  if (!all(c(to, from) %in% names(data))) {
    return(data)
  }
  dplyr::mutate(
    data,
    "{name}" := .afp_window(
      as.numeric(.data[[to]] - .data[[from]]),
      lo,
      hi
    )
  )
}

#' NA a numeric value outside `[lo, hi]`
#' @noRd
.afp_window <- function(x, lo, hi) {
  dplyr::if_else(x >= lo & x <= hi, x, NA_real_)
}

#' Add the standard AFP date intervals (sanitised to plausible windows)
#' @noRd
.afp_add_intervals <- function(data) {
  data |>
    .afp_days_between(
      "onset_to_notify",
      "notification_date",
      "paralysis_onset_date",
      0,
      365
    ) |>
    .afp_days_between(
      "onset_to_invest",
      "investigation_date",
      "paralysis_onset_date",
      0,
      365
    ) |>
    .afp_days_between(
      "onset_to_stool1",
      "stool1collection_date",
      "paralysis_onset_date",
      0,
      365
    ) |>
    .afp_days_between(
      "onset_to_stool2",
      "stool2collection_date",
      "paralysis_onset_date",
      0,
      365
    ) |>
    .afp_days_between(
      "notify_to_invest",
      "investigation_date",
      "notification_date",
      0,
      365
    ) |>
    # stool can legitimately precede formal investigation, so allow negatives
    .afp_days_between(
      "invest_to_stool1",
      "stool1collection_date",
      "investigation_date",
      -365,
      365
    ) |>
    .afp_days_between(
      "stool1_to_stool2",
      "stool2collection_date",
      "stool1collection_date",
      0,
      365
    ) |>
    .afp_days_between(
      "onset_to_followup",
      "followup_date",
      "paralysis_onset_date",
      0,
      400
    )
}

#' Flag stool specimens with neither a collection date nor a condition
#'
#' A specimen is treated as missing only when both its collection date and its
#' condition are absent, so a specimen with one field recorded still counts.
#' @noRd
.afp_add_stool_flags <- function(data) {
  stool_missing <- function(date_col, condition_col) {
    if (!all(c(date_col, condition_col) %in% names(data))) {
      return(NULL)
    }
    is.na(data[[date_col]]) & is.na(data[[condition_col]])
  }
  s1 <- stool_missing("stool1collection_date", "stool1condition")
  s2 <- stool_missing("stool2collection_date", "stool2condition")
  if (!is.null(s1)) data$stool1_missing <- s1
  if (!is.null(s2)) data$stool2_missing <- s2
  if (!is.null(s1) && !is.null(s2)) data$stool_missing <- s1 & s2
  data
}

#' Derive the fused AFP virus type and analytic classification
#'
#' Decodes the specific poliovirus and fuses it with the case classification
#' into one analytic label, using standard **WPV** (wild poliovirus)
#' nomenclature throughout -- *not* the legacy `WILD n` strings, which do not
#' match how downstream surveillance code filters (`grepl("WPV|cVDPV", ...)`).
#'
#' Two layers:
#'
#' 1. `vtype` / `vtype_fixed` decode the virus from `polio_virus_types` +
#'    `vdpv_classifications`. A VDPV always carries an explicit kind prefix --
#'    `cVDPV` (circulating), `aVDPV` (ambiguous), `iVDPV` (immune-deficient) --
#'    so the three are never merged or silently dropped; an untyped `VDPV n`
#'    only remains when the kind is genuinely unknown. A few historical country
#'    corrections patch early records (Congo 2010, Nigeria 2011, pre-2010 wild)
#'    where the virus field was not yet populated.
#' 2. `classification_all` is the single analysis label: the `vtype_fixed` virus
#'    string for virus-positive cases, otherwise the raw POLIS `classification`
#'    recoded -- Discarded -> NPAFP, Compatible -> COMPATIBLE, Not an
#'    AFP -> NOT-AFP, Pending -> PENDING (LAB PENDING when the specimen never
#'    reached the lab), VAPP -> VAPP, Not Applicable/Others/VDPV -> UNKNOWN.
#'    Cases matching none stay `none`/`NA` for manual review.
#'
#' @section Classification vocabulary (match on these prefixes, not free text):
#' \describe{
#'   \item{Wild}{`WPV 1`, `WPV 2`, `WPV 3`, `WPV1andWPV3` -- prefix `WPV`.}
#'   \item{Circulating VDPV}{`cVDPV 1/2/3` and combinations -- prefix `cVDPV`.}
#'   \item{Ambiguous VDPV}{`aVDPV 1/2/3` -- prefix `aVDPV`. **Include/exclude is
#'     a deliberate analyst choice**; these are labelled, never folded into
#'     `cVDPV`.}
#'   \item{Immune-deficient VDPV}{`iVDPV 1/2/3` -- prefix `iVDPV`. Same explicit
#'     choice as `aVDPV`.}
#'   \item{Untyped VDPV}{`VDPV 1/2/3` -- a VDPV whose kind is unknown.}
#'   \item{Wild + VDPV co-detection}{`WPV1and...` (e.g. `WPV1andcVDPV 2`).}
#'   \item{Non-virus}{`NPAFP`, `COMPATIBLE`, `NOT-AFP`, `PENDING`,
#'     `LAB PENDING`, `VAPP`, `UNKNOWN`.}
#' }
#' So "any WPV1" is `grepl("^WPV 1|^WPV1and", classification_all)`, and
#' "any circulating VDPV2" is `grepl("cVDPV 2", classification_all)`.
#'
#' Also derives the Sabin-detection flags (`sabin1`/`sabin2`/`sabin3`) and,
#' where the paralysis fields are present, a recomputed `hot_case` (POLIS also
#' ships `paralysis_hot_case`; this applies the standard
#' asymmetric + onset-fever + rapid-progression definition, which can differ).
#' Every condition is NA-safe: a missing classification/admin/year leaves the
#' prior value intact rather than nulling it.
#'
#' @param data A cleaned AFP data frame carrying at least `classification`
#'   (and ideally `polio_virus_types`, `vdpv_classifications`).
#'
#' @return `data` with `vtype`, `vtype_fixed`, `classification_all`,
#'   `sabin1`/`sabin2`/`sabin3` and (when derivable) `hot_case` added; the raw
#'   `classification`, `polio_virus_types` and `vdpv_classifications` columns are
#'   left untouched.
#'
#' @examples
#' clean_afp_classification(data.frame(
#'   classification = c("Discarded", "Confirmed (wild)"),
#'   polio_virus_types = c(NA, "WILD1"),
#'   vdpv_classifications = c(NA, NA)
#' ))
#'
#' @export
clean_afp_classification <- function(data) {
  if (!"classification" %in% names(data)) {
    return(data)
  }
  n <- nrow(data)
  classification <- data$classification
  pvt <- if ("polio_virus_types" %in% names(data)) {
    data$polio_virus_types
  } else {
    rep(NA_character_, n)
  }
  vdpv <- if ("vdpv_classifications" %in% names(data)) {
    data$vdpv_classifications
  } else {
    rep(NA_character_, n)
  }
  vdpv <- dplyr::if_else(is.na(vdpv), "", vdpv)
  # str_detect propagates NA (a missing virus field yields an NA vtype, which the
  # historical fixes below key on); a missing column is treated as all-NA.
  has <- function(pattern) stringr::str_detect(pvt, pattern)
  # coalesce a condition's NAs to FALSE so a missing input never overwrites a
  # value that was already assigned.
  safe <- function(cond) dplyr::coalesce(cond, FALSE)

  # ---- layer 1: specific virus type (WPV nomenclature) ---------------------
  # detection runs on the raw POLIS strings ("WILD1", ...), but the emitted
  # label uses WPV so it matches standard surveillance vocabulary.
  # compact wild label, kept so a wild+VDPV co-detection names the real serotype
  wild <- dplyr::case_when(
    has("WILD1") & has("WILD3") ~ "WPV1andWPV3",
    has("WILD3") ~ "WPV3",
    has("WILD2") ~ "WPV2",
    has("WILD1") ~ "WPV1",
    TRUE ~ NA_character_
  )
  vtype <- dplyr::if_else(has("WILD1"), "WPV 1", "none")
  vtype <- dplyr::if_else(has("WILD2"), "WPV 2", vtype)
  vtype <- dplyr::if_else(has("WILD3"), "WPV 3", vtype)
  vtype <- dplyr::if_else(has("WILD1") & has("WILD3"), "WPV1andWPV3", vtype)
  vtype <- dplyr::if_else(has("VDPV1"), "VDPV 1", vtype)
  vtype <- dplyr::if_else(has("VDPV2"), "VDPV 2", vtype)
  vtype <- dplyr::if_else(has("VDPV3"), "VDPV 3", vtype)
  vtype <- dplyr::if_else(has("VDPV1") & has("VDPV2"), "VDPV1andVDPV2", vtype)
  vtype <- dplyr::if_else(has("VDPV1") & has("VDPV3"), "VDPV1andVDPV3", vtype)
  vtype <- dplyr::if_else(has("VDPV2") & has("VDPV3"), "VDPV2andVDPV3", vtype)
  vtype <- dplyr::if_else(
    has("VDPV1") & has("VDPV2") & has("VDPV3"),
    "VDPV12and3",
    vtype
  )
  # a combined wild + VDPV detection -> "<wild>and<vdpv>"
  vtype <- dplyr::if_else(
    has("WILD") & has("VDPV"),
    paste0(wild, "and", vtype),
    vtype
  )
  # circulating / ambiguous / immune-deficient prefix
  vtype <- dplyr::if_else(
    vdpv == "Ambiguous" & !is.na(vtype),
    paste0("a", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    stringr::str_detect(vdpv, "Circulating") & !is.na(vtype),
    paste0("c", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    vdpv == "Immune Deficient" & !is.na(vtype),
    paste0("i", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    vtype %in% c("cnone", "anone", "inone"),
    "none",
    vtype
  )
  # move the kind prefix off the wild stem onto the VDPV component of a
  # co-detection (cWPV1andVDPV 2 -> WPV1andcVDPV 2), for any wild serotype
  vtype <- stringr::str_replace(vtype, "^([cai])(WPV.*?and)(VDPV)", "\\2\\1\\3")
  vtype <- dplyr::if_else(vtype == "cVDPV2andVDPV3", "cVDPV2andcVDPV3", vtype)

  # ---- vtype_fixed: historical country corrections -------------------------
  year_onset <- if ("year_onset" %in% names(data)) {
    data$year_onset
  } else {
    rep(NA_integer_, n)
  }
  adm0 <- if ("adm0" %in% names(data)) data$adm0 else rep(NA_character_, n)
  is_wild_conf <- classification == "Confirmed (wild)"
  vtype_fixed <- dplyr::if_else(
    safe(is_wild_conf & adm0 == "CONGO" & year_onset == 2010),
    "WPV 1",
    vtype
  )
  vtype_fixed <- dplyr::if_else(
    safe(is.na(vtype) & adm0 == "NIGERIA" & year_onset == 2011 & is_wild_conf),
    "WPV 1",
    vtype_fixed
  )
  vtype_fixed <- dplyr::if_else(
    safe(is.na(vtype) & year_onset < 2010 & is_wild_conf),
    "WPV 1",
    vtype_fixed
  )
  vtype_fixed <- dplyr::if_else(
    safe(vtype == "cVDPV1andVDPV2" & has("cVDPV2")),
    "VDPV1andcVDPV2",
    vtype_fixed
  )
  vtype_fixed <- dplyr::if_else(
    safe(vtype == "cVDPV1andVDPV2" & has("cVDPV2") & has("cVDPV1")),
    "cVDPV1andcVDPV2",
    vtype_fixed
  )

  # ---- layer 2: fuse virus type with recoded classification ----------------
  none_or_na <- is.na(vtype_fixed) | vtype_fixed == "none"
  classification_all <- vtype_fixed
  recodes <- c(
    "Compatible" = "COMPATIBLE",
    "Discarded" = "NPAFP",
    "Not an AFP" = "NOT-AFP",
    "Pending" = "PENDING",
    "VAPP" = "VAPP"
  )
  for (raw in names(recodes)) {
    classification_all <- dplyr::if_else(
      safe(none_or_na & classification == raw),
      recodes[[raw]],
      classification_all
    )
  }
  classification_all <- dplyr::if_else(
    safe(
      none_or_na & classification %in% c("Not Applicable", "Others", "VDPV")
    ),
    "UNKNOWN",
    classification_all
  )
  if ("final_culture_result" %in% names(data)) {
    classification_all <- dplyr::if_else(
      safe(
        data$final_culture_result == "Not received in lab" &
          classification_all == "PENDING"
      ),
      "LAB PENDING",
      classification_all
    )
  }

  # ---- Sabin flags + recomputed hot case -----------------------------------
  data$vtype <- vtype
  data$vtype_fixed <- vtype_fixed
  data$classification_all <- classification_all
  data$sabin1 <- dplyr::if_else(has("VACCINE1"), 1L, 0L)
  data$sabin2 <- dplyr::if_else(has("VACCINE2"), 1L, 0L)
  data$sabin3 <- dplyr::if_else(has("VACCINE3"), 1L, 0L)
  hot_fields <- c(
    "paralysis_asymmetric",
    "paralysis_onset_fever",
    "paralysis_rapid_progress"
  )
  if (all(hot_fields %in% names(data))) {
    data$hot_case <- dplyr::if_else(
      data$paralysis_asymmetric == "Yes" &
        data$paralysis_onset_fever == "Yes" &
        data$paralysis_rapid_progress == "Yes",
      1L,
      0L
    )
  }
  data
}

#' Flag onset-date coherence relative to notification/investigation
#'
#' Derived straight from the dates (not the sanitised intervals, which have
#' already nulled negatives) so the incoherence signal survives to drive the
#' timeliness verdict.
#' @noRd
.afp_add_onset_quality <- function(data) {
  if (!"paralysis_onset_date" %in% names(data)) {
    return(data)
  }
  onset <- data$paralysis_onset_date
  # precompute the comparisons outside case_when: a missing column yields an
  # all-FALSE vector rather than an evaluation error.
  before_onset <- function(col) {
    if (!col %in% names(data)) {
      return(rep(FALSE, length(onset)))
    }
    !is.na(data[[col]]) & data[[col]] < onset
  }
  data$onset_date_quality <- dplyr::case_when(
    is.na(onset) ~ "Missing onset",
    before_onset("notification_date") ~ "Onset after notification",
    before_onset("investigation_date") ~ "Onset after investigation",
    TRUE ~ "Good"
  )
  data
}

#' Derive stool-collection timeliness and the 60-day follow-up flags
#'
#' `timeliness` follows the standard adequate-interval rule: stool 1 within
#' 0-13 days of onset, stool 2 within 1-14 days, and stool 2 at least a day
#' after stool 1. Cases whose onset date is missing or incoherent (onset after
#' notification/investigation) are scored `"Unable to Assess"` rather than off
#' garbage dates. Cases not collected timely should receive a 60-day follow-up;
#' `followup_on_time` checks that visit landed 60-90 days after onset.
#' @noRd
.afp_add_timeliness <- function(data) {
  interval_cols <- c(
    "onset_to_stool1",
    "onset_to_stool2",
    "stool1_to_stool2"
  )
  if (!all(interval_cols %in% names(data))) {
    return(data)
  }
  incoherent <- if ("onset_date_quality" %in% names(data)) {
    data$onset_date_quality != "Good"
  } else {
    rep(FALSE, nrow(data))
  }
  data <- dplyr::mutate(
    data,
    timeliness = dplyr::case_when(
      incoherent ~ "Unable to Assess",
      onset_to_stool1 >= 0 &
        onset_to_stool1 <= 13 &
        onset_to_stool2 >= 1 &
        onset_to_stool2 <= 14 &
        stool1_to_stool2 >= 1 ~
        "Timely",
      TRUE ~ "Not Timely"
    ),
    needs_60day_followup = timeliness == "Not Timely"
  )
  if ("onset_to_followup" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      got_60day_followup = dplyr::if_else(
        needs_60day_followup,
        !is.na(onset_to_followup),
        NA
      ),
      followup_on_time = dplyr::if_else(
        needs_60day_followup,
        !is.na(onset_to_followup) &
          onset_to_followup >= 60 &
          onset_to_followup <= 90,
        NA
      )
    )
  }
  data
}

#' Enrich cleaned AFP data with country groupings, polio type and AFP flags
#'
#' The always-on enrichment layer: joins the country reference, reads the polio
#' serotype off the classification, and derives the surveillance AFP flags. Each
#' piece is a no-op when its source columns are absent, so a trimmed input still
#' passes through.
#' @noRd
.afp_enrich <- function(data) {
  data |>
    .polis_join_country() |>
    .polis_polio_type() |>
    .afp_enrich_flags()
}

#' Derive the surveillance AFP flags (afp_class / afp / npafp / pending_results)
#'
#' `afp_class` buckets AFP-surveillance cases by their raw `classification`
#' (AFP-Positive / Non-polio AFP / Not an AFP / VAPP / Others); `afp` and
#' `npafp` are its binary cuts and `pending_results` flags pending lab results.
#' A no-op when `classification` is absent.
#' @noRd
.afp_enrich_flags <- function(
  data,
  class_var = "classification",
  virus_var = "polio_virus_types",
  surv_var = "surveillance_type_name"
) {
  if (!class_var %in% names(data)) {
    return(data)
  }
  cls <- data[[class_var]]
  surv <- if (surv_var %in% names(data)) {
    data[[surv_var]]
  } else {
    rep(NA_character_, nrow(data))
  }
  virus <- if (virus_var %in% names(data)) {
    data[[virus_var]]
  } else {
    rep(NA_character_, nrow(data))
  }
  is_afp <- !is.na(surv) & surv == "AFP"
  data$afp_class <- dplyr::case_when(
    is_afp & cls %in% c("Compatible", "Confirmed (wild)") ~ "AFP-Positive",
    is_afp & cls == "Discarded" ~ "Non-polio AFP",
    is_afp & cls == "Not an AFP" ~ "Not an AFP",
    is_afp & cls == "VAPP" ~ "VAPP",
    TRUE ~ "Others"
  )
  data$afp <- dplyr::if_else(data$afp_class == "AFP-Positive", 1L, 0L)
  # non-polio AFP: discarded/pending and not a circulating VDPV (cVDPV1/2/3). The
  # "c" prefix is only on the derived `classification_all`, not raw virus types.
  circulating <- if ("classification_all" %in% names(data)) {
    data[["classification_all"]]
  } else {
    virus
  }
  data$npafp <- dplyr::if_else(
    cls %in%
      c("Discarded", "Pending") &
      !stringr::str_detect(dplyr::coalesce(circulating, ""), "cVDPV ?[123]"),
    1L,
    0L
  )
  data$pending_results <- cls %in% "Pending"
  data
}
