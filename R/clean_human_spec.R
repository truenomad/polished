# =============================================================================
# Clean human specimen (laboratory) data
#
# Reads one POLIS LabSpecimen table -- the per-specimen laboratory record (one
# row per stool specimen, with cell-culture / ITD / sequencing results and
# dates) -- and returns a canonically-named, deduped, ordered analytic tibble.
# It is the specimen-level companion to clean_afp() (which cleans the case-level
# linelist): where the case table summarises stool 1/2, this captures every
# specimen, including contact/community ones the case table never carries. Like
# the other cleaners it runs standalone and derives the same AFP-vocabulary virus
# classification, reusing the shared lab-virus classifier.
# =============================================================================

#' Clean POLIS human specimen (laboratory) data
#'
#' Standardises one raw POLIS lab-specimen table and derives the specimen-level
#' analytic variables surveillance relies on:
#' \itemize{
#'   \item canonical snake_case names (via the crosswalk + janitor);
#'   \item every collection/laboratory date parsed to `Date` and sanitised with
#'     the same "sensible date" rule [clean_afp()] uses -- a value before the
#'     dawn of surveillance (`min_year`) or in the future is a data-entry error
#'     and is set to `NA`, never dropped (audit timestamps such as
#'     `last_update_date` stay ISO strings for the keep-latest dedup);
#'   \item `year_collection` / `month_collection` from the sanitised
#'     `date_stool_collected`, plus the lab-turnaround intervals (in days)
#'     `collect_to_lab`, `lab_to_culture`, `culture_to_itd`, `sent_to_seq_result`
#'     and `collect_to_seq` -- the specimen timeliness measure (specimens carry
#'     no onset date, so this replaces clean_afp()'s onset-relative intervals);
#'   \item the AFP-style virus classification (via the shared lab-virus
#'     classifier, the same engine [clean_es_classification()] uses): a
#'     normalised `virus_type` list, `vtype` and the fused `classification_all`
#'     label in the **same** `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary the AFP and
#'     ES cleaners emit, the per-serotype Sabin flags `sabin1`/`sabin2`/`sabin3`,
#'     the `npev` / `nvaccine` flags and the fused `ev_detect` flag;
#'   \item the specimen `adequate` flag (`1`/`0` from `adequate_specimen`),
#'     alongside the raw `specimen_stool_condition_name` and
#'     `adequate_specimen_with_condition`;
#'   \item country-keyed enrichment from [polis_country_lookup()]
#'     (`country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2`) and the
#'     `polio_type` serotype;
#'   \item the same applicable geography cleaning as [clean_afp()]: normalised
#'     admin names, GUID reconciliation against `shape` and EPID-prefix admin
#'     recovery (both keyed on `year_collection`); coordinate recovery does not
#'     apply as specimens carry no coordinates;
#'   \item normalised admin names and one row per POLIS `id` (latest by
#'     `last_update_date`).
#' }
#' The raw POLIS `virus_types`, `vdpv_classification`, the per-serotype result
#' fields and the lab-result columns are kept as-is alongside the derived
#' columns. The business key `specimen_id` + `adm0` is asserted as a tripwire:
#' violations are flagged to QA, never dropped.
#'
#' @param data A raw POLIS lab-specimen data frame.
#' @param cfg A [polis_config()] object (default `polis_config()`). Supply
#'   `cfg$qa` to route ambiguity flags.
#' @param shape Optional district shape used to reconcile admin names/GUIDs via
#'   [reconcile_admin_guids()] (keyed on `year_collection`), exactly as
#'   [clean_afp()] uses it -- a long ADM2 attribute table or the polygon layer
#'   (the polygon is expanded to its long form here). Default `NULL`.
#' @param impute_geo If `TRUE` (default) specimens still missing `adm1`/`adm2`
#'   (and their GUIDs) after reconciliation have them recovered from the EPID
#'   prefix via [impute_geo_from_epid()], keyed on `year_collection`.
#' @param cases Optional cleaned AFP table (from [clean_afp()]). A specimen
#'   reuses its parent case's EPID, so the case's fully-recovered geography is
#'   the authoritative source for a specimen that carries no district of its own:
#'   when supplied, the case `adm1`/`adm2` (and their GUIDs) fill blank specimen
#'   cells by exact EPID match (a fast direct join, before the specimen-internal
#'   prefix match). Default `NULL`.
#' @param verbose Emit cli progress messages for each phase. Default `TRUE`.
#'
#' @return A tibble of cleaned specimen records, one row per POLIS `id`, with
#'   columns ordered identically to [clean_afp()] (id -> location -> time ->
#'   classification -> dates -> other). The derived columns (`year_collection`,
#'   `month_collection`, `collect_to_lab`, `lab_to_culture`, `culture_to_itd`,
#'   `sent_to_seq_result`, `collect_to_seq`, `virus_type`, `vtype`,
#'   `classification_all`, `sabin1`/`sabin2`/`sabin3`, `npev`, `nvaccine`,
#'   `ev_detect`, `adequate`) are added only when their prerequisite source
#'   columns are present in `data`, so a trimmed input yields a correspondingly
#'   trimmed output rather than an error.
#'
#' @examples
#' raw <- data.frame(
#'   Id = c(1, 1, 2),
#'   SpecimenId = c("S1", "S1", "S2"),
#'   Epid = c("A-1", "A-1", "B-2"),
#'   LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
#'   DateStoolCollected = c("2024-01-05", "2024-01-05", "2024-02-09"),
#'   VirusTypes = c("cVDPV2", "cVDPV2", NA),
#'   VdpvClassification = c("Circulating", "Circulating", NA),
#'   SpecimenStoolConditionName = c("Good", "Good", "Poor"),
#'   AdequateSpecimen = c("Yes", "Yes", "No"),
#'   Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_human_spec(raw)
#'
#' @export
clean_human_spec <- function(
  data,
  cfg = polis_active_config(),
  shape = NULL,
  impute_geo = TRUE,
  cases = NULL,
  verbose = TRUE
) {
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
  n_in <- .polis_big_num(if (is.data.frame(data)) nrow(data) else 0L)

  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(data, "human_spec")
  step(
    "Standardising names on {n_in} rows",
    "Standardised names on {n_in} rows"
  )
  data <- standardise_names(data, cfg$crosswalk) |>
    .polis_clean_strings()

  # ---- standardise dates ----------------------------------------------------
  # parse + sanitise every collection/laboratory date, then derive year/month
  # from the stool-collection date. last_update_date (and the other audit
  # timestamps) stay ISO-string for the dedup sort.
  step(
    "Parsing dates and deriving collection vars + lab intervals",
    "Parsed dates and derived collection vars + lab intervals"
  )
  data <- data |>
    .spec_parse_dates() |>
    .spec_add_collection_vars() |>
    .spec_add_intervals()

  # ---- classify virus detection + specimen adequacy -------------------------
  step(
    "Deriving virus classification and adequacy",
    "Derived virus classification and adequacy"
  )
  data <- .polis_classify_virus(data) |>
    .spec_add_adequacy()

  # ---- standardise geography ------------------------------------------------
  # the clean_afp() geo recipe that applies to specimens, keyed on
  # year_collection: fix admin names, reconcile GUIDs against a shape, and
  # recover missing admin from the EPID prefix. (Specimens carry no coordinates,
  # so the coordinate-recovery step clean_afp() runs does not apply here.)
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data)
  long_shape <- if (is.null(shape)) {
    NULL
  } else if (inherits(shape, "sf")) {
    step(
      "Building the long district lookup from the shape",
      "Built the long district lookup from the shape"
    )
    create_long_shape(shape, "adm2")
  } else {
    shape
  }
  if (!is.null(long_shape)) {
    step(
      "Reconciling admin GUIDs against the district shape",
      "Reconciled admin GUIDs against the district shape"
    )
    data <- reconcile_admin_guids(
      data,
      long_shape,
      year_var = "year_collection",
      verbose = FALSE
    )
  }
  if (isTRUE(impute_geo)) {
    na2_before <- if ("adm2" %in% names(data)) sum(is.na(data$adm2)) else 0L
    n_rec <- "0"
    epid_msg <- if (is.null(cases)) {
      "Recovering missing admin from the EPID"
    } else {
      "Recovering missing admin from the EPID and case geography"
    }
    step(epid_msg, "Recovered admin for {n_rec} specimens from the EPID")
    # exact-EPID fill from the parent case first (fast direct match), then the
    # specimen-internal self-reference + prefix match for whatever remains.
    data <- .spec_fill_from_cases(data, cases)
    data <- .spec_impute_geo(data)
    na2_after <- if ("adm2" %in% names(data)) sum(is.na(data$adm2)) else 0L
    n_rec <- .polis_big_num(max(na2_before - na2_after, 0L))
  }

  # ---- enrich: country groupings + polio type -------------------------------
  step("Enriching with country groupings", "Enriched with country groupings")
  data <- data |>
    .polis_join_country() |>
    .polis_polio_type()

  # ---- finalise: dedup by id, infer types, assert business key, order -------
  step("Deduplicating by id and finalising", "Deduplicated by id and finalised")
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    .geo_guid_display_cols() |>
    flag_ambiguous(key = c("specimen_id", "adm0"), sink = cfg$qa) |>
    order_columns(cfg$column_roles)
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    out_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Cleaned {out_fmt} specimens.")
  }
  out
}

#' Garbage floor for the specimen "sensible date" test (pre-surveillance era).
#' @noRd
.spec_min_sensible_year <- 1980L

#' POLIS specimen date columns to sanitise (audit timestamps excluded)
#'
#' Every genuine collection/laboratory date carries `date` in its canonical name
#' but in three different shapes -- `date_*` (`date_stool_collected`), `dateof_*`
#' (`dateof_sequencing`) and `*_date_*` (`report_date_sequence_result_sent`) --
#' so the match is deliberately the unanchored substring `date` rather than the
#' `^date_`/`_date$` pair the other cleaners use; the audit timestamps are then
#' held out by name so `last_update_date` stays a sortable ISO string for the
#' keep-latest dedup.
#' @noRd
.spec_date_cols <- function(data) {
  audit <- c(
    "last_update_date",
    "created_date",
    "publish_date",
    "updated_date"
  )
  setdiff(grep("date", names(data), value = TRUE), audit)
}

#' Parse specimen date columns to `Date`, NA-ing implausible values
#'
#' Parses each date column with `as_date` (tolerant of ISO date and datetime
#' strings) then nulls any value outside `[min_year-01-01, reference_date]` --
#' the same rule [clean_afp()] applies to case dates.
#' @noRd
.spec_parse_dates <- function(
  data,
  min_year = .spec_min_sensible_year,
  reference_date = Sys.Date()
) {
  cols <- .spec_date_cols(data)
  if (length(cols) == 0) {
    return(data)
  }
  floor_date <- lubridate::make_date(min_year, 1L, 1L)
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      \(x) {
        parsed <- suppressWarnings(lubridate::as_date(x))
        dplyr::if_else(
          parsed >= floor_date & parsed <= reference_date,
          parsed,
          lubridate::NA_Date_
        )
      }
    )
  )
}

#' Derive year/month of collection from the sanitised stool-collection date
#' @noRd
.spec_add_collection_vars <- function(data) {
  if (!"date_stool_collected" %in% names(data)) {
    return(data)
  }
  dplyr::mutate(
    data,
    year_collection = lubridate::year(date_stool_collected),
    month_collection = lubridate::month(date_stool_collected)
  )
}

#' Derive the binary specimen-adequacy flag from `adequate_specimen`
#'
#' `1` when the specimen is recorded adequate, `0` when inadequate, `NA` when
#' unrecorded. A no-op when `adequate_specimen` is absent.
#' @noRd
.spec_add_adequacy <- function(data) {
  if (!"adequate_specimen" %in% names(data)) {
    return(data)
  }
  dplyr::mutate(
    data,
    adequate = dplyr::case_when(
      adequate_specimen == "Yes" ~ 1L,
      adequate_specimen == "No" ~ 0L,
      TRUE ~ NA_integer_
    )
  )
}

#' Recover missing admin from the EPID prefix (specimen analogue of .afp_impute_geo)
#'
#' Specimens whose district is absent often still carry the case EPID and admin
#' names; [impute_geo_from_epid()] fills missing `adm1`/`adm2` (and their GUIDs)
#' from the EPID prefix, keyed on `year_collection`, keeping every row. A no-op
#' when the EPID, admin or year columns are absent.
#' @noRd
.spec_impute_geo <- function(data) {
  required <- c("epid", "adm0", "adm1", "adm2", "year_collection")
  if (!all(required %in% names(data))) {
    return(data)
  }
  guid_vars <- c(adm1 = "adm1_guid", adm2 = "adm2_guid")
  guid_vars <- guid_vars[guid_vars %in% names(data)]
  res <- impute_geo_from_epid(
    data,
    year_var = "year_collection",
    guid_vars = if (length(guid_vars) > 0) guid_vars else NULL,
    strategies = c("self_ref", "prefix_match"),
    audit = FALSE,
    verbose = FALSE
  )
  res$data
}

#' Fill a specimen's missing admin from its parent case by exact EPID match
#'
#' A specimen reuses its case EPID, so the cleaned case carries the authoritative
#' `adm1`/`adm2` (and GUIDs). This is a direct exact-EPID fill -- one normalise
#' and one `match()` per column, not the per-target reference rebuild
#' [impute_geo_from_epid()] would do over the whole 2M-row case table. Only blank
#' specimen cells are filled, and only from EPIDs that map to a single value in
#' the cases (ambiguous EPIDs are skipped). A no-op without a usable case table.
#' @noRd
.spec_fill_from_cases <- function(data, cases) {
  if (
    is.null(cases) ||
      !is.data.frame(cases) ||
      !"epid" %in% names(cases) ||
      !"epid" %in% names(data)
  ) {
    return(data)
  }
  cols <- intersect(
    c("adm1", "adm2", "adm1_guid", "adm2_guid"),
    intersect(names(data), names(cases))
  )
  if (length(cols) == 0L) {
    return(data)
  }
  # Build the per-EPID case geography once: collapse cases to distinct
  # (key, geography) rows, then drop any EPID that maps to more than one row
  # (ambiguous -- contributes nothing). One match() then fills every column,
  # rather than re-deduplicating the 2M-row case table per column.
  ref <- cases[c("epid", cols)]
  ref[[".k"]] <- .epid_base_key(ref[["epid"]])
  ref <- ref[!is.na(ref[[".k"]]), , drop = FALSE]
  # canonicalise GUID columns so brace/case variants of one GUID aren't read as
  # conflicting values (the finalise display step re-formats them).
  for (gc in intersect(c("adm0_guid", "adm1_guid", "adm2_guid"), cols)) {
    ref[[gc]] <- .geo_guid_canon(ref[[gc]])
  }
  ref <- dplyr::distinct(ref, dplyr::across(dplyr::all_of(c(".k", cols))))
  ambiguous <- duplicated(ref[[".k"]]) |
    duplicated(ref[[".k"]], fromLast = TRUE)
  ref <- ref[!ambiguous, , drop = FALSE]
  if (nrow(ref) == 0L) {
    return(data)
  }

  pos <- match(.epid_base_key(data[["epid"]]), ref[[".k"]])
  for (col in cols) {
    candidate <- ref[[col]][pos]
    fill <- .epid_blank(data[[col]]) & !is.na(candidate)
    data[[col]][fill] <- candidate[fill]
  }
  data
}

#' Add the specimen lab-turnaround intervals (in days, sanitised to a window)
#'
#' The specimen analogue of clean_afp()'s onset-relative intervals: since
#' specimens carry no onset date, timeliness is measured along the laboratory
#' chain -- collection -> lab -> cell culture -> ITD -> sequencing -- from the
#' specimen's own dates. Each interval is `to - from` in days, NA'd outside
#' `[0, hi]` so an out-of-order or implausible pair cannot fabricate a value.
#' Each is added only when both its dates are present.
#' @noRd
.spec_add_intervals <- function(data) {
  add <- function(d, name, to, from, hi) {
    if (!all(c(to, from) %in% names(d))) {
      return(d)
    }
    days <- as.numeric(d[[to]] - d[[from]])
    d[[name]] <- dplyr::if_else(days >= 0 & days <= hi, days, NA_real_)
    d
  }
  data <- add(
    data,
    "collect_to_lab",
    "date_stool_received_in_lab",
    "date_stool_collected",
    365
  )
  data <- add(
    data,
    "lab_to_culture",
    "date_final_cell_culture_results",
    "date_stool_received_in_lab",
    365
  )
  data <- add(
    data,
    "culture_to_itd",
    "dateof_itd_result",
    "date_final_cell_culture_results",
    365
  )
  data <- add(
    data,
    "sent_to_seq_result",
    "date_sequencing_results",
    "date_isolate_sent_for_sequencing",
    365
  )
  data <- add(
    data,
    "collect_to_seq",
    "dateof_sequencing",
    "date_stool_collected",
    730
  )
  data
}

# NSE column referenced bare in .spec_add_collection_vars().
utils::globalVariables("date_stool_collected")
