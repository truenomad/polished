# =============================================================================
# Build the virus (positives) analytic dataset
#
# Unlike the other cleaners, this one does NOT clean a raw POLIS table. The
# positives dataset is *constructed* from the already-cleaned human (AFP) and
# environmental (ES) outputs: every poliovirus-positive case or sample becomes
# one row in a single harmonised table, tagged by surveillance source, carrying
# the case/sample geography, coordinates and key dates alongside the analytic
# virus label. This is the one place the human and environmental streams meet.
# =============================================================================

#' Build the POLIS virus (positives) dataset from cleaned cases and ES
#'
#' Constructs the combined positives/virus analytic table from the outputs of
#' [clean_afp()] and [clean_es()] -- it does not read a raw POLIS viruses table.
#' Every poliovirus-positive record (classification matching `WPV`/`VDPV` --
#' i.e. `WPV`, `cVDPV`, `aVDPV`, `iVDPV` or untyped `VDPV`) becomes one row,
#' harmonised to a shared schema and tagged by source:
#' \itemize{
#'   \item `surveillance_type` (`"human"` / `"environmental"`) and the finer
#'     `source` (the case `surveillance_type_name` -- AFP / Community /
#'     Contact -- or `"Environmental"`);
#'   \item `measurement` and `classification_all`: the analytic virus label in
#'     the shared `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary both cleaners emit;
#'   \item the case/sample geography (`adm0`/`adm1`/`adm2` + GUIDs),
#'     `latitude`/`longitude`, the event `virus_date` (paralysis onset for cases,
#'     collection for ES) with `year_onset`/`month_onset`, and `notification_date`;
#'   \item `report_date`: the VDPV classification-change date for VDPV records,
#'     the notification date for WPV records;
#'   \item `emergence_group`, `nt_changes`, `virus_cluster`, and -- when a
#'     `nopv_emergence` reference is supplied -- the novel-OPV2 flag `nopv2`.
#' }
#'
#' @param cases Optional cleaned AFP table (from [clean_afp()]). Its
#'   poliovirus-positive rows become the human positives.
#' @param es Optional cleaned ES table (from [clean_es()]). Its
#'   poliovirus-positive rows become the environmental positives.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#' @param nopv_emergence Optional reference of novel-OPV2 (nOPV2) emergence-group
#'   names: a character vector, or a data frame with an `emergence_group` column.
#'   When supplied, records whose `emergence_group` matches are flagged `nopv2`.
#'   Default `NULL` (no nOPV2 flag).
#' @param separate_rows If `TRUE`, co-detection records (a fused label such as
#'   `WPV1andcVDPV 2` or `VDPV12and3`) are split into one row per detected
#'   serotype, with `measurement`/`classification_all` set to the component
#'   label. Default `FALSE` (one row per positive record, co-detections kept as
#'   the fused label).
#' @param verbose Emit cli progress messages. Default `TRUE`.
#'
#' @return A tibble of poliovirus positives, one row per positive case/sample,
#'   columns ordered id -> location -> time -> classification -> dates -> other.
#'   When neither input has any positives, returns a 0-row, 0-column tibble.
#'
#' @examples
#' cases <- clean_afp(data.frame(
#'   Id = 1, Epid = "A-1", `Last Update Date` = "2024-03-01",
#'   `Paralysis Onset Date` = "2024-01-02", `Notification Date` = "2024-01-09",
#'   `Polio Virus Types` = "WILD1", Classification = "Confirmed (wild)",
#'   `Admin0 Name` = "NIGERIA", check.names = FALSE
#' ), verbose = FALSE)
#' clean_virus(cases = cases, verbose = FALSE)
#'
#' @export
clean_virus <- function(
  cases = NULL,
  es = NULL,
  cfg = polis_config(),
  nopv_emergence = NULL,
  separate_rows = FALSE,
  verbose = TRUE
) {
  if (is.null(cases) && is.null(es)) {
    cli::cli_abort(
      "Supply at least one of {.arg cases} or {.arg es} (cleaned outputs)."
    )
  }
  # each call to step() marks the previous step done (past-tense tick) and starts
  # the next: present-continuous while running, `done` shown on the tick.
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }

  # ---- extract positives from each cleaned stream ---------------------------
  step(
    "Extracting poliovirus positives from cases and ES",
    "Extracted poliovirus positives from cases and ES"
  )
  human_positives <- .virus_from_cases(cases)
  es_positives <- .virus_from_es(es)
  parts <- Filter(Negate(is.null), list(human_positives, es_positives))
  if (length(parts) == 0) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("No poliovirus positives found.")
    }
    return(tibble::tibble())
  }

  # ---- combine, derive report date + nOPV2 flag, order ----------------------
  step(
    "Combining streams and deriving report_date",
    "Combined streams and derived report_date"
  )
  # no distinct(): two positives with different POLIS id but the same projected
  # values are genuinely distinct and must both survive.
  out <- .virus_flag_nopv(dplyr::bind_rows(parts), nopv_emergence)
  if (isTRUE(separate_rows)) {
    step(
      "Splitting co-detections to one row per serotype",
      "Split co-detections to one row per serotype"
    )
    out <- .virus_separate(out)
  }
  # report_date after any split, so each serotype row gets its own date.
  # Parse column base types like every other cleaner, so the derived positives
  # table carries typed columns too (gated by cfg$parse_types).
  out <- out |>
    .virus_add_report_date() |>
    .polis_parse_types(cfg) |>
    .geo_guid_display_cols() |>
    order_columns(cfg$column_roles)

  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    n_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Built {n_fmt} poliovirus positive{?s}.")
  }
  out
}

#' Regex matching the analytic labels that count as a poliovirus positive
#'
#' `WPV` (wild, incl. `WPV1and...`) and every VDPV kind (`cVDPV`/`aVDPV`/`iVDPV`
#' and untyped `VDPV`). Sabin/NPEV/NEGATIVE/PENDING/none are not positives.
#' @noRd
.virus_positive_pattern <- "WPV|VDPV"

#' Common harmonised schema columns shared by both streams
#' @noRd
.virus_shared_cols <- c(
  "adm0" = "adm0",
  "adm1" = "adm1",
  "adm2" = "adm2",
  "adm0_guid" = "adm0_guid",
  "adm1_guid" = "adm1_guid",
  "adm2_guid" = "adm2_guid",
  "vtype" = "vtype",
  "classification_all" = "classification_all",
  "emergence_group" = "vdpv_emergence_group_names",
  "nt_changes" = "nt_changes",
  "virus_cluster" = "virus_clusters",
  "vdpv_classification_change_date" = "vdpv_classification_change_date"
)

#' Select + rename a cleaned stream to the harmonised positives schema
#'
#' `dplyr::select(any_of())` with a named vector renames and silently skips any
#' column absent from this (possibly trimmed) input, so each derived column
#' appears only when its source is present.
#' @noRd
.virus_harmonise <- function(data, extra_map) {
  dplyr::select(data, dplyr::any_of(c(extra_map, .virus_shared_cols)))
}

#' Keep only poliovirus-positive rows, by the analytic classification
#' @noRd
.virus_keep_positive <- function(data) {
  if (!"classification_all" %in% names(data)) {
    return(data[0, , drop = FALSE])
  }
  dplyr::filter(
    data,
    !is.na(.data$classification_all) &
      stringr::str_detect(.data$classification_all, .virus_positive_pattern)
  )
}

#' Human (AFP) positives harmonised to the shared positives schema
#' @noRd
.virus_from_cases <- function(cases) {
  if (is.null(cases) || !"classification_all" %in% names(cases)) {
    return(NULL)
  }
  human_map <- c(
    "epid" = "epid",
    "latitude" = "latitude",
    "longitude" = "longitude",
    "virus_date" = "paralysis_onset_date",
    "year_onset" = "year_onset",
    "month_onset" = "month_onset",
    "notification_date" = "notification_date",
    "source" = "surveillance_type_name"
  )
  out <- .virus_harmonise(cases, human_map) |>
    .virus_keep_positive()
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$surveillance_type <- "human"
  out$measurement <- out$classification_all
  if (!"source" %in% names(out)) {
    out$source <- "AFP"
  }
  out$source <- dplyr::coalesce(out$source, "AFP")
  out
}

#' Environmental (ES) positives harmonised to the shared positives schema
#' @noRd
.virus_from_es <- function(es) {
  if (is.null(es) || !"classification_all" %in% names(es)) {
    return(NULL)
  }
  es_map <- c(
    "epid" = "sample_id",
    "latitude" = "site_y_coordinate",
    "longitude" = "site_x_coordinate",
    "virus_date" = "collection_date",
    "year_onset" = "year_collection",
    "month_onset" = "month_collection",
    "notification_date" = "date_notification_to_hq"
  )
  out <- .virus_harmonise(es, es_map) |>
    .virus_keep_positive()
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$surveillance_type <- "environmental"
  out$source <- "Environmental"
  out$measurement <- out$classification_all
  out
}

#' Derive the positives reporting date
#'
#' VDPV records report on the VDPV classification-change date; WPV records on the
#' notification date. NA-safe: a missing source date simply yields `NA`.
#' @noRd
.virus_add_report_date <- function(data) {
  if (!"measurement" %in% names(data)) {
    return(data)
  }
  vdpv_date <- if ("vdpv_classification_change_date" %in% names(data)) {
    lubridate::as_date(data$vdpv_classification_change_date)
  } else {
    rep(lubridate::NA_Date_, nrow(data))
  }
  notify_date <- if ("notification_date" %in% names(data)) {
    lubridate::as_date(data$notification_date)
  } else {
    rep(lubridate::NA_Date_, nrow(data))
  }
  data$report_date <- dplyr::case_when(
    stringr::str_detect(data$measurement, "VDPV") ~ vdpv_date,
    stringr::str_detect(data$measurement, "WPV") ~ notify_date,
    TRUE ~ lubridate::NA_Date_
  )
  data
}

#' Flag novel-OPV2 (nOPV2) records from an emergence-group reference
#'
#' `nopv2` is `1` when the record's `emergence_group` is in the supplied
#' reference, else `0`. A no-op (no column added) when no reference is given or
#' the `emergence_group` column is absent.
#' @noRd
.virus_flag_nopv <- function(data, nopv_emergence) {
  if (is.null(nopv_emergence) || !"emergence_group" %in% names(data)) {
    return(data)
  }
  ref <- if (is.data.frame(nopv_emergence)) {
    nopv_emergence$emergence_group
  } else {
    nopv_emergence
  }
  data$nopv2 <- as.integer(
    dplyr::coalesce(data$emergence_group %in% ref, FALSE)
  )
  data
}

#' Split one fused poliovirus label into its component serotype labels
#'
#' The classification vocabulary joins co-detections with `and`
#' (`WPV1andcVDPV 2`, `cVDPV2andcVDPV3`), with `VDPV12and3` as the only
#' non-splittable special case. Returns the component labels, each spaced
#' (`WPV1` -> `WPV 1`); a single label (or `NA`) is returned unchanged.
#' @noRd
.virus_split_label <- function(label) {
  if (is.na(label)) {
    return(NA_character_)
  }
  if (label == "VDPV12and3") {
    return(c("VDPV 1", "VDPV 2", "VDPV 3"))
  }
  parts <- trimws(strsplit(label, "and", fixed = TRUE)[[1]])
  sub("([A-Za-z])([123])$", "\\1 \\2", parts)
}

#' Expand co-detection rows to one row per detected serotype
#'
#' Each row whose `measurement` fuses several serotypes is repeated once per
#' component, with `measurement` (and `classification_all` / `vtype`) set to that
#' component; single-serotype rows pass through unchanged. A no-op on an empty
#' frame or one without `measurement`.
#' @noRd
.virus_separate <- function(data) {
  if (!"measurement" %in% names(data) || nrow(data) == 0) {
    return(data)
  }
  components <- lapply(data$measurement, .virus_split_label)
  rows_expanded <- data[
    rep(seq_len(nrow(data)), lengths(components)),
    ,
    drop = FALSE
  ]
  rows_expanded$measurement <- unlist(components, use.names = FALSE)
  rows_expanded$classification_all <- rows_expanded$measurement
  if ("vtype" %in% names(rows_expanded)) {
    rows_expanded$vtype <- rows_expanded$measurement
  }
  tibble::as_tibble(rows_expanded)
}
