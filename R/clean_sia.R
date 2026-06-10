# =============================================================================
# Clean SIA (supplementary immunisation activity) data
#
# Builds one analytic SIA table from the POLIS Activity and SubActivity tables.
# The sub-activity is the grain (one row per sub-activity round x admin2, with
# the admin detail and campaign dates); the activity table carries the parent
# campaign metadata and is joined on by the SIA sub-activity code. Like the other
# cleaners this is one linear recipe that runs standalone -- no archive, no files
# -- standardising names, sanitising dates, cleaning geography, and deduping to
# one row per POLIS id.
# =============================================================================

#' Clean POLIS SIA (campaign) data
#'
#' Combines the raw POLIS activity and sub-activity tables into one analytic SIA
#' dataset and standardises it the same way [clean_afp()] / [clean_es()] do:
#' \itemize{
#'   \item canonical snake_case names (via the crosswalk + janitor);
#'   \item the sub-activity grain enriched with its parent campaign: the activity
#'     table is restricted to the sub-activity codes actually present, then joined
#'     onto each sub-activity by `sia_sub_activity_code` (parent columns that
#'     clash with a sub-activity column take an `_activity` suffix);
#'   \item every campaign/planning date parsed to `Date` and sanitised with the
#'     same "sensible date" rule (a value before `min_year` or in the future is a
#'     data-entry error and set to `NA`); audit timestamps stay ISO strings for
#'     the keep-latest dedup;
#'   \item `year_start` / `month_start` from the sanitised `date_from` (the
#'     sub-activity start);
#'   \item normalised admin names and -- when a `shape` is supplied -- admin-GUID
#'     reconciliation against it (keyed on `year_start`), exactly as [clean_es()]
#'     uses it;
#'   \item GUIDs emitted in the braced upper-case POLIS form and one row per
#'     POLIS `id` (latest by `last_update_date`).
#' }
#'
#' @param activity A raw POLIS activity data frame.
#' @param subactivity Optional raw POLIS sub-activity data frame. When supplied it
#'   is the grain of the output and `activity` is joined onto it; when `NULL` the
#'   activity table is cleaned on its own.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#' @param shape Optional district shape used to reconcile admin names/GUIDs via
#'   [reconcile_admin_guids()] (keyed on `year_start`), exactly as [clean_es()]
#'   uses it. Either a long ADM2 attribute table or the polygon layer (expanded to
#'   its long form here). Default `NULL` (no shape-based recovery).
#' @param verbose Emit cli progress messages for each phase. Default `TRUE`.
#'
#' @return A tibble of cleaned SIA records, one row per POLIS `id`, with columns
#'   ordered id -> location -> time -> other. Derived columns (`year_start`,
#'   `month_start`) are added only when their source columns are present.
#'
#' @examples
#' activity <- data.frame(
#'   Id = 1,
#'   SIASubActivityCode = "S1",
#'   LastUpdateDate = "2024-03-01",
#'   VaccineType = "bOPV",
#'   check.names = FALSE
#' )
#' subactivity <- data.frame(
#'   Id = 10,
#'   SIASubActivityCode = "S1",
#'   LastModificationDate = "2024-03-01",
#'   DateFrom = "2024-03-10",
#'   Admin0Name = "NIGERIA",
#'   check.names = FALSE
#' )
#' clean_sia(activity, subactivity, verbose = FALSE)
#'
#' @export
clean_sia <- function(
  activity,
  subactivity = NULL,
  cfg = polis_config(),
  shape = NULL,
  verbose = TRUE
) {
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
  n_in <- .polis_big_num(if (is.data.frame(activity)) nrow(activity) else 0L)

  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(activity, "sia")
  step(
    "Standardising names on {n_in} activities",
    "Standardised names on {n_in} activities"
  )
  activity <- standardise_names(activity, cfg$crosswalk) |>
    .polis_clean_strings()

  # ---- combine activity + sub-activity --------------------------------------
  # the sub-activity is the grain; the parent campaign is joined on by the SIA
  # sub-activity code. With no sub-activity table the activity stands alone.
  if (!is.null(subactivity)) {
    step(
      "Combining activities with sub-activities",
      "Combined activities with sub-activities"
    )
    subactivity <- standardise_names(subactivity, cfg$crosswalk) |>
      .polis_clean_strings()
    data <- .sia_combine(activity, subactivity)
  } else {
    data <- activity
  }

  # ---- standardise dates ----------------------------------------------------
  # parse + sanitise every campaign/planning date, then derive year/month from
  # the sub-activity start (date_from). Audit timestamps stay ISO strings.
  step(
    "Parsing dates and deriving year/month of campaign start",
    "Parsed dates and derived year/month of campaign start"
  )
  data <- data |>
    .sia_parse_dates() |>
    .sia_add_start_vars()

  # ---- standardise geography ------------------------------------------------
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data)
  long_shape <- NULL
  if (!is.null(shape)) {
    if (inherits(shape, "sf")) {
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
    data <- reconcile_admin_guids(
      data,
      long_shape,
      year_var = "year_start",
      verbose = FALSE
    )
  }

  # ---- finalise: dedup by id, infer types, order ----------------------------
  step("Deduplicating by id and finalising", "Deduplicated by id and finalised")
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    .geo_guid_display_cols() |>
    order_columns(cfg$column_roles)
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    out_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Cleaned {out_fmt} SIA record{?s}.")
  }
  out
}

#' Join the parent campaign onto the sub-activity grain
#'
#' Restricts the activity table to the sub-activity codes that actually occur,
#' deduplicates it, and left-joins it onto the sub-activities by
#' `sia_sub_activity_code`. A no-op join (returns the sub-activities unchanged)
#' when either side lacks the code column.
#' @noRd
.sia_combine <- function(activity, subactivity) {
  key <- "sia_sub_activity_code"
  if (!(key %in% names(activity) && key %in% names(subactivity))) {
    return(subactivity)
  }
  activity <- activity |>
    dplyr::semi_join(subactivity, by = key) |>
    dplyr::distinct()
  dplyr::left_join(
    subactivity,
    activity,
    by = key,
    relationship = "many-to-many",
    suffix = c("", "_activity")
  )
}

#' Garbage floor for the SIA "sensible date" test (pre-surveillance era).
#' @noRd
.sia_min_sensible_year <- 1980L

#' POLIS SIA date columns to sanitise (audit timestamps excluded)
#'
#' Every genuine campaign/planning date is `^date_*` or `*_date`; the audit
#' timestamps are held out so `last_update_date` stays a sortable ISO string for
#' the keep-latest dedup.
#' @noRd
.sia_date_cols <- function(data) {
  audit <- c(
    "last_update_date",
    "last_modification_date",
    "created_date",
    "publish_date",
    "uploaded_date",
    "activity_parent_updated_date"
  )
  cols <- union(
    grep("^date_", names(data), value = TRUE),
    grep("_date$", names(data), value = TRUE)
  )
  setdiff(cols, audit)
}

#' Parse SIA date columns to `Date`, NA-ing implausible values
#'
#' The SIA analogue of [clean_es()]'s date sanitiser: parse each date column then
#' null any value outside `[min_year-01-01, reference_date]`.
#' @noRd
.sia_parse_dates <- function(
  data,
  min_year = .sia_min_sensible_year,
  reference_date = Sys.Date()
) {
  cols <- .sia_date_cols(data)
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

#' Derive year/month of campaign start from the sanitised `date_from`
#' @noRd
.sia_add_start_vars <- function(data) {
  if (!"date_from" %in% names(data)) {
    return(data)
  }
  dplyr::mutate(
    data,
    year_start = lubridate::year(date_from),
    month_start = lubridate::month(date_from)
  )
}
