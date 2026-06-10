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
#'     POLIS `id` (latest by `last_update_date`);
#'   \item campaign rounds: within each district (`adm2_guid`) x `vaccine_type`,
#'     sub-activities are ordered by `date_from` and split into rounds wherever
#'     the gap to the previous campaign exceeds `round_gap_days`, giving a
#'     sequential `round_num`; `max_round_date` / `last_campaign` flag each
#'     district's most recent campaign.
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
#' @param round_gap_days Maximum number of days between consecutive campaigns in
#'   the same district and `vaccine_type` for them to count as one round; a
#'   larger gap starts a new round. Default `21`.
#' @param cache_dir Optional directory for an opt-in, content-addressed cache.
#'   When set, the cleaned table is written to (and on a later identical call
#'   read back from) a `qs2` file whose name hashes every input that affects the
#'   output (`activity`, `subactivity`, `cfg`, `shape`, `round_gap_days`); any
#'   change to an input recomputes and writes a new entry. Default `NULL` (no
#'   caching).
#' @param cache_key Optional cheap stand-in for the raw tables in the cache key
#'   (e.g. a download snapshot id). When supplied, the key is built from it
#'   instead of hashing `activity`/`subactivity`, avoiding a full content hash of
#'   large inputs; `cfg`, `shape` and `round_gap_days` still contribute. Ignored
#'   unless `cache_dir` is set. Default `NULL` (hash the tables).
#' @param verbose Emit cli progress messages for each phase. Default `TRUE`.
#'
#' @return A tibble of cleaned SIA records, one row per POLIS `id`, with columns
#'   ordered id -> location -> time -> other. Derived columns (`year_start`,
#'   `month_start`, `round_num`, `max_round_date`, `last_campaign`) are added
#'   only when their source columns are present.
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
  round_gap_days = 21L,
  cache_dir = NULL,
  cache_key = NULL,
  verbose = TRUE
) {
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
  n_in <- .polis_big_num(if (is.data.frame(activity)) nrow(activity) else 0L)

  # ---- validate inputs ------------------------------------------------------
  .polis_check_input(activity, "sia")

  # ---- cache lookup ---------------------------------------------------------
  # opt-in content-addressed cache: a hit returns the cleaned table without
  # recomputing, keyed on everything that determines the output.
  cache_path <- NULL
  if (!is.null(cache_dir)) {
    cache_path <- .sia_cache_path(
      cache_dir,
      activity,
      subactivity,
      cfg,
      shape,
      round_gap_days,
      cache_key = cache_key
    )
    if (file.exists(cache_path)) {
      if (isTRUE(verbose)) {
        cli::cli_alert_success(
          "Loaded cached SIA result from {.file {basename(cache_path)}}."
        )
      }
      return(.polis_io_read(cache_path, "qs2"))
    }
  }

  # ---- standardise names ----------------------------------------------------
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

  # ---- finalise: dedup by id, infer types, assign rounds, order -------------
  # rounds are grouped on the deduped grain so each sub-activity x district is
  # counted once before its campaign date is binned into a round.
  step(
    "Deduplicating by id, assigning rounds and finalising",
    "Deduplicated by id, assigned rounds and finalised"
  )
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    .geo_guid_display_cols() |>
    .sia_assign_rounds(gap_days = round_gap_days) |>
    order_columns(cfg$column_roles)
  if (!is.null(cache_path)) {
    .sia_cache_write(out, cache_path)
    if (isTRUE(verbose)) {
      cli::cli_alert_info(
        "Cached SIA result to {.file {basename(cache_path)}}."
      )
    }
  }
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    out_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Cleaned {out_fmt} SIA record{?s}.")
  }
  out
}

#' Content-addressed cache path for `clean_sia()`; `cache_key` (a snapshot id)
#' substitutes for hashing the raw tables. The version tag self-invalidates.
#' @noRd
.sia_cache_path <- function(
  cache_dir,
  activity,
  subactivity,
  cfg,
  shape,
  gap_days,
  cache_key = NULL
) {
  data_key <- cache_key %||%
    list(activity = activity, subactivity = subactivity)
  key <- .polis_hash(list(
    data = data_key,
    cfg = cfg,
    shape = shape,
    gap_days = gap_days,
    version = .sia_cache_version
  ))
  file.path(cache_dir, paste0("clean_sia_", key, ".qs2"))
}

#' Logic-version tag for the SIA cache; bump when `clean_sia()` output changes.
#' @noRd
.sia_cache_version <- 1L

#' Write a cleaned SIA table to the cache atomically
#'
#' Writes to a temporary sibling file and renames it into place (a POSIX-atomic
#' rename) so an interrupted write can never leave a half-written cache file that
#' a later run would read back as valid.
#' @noRd
.sia_cache_write <- function(cleaned, cache_path) {
  dir <- dirname(cache_path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  tmp_path <- paste0(cache_path, ".tmp")
  .polis_io_write(cleaned, tmp_path, "qs2")
  # rename can fail (e.g. tmp and cache on different volumes); surface it and
  # clean up the stray temp file rather than leaving a silent miss.
  if (!file.rename(tmp_path, cache_path)) {
    unlink(tmp_path)
    cli::cli_warn(
      "Could not write SIA cache to {.file {cache_path}}; skipping cache."
    )
  }
  invisible(cache_path)
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

#' Assign campaign rounds by binning nearby campaign dates
#'
#' Within each district x vaccine type, orders sub-activities by their campaign
#' start and starts a new round whenever the gap to the previous campaign exceeds
#' `gap_days`. Implemented as a single ordered, fully vectorised pass (one sort
#' plus boundary `cumsum`es and one grouped `max`), so it stays fast even with
#' tens of thousands of districts -- a grouped `dplyr` apply is ~100x slower here
#' because it pays per-group overhead across thousands of tiny groups.
#'
#' Adds `round_num` (sequential round index within district x vaccine type) and,
#' per district, `max_round_date` / `last_campaign` (the most recent campaign date
#' and a 0/1 flag for it). Undated rows get `NA` round/flag. A no-op when any key
#' column is absent or the data is empty.
#' @noRd
.sia_assign_rounds <- function(
  data,
  gap_days = 21L,
  district_col = "adm2_guid",
  vaccine_col = "vaccine_type",
  date_col = "date_from"
) {
  n <- nrow(data)
  if (
    n == 0L || !all(c(district_col, vaccine_col, date_col) %in% names(data))
  ) {
    return(data)
  }
  district <- data[[district_col]]
  vaccine <- data[[vaccine_col]]
  day <- as.numeric(data[[date_col]])

  # work in (district, vaccine, date) order; undated rows sort to the end
  ord <- order(district, vaccine, day, method = "radix", na.last = TRUE)
  district_o <- district[ord]
  vaccine_o <- vaccine[ord]
  day_o <- day[ord]

  # round boundary: a new district/vaccine block, or a gap over the threshold
  new_group <- c(
    TRUE,
    district_o[-1] != district_o[-n] | vaccine_o[-1] != vaccine_o[-n]
  )
  new_group[is.na(new_group)] <- TRUE
  gap <- day_o - c(NA_real_, day_o[-n])
  new_round <- new_group | (!is.na(gap) & gap > gap_days)
  # round index = running rounds, rebased to 1 at each group's first row
  running <- cumsum(new_round)
  group_id <- cumsum(new_group)
  round_o <- running - running[new_group][group_id] + 1L
  round_o[is.na(day_o)] <- NA_integer_

  # latest campaign date per district (single grouped reduce; -Inf -> NA)
  new_district <- c(TRUE, district_o[-1] != district_o[-n])
  new_district[is.na(new_district)] <- TRUE
  district_id <- cumsum(new_district)
  finite_day <- ifelse(is.na(day_o), -Inf, day_o)
  district_max <- as.numeric(tapply(finite_day, district_id, max))
  district_max[!is.finite(district_max)] <- NA_real_
  max_o <- district_max[district_id]
  last_o <- as.integer(!is.na(day_o) & day_o == max_o)

  # scatter back to the original row order
  round_num <- integer(n)
  max_round_date <- numeric(n)
  last_campaign <- integer(n)
  round_num[ord] <- round_o
  max_round_date[ord] <- max_o
  last_campaign[ord] <- last_o

  data$round_num <- round_num
  data$max_round_date <- lubridate::as_date(max_round_date)
  data$last_campaign <- last_campaign
  data
}
