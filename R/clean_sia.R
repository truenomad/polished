# =============================================================================
# Clean SIA (supplementary immunisation activity) data
#
# Reads a POLIS Activity table (optionally enriched with a SubActivity table)
# and returns a canonically-named, deduped, ordered analytic tibble. Runs
# standalone on the activity table alone; pass `subactivity` to join detail.
# =============================================================================

#' Clean POLIS SIA (campaign) data
#'
#' Standardises a raw POLIS activity table: canonical snake_case names (via the
#' crosswalk + janitor), parsed `date_*` columns with derived
#' `year_start`/`month_start`, normalised admin names, and one row per POLIS
#' `id` (latest by `last_update_date`).
#'
#' @param activity A raw POLIS activity data frame.
#' @param subactivity Optional raw POLIS sub-activity data frame. When supplied,
#'   it is standardised and left-joined onto `activity` by `sub_activity_id`.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#'
#' @return A tibble of cleaned SIA records, columns ordered id -> location ->
#'   time -> other.
#'
#' @examples
#' activity <- data.frame(
#'   Id = c(1, 2),
#'   SubActivityId = c("S1", "S2"),
#'   LastUpdateDate = c("2024-03-01", "2024-02-01"),
#'   DateStart = c("2024-03-10", "2024-02-12"),
#'   Admin0Name = c("NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_sia(activity)
#'
#' @export
clean_sia <- function(activity, subactivity = NULL, cfg = polis_config()) {
  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(activity, "sia")
  data <- standardise_names(activity, cfg$crosswalk) |>
    .polis_clean_strings()

  # ---- enrich with sub-activity detail (optional) ---------------------------
  if (!is.null(subactivity)) {
    sub <- standardise_names(subactivity, cfg$crosswalk)
    if (
      "sub_activity_id" %in% names(data) && "sub_activity_id" %in% names(sub)
    ) {
      data <- dplyr::left_join(
        data,
        sub,
        by = dplyr::join_by(sub_activity_id),
        relationship = "many-to-many",
        suffix = c("", "_sub")
      )
    }
  }

  # ---- standardise dates ----------------------------------------------------
  # parse the date_* columns and derive year/month from the sub-activity start
  # (`date_from`). last_update_date stays ISO-string for the dedup sort.
  date_cols <- grep("^date_", names(data), value = TRUE)
  if (length(date_cols) > 0) {
    data <- dplyr::mutate(
      data,
      dplyr::across(dplyr::all_of(date_cols), lubridate::ymd)
    )
  }
  if ("date_from" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      year_start = lubridate::year(date_from),
      month_start = lubridate::month(date_from)
    )
  }

  # ---- standardise geography ------------------------------------------------
  data <- fix_geo_names(data)

  # ---- finalise: dedup by id, order -----------------------------------------
  data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    order_columns(cfg$column_roles)
}
