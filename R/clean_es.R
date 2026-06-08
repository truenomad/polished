# =============================================================================
# Clean environmental surveillance (ES) data
#
# Reads one POLIS EnvSamples table and returns a canonically-named, deduped,
# ordered analytic tibble. The function is a single linear recipe: read it top
# to bottom and you see every step. It runs standalone -- no archive, no other
# dataset, no files -- so you can clean ES on its own.
# =============================================================================

#' Clean POLIS environmental surveillance data
#'
#' Standardises one raw POLIS environmental-samples table: canonical snake_case
#' names (via the crosswalk + janitor), parsed `date_*` columns with derived
#' `year_collection`/`month_collection`, normalised admin names, and one row per
#' POLIS `id` (latest by `last_update_date`).
#'
#' @param data A raw POLIS environmental-samples data frame.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#'
#' @return A tibble of cleaned ES records, columns ordered id -> location ->
#'   time -> other.
#'
#' @examples
#' raw <- data.frame(
#'   Id = c(1, 1, 2),
#'   EnvSampleId = c("E1", "E1", "E2"),
#'   LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
#'   DateCollection = c("2024-01-05", "2024-01-05", "2024-02-09"),
#'   Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_es(raw)
#'
#' @export
clean_es <- function(data, cfg = polis_config()) {
  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(data, "es")
  data <- standardise_names(data, cfg$crosswalk) |>
    .polis_clean_strings()

  # ---- standardise dates ----------------------------------------------------
  # parse the date_* columns plus the canonical collection date, then derive
  # year/month from it. last_update_date stays ISO-string for the dedup sort.
  date_cols <- union(
    grep("^date_", names(data), value = TRUE),
    intersect("collection_date", names(data))
  )
  if (length(date_cols) > 0) {
    data <- dplyr::mutate(
      data,
      dplyr::across(dplyr::all_of(date_cols), lubridate::ymd)
    )
  }
  if ("collection_date" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      year_collection = lubridate::year(collection_date),
      month_collection = lubridate::month(collection_date)
    )
  }

  # ---- standardise geography ------------------------------------------------
  data <- fix_geo_names(data)

  # ---- finalise: dedup by id (latest), order --------------------------------
  data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    order_columns(cfg$column_roles)
}
