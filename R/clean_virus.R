# =============================================================================
# Clean virus (positives) data
#
# Reads a POLIS Viruses table and returns a canonically-named, deduped, ordered
# analytic tibble. Virus cleans standalone; pass cleaned `cases` and/or `es` to
# tag surveillance linkage via integrate_virus(). This is the one place the
# human (AFP) and environmental streams meet -- and it stays optional, so you
# can still clean virus on its own.
# =============================================================================

#' Clean POLIS virus (positives) data
#'
#' Standardises a raw POLIS viruses table: canonical snake_case names (via the
#' crosswalk + janitor), parsed `date_*` columns with derived
#' `year_onset`/`month_onset`, normalised admin names, merged-EPID remapping,
#' and one row per POLIS `id` (latest by `last_update_date`). Optionally tags
#' surveillance source from cleaned cases/ES.
#'
#' @param data A raw POLIS viruses data frame.
#' @param cases Optional cleaned AFP table (from [clean_afp()]) used to tag
#'   human-derived viruses by `epid`.
#' @param es Optional cleaned ES table (from [clean_es()]) used to tag
#'   environment-derived viruses by `env_sample_id`.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#'
#' @return A tibble of cleaned virus records, columns ordered id -> location ->
#'   time -> other.
#'
#' @examples
#' raw <- data.frame(
#'   Id = c(1, 2),
#'   Epid = c("A-1", "B-2"),
#'   LastUpdateDate = c("2024-03-01", "2024-02-01"),
#'   DateOnset = c("2024-01-02", "2024-02-03"),
#'   VirusTypeName = c("cVDPV2", "WILD1"),
#'   Admin0Name = c("NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_virus(raw)
#'
#' @export
clean_virus <- function(data, cases = NULL, es = NULL, cfg = polis_config()) {
  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(data, "virus")
  data <- data |>
    standardise_names(cfg$crosswalk) |>
    remap_synonyms(cfg$synonyms) |>
    .polis_clean_strings()

  # ---- standardise dates ----------------------------------------------------
  # parse the date_* columns plus the canonical virus date, then derive
  # year/month from it. last_update_date stays ISO-string for the dedup sort.
  date_cols <- union(
    grep("^date_", names(data), value = TRUE),
    intersect("virus_date", names(data))
  )
  if (length(date_cols) > 0) {
    data <- dplyr::mutate(
      data,
      dplyr::across(dplyr::all_of(date_cols), lubridate::ymd)
    )
  }
  if ("virus_date" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      year_onset = lubridate::year(virus_date),
      month_onset = lubridate::month(virus_date)
    )
  }

  # ---- standardise geography ------------------------------------------------
  data <- fix_geo_names(data)

  # ---- integrate human / environmental linkage (optional) -------------------
  data <- integrate_virus(data, cases = cases, es = es)

  # ---- finalise: dedup by id, order -----------------------------------------
  data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    order_columns(cfg$column_roles)
}

#' Tag virus records with their surveillance source
#'
#' Sets `surveillance_type` to `"human"` for viruses whose `epid` is present in
#' cleaned cases, and `"environmental"` for those whose `env_sample_id` is
#' present in cleaned ES. Existing non-missing values are preserved. A no-op when
#' neither reference is supplied, so [clean_virus()] runs standalone.
#'
#' @param virus A virus data frame (canonical names).
#' @param cases Optional cleaned AFP table.
#' @param es Optional cleaned ES table.
#'
#' @return `virus` with a `surveillance_type` column populated where derivable.
#'
#' @export
integrate_virus <- function(virus, cases = NULL, es = NULL) {
  if (is.null(cases) && is.null(es)) {
    return(virus)
  }
  if (!"surveillance_type" %in% names(virus)) {
    virus$surveillance_type <- NA_character_
  }

  if (!is.null(cases) && "epid" %in% names(virus) && "epid" %in% names(cases)) {
    is_human <- is.na(virus$surveillance_type) & virus$epid %in% cases$epid
    virus$surveillance_type[is_human] <- "human"
  }
  if (
    !is.null(es) &&
      "env_sample_id" %in% names(virus) &&
      "env_sample_id" %in% names(es)
  ) {
    is_env <- is.na(virus$surveillance_type) &
      virus$env_sample_id %in% es$env_sample_id
    virus$surveillance_type[is_env] <- "environmental"
  }
  virus
}
