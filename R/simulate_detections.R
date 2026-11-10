# detections -------------------------------------------------------------------

#' Simulate Virus Detections
#'
#' Builds one row per poliovirus detection from the positive AFP cases and
#' the positive environmental samples, shaped like the cleaned POLIS virus
#' table (`polished_virus`) that rise-afg reads. No new values are drawn:
#' each detection carries the place, dates and virus fields of the case or
#' sample it comes from, so the three tables agree with each other.
#'
#' A case contributes its onset as `virus_date` and its notification date;
#' a sample contributes its collection date and its notification to
#' headquarters. `report_date` is the notification date for a wild virus
#' and the vaccine-derived classification date for a vaccine-derived
#' virus, each falling back on the other, as the cleaning derives it.
#'
#' Column names and the `source` and `surveillance_type` labels follow how
#' rise-afg reads `polished_virus` in
#' `02_scripts/2c_indicators/2ci_detections.R`, `2cii_priority.R`,
#' `2cxxi_detection_context.R`, `2cxxvi_region_profile.R` and
#' `2cxxvii_detection_history.R`, and the reader in
#' `02_scripts/00_helper_functions/0a_polis_helpers.R`, which adds
#' `source_system`.
#'
#' @param afp_cases A table from [simulate_afp_cases()].
#' @param es_samples A table from [simulate_es_samples()].
#' @param positive_pattern Regular expression a `classification_all` label
#'   must match to count as a detection. Default `"WPV|VDPV"`.
#' @param environmental_source Value of `source` for a sample. Default
#'   `"Environmental"`.
#' @param surveillance_types Values of `surveillance_type` for a case and a
#'   sample. Default `c(human = "human", environmental = "environmental")`.
#' @param source_system Value of `source_system` on every row. Default
#'   `"POLIS"`.
#'
#' @return A tibble with one row per detection, ordered by `virus_date`.
#'   Dates are `Date`, `year_onset` and `month_onset` are integer,
#'   `latitude` and `longitude` are double, `virus_is_orphan` is logical
#'   and the remaining columns are character.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' cases <- simulate_afp_cases(admin, n_positive = 2)
#' samples <- simulate_es_samples(admin, n_sites = 3)
#' simulate_detections(cases, samples)
#' @export
simulate_detections <- function(afp_cases,
                                es_samples,
                                positive_pattern = "WPV|VDPV",
                                environmental_source = "Environmental",
                                surveillance_types = c(
                                  human = "human",
                                  environmental = "environmental"
                                ),
                                source_system = "POLIS") {
  .sim_check_columns(afp_cases, .sim_afp_columns, "simulate_afp_cases")
  .sim_check_columns(es_samples, .sim_es_columns, "simulate_es_samples")
  .sim_check_labels(positive_pattern)
  .sim_check_labels(environmental_source)
  .sim_check_labels(
    surveillance_types, names = c("human", "environmental")
  )
  .sim_check_labels(source_system)

  human <- afp_cases |>
    dplyr::filter(
      !is.na(.data$classification_all),
      grepl(positive_pattern, .data$classification_all)
    ) |>
    dplyr::transmute(
      epid = .data$epid,
      surveillance_type = surveillance_types[["human"]],
      source = .data$surveillance_type_name,
      country_iso3code = .data$country_iso3code,
      adm0 = .data$adm0, adm1 = .data$adm1, adm2 = .data$adm2,
      adm0_guid = .data$adm0_guid, adm1_guid = .data$adm1_guid,
      adm2_guid = .data$adm2_guid,
      latitude = .data$latitude, longitude = .data$longitude,
      vtype = .data$vtype,
      classification_all = .data$classification_all,
      emergence_group = .data$vdpv_emergence_group_names,
      nt_changes = .data$nt_changes,
      virus_cluster = .data$virus_clusters,
      virus_is_orphan = .data$virus_is_orphan,
      vdpv_classification_change_date =
        .data$vdpv_classification_change_date,
      virus_date = .data$paralysis_onset_date,
      year_onset = .data$year_onset,
      month_onset = .data$month_onset,
      notification_date = .data$notification_date
    )

  environmental <- es_samples |>
    dplyr::filter(
      !is.na(.data$classification_all),
      grepl(positive_pattern, .data$classification_all)
    ) |>
    dplyr::transmute(
      epid = .data$sample_id,
      surveillance_type = surveillance_types[["environmental"]],
      source = environmental_source,
      country_iso3code = .data$country_iso3code,
      adm0 = .data$adm0, adm1 = .data$adm1, adm2 = .data$adm2,
      adm0_guid = .data$adm0_guid, adm1_guid = .data$adm1_guid,
      adm2_guid = .data$adm2_guid,
      latitude = .data$site_y_coordinate,
      longitude = .data$site_x_coordinate,
      vtype = .data$vtype,
      classification_all = .data$classification_all,
      emergence_group = .data$vdpv_emergence_group_names,
      nt_changes = .data$nt_changes,
      virus_cluster = .data$virus_clusters,
      virus_is_orphan = NA,
      vdpv_classification_change_date =
        .data$vdpv_classification_change_date,
      virus_date = .data$collection_date,
      year_onset = .data$year_collection,
      month_onset = .data$month_collection,
      notification_date = .data$date_notification_to_hq
    )

  dplyr::bind_rows(human, environmental) |>
    dplyr::mutate(
      measurement = .data$classification_all,
      report_date = dplyr::case_when(
        grepl("WPV", .data$measurement) ~ dplyr::coalesce(
          .data$notification_date, .data$vdpv_classification_change_date
        ),
        grepl("VDPV", .data$measurement) ~ dplyr::coalesce(
          .data$vdpv_classification_change_date, .data$notification_date
        ),
        .default = as.Date(NA)
      ),
      source_system = source_system
    ) |>
    dplyr::arrange(.data$virus_date, .data$epid) |>
    dplyr::select(dplyr::all_of(.sim_detection_columns))
}

.sim_detection_columns <- c(
  "epid", "surveillance_type", "source", "country_iso3code", "adm0",
  "adm1", "adm2", "adm0_guid", "adm1_guid", "adm2_guid", "latitude",
  "longitude", "vtype", "classification_all", "measurement",
  "emergence_group", "nt_changes", "virus_cluster", "virus_is_orphan",
  "vdpv_classification_change_date", "virus_date", "year_onset",
  "month_onset", "notification_date", "report_date", "source_system"
)

#' @noRd
.sim_check_columns <- function(x, needed, maker, arg = rlang::caller_arg(x),
                               call = rlang::caller_env()) {
  missing <- setdiff(needed, names(x))
  if (!is.data.frame(x) || length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a table from {.fn {maker}}.",
        "x" = "Missing column{?s}: {.val {missing}}."
      ),
      call = call
    )
  }
  invisible(x)
}
