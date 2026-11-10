# human specimens --------------------------------------------------------------

.sim_spec_delay_names <- c(
  "collect_to_lab", "lab_to_culture", "culture_to_itd", "itd_to_sequencing"
)

#' Simulate Human Stool Specimens
#'
#' Builds one row per stool specimen from the cases in a table from
#' [simulate_afp_cases()], shaped like the cleaned POLIS specimen table that
#' [clean_human_spec()] writes. A case contributes one specimen for each
#' stool collection date it carries, so a case with a single stool has one
#' specimen and a case whose stools fall after the extraction has none.
#'
#' Each specimen is received in the laboratory, given a final cell culture
#' result and, when its case is poliovirus positive, an intratypic
#' differentiation (ITD) result and a sequencing result. Each step follows
#' the one before after a delay drawn from a negative binomial distribution,
#' so the dates of a specimen always run in order. The specimens keep the
#' state of their case. A specimen of a case classified as laboratory
#' pending has not been received. A specimen of a case pending
#' classification has been received, and `share_pending_resulted` of those
#' cases also have their culture result. A classified case has every date up
#' to its culture result, with dates that would fall after `end_date` set to
#' `end_date`. For a pending case, a date after `end_date` has not happened
#' yet and is left missing, along with every later date.
#'
#' The year and month of collection and the laboratory intervals
#' `collect_to_lab`, `lab_to_culture` and `collect_to_seq` are derived by the
#' same code [clean_human_spec()] uses.
#'
#' @param afp_cases A table from [simulate_afp_cases()].
#' @param end_date The date the data are taken to be extracted. Default
#'   `as.Date("2024-12-31")`.
#' @param delay_means Mean delays in days, one for each named step:
#'   `collect_to_lab`, `lab_to_culture`, `culture_to_itd` and
#'   `itd_to_sequencing`. Default `c(collect_to_lab = 3, lab_to_culture = 14,
#'   culture_to_itd = 7, itd_to_sequencing = 14)`.
#' @param delay_size Size parameter of the negative binomial each delay is
#'   drawn from; smaller values give longer tails. Default `2`.
#' @param share_pending_resulted Share of the cases pending classification,
#'   other than laboratory pending, whose specimens have a culture result.
#'   Default `0.5`.
#' @param pending_labels Classification labels of a case pending
#'   classification and of a laboratory pending case. Default
#'   `c(pending = "PENDING", lab_pending = "LAB PENDING")`.
#' @param positive_pattern Regular expression a `classification_all` label
#'   must match for the specimens of a case to receive ITD and sequencing
#'   results. Default `"WPV|VDPV"`.
#' @param seed Seed for the random draws. Default `5L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per specimen and the columns `specimen_id`,
#'   `epid`, `specimen_number` (integer), `country_iso3code`, `adm0`,
#'   `adm1`, `adm2`, `adm0_guid`, `adm1_guid`, `adm2_guid`,
#'   `date_stool_collected`, `year_collection`, `month_collection`,
#'   `specimen_stool_condition_name`, `date_stool_received_in_lab`,
#'   `date_final_cell_culture_results`, `date_itd_result_reported`,
#'   `dateof_sequencing`, `collect_to_lab`, `lab_to_culture` and
#'   `collect_to_seq`. Dates are `Date`; years, months and intervals are
#'   double.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' cases <- simulate_afp_cases(admin, n_positive = 2)
#' specimens <- simulate_human_specimens(cases)
#' dplyr::count(specimens, specimen_number)
#' @export
simulate_human_specimens <- function(afp_cases,
                                     end_date = as.Date("2024-12-31"),
                                     delay_means = c(
                                       collect_to_lab = 3,
                                       lab_to_culture = 14,
                                       culture_to_itd = 7,
                                       itd_to_sequencing = 14
                                     ),
                                     delay_size = 2,
                                     share_pending_resulted = 0.5,
                                     pending_labels = c(
                                       pending = "PENDING",
                                       lab_pending = "LAB PENDING"
                                     ),
                                     positive_pattern = "WPV|VDPV",
                                     seed = 5L) {
  .sim_check_columns(afp_cases, .sim_afp_columns, "simulate_afp_cases")
  end_date <- .sim_check_date(end_date)
  .sim_check_probs(delay_means, names = .sim_spec_delay_names)
  .sim_check_number(delay_size, positive = TRUE)
  .sim_check_share(share_pending_resulted)
  .sim_check_labels(pending_labels, names = c("pending", "lab_pending"))
  .sim_check_labels(positive_pattern)
  .sim_check_seed(seed)

  cases <- afp_cases |>
    dplyr::mutate(
      lab_pending = .data$classification_all %in%
        pending_labels[["lab_pending"]],
      pending = .data$lab_pending |
        .data$classification_all %in% pending_labels[["pending"]],
      positive = !is.na(.data$classification_all) &
        grepl(positive_pattern, .data$classification_all)
    )

  stools <- dplyr::bind_rows(
    dplyr::mutate(
      cases,
      specimen_number = 1L,
      date_stool_collected = .data$stool1collection_date,
      specimen_stool_condition_name = .data$stool1condition
    ),
    dplyr::mutate(
      cases,
      specimen_number = 2L,
      date_stool_collected = .data$stool2collection_date,
      specimen_stool_condition_name = .data$stool2condition
    )
  ) |>
    dplyr::filter(!is.na(.data$date_stool_collected)) |>
    dplyr::arrange(.data$epid, .data$specimen_number)

  .sim_with_seed(seed, {
    stools |>
      .sim_spec_dates(
        end_date = end_date, delay_means = delay_means,
        delay_size = delay_size,
        share_pending_resulted = share_pending_resulted
      ) |>
      .spec_add_collection_vars() |>
      .spec_add_intervals() |>
      dplyr::mutate(
        specimen_id = sprintf("%s-S%d", .data$epid, .data$specimen_number)
      ) |>
      dplyr::select(dplyr::all_of(.sim_spec_columns))
  })
}

# the laboratory date chain of each specimen, kept to the state of its case
#' @noRd
.sim_spec_dates <- function(stools, end_date, delay_means, delay_size,
                            share_pending_resulted) {
  n <- nrow(stools)
  days <- \(step) .sim_draw_days(n, delay_means[[step]], delay_size)
  resulted_draw <- stats::runif(n)

  stools |>
    dplyr::mutate(
      received = .data$date_stool_collected + days("collect_to_lab"),
      culture = .data$received + days("lab_to_culture"),
      itd = .data$culture + days("culture_to_itd"),
      sequencing = .data$itd + days("itd_to_sequencing"),
      # one draw per case, so both specimens of a case share it
      resulted = stats::ave(
        resulted_draw, .data$epid, FUN = \(x) x[[1]]
      ) < share_pending_resulted,
      date_stool_received_in_lab = dplyr::case_when(
        .data$lab_pending ~ as.Date(NA),
        .data$pending ~ .sim_censor_after(.data$received, end_date),
        .default = pmin(.data$received, end_date)
      ),
      date_final_cell_culture_results = dplyr::case_when(
        .data$lab_pending ~ as.Date(NA),
        .data$pending & !.data$resulted ~ as.Date(NA),
        .data$pending ~ .sim_censor_after(.data$culture, end_date),
        .default = pmin(.data$culture, end_date)
      ),
      date_itd_result_reported = dplyr::if_else(
        .data$positive & !.data$pending,
        pmin(.data$itd, end_date), as.Date(NA)
      ),
      dateof_sequencing = dplyr::if_else(
        .data$positive & !.data$pending,
        pmin(.data$sequencing, end_date), as.Date(NA)
      ),
      # a missing receipt leaves every later laboratory date missing
      date_final_cell_culture_results = dplyr::if_else(
        is.na(.data$date_stool_received_in_lab), as.Date(NA),
        .data$date_final_cell_culture_results
      )
    )
}

.sim_spec_columns <- c(
  "specimen_id", "epid", "specimen_number", "country_iso3code", "adm0",
  "adm1", "adm2", "adm0_guid", "adm1_guid", "adm2_guid",
  "date_stool_collected", "year_collection", "month_collection",
  "specimen_stool_condition_name", "date_stool_received_in_lab",
  "date_final_cell_culture_results", "date_itd_result_reported",
  "dateof_sequencing", "collect_to_lab", "lab_to_culture", "collect_to_seq"
)
