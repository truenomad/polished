# campaigns --------------------------------------------------------------------

#' Simulate Vaccination Campaign Rounds
#'
#' Generates one row per district reached by a supplementary immunisation
#' activity round, shaped like the cleaned POLIS campaign table that
#' [clean_sia()] writes. Rounds start every `round_interval_days` from a
#' random first day, each with one vaccine type and one status, and each
#' district takes part in a round with probability `share_districts`. Every
#' country in the geography follows the same calendar of rounds.
#'
#' The table carries the activity columns the environmental site scorecard
#' reads ([es_score_cols()]) and the sub-activity columns
#' [calc_polio_indicators()] reads, so `vaccine_type` repeats
#' `activity_vaccine_type` and `date_from` repeats `activity_date_from`.
#'
#' @param admin A geography from [simulate_admin_frame()]. Default
#'   `simulate_admin_frame()`.
#' @param start_date,end_date The window in which rounds start. Defaults
#'   `as.Date("2023-01-01")` and `as.Date("2024-12-31")`.
#' @param round_interval_days Days between the starts of two rounds. Default
#'   `60`.
#' @param round_days Length of a round in days. Default `4`.
#' @param share_districts Chance that a district takes part in a round.
#'   Default `0.8`.
#' @param vaccine_types,vaccine_type_probs Vaccine types a round can use and
#'   their weights. Defaults `c("bOPV", "nOPV2")` and `c(0.7, 0.3)`.
#' @param statuses,status_probs Round statuses and their weights. Defaults
#'   `c("Done", "Cancelled")` and `c(0.95, 0.05)`.
#' @param seed Seed for the random draws. Default `6L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per round and district and the columns
#'   `sia_sub_activity_code`, `country_iso3code`, `adm0`, `adm1`, `adm2`,
#'   `adm0_guid`, `adm1_guid`, `adm2_guid`, `activity_vaccine_type`,
#'   `vaccine_type`, `activity_status`, `activity_date_from`, `date_from`,
#'   `date_to`, `year_start` and `month_start`. Dates are `Date`, the year
#'   and month are integer and the other columns are character.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' campaigns <- simulate_campaigns(admin)
#' dplyr::count(campaigns, activity_vaccine_type)
#' @export
simulate_campaigns <- function(admin = simulate_admin_frame(),
                               start_date = as.Date("2023-01-01"),
                               end_date = as.Date("2024-12-31"),
                               round_interval_days = 60,
                               round_days = 4,
                               share_districts = 0.8,
                               vaccine_types = c("bOPV", "nOPV2"),
                               vaccine_type_probs = c(0.7, 0.3),
                               statuses = c("Done", "Cancelled"),
                               status_probs = c(0.95, 0.05),
                               seed = 6L) {
  .sim_check_admin(admin)
  start_date <- .sim_check_date(start_date)
  end_date <- .sim_check_date(end_date)
  .sim_check_window(start_date, end_date)
  .sim_check_count(round_interval_days, min = 1)
  .sim_check_count(round_days, min = 1)
  .sim_check_share(share_districts)
  .sim_check_labels(vaccine_types)
  .sim_check_probs(vaccine_type_probs, n = length(vaccine_types))
  .sim_check_labels(statuses)
  .sim_check_probs(status_probs, n = length(statuses))
  .sim_check_seed(seed)

  .sim_with_seed(seed, {
    first <- start_date + floor(stats::runif(1) * round_interval_days)
    starts <- if (first > end_date) {
      as.Date(character(0))
    } else {
      seq(first, end_date, by = round_interval_days)
    }
    rounds <- tibble::tibble(
      round_number = seq_along(starts),
      date_from = starts,
      vaccine_type = sample(
        vaccine_types, length(starts), replace = TRUE,
        prob = vaccine_type_probs
      ),
      activity_status = sample(
        statuses, length(starts), replace = TRUE, prob = status_probs
      )
    )

    tidyr::expand_grid(
      rounds,
      dplyr::select(
        admin, "country_iso3code", "adm0", "adm1", "adm2", "adm0_guid",
        "adm1_guid", "adm2_guid"
      )
    ) |>
      dplyr::filter(stats::runif(dplyr::n()) < share_districts) |>
      dplyr::mutate(
        sia_sub_activity_code = sprintf(
          "SIA-%s-%s-%02d", .data$country_iso3code, .sim_yy(.data$date_from),
          .data$round_number
        ),
        activity_vaccine_type = .data$vaccine_type,
        activity_date_from = .data$date_from,
        date_to = .data$date_from + round_days - 1,
        year_start = as.integer(format(.data$date_from, "%Y")),
        month_start = as.integer(format(.data$date_from, "%m"))
      ) |>
      dplyr::select(dplyr::all_of(.sim_campaign_columns))
  })
}

.sim_campaign_columns <- c(
  "sia_sub_activity_code", "country_iso3code", "adm0", "adm1", "adm2",
  "adm0_guid", "adm1_guid", "adm2_guid", "activity_vaccine_type",
  "vaccine_type", "activity_status", "activity_date_from", "date_from",
  "date_to", "year_start", "month_start"
)
