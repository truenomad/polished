# all tables -------------------------------------------------------------------

#' Simulate a Complete Set of Polio Surveillance Tables
#'
#' Builds one synthetic geography and, on it, the district population, the
#' AFP cases, the environmental samples, the virus detections drawn from
#' them, the stool specimens of the cases and the campaign rounds. The
#' tables share their districts, GUIDs and identifiers, so they join as the
#' real tables do.
#'
#' Each table is drawn with its own seed, taken from `seed` in turn, so
#' changing the arguments of one table leaves the others unchanged.
#'
#' @param start_date,end_date The window every table covers. Defaults
#'   `as.Date("2023-01-01")` and `as.Date("2024-12-31")`. The population
#'   covers each calendar year the window touches.
#' @param admin_args Further arguments to [simulate_admin_frame()], as a
#'   named list. Default `list()`.
#' @param population_args Further arguments to [simulate_population()].
#'   Default `list()`.
#' @param afp_args Further arguments to [simulate_afp_cases()]. Default
#'   `list()`.
#' @param es_args Further arguments to [simulate_es_samples()]. Default
#'   `list()`.
#' @param detection_args Further arguments to [simulate_detections()].
#'   Default `list()`.
#' @param specimen_args Further arguments to [simulate_human_specimens()].
#'   Default `list()`.
#' @param campaign_args Further arguments to [simulate_campaigns()]. Default
#'   `list()`.
#' @param seed Seed for the random draws. Default `1L`. The geography uses
#'   `seed`, the population `seed + 1`, the cases `seed + 2`, the samples
#'   `seed + 3`, the specimens `seed + 4` and the campaigns `seed + 5`. The
#'   caller's random state is left as it was.
#'
#' @return A named list of tibbles: `admin`, `population`, `afp_cases`,
#'   `es_samples`, `detections`, `human_specimens` and `campaigns`.
#'
#' @examples
#' polio <- simulate_polio_data(
#'   admin_args = list(n_provinces = 2, n_districts = 3)
#' )
#' names(polio)
#' polio$detections
#' @export
simulate_polio_data <- function(start_date = as.Date("2023-01-01"),
                                end_date = as.Date("2024-12-31"),
                                admin_args = list(),
                                population_args = list(),
                                afp_args = list(),
                                es_args = list(),
                                detection_args = list(),
                                specimen_args = list(),
                                campaign_args = list(),
                                seed = 1L) {
  start_date <- .sim_check_date(start_date)
  end_date <- .sim_check_date(end_date)
  .sim_check_window(start_date, end_date)
  .sim_check_seed(seed)
  arg_lists <- list(
    admin_args = admin_args, population_args = population_args,
    afp_args = afp_args, es_args = es_args,
    detection_args = detection_args, specimen_args = specimen_args,
    campaign_args = campaign_args
  )
  for (name in names(arg_lists)) {
    .sim_check_arg_list(arg_lists[[name]], name)
  }

  years <- .sim_year_spans(start_date, end_date)$year

  admin <- rlang::exec(simulate_admin_frame, !!!admin_args, seed = seed)

  population <- rlang::exec(
    simulate_population, admin = admin, years = years, !!!population_args,
    seed = seed + 1
  )

  afp_cases <- rlang::exec(
    simulate_afp_cases, admin = admin, population = population,
    start_date = start_date, end_date = end_date, !!!afp_args,
    seed = seed + 2
  )

  es_samples <- rlang::exec(
    simulate_es_samples, admin = admin, start_date = start_date,
    end_date = end_date, !!!es_args, seed = seed + 3
  )

  detections <- rlang::exec(
    simulate_detections, afp_cases = afp_cases, es_samples = es_samples,
    !!!detection_args
  )

  human_specimens <- rlang::exec(
    simulate_human_specimens, afp_cases = afp_cases, end_date = end_date,
    !!!specimen_args, seed = seed + 4
  )

  campaigns <- rlang::exec(
    simulate_campaigns, admin = admin, start_date = start_date,
    end_date = end_date, !!!campaign_args, seed = seed + 5
  )

  list(
    admin = admin,
    population = population,
    afp_cases = afp_cases,
    es_samples = es_samples,
    detections = detections,
    human_specimens = human_specimens,
    campaigns = campaigns
  )
}

# a list of further arguments: named, and not one this function sets itself
#' @noRd
.sim_check_arg_list <- function(x, name, call = rlang::caller_env()) {
  reserved <- c(
    "seed", "admin", "population", "years", "start_date", "end_date",
    "afp_cases", "es_samples"
  )
  if (!is.list(x) || (length(x) > 0 && !rlang::is_named(x))) {
    cli::cli_abort("{.arg {name}} must be a named list.", call = call)
  }
  clash <- intersect(names(x), reserved)
  if (length(clash) > 0) {
    cli::cli_abort(
      c(
        "{.arg {name}} cannot set {.arg {clash}}.",
        "i" = "Pass the window and {.arg seed} to
               {.fn simulate_polio_data} itself."
      ),
      call = call
    )
  }
  invisible(x)
}
