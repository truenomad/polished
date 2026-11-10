# geography --------------------------------------------------------------------

#' Simulate a Synthetic Geography
#'
#' Builds an invented geography of countries, provinces and districts, one
#' row per district, that the other simulators place cases, samples and
#' population on. Names follow the pattern `PROVINCE 01` and
#' `DISTRICT 01-03`, where the district name carries its province number.
#' Country codes come from the ISO 3166 range reserved for user-assigned
#' codes, so no code belongs to a real country. GUIDs are drawn at random.
#'
#' Names are upper case by default, as POLIS writes them; pass other
#' prefixes to change that. The column names follow the boundary and
#' population tables read in rise-afg
#' `02_scripts/2c_indicators/2civ_surveillance.R` and
#' `02_scripts/2c_indicators/2cxxviii_genetic.R`. The `adm1_code` and
#' `adm2_code` columns are added here to build identifiers.
#'
#' District centres are drawn uniformly inside a longitude and latitude
#' box. The default box lies in the open South Atlantic, so no synthetic
#' point falls inside a real country.
#'
#' @param n_countries Number of countries. Default `1L`.
#' @param n_provinces Number of provinces in each country. Default `4L`.
#' @param n_districts Number of districts in each province. Default `6L`.
#' @param u15_pop_range Lower and upper bound of the under-15 population of
#'   a district, drawn uniformly on the log scale. Default
#'   `c(20000, 200000)`.
#' @param iso3_codes Three-letter country codes, one for each country.
#'   Default `NULL`, which gives `QZA`, `QZB` and so on from the
#'   user-assigned range.
#' @param country_prefix,province_prefix,district_prefix Words that start
#'   each country, province and district name. Defaults `"COUNTRY"`,
#'   `"PROVINCE"` and `"DISTRICT"`.
#' @param lon_range,lat_range Bounds of the box district centres are drawn
#'   in, in decimal degrees. Defaults `c(-30, -20)` and `c(-30, -20)`.
#' @param seed Seed for the random draws. Default `1L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per district and the columns
#'   `country_iso3code`, `adm0`, `adm0_guid`, `adm1`, `adm1_guid`,
#'   `adm1_code`, `adm2`, `adm2_guid`, `adm2_code`, `center_lon`,
#'   `center_lat` and `u15_pop`.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' admin
#' @export
simulate_admin_frame <- function(n_countries = 1L,
                                 n_provinces = 4L,
                                 n_districts = 6L,
                                 u15_pop_range = c(20000, 200000),
                                 iso3_codes = NULL,
                                 country_prefix = "COUNTRY",
                                 province_prefix = "PROVINCE",
                                 district_prefix = "DISTRICT",
                                 lon_range = c(-30, -20),
                                 lat_range = c(-30, -20),
                                 seed = 1L) {
  .sim_check_count(n_countries, min = 1)
  .sim_check_count(n_provinces, min = 1)
  .sim_check_count(n_districts, min = 1)
  .sim_check_range(u15_pop_range)
  .sim_check_number(u15_pop_range[[1]], "u15_pop_range", positive = TRUE)
  .sim_check_range(lon_range)
  .sim_check_range(lat_range)
  .sim_check_seed(seed)
  for (x in list(country_prefix, province_prefix, district_prefix)) {
    .sim_check_labels(x, arg = "prefix")
  }

  if (is.null(iso3_codes)) {
    if (n_countries > 26) {
      cli::cli_abort(
        "Pass {.arg iso3_codes} for more than 26 countries."
      )
    }
    iso3_codes <- paste0("QZ", LETTERS[seq_len(n_countries)])
  }
  .sim_check_labels(iso3_codes)
  if (length(iso3_codes) != n_countries || anyDuplicated(iso3_codes) > 0) {
    cli::cli_abort(
      "{.arg iso3_codes} must hold {n_countries} distinct code{?s}."
    )
  }

  .sim_with_seed(seed, {
    countries <- tibble::tibble(
      country_number = seq_len(n_countries),
      country_iso3code = iso3_codes,
      adm0 = sprintf("%s %02d", country_prefix, seq_len(n_countries)),
      adm0_guid = .sim_guid(n_countries)
    )

    provinces <- tidyr::expand_grid(
      country_number = seq_len(n_countries),
      province_in_country = seq_len(n_provinces)
    ) |>
      dplyr::mutate(
        province_number = dplyr::row_number(),
        adm1 = sprintf("%s %02d", province_prefix, .data$province_number),
        adm1_code = sprintf("P%02d", .data$province_number),
        adm1_guid = .sim_guid(dplyr::n())
      )

    districts <- tidyr::expand_grid(
      province_number = provinces$province_number,
      district_number = seq_len(n_districts)
    ) |>
      dplyr::mutate(
        adm2 = sprintf(
          "%s %02d-%02d", district_prefix, .data$province_number,
          .data$district_number
        ),
        adm2_code = sprintf("D%02d", .data$district_number),
        adm2_guid = .sim_guid(dplyr::n()),
        center_lon = round(stats::runif(dplyr::n(), lon_range[[1]],
                                        lon_range[[2]]), 4),
        center_lat = round(stats::runif(dplyr::n(), lat_range[[1]],
                                        lat_range[[2]]), 4),
        u15_pop = round(exp(stats::runif(
          dplyr::n(), log(u15_pop_range[[1]]), log(u15_pop_range[[2]])
        )))
      )

    districts |>
      dplyr::left_join(provinces, by = "province_number") |>
      dplyr::left_join(countries, by = "country_number") |>
      dplyr::select(
        "country_iso3code", "adm0", "adm0_guid", "adm1", "adm1_guid",
        "adm1_code", "adm2", "adm2_guid", "adm2_code", "center_lon",
        "center_lat", "u15_pop"
      )
  })
}

# population -------------------------------------------------------------------

#' Simulate District Population by Year
#'
#' Projects the under-15 population of each district in a synthetic
#' geography over a run of years, and derives the under-5 and total
#' populations from it. The first year takes the under-15 population of
#' the geography, each later year grows by `growth_rate`, and every
#' district-year carries a small random departure from that trend.
#'
#' The column names follow how rise-afg reads `polished_pop_adm2` in
#' `02_scripts/2c_indicators/2civ_surveillance.R`,
#' `02_scripts/2c_indicators/2cvi_timeliness.R` and
#' `02_scripts/00_helper_functions/0j_spi_helpers.R`.
#'
#' @param admin A geography from [simulate_admin_frame()]. Default
#'   `simulate_admin_frame()`.
#' @param years Calendar years to cover. Default `2023:2024`.
#' @param growth_rate Annual growth of the population, as a proportion.
#'   Default `0.03`.
#' @param noise_sd Standard deviation of the random departure from trend,
#'   on the log scale. Default `0.02`.
#' @param u5_share Share of the total population aged under 5. Default
#'   `0.17`.
#' @param u15_share Share of the total population aged under 15. Default
#'   `0.45`. Must be at least `u5_share`.
#' @param seed Seed for the random draws. Default `1L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per district and year and the columns
#'   `country_iso3code`, `adm0`, `adm1`, `adm2`, `adm0_guid`, `adm1_guid`,
#'   `adm2_guid`, `year` (integer), and `u5_pop`, `u15_pop` and
#'   `total_pop` (double, whole numbers).
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' simulate_population(admin, years = 2022:2024)
#' @export
simulate_population <- function(admin = simulate_admin_frame(),
                                years = 2023:2024,
                                growth_rate = 0.03,
                                noise_sd = 0.02,
                                u5_share = 0.17,
                                u15_share = 0.45,
                                seed = 1L) {
  .sim_check_admin(admin)
  if (!is.numeric(years) || length(years) == 0 || anyNA(years) ||
        any(years != round(years))) {
    cli::cli_abort("{.arg years} must be whole numbers.")
  }
  if (!is.numeric(growth_rate) || length(growth_rate) != 1 ||
        is.na(growth_rate) || growth_rate <= -1) {
    cli::cli_abort("{.arg growth_rate} must be a single number above -1.")
  }
  .sim_check_number(noise_sd)
  .sim_check_share(u5_share)
  .sim_check_share(u15_share)
  if (u15_share == 0 || u5_share > u15_share) {
    cli::cli_abort(
      "{.arg u15_share} must be above 0 and at least {.arg u5_share}."
    )
  }
  .sim_check_seed(seed)

  first_year <- min(years)

  .sim_with_seed(seed, {
    tidyr::expand_grid(
      admin |>
        dplyr::select(
          "country_iso3code", "adm0", "adm1", "adm2", "adm0_guid",
          "adm1_guid", "adm2_guid", base_pop = "u15_pop"
        ),
      year = as.integer(sort(unique(years)))
    ) |>
      dplyr::mutate(
        u15_pop = round(
          .data$base_pop * (1 + growth_rate)^(.data$year - first_year) *
            exp(stats::rnorm(dplyr::n(), 0, noise_sd))
        ),
        total_pop = round(.data$u15_pop / u15_share),
        u5_pop = round(.data$total_pop * u5_share)
      ) |>
      dplyr::select(
        "country_iso3code", "adm0", "adm1", "adm2", "adm0_guid",
        "adm1_guid", "adm2_guid", "year", "u5_pop", "u15_pop", "total_pop"
      )
  })
}
