# environmental samples --------------------------------------------------------

#' Simulate Environmental Surveillance Samples
#'
#' Generates one row per environmental surveillance sample, shaped like the
#' cleaned POLIS sample table (`polished_es`) that rise-afg reads. Sites are
#' placed in districts with weight by population and sampled every
#' `sampling_interval_days`, with a share of collections missed. A sample
#' finds poliovirus with probability `share_positive`; otherwise it finds
#' Sabin virus, a non-polio enterovirus or nothing. A sample whose result is
#' not due by `end_date` is pending and carries no classification.
#'
#' The labels follow the cleaning's sample outcomes: a poliovirus sample
#' carries its virus label in `vtype` and `classification_all`; a Sabin or
#' non-polio enterovirus sample carries `vtype` `"none"`; a negative sample
#' carries no `vtype`; and a pending sample carries neither. The raw
#' detection fields (`wild1`, `vdpv1` to `vdpv3`, `vaccine1` to `vaccine3`)
#' hold `detected_flag` where the virus was found and are missing
#' otherwise.
#'
#' Column names and outcome labels follow how rise-afg reads `polished_es`
#' in `02_scripts/2c_indicators/2cvii_es.R`, `2cviii_es_scorecard.R`,
#' `2cxxxvi_es_profiles.R`, `2ci_detections.R` and
#' `2cxl_detection_surveillance.R`, and in
#' `02_scripts/00_helper_functions/0i_pending_helpers.R`. The virus pattern
#' matches `detection_vtype_pattern` in
#' `02_scripts/00_helper_functions/00_params.R`. The sample condition
#' labels are not fixed by that code and are chosen here.
#'
#' @param admin A geography from [simulate_admin_frame()]. Default
#'   `simulate_admin_frame()`.
#' @param start_date,end_date First and last day of collection. Defaults
#'   `as.Date("2023-01-01")` and `as.Date("2024-12-31")`. `end_date` is
#'   also the date the data are taken to be extracted.
#' @param n_sites Number of sampling sites. Default `8L`.
#' @param sampling_interval_days Days between scheduled collections at a
#'   site. Default `28`.
#' @param share_missed Share of scheduled collections not made. Default
#'   `0.05`.
#' @param share_positive Share of samples in which poliovirus is found.
#'   Default `0.1`.
#' @param positive_types,positive_type_probs Virus labels a positive sample
#'   can carry, and their weights. Defaults `"WPV 1"` and `1`.
#' @param outcome_probs Weights for the outcome of a sample without
#'   poliovirus. Default `c(sabin = 0.3, npev = 0.45, negative = 0.25)`.
#' @param outcome_labels Classification labels for those outcomes. Default
#'   `c(sabin = "SABIN", npev = "NPEV", negative = "NEGATIVE")`.
#' @param sabin_serotype_probs Chance that a Sabin sample carries Sabin
#'   type 1, 2 and 3. A Sabin sample with none drawn carries type 1.
#'   Default `c(0.6, 0, 0.6)`.
#' @param result_delay_mean Mean days from collection to the final result.
#'   Default `21`.
#' @param notify_delay_mean Mean days from a positive result to
#'   notification to headquarters. Default `3`.
#' @param created_delay_mean Mean days from collection to the record being
#'   created. Default `2`.
#' @param delay_size Size parameter of the negative binomial each delay is
#'   drawn from. Default `2`.
#' @param sample_conditions,sample_condition_probs Sample condition labels
#'   and their weights. Defaults `c("Good", "Poor")` and `c(0.95, 0.05)`.
#' @param clusters Cluster names a wild positive can carry. Default
#'   `c("CLUSTER A", "CLUSTER B")`.
#' @param nt_changes_mean Mean nucleotide changes of a vaccine-derived
#'   positive. Default `8`.
#' @param detected_flag Value written in a raw detection field when the
#'   virus was found. Default `"Yes"`.
#' @param jitter_deg Largest offset of a site from its district centre, in
#'   decimal degrees. Default `0.05`.
#' @param seed Seed for the random draws. Default `3L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per sample. Dates are `Date`,
#'   `year_collection` and `month_collection` are integer, the Sabin,
#'   `npev` and `nvaccine` flags are integer 0 or 1, the site coordinates
#'   are double, and the remaining columns are character.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' samples <- simulate_es_samples(admin, n_sites = 3)
#' dplyr::count(samples, classification_all)
#' @export
simulate_es_samples <- function(admin = simulate_admin_frame(),
                                start_date = as.Date("2023-01-01"),
                                end_date = as.Date("2024-12-31"),
                                n_sites = 8L,
                                sampling_interval_days = 28,
                                share_missed = 0.05,
                                share_positive = 0.1,
                                positive_types = "WPV 1",
                                positive_type_probs = 1,
                                outcome_probs = c(
                                  sabin = 0.3, npev = 0.45, negative = 0.25
                                ),
                                outcome_labels = c(
                                  sabin = "SABIN", npev = "NPEV",
                                  negative = "NEGATIVE"
                                ),
                                sabin_serotype_probs = c(0.6, 0, 0.6),
                                result_delay_mean = 21,
                                notify_delay_mean = 3,
                                created_delay_mean = 2,
                                delay_size = 2,
                                sample_conditions = c("Good", "Poor"),
                                sample_condition_probs = c(0.95, 0.05),
                                clusters = c("CLUSTER A", "CLUSTER B"),
                                nt_changes_mean = 8,
                                detected_flag = "Yes",
                                jitter_deg = 0.05,
                                seed = 3L) {
  .sim_check_admin(admin)
  start_date <- .sim_check_date(start_date)
  end_date <- .sim_check_date(end_date)
  .sim_check_window(start_date, end_date)
  .sim_check_count(n_sites)
  .sim_check_count(sampling_interval_days, min = 1)
  .sim_check_share(share_missed)
  .sim_check_share(share_positive)
  .sim_check_labels(positive_types)
  .sim_check_probs(positive_type_probs, n = length(positive_types))
  .sim_check_probs(outcome_probs, names = c("sabin", "npev", "negative"))
  .sim_check_labels(outcome_labels, names = c("sabin", "npev", "negative"))
  if (length(sabin_serotype_probs) != 3) {
    cli::cli_abort("{.arg sabin_serotype_probs} must hold three shares.")
  }
  for (x in sabin_serotype_probs) {
    .sim_check_share(x, arg = "sabin_serotype_probs")
  }
  .sim_check_number(result_delay_mean)
  .sim_check_number(notify_delay_mean)
  .sim_check_number(created_delay_mean)
  .sim_check_number(delay_size, positive = TRUE)
  .sim_check_labels(sample_conditions)
  .sim_check_probs(sample_condition_probs, n = length(sample_conditions))
  .sim_check_labels(clusters)
  .sim_check_number(nt_changes_mean)
  .sim_check_labels(detected_flag)
  .sim_check_number(jitter_deg)
  .sim_check_seed(seed)

  .sim_with_seed(seed, {
    sites <- .sim_es_sites(admin, n_sites, jitter_deg)

    sites |>
      .sim_es_schedule(start_date, end_date, sampling_interval_days) |>
      dplyr::filter(stats::runif(dplyr::n()) >= share_missed) |>
      .sim_es_results(
        end_date = end_date, share_positive = share_positive,
        positive_types = positive_types,
        positive_type_probs = positive_type_probs,
        outcome_probs = outcome_probs, outcome_labels = outcome_labels,
        sabin_serotype_probs = sabin_serotype_probs,
        result_delay_mean = result_delay_mean,
        notify_delay_mean = notify_delay_mean,
        created_delay_mean = created_delay_mean, delay_size = delay_size,
        sample_conditions = sample_conditions,
        sample_condition_probs = sample_condition_probs,
        clusters = clusters, nt_changes_mean = nt_changes_mean,
        detected_flag = detected_flag
      )
  })
}

# the sites, each in a district drawn with weight by population
#' @noRd
.sim_es_sites <- function(admin, n_sites, jitter_deg) {
  admin |>
    dplyr::slice_sample(
      n = n_sites, replace = TRUE, weight_by = .data$u15_pop
    ) |>
    dplyr::select(-"u15_pop") |>
    dplyr::mutate(
      site_number = dplyr::row_number(),
      site_code = sprintf("S%02d", .data$site_number),
      site_name = sprintf("SITE %02d", .data$site_number),
      site_guid = .sim_guid(dplyr::n()),
      site_x_coordinate = .data$center_lon +
        stats::runif(dplyr::n(), -jitter_deg, jitter_deg),
      site_y_coordinate = .data$center_lat +
        stats::runif(dplyr::n(), -jitter_deg, jitter_deg)
    )
}

# one row per scheduled collection, from a random first day at each site
#' @noRd
.sim_es_schedule <- function(sites, start_date, end_date, interval) {
  sites |>
    dplyr::mutate(
      first = start_date + floor(stats::runif(dplyr::n()) * interval),
      collection_date = lapply(
        .data$first,
        \(first) {
          if (first > end_date) {
            return(as.Date(character(0)))
          }
          seq(first, end_date, by = interval)
        }
      )
    ) |>
    tidyr::unnest("collection_date") |>
    dplyr::arrange(.data$collection_date, .data$site_number)
}

# outcome, result dates and the labels the cleaning derives from them
#' @noRd
.sim_es_results <- function(samples, end_date, share_positive, positive_types,
                            positive_type_probs, outcome_probs, outcome_labels,
                            sabin_serotype_probs, result_delay_mean,
                            notify_delay_mean, created_delay_mean, delay_size,
                            sample_conditions, sample_condition_probs, clusters,
                            nt_changes_mean, detected_flag) {
  n <- nrow(samples)

  samples |>
    dplyr::mutate(
      positive_draw = stats::runif(n) < share_positive,
      positive_type = sample(
        positive_types, n, replace = TRUE, prob = positive_type_probs
      ),
      outcome = sample(
        names(outcome_probs), n, replace = TRUE, prob = outcome_probs
      ),
      s1 = stats::runif(n) < sabin_serotype_probs[[1]],
      s2 = stats::runif(n) < sabin_serotype_probs[[2]],
      s3 = stats::runif(n) < sabin_serotype_probs[[3]],
      s1 = .data$s1 | (!.data$s2 & !.data$s3),
      result_date = .data$collection_date +
        .sim_draw_days(n, result_delay_mean, delay_size),
      notified = .data$result_date +
        .sim_draw_days(n, notify_delay_mean, delay_size),
      created_date = pmin(
        .data$collection_date +
          .sim_draw_days(n, created_delay_mean, delay_size),
        end_date
      ),
      sample_condition = sample(
        sample_conditions, n, replace = TRUE, prob = sample_condition_probs
      ),
      cluster = sample(clusters, n, replace = TRUE),
      nt = stats::rpois(n, nt_changes_mean),
      pending = .data$result_date > end_date,
      outcome = dplyr::case_when(
        .data$pending ~ "pending",
        .data$positive_draw ~ "positive",
        .default = .data$outcome
      ),
      virus = dplyr::if_else(
        .data$outcome == "positive", .data$positive_type, NA_character_
      ),
      is_vdpv = !is.na(.data$virus) & grepl("VDPV", .data$virus),
      vtype = dplyr::case_when(
        .data$outcome == "positive" ~ .data$virus,
        .data$outcome %in% c("sabin", "npev") ~ "none",
        .default = NA_character_
      ),
      classification_all = dplyr::case_when(
        .data$outcome == "positive" ~ .data$virus,
        .data$outcome == "sabin" ~ outcome_labels[["sabin"]],
        .data$outcome == "npev" ~ outcome_labels[["npev"]],
        .data$outcome == "negative" ~ outcome_labels[["negative"]],
        .default = NA_character_
      ),
      sabin = .data$outcome == "sabin",
      sabin1 = dplyr::if_else(.data$pending, NA_integer_,
                              as.integer(.data$sabin & .data$s1)),
      sabin2 = dplyr::if_else(.data$pending, NA_integer_,
                              as.integer(.data$sabin & .data$s2)),
      sabin3 = dplyr::if_else(.data$pending, NA_integer_,
                              as.integer(.data$sabin & .data$s3)),
      npev = as.integer(.data$outcome == "npev"),
      nvaccine = 0L,
      wild1 = .sim_virus_flag(.data$virus, "^WPV ?1", detected_flag),
      vdpv1 = .sim_virus_flag(.data$virus, "VDPV ?1", detected_flag),
      vdpv2 = .sim_virus_flag(.data$virus, "VDPV ?2", detected_flag),
      vdpv3 = .sim_virus_flag(.data$virus, "VDPV ?3", detected_flag),
      vaccine1 = dplyr::if_else(
        dplyr::coalesce(.data$sabin1 == 1L, FALSE), detected_flag,
        NA_character_
      ),
      vaccine2 = dplyr::if_else(
        dplyr::coalesce(.data$sabin2 == 1L, FALSE), detected_flag,
        NA_character_
      ),
      vaccine3 = dplyr::if_else(
        dplyr::coalesce(.data$sabin3 == 1L, FALSE), detected_flag,
        NA_character_
      ),
      date_final_combined_result = dplyr::if_else(
        .data$pending, as.Date(NA), .data$result_date
      ),
      date_notification_to_hq = dplyr::if_else(
        .data$outcome == "positive", .data$notified, as.Date(NA)
      ) |>
        .sim_censor_after(end_date),
      virus_clusters = dplyr::if_else(
        .data$outcome == "positive" & grepl("^WPV", .data$virus),
        .data$cluster, NA_character_
      ),
      vdpv_emergence_group_names = dplyr::if_else(
        .data$is_vdpv, paste0(.data$country_iso3code, "-SYN-1"),
        NA_character_
      ),
      nt_changes = dplyr::if_else(
        .data$is_vdpv, as.character(.data$nt), NA_character_
      ),
      vdpv_classification_change_date = dplyr::if_else(
        .data$is_vdpv, .data$date_notification_to_hq, as.Date(NA)
      ),
      year_collection = as.integer(format(.data$collection_date, "%Y")),
      month_collection = as.integer(format(.data$collection_date, "%m"))
    ) |>
    dplyr::mutate(
      sample_seq = dplyr::row_number(),
      .by = c("site_guid", "year_collection")
    ) |>
    dplyr::mutate(
      sample_id = sprintf(
        "ENV-%s-%s-%s-%s-%s-%03d", .data$country_iso3code, .data$adm1_code,
        .data$adm2_code, .data$site_code, .sim_yy(.data$collection_date),
        .data$sample_seq
      )
    ) |>
    dplyr::select(dplyr::all_of(.sim_es_columns))
}

.sim_es_columns <- c(
  "sample_id", "country_iso3code", "adm0", "adm1", "adm2", "adm0_guid",
  "adm1_guid", "adm2_guid", "site_guid", "site_code", "site_name",
  "site_x_coordinate", "site_y_coordinate", "collection_date",
  "year_collection", "month_collection", "sample_condition", "created_date",
  "date_final_combined_result", "date_notification_to_hq", "vtype",
  "classification_all", "sabin1", "sabin2", "sabin3", "npev", "nvaccine",
  "wild1", "vdpv1", "vdpv2", "vdpv3", "vaccine1", "vaccine2", "vaccine3",
  "virus_clusters", "vdpv_emergence_group_names", "nt_changes",
  "vdpv_classification_change_date"
)
