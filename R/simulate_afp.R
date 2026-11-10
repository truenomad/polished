# afp cases --------------------------------------------------------------------

.sim_afp_delay_names <- c(
  "onset_to_notify", "notify_to_invest", "invest_to_stool1",
  "stool1_to_stool2", "stool_to_sent", "sent_to_lab", "lab_to_culture",
  "culture_to_itd", "itd_to_hq", "late_extra", "onset_to_followup_extra",
  "last_dose_before_onset"
)

#' Simulate AFP Case Records
#'
#' Generates one row per acute flaccid paralysis (AFP) case, shaped like
#' the cleaned POLIS case table (`polished_afp`) that rise-afg reads.
#' Non-polio cases arise in each district and year at `npafp_rate` per
#' 100,000 children under 15, and `n_positive` poliovirus cases are added
#' in a few districts. Each case carries a date chain that runs in order
#' from onset through notification, investigation, the two stool
#' collections, dispatch and arrival at the laboratory, to the culture,
#' intratypic differentiation (ITD) and headquarters results.
#'
#' A case whose notification falls after `end_date` has not yet been
#' reported and is left out. Cases are then held pending classification
#' until their culture result is due, and further cases are made pending,
#' with recent onsets more likely, until `share_pending` of all cases are
#' pending. A pending case has no result dates and, by default, no age, as
#' POLIS records it. Among the pending cases, those whose specimens have not
#' reached the laboratory are classified `LAB PENDING`. Stool adequacy is
#' derived from the dates and conditions that result, with `share_adequate`
#' of cases drawn to meet the standard.
#'
#' Column names, the classification vocabulary and the interval columns
#' follow how rise-afg reads `polished_afp` in
#' `02_scripts/00_helper_functions/0i_pending_helpers.R`,
#' `0j_spi_helpers.R`, `0zb_specimen_helpers.R` and `0a_polis_helpers.R`,
#' and in `02_scripts/2c_indicators/2cvi_timeliness.R`, `2ci_detections.R`,
#' `2cxl_detection_surveillance.R`, `2cxxxiii_followup.R` and
#' `2cxxxvi_es_profiles.R`. The
#' pending labels match `polis_pending_classes` and the surveillance type
#' matches `pending_surveillance_type` in
#' `02_scripts/00_helper_functions/00_params.R`. The final culture result
#' vocabulary beyond `Not received in lab`, the sex labels and the
#' follow-up findings are not fixed by that code and are chosen here.
#'
#' @param admin A geography from [simulate_admin_frame()]. Default
#'   `simulate_admin_frame()`.
#' @param population A table from [simulate_population()] covering every
#'   year in the window. Default `NULL`, which holds the under-15
#'   population of `admin` constant across years.
#' @param start_date,end_date First and last day of onset. Defaults
#'   `as.Date("2023-01-01")` and `as.Date("2024-12-31")`. `end_date` is
#'   also the date the data are taken to be extracted.
#' @param npafp_rate Non-polio AFP cases per 100,000 children under 15 per
#'   year. Default `3`.
#' @param n_positive Number of poliovirus-positive AFP cases. Default `4L`.
#' @param positive_types Virus labels a positive case can carry. Default
#'   `c("WPV 1", "cVDPV 2")`.
#' @param positive_type_probs Weights for `positive_types`. Default
#'   `c(0.75, 0.25)`.
#' @param positive_districts Number of districts the positive cases fall
#'   in, chosen with weight by population. Default `3L`.
#' @param positive_onset_margin_days Positive cases have onset at least this
#'   many days before `end_date`, so their results are in. Default `60`.
#' @param share_adequate Share of cases drawn to meet the stool adequacy
#'   standard. Default `0.85`.
#' @param inadequate_mode_probs Weights for how an inadequate case fails:
#'   a late first stool, a single stool, or a stool in poor condition.
#'   Default `c(late = 0.5, one_stool = 0.3, poor_condition = 0.2)`.
#' @param share_pending Share of cases pending classification at
#'   `end_date`. Default `0.1`.
#' @param share_lab_pending Share of the pending cases whose specimens have
#'   not reached the laboratory. Default `0.3`.
#' @param pending_decay_days How quickly the chance of being pending falls
#'   with time since onset, in days. Default `90`.
#' @param share_compatible Share of the classified non-polio pool labelled
#'   compatible. The non-polio rate is inflated to make room for them, so
#'   non-polio cases still follow `npafp_rate`. Default `0.01`.
#' @param share_npev Share of non-polio cases whose culture grew a non-polio
#'   enterovirus. Default `0.1`.
#' @param delay_means Mean delays in days, one for each named step:
#'   `onset_to_notify`, `notify_to_invest`, `invest_to_stool1`,
#'   `stool1_to_stool2` (at least 1), `stool_to_sent`, `sent_to_lab`,
#'   `lab_to_culture`, `culture_to_itd`, `itd_to_hq`, `late_extra` (days
#'   past the 14-day window for a late first stool),
#'   `onset_to_followup_extra` (days past day 60 for the follow-up visit)
#'   and `last_dose_before_onset`. Default `c(onset_to_notify = 4,
#'   notify_to_invest = 1, invest_to_stool1 = 1, stool1_to_stool2 = 1,
#'   stool_to_sent = 1, sent_to_lab = 2, lab_to_culture = 14,
#'   culture_to_itd = 7, itd_to_hq = 2, late_extra = 5,
#'   onset_to_followup_extra = 10, last_dose_before_onset = 60)`.
#' @param delay_size Size parameter of the negative binomial each delay is
#'   drawn from; smaller values give longer tails. Default `2`.
#' @param age_months_range Lower and upper age in months. Default
#'   `c(6, 179)`.
#' @param pending_age_missing Whether pending cases carry no age, as POLIS
#'   records them. Default `TRUE`.
#' @param sexes,sex_probs Sex labels and their weights. Defaults
#'   `c("Male", "Female")` and `c(0.55, 0.45)`.
#' @param opv_doses_mean Mean number of oral polio vaccine doses. Default
#'   `5`.
#' @param zero_dose_share Share of cases with no vaccine dose. Default
#'   `0.05`.
#' @param routine_dose_share Share of oral doses given in routine
#'   immunisation rather than campaigns. Default `0.4`.
#' @param ipv_share Share of dosed cases with one inactivated polio vaccine
#'   dose. Default `0.6`.
#' @param paralysis_sign_probs Chance that a case shows each clinical sign.
#'   A case with all three is a hot case, as the cleaning defines it.
#'   Default `c(asymmetric = 0.6, onset_fever = 0.6, rapid_progress = 0.7)`.
#' @param classification_labels Labels for cases that are not positive.
#'   Default `c(npafp = "NPAFP", compatible = "COMPATIBLE",
#'   pending = "PENDING", lab_pending = "LAB PENDING")`.
#' @param culture_results Final cell culture result labels. Default
#'   `c(negative = "Negative", npev = "NPEV",
#'   poliovirus = "Suspected Poliovirus",
#'   not_received = "Not received in lab")`.
#' @param stool_conditions Stool condition labels. Default
#'   `c(good = "Good", poor = "Poor")`.
#' @param yes_no Labels for the adequacy fields. Default
#'   `c(yes = "Yes", no = "No")`.
#' @param followup_findings,followup_finding_probs Findings at the 60-day
#'   follow-up and their weights. Defaults
#'   `c("No residual weakness/paralysis", "Residual weakness/paralysis",
#'   "Died before follow-up")` and `c(0.6, 0.35, 0.05)`.
#' @param surveillance_type Surveillance type of every case. Default
#'   `"AFP"`.
#' @param clusters Cluster names a wild positive can carry. Default
#'   `c("CLUSTER A", "CLUSTER B")`.
#' @param nt_changes_mean Mean nucleotide changes of a vaccine-derived
#'   positive. Default `8`.
#' @param detected_flag Value written in a raw detection field such as
#'   `wild1` when the virus was found. Default `"Yes"`.
#' @param jitter_deg Largest offset of a case from its district centre, in
#'   decimal degrees. Default `0.05`.
#' @param seed Seed for the random draws. Default `2L`. The caller's random
#'   state is left as it was.
#'
#' @return A tibble with one row per case. Dates are `Date`, interval
#'   columns (`onset_to_notify` and the like) are double and missing
#'   outside the range the cleaning allows, `year_onset` and `month_onset`
#'   are integer, `age_months` and the dose counts are double, and
#'   the follow-up flags, `hot_case` and `virus_is_orphan` are logical.
#'   Coded columns are character.
#'
#' @examples
#' admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
#' cases <- simulate_afp_cases(admin, n_positive = 2)
#' dplyr::count(cases, classification_all)
#' @export
simulate_afp_cases <- function(admin = simulate_admin_frame(),
                               population = NULL,
                               start_date = as.Date("2023-01-01"),
                               end_date = as.Date("2024-12-31"),
                               npafp_rate = 3,
                               n_positive = 4L,
                               positive_types = c("WPV 1", "cVDPV 2"),
                               positive_type_probs = c(0.75, 0.25),
                               positive_districts = 3L,
                               positive_onset_margin_days = 60,
                               share_adequate = 0.85,
                               inadequate_mode_probs = c(
                                 late = 0.5, one_stool = 0.3,
                                 poor_condition = 0.2
                               ),
                               share_pending = 0.1,
                               share_lab_pending = 0.3,
                               pending_decay_days = 90,
                               share_compatible = 0.01,
                               share_npev = 0.1,
                               delay_means = c(
                                 onset_to_notify = 4, notify_to_invest = 1,
                                 invest_to_stool1 = 1, stool1_to_stool2 = 1,
                                 stool_to_sent = 1, sent_to_lab = 2,
                                 lab_to_culture = 14, culture_to_itd = 7,
                                 itd_to_hq = 2, late_extra = 5,
                                 onset_to_followup_extra = 10,
                                 last_dose_before_onset = 60
                               ),
                               delay_size = 2,
                               age_months_range = c(6, 179),
                               pending_age_missing = TRUE,
                               sexes = c("Male", "Female"),
                               sex_probs = c(0.55, 0.45),
                               opv_doses_mean = 5,
                               zero_dose_share = 0.05,
                               routine_dose_share = 0.4,
                               ipv_share = 0.6,
                               paralysis_sign_probs = c(
                                 asymmetric = 0.6, onset_fever = 0.6,
                                 rapid_progress = 0.7
                               ),
                               classification_labels = c(
                                 npafp = "NPAFP", compatible = "COMPATIBLE",
                                 pending = "PENDING",
                                 lab_pending = "LAB PENDING"
                               ),
                               culture_results = c(
                                 negative = "Negative", npev = "NPEV",
                                 poliovirus = "Suspected Poliovirus",
                                 not_received = "Not received in lab"
                               ),
                               stool_conditions = c(
                                 good = "Good", poor = "Poor"
                               ),
                               yes_no = c(yes = "Yes", no = "No"),
                               followup_findings = c(
                                 "No residual weakness/paralysis",
                                 "Residual weakness/paralysis",
                                 "Died before follow-up"
                               ),
                               followup_finding_probs = c(0.6, 0.35, 0.05),
                               surveillance_type = "AFP",
                               clusters = c("CLUSTER A", "CLUSTER B"),
                               nt_changes_mean = 8,
                               detected_flag = "Yes",
                               jitter_deg = 0.05,
                               seed = 2L) {
  .sim_check_admin(admin)
  start_date <- .sim_check_date(start_date)
  end_date <- .sim_check_date(end_date)
  .sim_check_window(start_date, end_date)
  .sim_check_number(npafp_rate)
  .sim_check_count(n_positive)
  .sim_check_labels(positive_types)
  .sim_check_probs(positive_type_probs, n = length(positive_types))
  .sim_check_count(positive_districts, min = 1)
  .sim_check_number(positive_onset_margin_days)
  .sim_check_share(share_adequate)
  .sim_check_probs(
    inadequate_mode_probs, names = c("late", "one_stool", "poor_condition")
  )
  .sim_check_share(share_pending)
  .sim_check_share(share_lab_pending)
  .sim_check_number(pending_decay_days, positive = TRUE)
  .sim_check_share(share_compatible)
  if (share_compatible == 1) {
    cli::cli_abort("{.arg share_compatible} must be below 1.")
  }
  .sim_check_share(share_npev)
  .sim_check_probs(delay_means, names = .sim_afp_delay_names)
  if (delay_means[["stool1_to_stool2"]] < 1) {
    cli::cli_abort(
      "{.arg delay_means} must give {.field stool1_to_stool2} of at least 1."
    )
  }
  .sim_check_number(delay_size, positive = TRUE)
  .sim_check_range(age_months_range)
  if (!rlang::is_bool(pending_age_missing)) {
    cli::cli_abort("{.arg pending_age_missing} must be TRUE or FALSE.")
  }
  .sim_check_labels(sexes)
  .sim_check_probs(sex_probs, n = length(sexes))
  .sim_check_number(opv_doses_mean)
  .sim_check_share(zero_dose_share)
  .sim_check_share(routine_dose_share)
  .sim_check_share(ipv_share)
  .sim_check_probs(
    paralysis_sign_probs,
    names = c("asymmetric", "onset_fever", "rapid_progress")
  )
  for (x in paralysis_sign_probs) {
    .sim_check_share(x, arg = "paralysis_sign_probs")
  }
  .sim_check_labels(
    classification_labels,
    names = c("npafp", "compatible", "pending", "lab_pending")
  )
  .sim_check_labels(
    culture_results,
    names = c("negative", "npev", "poliovirus", "not_received")
  )
  .sim_check_labels(stool_conditions, names = c("good", "poor"))
  .sim_check_labels(yes_no, names = c("yes", "no"))
  .sim_check_labels(followup_findings)
  .sim_check_probs(followup_finding_probs, n = length(followup_findings))
  .sim_check_labels(surveillance_type)
  .sim_check_labels(clusters)
  .sim_check_number(nt_changes_mean)
  .sim_check_labels(detected_flag)
  .sim_check_number(jitter_deg)
  .sim_check_seed(seed)

  spans <- .sim_year_spans(start_date, end_date)
  pop <- .sim_afp_population(admin, population, spans$year)

  .sim_with_seed(seed, {
    onsets <- .sim_afp_onsets(
      admin, pop, spans,
      rate = npafp_rate / (1 - share_compatible),
      n_positive = n_positive, positive_types = positive_types,
      positive_type_probs = positive_type_probs,
      positive_districts = positive_districts,
      first_onset = start_date,
      last_onset = max(start_date, end_date - positive_onset_margin_days)
    )

    drawn <- onsets |>
      dplyr::left_join(
        dplyr::select(admin, -"u15_pop"), by = "adm2_guid"
      ) |>
      dplyr::arrange(.data$paralysis_onset_date, .data$adm2_guid) |>
      .sim_afp_draws(
        delay_means = delay_means, delay_size = delay_size,
        share_adequate = share_adequate,
        inadequate_mode_probs = inadequate_mode_probs,
        age_months_range = age_months_range, sexes = sexes,
        sex_probs = sex_probs, opv_doses_mean = opv_doses_mean,
        zero_dose_share = zero_dose_share,
        routine_dose_share = routine_dose_share, ipv_share = ipv_share,
        paralysis_sign_probs = paralysis_sign_probs,
        followup_findings = followup_findings,
        followup_finding_probs = followup_finding_probs,
        clusters = clusters, nt_changes_mean = nt_changes_mean,
        jitter_deg = jitter_deg
      ) |>
      .sim_afp_dates()

    drawn |>
      dplyr::filter(.data$notification_date <= end_date) |>
      .sim_afp_classify(
        end_date = end_date, share_pending = share_pending,
        share_lab_pending = share_lab_pending,
        pending_decay_days = pending_decay_days,
        share_compatible = share_compatible, share_npev = share_npev,
        labels = classification_labels, culture = culture_results
      ) |>
      .sim_afp_censor(end_date) |>
      .sim_afp_finish(
        end_date = end_date,
        stool_conditions = stool_conditions, yes_no = yes_no,
        pending_age_missing = pending_age_missing,
        surveillance_type = surveillance_type,
        detected_flag = detected_flag
      )
  })
}

# the under-15 population of each district for each year of the window
#' @noRd
.sim_afp_population <- function(admin, population, years,
                                call = rlang::caller_env()) {
  if (is.null(population)) {
    return(
      tidyr::expand_grid(
        dplyr::select(admin, "adm2_guid", "u15_pop"), year = years
      )
    )
  }
  needed <- c("adm2_guid", "year", "u15_pop")
  if (!is.data.frame(population) || !all(needed %in% names(population))) {
    cli::cli_abort(
      "{.arg population} must be a table from {.fn simulate_population}.",
      call = call
    )
  }
  out <- population |>
    dplyr::filter(
      .data$adm2_guid %in% admin$adm2_guid, .data$year %in% years
    ) |>
    dplyr::select("adm2_guid", "year", "u15_pop")
  if (nrow(out) != nrow(admin) * length(years)) {
    cli::cli_abort(
      c(
        "{.arg population} must cover every district in every year.",
        "i" = "The window covers {.val {years}}."
      ),
      call = call
    )
  }
  out
}

# onset dates: non-polio cases by district-year, then the positive cases
#' @noRd
.sim_afp_onsets <- function(admin, pop, spans, rate, n_positive,
                            positive_types, positive_type_probs,
                            positive_districts, first_onset, last_onset) {
  background <- pop |>
    dplyr::inner_join(spans, by = "year") |>
    dplyr::mutate(
      expected = rate / 1e5 * .data$u15_pop * .data$days / .data$year_days,
      n = stats::rpois(dplyr::n(), .data$expected)
    ) |>
    tidyr::uncount(.data$n) |>
    dplyr::transmute(
      adm2_guid = .data$adm2_guid,
      paralysis_onset_date = .data$from +
        floor(stats::runif(dplyr::n()) * .data$days),
      positive_type = NA_character_
    )

  hot <- admin |>
    dplyr::slice_sample(
      n = min(positive_districts, nrow(admin)), weight_by = .data$u15_pop
    )
  span_days <- as.numeric(last_onset - first_onset) + 1

  positives <- tibble::tibble(
    adm2_guid = sample(hot$adm2_guid, n_positive, replace = TRUE),
    paralysis_onset_date = first_onset +
      floor(stats::runif(n_positive) * span_days),
    positive_type = sample(
      positive_types, n_positive, replace = TRUE, prob = positive_type_probs
    )
  )

  dplyr::bind_rows(background, positives)
}

# every random value a case needs, drawn in one pass
#' @noRd
.sim_afp_draws <- function(cases, delay_means, delay_size, share_adequate,
                           inadequate_mode_probs, age_months_range, sexes,
                           sex_probs, opv_doses_mean, zero_dose_share,
                           routine_dose_share, ipv_share, paralysis_sign_probs,
                           followup_findings, followup_finding_probs, clusters,
                           nt_changes_mean, jitter_deg) {
  days <- \(step) .sim_draw_days(nrow(cases), delay_means[[step]], delay_size)
  n <- nrow(cases)

  cases |>
    dplyr::mutate(
      positive = !is.na(.data$positive_type),
      adequate_draw = stats::runif(n) < share_adequate,
      mode = dplyr::if_else(
        .data$adequate_draw, "adequate",
        sample(
          names(inadequate_mode_probs), n, replace = TRUE,
          prob = inadequate_mode_probs
        )
      ),
      d_notify = days("onset_to_notify"),
      d_invest = days("notify_to_invest"),
      d_stool1 = days("invest_to_stool1"),
      d_gap = 1 + .sim_draw_days(
        n, delay_means[["stool1_to_stool2"]] - 1, delay_size
      ),
      d_late = days("late_extra"),
      d_sent = days("stool_to_sent"),
      d_lab = days("sent_to_lab"),
      d_culture = days("lab_to_culture"),
      d_itd = days("culture_to_itd"),
      d_hq = days("itd_to_hq"),
      d_followup = days("onset_to_followup_extra"),
      d_last_dose = 1 + days("last_dose_before_onset"),
      age_months = round(stats::runif(
        n, age_months_range[[1]], age_months_range[[2]]
      )),
      person_sex = sample(sexes, n, replace = TRUE, prob = sex_probs),
      zero_dose = stats::runif(n) < zero_dose_share,
      doses_opv_number = dplyr::if_else(
        .data$zero_dose, 0, pmax(1, stats::rpois(n, opv_doses_mean))
      ),
      doses_opv_routine = as.numeric(stats::rbinom(
        n, .data$doses_opv_number, routine_dose_share
      )),
      doses_opvsia = .data$doses_opv_number - .data$doses_opv_routine,
      doses_ipv_number = dplyr::if_else(
        .data$zero_dose, 0, as.numeric(stats::rbinom(n, 1, ipv_share))
      ),
      total_number_of_doses = .data$doses_opv_number +
        .data$doses_ipv_number,
      calc_doses_risi = .data$doses_opv_routine + .data$doses_opvsia,
      sign_asymmetric = stats::runif(n) < paralysis_sign_probs[["asymmetric"]],
      sign_fever = stats::runif(n) < paralysis_sign_probs[["onset_fever"]],
      sign_rapid = stats::runif(n) <
        paralysis_sign_probs[["rapid_progress"]],
      finding = sample(
        followup_findings, n, replace = TRUE, prob = followup_finding_probs
      ),
      cluster = sample(clusters, n, replace = TRUE),
      nt = stats::rpois(n, nt_changes_mean),
      compatible_draw = stats::runif(n),
      npev_draw = stats::runif(n),
      lab_pending_draw = stats::runif(n),
      latitude = .data$center_lat + stats::runif(n, -jitter_deg, jitter_deg),
      longitude = .data$center_lon + stats::runif(n, -jitter_deg, jitter_deg)
    )
}

# the date chain, shaped so each case meets or misses the stool standard
#' @noRd
.sim_afp_dates <- function(cases) {
  cases |>
    dplyr::mutate(
      timely = .data$mode %in% c("adequate", "poor_condition"),
      to_stool1 = .data$d_notify + .data$d_invest + .data$d_stool1,
      # a timely case is pulled inside day 13 by scaling its early delays
      squeeze = dplyr::if_else(
        .data$timely & .data$to_stool1 > 13, 13 / .data$to_stool1, 1
      ),
      d_notify = floor(.data$d_notify * .data$squeeze),
      d_invest = floor(.data$d_invest * .data$squeeze),
      d_stool1 = floor(.data$d_stool1 * .data$squeeze),
      to_stool1 = .data$d_notify + .data$d_invest + .data$d_stool1,
      d_gap = dplyr::if_else(
        .data$timely, pmin(.data$d_gap, 14 - .data$to_stool1), .data$d_gap
      ),
      # a late case has its first stool past the 14-day window
      d_stool1 = dplyr::if_else(
        .data$mode == "late" & .data$to_stool1 <= 14,
        .data$d_stool1 + 15 - .data$to_stool1 + .data$d_late,
        .data$d_stool1
      ),
      notification_date = .data$paralysis_onset_date + .data$d_notify,
      investigation_date = .data$notification_date + .data$d_invest,
      stool1collection_date = .data$investigation_date + .data$d_stool1,
      stool2collection_date = dplyr::if_else(
        .data$mode == "one_stool", as.Date(NA),
        .data$stool1collection_date + .data$d_gap
      ),
      stool_date_sent_to_lab = dplyr::coalesce(
        .data$stool2collection_date, .data$stool1collection_date
      ) + .data$d_sent,
      spec_date_received_by_nat_lab = .data$stool_date_sent_to_lab +
        .data$d_lab,
      epi_date_isolation_results_received =
        .data$spec_date_received_by_nat_lab + .data$d_culture,
      epi_date_itd_results_received = dplyr::if_else(
        .data$positive,
        .data$epi_date_isolation_results_received + .data$d_itd,
        as.Date(NA)
      ),
      date_notificationto_hq = .data$epi_date_itd_results_received +
        .data$d_hq
    )
}

# pending, laboratory pending, compatible and the culture result
#' @noRd
.sim_afp_classify <- function(cases, end_date, share_pending, share_lab_pending,
                              pending_decay_days, share_compatible, share_npev,
                              labels, culture) {
  cases |>
    dplyr::mutate(
      # a result not yet due leaves the case pending
      forced = !.data$positive &
        .data$epi_date_isolation_results_received > end_date,
      recency = exp(
        -as.numeric(end_date - .data$paralysis_onset_date) /
          pending_decay_days
      ),
      pending = .data$forced | .sim_pick_weighted(
        !.data$positive & !.data$forced, .data$recency,
        max(round(share_pending * dplyr::n()) - sum(.data$forced), 0)
      ),
      lab_pending = .data$pending & (
        .data$spec_date_received_by_nat_lab > end_date |
          .data$lab_pending_draw < share_lab_pending
      ),
      compatible = !.data$positive & !.data$pending &
        .data$compatible_draw < share_compatible,
      classification_all = dplyr::case_when(
        .data$positive ~ .data$positive_type,
        .data$lab_pending ~ labels[["lab_pending"]],
        .data$pending ~ labels[["pending"]],
        .data$compatible ~ labels[["compatible"]],
        .default = labels[["npafp"]]
      ),
      final_culture_result = dplyr::case_when(
        .data$positive ~ culture[["poliovirus"]],
        .data$lab_pending ~ culture[["not_received"]],
        .data$pending ~ NA_character_,
        .data$npev_draw < share_npev ~ culture[["npev"]],
        .default = culture[["negative"]]
      )
    )
}

# dates after the extraction are dropped; positives keep theirs, capped
#' @noRd
.sim_afp_censor <- function(cases, end_date) {
  chain <- c(
    "investigation_date", "stool1collection_date", "stool2collection_date",
    "stool_date_sent_to_lab", "spec_date_received_by_nat_lab",
    "epi_date_isolation_results_received", "epi_date_itd_results_received",
    "date_notificationto_hq"
  )
  results <- c(
    "epi_date_isolation_results_received", "epi_date_itd_results_received",
    "date_notificationto_hq"
  )

  positive <- cases$positive
  pending <- cases$pending
  lab_pending <- cases$lab_pending

  cases |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(chain),
        \(x) dplyr::if_else(
          positive, pmin(x, end_date), .sim_censor_after(x, end_date)
        )
      ),
      dplyr::across(
        dplyr::all_of(results),
        \(x) dplyr::if_else(pending, as.Date(NA), x)
      ),
      dplyr::across(
        c("stool_date_sent_to_lab", "spec_date_received_by_nat_lab"),
        \(x) dplyr::if_else(lab_pending, as.Date(NA), x)
      )
    )
}

# identifiers, intervals, adequacy and the virus fields, in table order
#' @noRd
.sim_afp_finish <- function(cases, end_date, stool_conditions, yes_no,
                            pending_age_missing, surveillance_type,
                            detected_flag) {
  out <- cases |>
    dplyr::mutate(
      year_onset = as.integer(format(.data$paralysis_onset_date, "%Y")),
      month_onset = as.integer(format(.data$paralysis_onset_date, "%m"))
    ) |>
    dplyr::mutate(
      epid_seq = dplyr::row_number(),
      .by = c("adm2_guid", "year_onset")
    ) |>
    dplyr::mutate(
      epid = sprintf(
        "%s-%s-%s-%s-%03d", .data$country_iso3code, .data$adm1_code,
        .data$adm2_code, .sim_yy(.data$paralysis_onset_date), .data$epid_seq
      ),
      surveillance_type_name = surveillance_type,
      case_date = .data$notification_date,
      stool1condition = dplyr::if_else(
        is.na(.data$stool1collection_date), NA_character_,
        stool_conditions[["good"]]
      ),
      stool2condition = dplyr::case_when(
        is.na(.data$stool2collection_date) ~ NA_character_,
        .data$mode == "poor_condition" ~ stool_conditions[["poor"]],
        .default = stool_conditions[["good"]]
      )
    ) |>
    .afp_add_intervals()

  # adequacy with condition is the follow-up rule of afp_stool_adequacy()
  out$adequate_cond <- afp_stool_adequacy(
    out, definition = "followup", good_condition = stool_conditions[["good"]]
  )

  out <- out |>
    dplyr::mutate(
      timing_ok = dplyr::coalesce(
        .data$onset_to_stool1 <= 13 & .data$onset_to_stool2 >= 1 &
          .data$onset_to_stool2 <= 14 & .data$stool1_to_stool2 >= 1,
        FALSE
      ),
      adequate_stool = dplyr::if_else(
        .data$timing_ok, yes_no[["yes"]], yes_no[["no"]]
      ),
      adequate_stool_with_condition = dplyr::if_else(
        .data$adequate_cond, yes_no[["yes"]], yes_no[["no"]]
      ),
      needs_60day_followup = !.data$timing_ok,
      followup_date = dplyr::if_else(
        .data$needs_60day_followup,
        .data$paralysis_onset_date + 60 + .data$d_followup,
        as.Date(NA)
      ) |>
        .sim_censor_after(end_date),
      followup_findings = dplyr::if_else(
        is.na(.data$followup_date), NA_character_, .data$finding
      ),
      got_60day_followup = dplyr::if_else(
        .data$needs_60day_followup, !is.na(.data$followup_date), NA
      ),
      followup_on_time = dplyr::if_else(
        .data$needs_60day_followup,
        dplyr::coalesce(
          .data$followup_date - .data$paralysis_onset_date >= 60 &
            .data$followup_date - .data$paralysis_onset_date <= 90,
          FALSE
        ),
        NA
      ),
      age_months = dplyr::if_else(
        pending_age_missing & .data$pending, NA_real_, .data$age_months
      ),
      doses_opv_dateof_last = dplyr::if_else(
        .data$doses_opv_number > 0,
        .data$paralysis_onset_date - .data$d_last_dose, as.Date(NA)
      ),
      paralysis_asymmetric = dplyr::if_else(
        .data$sign_asymmetric, yes_no[["yes"]], yes_no[["no"]]
      ),
      paralysis_onset_fever = dplyr::if_else(
        .data$sign_fever, yes_no[["yes"]], yes_no[["no"]]
      ),
      paralysis_rapid_progress = dplyr::if_else(
        .data$sign_rapid, yes_no[["yes"]], yes_no[["no"]]
      ),
      hot_case = .data$sign_asymmetric & .data$sign_fever & .data$sign_rapid,
      vtype = dplyr::if_else(.data$positive, .data$positive_type, "none"),
      vtype_fixed = .data$vtype,
      is_vdpv = .data$positive & grepl("VDPV", .data$vtype),
      wild1 = .sim_virus_flag(.data$positive_type, "^WPV ?1", detected_flag),
      virus_clusters = dplyr::if_else(
        .data$positive & grepl("^WPV", .data$vtype), .data$cluster,
        NA_character_
      ),
      vdpv_emergence_group_names = dplyr::if_else(
        .data$is_vdpv, paste0(.data$country_iso3code, "-SYN-1"),
        NA_character_
      ),
      nt_changes = dplyr::if_else(
        .data$is_vdpv, as.character(.data$nt), NA_character_
      ),
      virus_is_orphan = dplyr::if_else(.data$positive, FALSE, NA),
      vdpv_classification_change_date = dplyr::if_else(
        .data$is_vdpv, .data$date_notificationto_hq, as.Date(NA)
      )
    )

  out |>
    dplyr::select(dplyr::all_of(.sim_afp_columns))
}

.sim_afp_columns <- c(
  "epid", "surveillance_type_name", "country_iso3code", "adm0", "adm1",
  "adm2", "adm0_guid", "adm1_guid", "adm2_guid", "latitude", "longitude",
  "paralysis_onset_date", "year_onset", "month_onset", "case_date",
  "notification_date", "investigation_date", "stool1collection_date",
  "stool2collection_date", "stool1condition", "stool2condition",
  "stool_date_sent_to_lab", "spec_date_received_by_nat_lab",
  "epi_date_isolation_results_received", "epi_date_itd_results_received",
  "date_notificationto_hq", "onset_to_notify", "onset_to_invest",
  "onset_to_stool1", "onset_to_stool2", "notify_to_invest",
  "invest_to_stool1", "stool1_to_stool2", "adequate_stool",
  "adequate_stool_with_condition", "final_culture_result", "vtype",
  "vtype_fixed", "classification_all", "wild1", "virus_clusters",
  "vdpv_emergence_group_names", "nt_changes", "virus_is_orphan",
  "vdpv_classification_change_date", "age_months", "person_sex",
  "doses_opv_number", "doses_opv_routine", "doses_opvsia",
  "doses_opv_dateof_last", "doses_ipv_number", "total_number_of_doses",
  "calc_doses_risi", "paralysis_asymmetric", "paralysis_onset_fever",
  "paralysis_rapid_progress", "hot_case", "needs_60day_followup",
  "got_60day_followup", "followup_date", "followup_on_time",
  "followup_findings"
)
