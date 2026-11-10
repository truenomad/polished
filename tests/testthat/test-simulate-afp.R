afp_chain <- c(
  "paralysis_onset_date", "notification_date", "investigation_date",
  "stool1collection_date", "stool2collection_date", "stool_date_sent_to_lab",
  "spec_date_received_by_nat_lab", "epi_date_isolation_results_received",
  "epi_date_itd_results_received", "date_notificationto_hq"
)

large_admin <- function() {
  simulate_admin_frame(
    n_provinces = 20, n_districts = 20, u15_pop_range = c(1e5, 1e5)
  )
}

test_that("simulate_afp_cases returns the columns the pipeline reads", {
  cases <- simulate_afp_cases()
  classes <- column_classes(cases)

  expect_s3_class(cases, "tbl_df")
  expect_true(all(
    c(
      "epid", "country_iso3code", "surveillance_type_name", "adm2_guid",
      "classification_all", "paralysis_onset_date", "onset_to_notify",
      "notify_to_invest", "invest_to_stool1", "adequate_stool_with_condition",
      "stool1collection_date", "stool2collection_date", "year_onset",
      "age_months", "wild1"
    ) %in% names(cases)
  ))
  expect_true(all(classes[grepl("_date$", names(classes))] == "Date"))
  expect_equal(unname(classes[c("year_onset", "month_onset")]),
               c("integer", "integer"))
  expect_equal(
    unname(classes[c("onset_to_notify", "notify_to_invest", "age_months")]),
    rep("numeric", 3)
  )
  expect_equal(
    unname(classes[c("needs_60day_followup", "followup_on_time", "hot_case")]),
    rep("logical", 3)
  )
  expect_equal(unname(classes["classification_all"]), "character")
})

test_that("simulate_afp_cases uses the pipeline's coded vocabulary", {
  cases <- simulate_afp_cases(large_admin(), n_positive = 20)

  expect_true(all(
    cases$classification_all %in%
      c("WPV 1", "cVDPV 2", "NPAFP", "COMPATIBLE", "PENDING", "LAB PENDING")
  ))
  expect_true(all(cases$surveillance_type_name == "AFP"))
  expect_true(all(cases$adequate_stool_with_condition %in% c("Yes", "No")))
  expect_true(all(
    cases$stool1condition %in% c("Good", "Poor", NA) &
      cases$stool2condition %in% c("Good", "Poor", NA)
  ))
  expect_true(all(
    cases$final_culture_result %in%
      c("Negative", "NPEV", "Suspected Poliovirus", "Not received in lab", NA)
  ))
  expect_true(all(cases$vtype[cases$classification_all == "NPAFP"] == "none"))
  expect_true(all(
    !is.na(cases$wild1) == (cases$classification_all == "WPV 1")
  ))
  expect_equal(dplyr::n_distinct(cases$epid), nrow(cases))
  expect_true(all(
    c(cases$paralysis_asymmetric, cases$paralysis_onset_fever,
      cases$paralysis_rapid_progress) %in% c("Yes", "No")
  ))
  expect_equal(
    cases$hot_case,
    cases$paralysis_asymmetric == "Yes" &
      cases$paralysis_onset_fever == "Yes" &
      cases$paralysis_rapid_progress == "Yes"
  )
  expect_true(all(is.na(cases$got_60day_followup[
    !cases$needs_60day_followup
  ])))
})

test_that("simulate_afp_cases follows the non-polio AFP rate", {
  end_date <- as.Date("2023-12-31")
  cut <- end_date - 90
  cases <- simulate_afp_cases(
    large_admin(), start_date = as.Date("2023-01-01"), end_date = end_date,
    npafp_rate = 3, n_positive = 0
  )
  non_polio <- sum(
    !grepl("WPV|VDPV|COMPATIBLE", cases$classification_all) &
      cases$paralysis_onset_date <= cut
  )
  person_years <- 400 * 1e5 * (as.numeric(cut - as.Date("2023-01-01")) + 1) /
    365

  expect_equal(1e5 * non_polio / person_years, 3, tolerance = 0.1)
})

test_that("simulate_afp_cases meets its shares on a large draw", {
  end_date <- as.Date("2024-12-31")
  cases <- simulate_afp_cases(
    large_admin(), n_positive = 0, share_adequate = 0.8, share_pending = 0.15
  )
  pending <- cases$classification_all %in% c("PENDING", "LAB PENDING")
  settled <- cases$paralysis_onset_date <= end_date - 60

  expect_equal(mean(pending), 0.15, tolerance = 0.05)
  expect_equal(
    mean(cases$adequate_stool_with_condition[settled] == "Yes"), 0.8,
    tolerance = 0.05
  )
  expect_true(all(is.na(cases$age_months[pending])))
  expect_true(all(
    is.na(cases$epi_date_isolation_results_received[pending])
  ))
})

test_that("simulate_afp_cases keeps every date chain in order", {
  cases <- simulate_afp_cases(large_admin(), n_positive = 20)

  expect_true(dates_in_order(cases, afp_chain))
  expect_true(all(
    cases$notification_date <= as.Date("2024-12-31")
  ))
  expect_true(all(
    is.na(cases$followup_date) |
      cases$followup_date >= cases$paralysis_onset_date + 60
  ))
})

test_that("adequate cases meet the stool standard the pipeline applies", {
  cases <- simulate_afp_cases(large_admin(), n_positive = 0)
  adequate <- dplyr::filter(cases, adequate_stool_with_condition == "Yes")

  expect_true(all(adequate$onset_to_stool1 <= 13))
  expect_true(all(adequate$onset_to_stool2 <= 14))
  expect_true(all(adequate$stool1_to_stool2 >= 1))
  expect_true(all(adequate$stool2condition == "Good"))
})

test_that("simulate_afp_cases places positives in the chosen districts", {
  cases <- simulate_afp_cases(
    large_admin(), n_positive = 30, positive_districts = 2,
    positive_types = "WPV 1", positive_type_probs = 1
  )
  positive <- dplyr::filter(cases, classification_all == "WPV 1")

  expect_equal(nrow(positive), 30)
  expect_lte(dplyr::n_distinct(positive$adm2_guid), 2)
  expect_true(all(!is.na(positive$epi_date_itd_results_received)))
})

test_that("simulate_afp_cases uses population by year when given", {
  admin <- simulate_admin_frame(n_provinces = 2, n_districts = 2)
  population <- simulate_population(admin, years = 2023:2024)

  expect_s3_class(
    simulate_afp_cases(admin, population = population), "tbl_df"
  )
  expect_error(
    simulate_afp_cases(admin, population = population,
                       end_date = as.Date("2025-06-30")),
    "every district in every year"
  )
})

test_that("simulate_afp_cases is deterministic for a seed", {
  admin <- simulate_admin_frame()

  expect_identical(simulate_afp_cases(admin, seed = 3),
                   simulate_afp_cases(admin, seed = 3))
  expect_false(identical(simulate_afp_cases(admin, seed = 3),
                         simulate_afp_cases(admin, seed = 4)))
})

test_that("simulate_afp_cases leaves the caller's random state alone", {
  admin <- simulate_admin_frame()
  set.seed(99)
  before <- .Random.seed
  simulate_afp_cases(admin)

  expect_identical(.Random.seed, before)
})

test_that("simulate_afp_cases rejects bad arguments", {
  admin <- simulate_admin_frame(n_provinces = 1, n_districts = 2)

  expect_error(simulate_afp_cases(admin, n_positive = -2), "at least 0")
  expect_error(
    simulate_afp_cases(admin, start_date = as.Date("2024-01-01"),
                       end_date = as.Date("2023-01-01")),
    "must not fall before"
  )
  expect_error(simulate_afp_cases(admin, share_pending = 1.5),
               "between 0 and 1")
  expect_error(simulate_afp_cases(admin, share_adequate = -0.1),
               "between 0 and 1")
  expect_error(simulate_afp_cases(admin, npafp_rate = -1), "0 or more")
  expect_error(
    simulate_afp_cases(admin, delay_means = c(onset_to_notify = 3)),
    "must be named"
  )
  expect_error(
    simulate_afp_cases(admin, positive_type_probs = c(1, 2, 3)),
    "one for each label"
  )
  expect_error(simulate_afp_cases(admin, end_date = "not a date"),
               "single date")
})
