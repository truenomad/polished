test_that("simulate_campaigns returns the columns the scorecard reads", {
  campaigns <- simulate_campaigns()
  classes <- column_classes(campaigns)

  expect_s3_class(campaigns, "tbl_df")
  expect_true(all(
    unlist(es_score_cols()$sia, use.names = FALSE) %in% names(campaigns)
  ))
  expect_true(all(
    classes[c("activity_date_from", "date_from", "date_to")] == "Date"
  ))
  expect_equal(unname(classes[c("year_start", "month_start")]),
               c("integer", "integer"))
})

test_that("simulate_campaigns follows the round calendar", {
  admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
  campaigns <- simulate_campaigns(
    admin, round_interval_days = 30, round_days = 5, share_districts = 1
  )
  starts <- sort(unique(campaigns$date_from))

  expect_true(all(diff(starts) == 30))
  expect_true(all(campaigns$date_to - campaigns$date_from == 4))
  expect_equal(nrow(campaigns), length(starts) * nrow(admin))
  expect_true(all(campaigns$date_from <= as.Date("2024-12-31")))
  expect_true(all(campaigns$activity_vaccine_type %in% c("bOPV", "nOPV2")))
  expect_true(all(campaigns$activity_status %in% c("Done", "Cancelled")))
})

test_that("simulate_campaigns is deterministic for a seed", {
  admin <- simulate_admin_frame()

  expect_identical(simulate_campaigns(admin, seed = 3),
                   simulate_campaigns(admin, seed = 3))
  expect_false(identical(simulate_campaigns(admin, seed = 3),
                         simulate_campaigns(admin, seed = 4)))
})

test_that("simulate_campaigns leaves the caller's random state alone", {
  admin <- simulate_admin_frame()
  set.seed(99)
  before <- .Random.seed
  simulate_campaigns(admin)

  expect_identical(.Random.seed, before)
})

test_that("simulate_campaigns rejects bad arguments", {
  expect_error(simulate_campaigns(round_interval_days = 0), "at least 1")
  expect_error(simulate_campaigns(share_districts = 2), "between 0 and 1")
  expect_error(simulate_campaigns(vaccine_type_probs = 1), "one for each")
  expect_error(simulate_campaigns(admin = data.frame(x = 1)), "Missing")
})
