spec_chain <- c(
  "date_stool_collected", "date_stool_received_in_lab",
  "date_final_cell_culture_results", "date_itd_result_reported",
  "dateof_sequencing"
)

specimen_cases <- function() {
  admin <- simulate_admin_frame(n_provinces = 10, n_districts = 10)
  simulate_afp_cases(admin, n_positive = 15, share_pending = 0.2)
}

test_that("simulate_human_specimens returns the columns the modules read", {
  cases <- specimen_cases()
  specimens <- simulate_human_specimens(cases)
  classes <- column_classes(specimens)

  expect_s3_class(specimens, "tbl_df")
  expect_true(all(c("epid", spec_chain) %in% names(specimens)))
  expect_true(all(classes[spec_chain] == "Date"))
  expect_equal(unname(classes["specimen_number"]), "integer")
  expect_equal(dplyr::n_distinct(specimens$specimen_id), nrow(specimens))
})

test_that("simulate_human_specimens gives one specimen per stool", {
  cases <- specimen_cases()
  specimens <- simulate_human_specimens(cases)

  expect_equal(
    nrow(specimens),
    sum(!is.na(cases$stool1collection_date)) +
      sum(!is.na(cases$stool2collection_date))
  )
  expect_true(all(specimens$epid %in% cases$epid))
  first <- dplyr::filter(specimens, specimen_number == 1L) |>
    dplyr::left_join(cases, by = "epid")
  expect_equal(first$date_stool_collected, first$stool1collection_date)
})

test_that("simulate_human_specimens keeps every date chain in order", {
  specimens <- simulate_human_specimens(specimen_cases())

  expect_true(dates_in_order(specimens, spec_chain))
  expect_true(all(
    is.na(specimens$date_stool_received_in_lab) |
      specimens$date_stool_received_in_lab <= as.Date("2024-12-31")
  ))
  expect_true(all(
    is.na(specimens$date_final_cell_culture_results) |
      !is.na(specimens$date_stool_received_in_lab)
  ))
})

test_that("simulate_human_specimens keeps the state of each case", {
  cases <- specimen_cases()
  specimens <- simulate_human_specimens(cases) |>
    dplyr::left_join(
      dplyr::select(cases, "epid", "classification_all"), by = "epid"
    )
  lab_pending <- specimens$classification_all == "LAB PENDING"
  pending <- specimens$classification_all %in% c("PENDING", "LAB PENDING")
  positive <- grepl("WPV|VDPV", specimens$classification_all)

  expect_true(all(is.na(specimens$date_stool_received_in_lab[lab_pending])))
  expect_true(all(is.na(specimens$date_itd_result_reported[pending])))
  expect_true(all(!is.na(specimens$date_itd_result_reported[positive])))
  expect_true(all(is.na(specimens$date_itd_result_reported[!positive])))
})

test_that("simulate_human_specimens follows its delay arguments", {
  cases <- specimen_cases()
  fast <- simulate_human_specimens(
    cases,
    delay_means = c(
      collect_to_lab = 1, lab_to_culture = 1, culture_to_itd = 1,
      itd_to_sequencing = 1
    )
  )
  slow <- simulate_human_specimens(
    cases,
    delay_means = c(
      collect_to_lab = 20, lab_to_culture = 1, culture_to_itd = 1,
      itd_to_sequencing = 1
    )
  )

  expect_lt(
    mean(fast$collect_to_lab, na.rm = TRUE),
    mean(slow$collect_to_lab, na.rm = TRUE)
  )
})

test_that("simulate_human_specimens is deterministic for a seed", {
  cases <- specimen_cases()

  expect_identical(simulate_human_specimens(cases, seed = 3),
                   simulate_human_specimens(cases, seed = 3))
  expect_false(identical(simulate_human_specimens(cases, seed = 3),
                         simulate_human_specimens(cases, seed = 4)))
})

test_that("simulate_human_specimens leaves the caller's random state alone", {
  cases <- specimen_cases()
  set.seed(99)
  before <- .Random.seed
  simulate_human_specimens(cases)

  expect_identical(.Random.seed, before)
})

test_that("simulate_human_specimens rejects bad arguments", {
  cases <- specimen_cases()

  expect_error(simulate_human_specimens(data.frame(x = 1)),
               "simulate_afp_cases")
  expect_error(
    simulate_human_specimens(cases, delay_means = c(collect_to_lab = 3)),
    "must be named"
  )
  expect_error(
    simulate_human_specimens(cases, share_pending_resulted = 2),
    "between 0 and 1"
  )
  expect_error(simulate_human_specimens(cases, end_date = "x"),
               "single date")
})
