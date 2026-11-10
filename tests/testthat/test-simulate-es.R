test_that("simulate_es_samples returns the columns the pipeline reads", {
  samples <- simulate_es_samples()
  classes <- column_classes(samples)

  expect_s3_class(samples, "tbl_df")
  expect_true(all(
    c(
      "sample_id", "country_iso3code", "collection_date", "site_guid",
      "site_name", "site_code", "site_x_coordinate", "site_y_coordinate",
      "classification_all", "vtype", "wild1", "vdpv2", "vaccine1",
      "vaccine2", "vaccine3", "adm2_guid", "date_notification_to_hq",
      "created_date", "npev"
    ) %in% names(samples)
  ))
  expect_true(all(classes[grepl("_date$", names(classes))] == "Date"))
  expect_equal(
    unname(classes[c("sabin1", "npev", "year_collection")]),
    rep("integer", 3)
  )
  expect_equal(
    unname(classes[c("site_x_coordinate", "site_y_coordinate")]),
    rep("numeric", 2)
  )
  expect_equal(dplyr::n_distinct(samples$sample_id), nrow(samples))
})

test_that("simulate_es_samples uses the cleaning's outcome labels", {
  samples <- simulate_es_samples(n_sites = 40)

  expect_true(all(
    samples$classification_all %in%
      c("WPV 1", "SABIN", "NPEV", "NEGATIVE", NA)
  ))
  expect_true(all(
    is.na(samples$vtype[samples$classification_all %in% "NEGATIVE"])
  ))
  expect_true(all(
    samples$vtype[samples$classification_all %in% c("SABIN", "NPEV")] ==
      "none"
  ))
  expect_true(all(is.na(samples$vtype[is.na(samples$classification_all)])))
  expect_true(all(
    !is.na(samples$wild1) == (samples$classification_all %in% "WPV 1")
  ))
  sabin <- dplyr::filter(samples, classification_all == "SABIN")
  expect_true(all(
    !is.na(sabin$vaccine1) | !is.na(sabin$vaccine2) | !is.na(sabin$vaccine3)
  ))
})

test_that("simulate_es_samples follows the sampling interval", {
  samples <- simulate_es_samples(
    n_sites = 5, sampling_interval_days = 14, share_missed = 0
  )
  gaps <- samples |>
    dplyr::arrange(site_guid, collection_date) |>
    dplyr::mutate(
      gap = as.numeric(collection_date - dplyr::lag(collection_date)),
      .by = site_guid
    ) |>
    dplyr::filter(!is.na(gap))

  expect_true(all(gaps$gap == 14))
  expect_equal(dplyr::n_distinct(samples$site_guid), 5)
})

test_that("simulate_es_samples meets its shares on a large draw", {
  samples <- simulate_es_samples(
    n_sites = 200, sampling_interval_days = 14, share_positive = 0.2,
    share_missed = 0.1
  )
  resolved <- dplyr::filter(samples, !is.na(classification_all))
  scheduled <- 200 * 731 / 14

  expect_equal(mean(grepl("WPV", resolved$classification_all)), 0.2,
               tolerance = 0.05)
  expect_equal(nrow(samples) / scheduled, 0.9, tolerance = 0.03)
  expect_true(all(
    is.na(samples$classification_all) ==
      is.na(samples$date_final_combined_result)
  ))
})

test_that("simulate_es_samples keeps every date chain in order", {
  samples <- simulate_es_samples(n_sites = 40)

  expect_true(dates_in_order(
    samples,
    c("collection_date", "date_final_combined_result",
      "date_notification_to_hq")
  ))
  expect_true(all(samples$created_date >= samples$collection_date))
  expect_true(all(samples$collection_date <= as.Date("2024-12-31")))
})

test_that("simulate_es_samples is deterministic for a seed", {
  admin <- simulate_admin_frame()

  expect_identical(simulate_es_samples(admin, seed = 3),
                   simulate_es_samples(admin, seed = 3))
  expect_false(identical(simulate_es_samples(admin, seed = 3),
                         simulate_es_samples(admin, seed = 4)))
})

test_that("simulate_es_samples leaves the caller's random state alone", {
  admin <- simulate_admin_frame()
  set.seed(99)
  before <- .Random.seed
  simulate_es_samples(admin)

  expect_identical(.Random.seed, before)
})

test_that("simulate_es_samples uses no real place name", {
  samples <- simulate_es_samples(n_sites = 20)

  expect_true(no_real_names(
    c(samples$adm0, samples$adm1, samples$adm2, samples$site_name)
  ))
})

test_that("simulate_es_samples rejects bad arguments", {
  expect_error(simulate_es_samples(n_sites = -1), "at least 0")
  expect_error(simulate_es_samples(sampling_interval_days = 0), "at least 1")
  expect_error(simulate_es_samples(share_positive = 2), "between 0 and 1")
  expect_error(
    simulate_es_samples(start_date = as.Date("2024-01-01"),
                        end_date = as.Date("2023-01-01")),
    "must not fall before"
  )
  expect_error(
    simulate_es_samples(outcome_probs = c(sabin = 1, npev = 1)),
    "must be named"
  )
})
