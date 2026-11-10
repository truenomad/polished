test_that("simulate_admin_frame returns the expected columns and types", {
  admin <- simulate_admin_frame()

  expect_s3_class(admin, "tbl_df")
  expect_equal(
    column_classes(admin),
    c(
      country_iso3code = "character", adm0 = "character",
      adm0_guid = "character", adm1 = "character", adm1_guid = "character",
      adm1_code = "character", adm2 = "character", adm2_guid = "character",
      adm2_code = "character", center_lon = "numeric",
      center_lat = "numeric", u15_pop = "numeric"
    )
  )
})

test_that("simulate_admin_frame is sized by its arguments", {
  admin <- simulate_admin_frame(n_countries = 2, n_provinces = 3,
                                n_districts = 4)

  expect_equal(nrow(admin), 2 * 3 * 4)
  expect_equal(dplyr::n_distinct(admin$country_iso3code), 2)
  expect_equal(dplyr::n_distinct(admin$adm1_guid), 6)
  expect_equal(dplyr::n_distinct(admin$adm2_guid), 24)
  expect_equal(dplyr::n_distinct(admin$adm2), 24)
})

test_that("simulate_admin_frame names follow the synthetic pattern", {
  admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)

  expect_true(all(grepl("^PROVINCE [0-9]{2}$", admin$adm1)))
  expect_true(all(grepl("^DISTRICT [0-9]{2}-[0-9]{2}$", admin$adm2)))
  expect_true(all(grepl(
    "^\\{[0-9A-F]{8}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{4}-[0-9A-F]{12}\\}$",
    c(admin$adm0_guid, admin$adm1_guid, admin$adm2_guid)
  )))
  expect_equal(
    simulate_admin_frame(province_prefix = "Province")$adm1[[1]],
    "Province 01"
  )
})

test_that("simulate_admin_frame keeps population and points in range", {
  admin <- simulate_admin_frame(u15_pop_range = c(1000, 2000))

  expect_true(all(admin$u15_pop >= 1000 & admin$u15_pop <= 2000))
  expect_true(all(admin$center_lon >= -30 & admin$center_lon <= -20))
  expect_true(all(admin$center_lat >= -30 & admin$center_lat <= -20))
})

test_that("simulate_admin_frame is deterministic for a seed", {
  expect_identical(simulate_admin_frame(seed = 5),
                   simulate_admin_frame(seed = 5))
  expect_false(identical(simulate_admin_frame(seed = 5),
                         simulate_admin_frame(seed = 6)))
})

test_that("simulate_admin_frame leaves the caller's random state alone", {
  set.seed(99)
  before <- .Random.seed
  simulate_admin_frame()

  expect_identical(.Random.seed, before)
})

test_that("simulate_admin_frame uses no real country name or code", {
  admin <- simulate_admin_frame(n_countries = 3)

  expect_true(no_real_names(c(admin$adm0, admin$adm1, admin$adm2)))
  expect_false(any(admin$country_iso3code %in% real_iso3))
})

test_that("simulate_admin_frame rejects bad arguments", {
  expect_error(simulate_admin_frame(n_provinces = -1), "at least 1")
  expect_error(simulate_admin_frame(n_districts = 1.5), "whole number")
  expect_error(simulate_admin_frame(u15_pop_range = c(10, 5)), "lower")
  expect_error(
    simulate_admin_frame(n_countries = 2, iso3_codes = "QZA"),
    "2 distinct"
  )
  expect_error(simulate_admin_frame(seed = "a"), "whole number")
})
