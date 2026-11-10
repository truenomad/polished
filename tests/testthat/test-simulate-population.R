test_that("simulate_population returns the expected columns and types", {
  population <- simulate_population()

  expect_s3_class(population, "tbl_df")
  expect_equal(
    column_classes(population),
    c(
      country_iso3code = "character", adm0 = "character",
      adm1 = "character", adm2 = "character", adm0_guid = "character",
      adm1_guid = "character", adm2_guid = "character", year = "integer",
      u5_pop = "numeric", u15_pop = "numeric", total_pop = "numeric"
    )
  )
})

test_that("simulate_population has one row per district and year", {
  admin <- simulate_admin_frame(n_provinces = 2, n_districts = 3)
  population <- simulate_population(admin, years = 2020:2024)

  expect_equal(nrow(population), nrow(admin) * 5)
  expect_equal(
    nrow(dplyr::distinct(population, adm2_guid, year)), nrow(population)
  )
  expect_setequal(population$adm2_guid, admin$adm2_guid)
})

test_that("simulate_population keeps the age bands nested and growing", {
  admin <- simulate_admin_frame(n_provinces = 5, n_districts = 10)
  population <- simulate_population(
    admin, years = 2020:2030, growth_rate = 0.05
  )

  expect_true(all(population$u5_pop <= population$u15_pop))
  expect_true(all(population$u15_pop <= population$total_pop))

  totals <- population |>
    dplyr::summarise(u15 = sum(u15_pop), .by = year) |>
    dplyr::arrange(year)

  expect_equal(
    totals$u15[[11]] / totals$u15[[1]], 1.05^10, tolerance = 0.02
  )
})

test_that("simulate_population is deterministic for a seed", {
  expect_identical(simulate_population(seed = 4), simulate_population(seed = 4))
  expect_false(identical(simulate_population(seed = 4),
                         simulate_population(seed = 7)))
})

test_that("simulate_population leaves the caller's random state alone", {
  admin <- simulate_admin_frame()
  set.seed(99)
  before <- .Random.seed
  simulate_population(admin)

  expect_identical(.Random.seed, before)
})

test_that("simulate_population rejects bad arguments", {
  expect_error(simulate_population(years = 2020.5), "whole numbers")
  expect_error(simulate_population(u5_share = 1.2), "between 0 and 1")
  expect_error(simulate_population(u5_share = 0.5, u15_share = 0.4),
               "at least")
  expect_error(simulate_population(admin = data.frame(x = 1)), "Missing")
})
