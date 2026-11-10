test_that("simulate_polio_data returns every table on one geography", {
  polio <- simulate_polio_data(
    admin_args = list(n_provinces = 3, n_districts = 4)
  )

  expect_named(
    polio,
    c(
      "admin", "population", "afp_cases", "es_samples", "detections",
      "human_specimens", "campaigns"
    )
  )
  expect_true(all(vapply(polio, tibble::is_tibble, logical(1))))
  expect_true(all(polio$human_specimens$epid %in% polio$afp_cases$epid))
  expect_true(all(polio$campaigns$adm2_guid %in% polio$admin$adm2_guid))
  expect_equal(nrow(polio$admin), 12)
  expect_equal(sort(unique(polio$population$year)), 2023:2024)
  expect_true(all(polio$afp_cases$adm2_guid %in% polio$admin$adm2_guid))
  expect_true(all(polio$es_samples$adm2_guid %in% polio$admin$adm2_guid))
  expect_true(all(polio$detections$adm2_guid %in% polio$admin$adm2_guid))
})

test_that("simulate_polio_data passes arguments on to each table", {
  polio <- simulate_polio_data(
    admin_args = list(n_provinces = 2, n_districts = 2),
    afp_args = list(n_positive = 0),
    es_args = list(n_sites = 2, share_positive = 0)
  )

  expect_equal(nrow(polio$detections), 0)
  expect_equal(dplyr::n_distinct(polio$es_samples$site_guid), 2)
})

test_that("simulate_polio_data is deterministic for a seed", {
  expect_identical(simulate_polio_data(seed = 8), simulate_polio_data(seed = 8))
  expect_false(identical(simulate_polio_data(seed = 8),
                         simulate_polio_data(seed = 9)))
})

test_that("simulate_polio_data leaves the caller's random state alone", {
  set.seed(99)
  before <- .Random.seed
  simulate_polio_data()

  expect_identical(.Random.seed, before)
})

test_that("simulate_polio_data uses no real country name", {
  polio <- simulate_polio_data(admin_args = list(n_countries = 2))

  expect_true(no_real_names(c(
    polio$admin$adm0, polio$admin$adm1, polio$admin$adm2,
    polio$es_samples$site_name
  )))
  expect_false(any(polio$admin$country_iso3code %in% real_iso3))
})

test_that("simulate_polio_data rejects bad arguments", {
  expect_error(
    simulate_polio_data(start_date = as.Date("2025-01-01")),
    "must not fall before"
  )
  expect_error(simulate_polio_data(afp_args = list(seed = 3)), "cannot set")
  expect_error(simulate_polio_data(es_args = list(1)), "named list")
})
