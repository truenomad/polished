# Environmental surveillance performance. Synthetic, hand-checkable samples
# only. Site S1 (district D1) is sampled every month from January to June 2024
# with one unresolved sample in February; site S2 (district D2) opens in March
# and is sampled twice. July is only half resolved, so the window ends in June.

esp_as_of <- as.Date("2024-07-15")

esp_samples <- function() {
  tibble::tibble(
    adm1 = "P1",
    adm2 = c(rep("Da", 9), "Db", "Db"),
    adm2_guid = c(rep("D1", 9), "D2", "D2"),
    site_name = c(rep("Site 1", 9), "Site 2", "Site 2"),
    site_guid = c(rep("S1", 9), "S2", "S2"),
    site_x_coordinate = c(rep(65, 9), 66, 66),
    site_y_coordinate = c(rep(31, 9), 32, 32),
    collection_date = as.Date(c(
      "2024-01-10", "2024-02-10", "2024-02-20", "2024-03-10", "2024-04-10",
      "2024-05-10", "2024-06-10", "2024-07-05", "2024-07-08",
      "2024-03-12", "2024-05-12"
    )),
    classification_all = c(
      "DONE", "DONE", NA, "DONE", "DONE", "DONE", "DONE", "DONE", NA,
      "DONE", "DONE"
    ),
    vtype = c(
      "NPEV", "NPEV", NA, NA, "cVDPV2", "NPEV", NA, "NPEV", NA,
      "NPEV", NA
    )
  )
}

esp_districts <- function() {
  tibble::tibble(
    adm1 = "P1",
    adm2 = c("Da", "Db", "Dc"),
    adm2_guid = c("D1", "D2", "D3")
  )
}

testthat::test_that("monthly resolution counts resolved and pending samples", {
  res <- calc_es_resolution(esp_samples(), esp_as_of)
  testthat::expect_equal(nrow(res), 7)
  testthat::expect_equal(res$samples, c(1, 2, 2, 1, 2, 1, 2))
  testthat::expect_equal(res$pending, c(0, 1, 0, 0, 0, 0, 1))
  testthat::expect_equal(res$resolved_share, c(1, 0.5, 1, 1, 1, 1, 0.5))
})

testthat::test_that("the window stops before a recent month that fails", {
  res <- calc_es_resolution(esp_samples(), esp_as_of)
  # February fails too, but it is older than the settle period
  testthat::expect_equal(
    es_complete_end(res, esp_as_of), as.Date("2024-06-30")
  )
  win <- es_reporting_window(esp_samples(), esp_as_of, window_months = 6)
  testthat::expect_equal(win$window_start, as.Date("2024-01-01"))
  testthat::expect_equal(win$window_end, as.Date("2024-06-30"))
  testthat::expect_equal(win$last_month, as.Date("2024-06-01"))
  testthat::expect_equal(win$lag_months, 1L)
  # with no settle period, February holds the window back
  testthat::expect_equal(
    es_complete_end(res, esp_as_of, settle_months = 6), as.Date("2024-01-31")
  )
})

testthat::test_that("the complete end runs to the last month, capped", {
  res <- tibble::tibble(
    month = as.Date(c("2024-01-01", "2024-02-01")),
    resolved_share = c(1, 0.9)
  )
  testthat::expect_equal(
    es_complete_end(res, as.Date("2024-05-01")), as.Date("2024-02-29")
  )
  testthat::expect_equal(
    es_complete_end(res, as.Date("2024-02-10")), as.Date("2024-02-10")
  )
  testthat::expect_true(is.na(es_complete_end(res[0, ], as.Date("2024-05-01"))))
})

testthat::test_that("no complete month is an error", {
  es <- esp_samples() |>
    dplyr::filter(collection_date >= as.Date("2024-06-01")) |>
    dplyr::mutate(classification_all = NA_character_)
  testthat::expect_error(
    es_reporting_window(es, esp_as_of), "No collection month"
  )
})

testthat::test_that("district shares divide by resolved samples", {
  out <- calc_es_district_performance(
    esp_samples(), esp_as_of,
    admin_units = esp_districts(), window_months = 6
  )
  testthat::expect_equal(names(out)[1:4], c("level", "guid", "name", "adm1"))
  testthat::expect_equal(unique(out$level), "adm2")
  testthat::expect_equal(out$guid, c("D1", "D2", "D3"))
  testthat::expect_equal(out$name, c("Da", "Db", "Dc"))
  testthat::expect_equal(out$sites, c(1L, 1L, 0L))
  testthat::expect_equal(out$samples, c(7L, 2L, 0L))
  testthat::expect_equal(out$resolved, c(6L, 2L, 0L))
  testthat::expect_equal(out$pending, c(1L, 0L, 0L))
  testthat::expect_equal(out$ev_n, c(4L, 1L, 0L))
  testthat::expect_equal(out$pv_n, c(1L, 0L, 0L))
  testthat::expect_equal(out$ev_pct, c(400 / 6, 50, NA))
  testthat::expect_equal(out$pv_pct, c(100 / 6, 0, NA))
  testthat::expect_equal(
    as.character(out$ev_state), c("Measured", "Too few samples", "No ES site")
  )
  testthat::expect_equal(out$ev_meets_standard, c(TRUE, TRUE, NA))
})

testthat::test_that("without a district table only sampled districts appear", {
  out <- calc_es_district_performance(
    esp_samples(), esp_as_of, window_months = 6
  )
  testthat::expect_equal(out$guid, c("D1", "D2"))
  testthat::expect_equal(out$name, c("Da", "Db"))
})

testthat::test_that("a district with only pending samples has no share", {
  es <- esp_samples() |>
    dplyr::mutate(
      classification_all = dplyr::if_else(
        adm2_guid == "D2", NA_character_, classification_all
      ),
      vtype = dplyr::if_else(adm2_guid == "D2", NA_character_, vtype)
    )
  out <- calc_es_district_performance(
    es, esp_as_of,
    from = "2024-01-01", to = "2024-06-30"
  )
  d2 <- dplyr::filter(out, guid == "D2")
  testthat::expect_equal(d2$resolved, 0L)
  testthat::expect_equal(d2$pending, 2L)
  testthat::expect_true(is.na(d2$ev_pct))
  testthat::expect_equal(as.character(d2$ev_state), "No ES site")
})

testthat::test_that("a retired district key is repaired through the lookup", {
  es <- esp_samples() |>
    dplyr::mutate(adm2_guid = dplyr::if_else(
      adm2_guid == "D2", "OLD", adm2_guid
    ))
  lookup <- tibble::tibble(from = "OLD", to = "D2")
  out <- calc_es_district_performance(
    es, esp_as_of,
    admin_units = esp_districts(), guid_lookup = lookup, window_months = 6
  )
  testthat::expect_equal(out$samples, c(7L, 2L, 0L))
  # the repaired key can also come in as a column
  es$repaired <- esp_samples()$adm2_guid
  out2 <- calc_es_district_performance(
    es, esp_as_of,
    admin_units = esp_districts(), window_months = 6,
    cols = list(samples = list(district_key = "repaired"))
  )
  testthat::expect_equal(out2$samples, c(7L, 2L, 0L))
})

testthat::test_that("site regularity counts only the months a site was open", {
  out <- calc_es_site_performance(esp_samples(), esp_as_of, window_months = 6)
  # sorted lowest enterovirus share first
  testthat::expect_equal(out$site_guid, c("S2", "S1"))
  testthat::expect_equal(out$samples, c(2L, 7L))
  testthat::expect_equal(out$months_sampled, c(2L, 6L))
  testthat::expect_equal(out$months_open, c(4L, 6L))
  testthat::expect_equal(out$regularity_pct, c(50, 100))
  testthat::expect_equal(
    as.character(out$regularity_band),
    c("Fewer than 3 in 4 months", "Every month")
  )
  testthat::expect_equal(out$ev_pct, c(50, 66.7))
  testthat::expect_equal(out$pv_pct, c(0, 16.7))
  testthat::expect_equal(
    out$first_month, as.Date(c("2024-03-01", "2024-01-01"))
  )
  testthat::expect_equal(out$x, c(66, 65))
  testthat::expect_equal(out$province_ev_pct, c(62.5, 62.5))
  testthat::expect_equal(out$province_regularity_pct, c(80, 80))
  testthat::expect_equal(out$province_sites, c(2L, 2L))
  testthat::expect_equal(
    as.character(out$ev_state), c("Too few samples", "Measured")
  )
})

testthat::test_that("rounding and thresholds follow the arguments", {
  out <- calc_es_site_performance(
    esp_samples(), esp_as_of,
    window_months = 6, digits = NULL, ev_threshold = 60, min_samples = 2
  )
  testthat::expect_equal(out$ev_pct, c(50, 400 / 6))
  testthat::expect_equal(out$ev_meets_standard, c(FALSE, TRUE))
  testthat::expect_equal(as.character(out$ev_state), c("Measured", "Measured"))
  three_in_four <- calc_es_site_performance(
    esp_samples(), esp_as_of,
    window_months = 6, regularity_breaks = c(50, 100)
  )
  testthat::expect_equal(
    as.character(three_in_four$regularity_band)[1], "3 in 4 months or more"
  )
})

testthat::test_that("column names can be overridden", {
  es <- dplyr::rename(esp_samples(), virus_type = vtype)
  out <- calc_es_site_performance(
    es, esp_as_of,
    window_months = 6, cols = list(samples = list(vtype = "virus_type"))
  )
  testthat::expect_equal(out$ev_n, c(1L, 4L))

  es <- dplyr::rename(esp_samples(), lon = site_x_coordinate)
  out <- calc_es_site_performance(
    es, esp_as_of,
    window_months = 6, cols = list(samples = list(longitude = "lon"))
  )
  testthat::expect_equal(out$x, c(66, 65))
})

testthat::test_that("a window given by dates matches the computed window", {
  win <- es_reporting_window(esp_samples(), esp_as_of, window_months = 6)
  testthat::expect_equal(
    calc_es_site_performance(
      esp_samples(), esp_as_of,
      from = win$window_start, to = win$window_end
    ),
    calc_es_site_performance(esp_samples(), esp_as_of, window_months = 6)
  )
})

testthat::test_that("the minimum-sample rule has three states", {
  st <- es_sample_state(c(0, NA, 4, 5))
  testthat::expect_equal(
    as.character(st),
    c("No ES site", "No ES site", "Too few samples", "Measured")
  )
  testthat::expect_equal(
    levels(st), c("Measured", "Too few samples", "No ES site")
  )
})

testthat::test_that("arguments are validated", {
  es <- esp_samples()
  testthat::expect_error(calc_es_resolution("x", esp_as_of), "data frame")
  testthat::expect_error(calc_es_resolution(es, "not a date"), "single date")
  testthat::expect_error(
    calc_es_resolution(dplyr::select(es, -classification_all), esp_as_of),
    "missing column"
  )
  testthat::expect_error(
    es_complete_end(tibble::tibble(month = Sys.Date()), esp_as_of),
    "resolved_share"
  )
  testthat::expect_error(
    es_reporting_window(es, esp_as_of, resolved_min = 2), "between 0 and 1"
  )
  testthat::expect_error(
    es_reporting_window(es, esp_as_of, window_months = 0), "whole number"
  )
  testthat::expect_error(
    calc_es_district_performance(es, esp_as_of, pv_pattern = NA),
    "regular expression"
  )
  testthat::expect_error(
    calc_es_district_performance(es, esp_as_of, ev_threshold = 150),
    "percentage"
  )
  testthat::expect_error(
    calc_es_district_performance(
      es, esp_as_of,
      from = "2024-06-30", to = "2024-01-01"
    ),
    "single dates"
  )
  testthat::expect_error(
    calc_es_district_performance(es, esp_as_of, from = "2024-01-01"),
    "both"
  )
  testthat::expect_error(
    calc_es_district_performance(
      es, esp_as_of,
      window_months = 6, guid_lookup = tibble::tibble(from = "D1")
    ),
    "missing column"
  )
  testthat::expect_error(
    calc_es_district_performance(
      es, esp_as_of,
      window_months = 6, admin_units = tibble::tibble(adm1 = "P1")
    ),
    "missing column"
  )
  testthat::expect_error(
    calc_es_district_performance(
      es, esp_as_of, window_months = 6, cols = list(es = list())
    ),
    "Unknown source"
  )
  testthat::expect_error(
    calc_es_site_performance(es, esp_as_of, regularity_breaks = c(100, 75)),
    "strictly increasing"
  )
  testthat::expect_error(
    calc_es_site_performance(es, esp_as_of, regularity_labels = c("a", "b")),
    "distinct labels"
  )
  testthat::expect_error(
    calc_es_site_performance(es, esp_as_of, cols = "x"), "list"
  )
  testthat::expect_error(es_sample_state("a"), "numeric")
  testthat::expect_error(
    es_sample_state(1, thin_label = "Measured"), "distinct"
  )
})
