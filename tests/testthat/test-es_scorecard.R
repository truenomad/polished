# Environmental site scorecard. Synthetic, hand-checkable fixtures only.

# one site's samples: a district, a position and one row per collection date
sc_site <- function(site, district, lat, dates, sabin = TRUE, wild = FALSE,
                    iso = "AFG") {
  dates <- as.Date(dates)
  tibble::tibble(
    country_iso3code = iso,
    site_guid = site,
    site_code = paste0("C-", site),
    site_name = paste("Site", site),
    adm1 = "Prov",
    adm2 = district,
    adm2_guid = tolower(district),
    site_x_coordinate = 68,
    site_y_coordinate = lat,
    collection_date = dates,
    vaccine1 = dplyr::if_else(rep_len(sabin, length(dates)), TRUE, NA),
    vaccine2 = NA,
    vaccine3 = NA,
    wild1 = dplyr::if_else(rep_len(wild, length(dates)), TRUE, NA),
    vdpv2 = NA
  )
}

sc_ids <- function(es) {
  dplyr::mutate(es, sample_id = paste0("S", dplyr::row_number()))
}

# Sites A and B about 11 km apart in district D1, C about 111 km north in D3
# with no campaign, and D in D1 last sampled long before the others.
sc_measure_es <- function() {
  dplyr::bind_rows(
    sc_site(
      "A", "D1", 31,
      c(
        "2023-12-20", "2024-01-01", "2024-01-15", "2024-01-29",
        "2024-01-30", "2024-03-01", "2024-06-05", "2024-11-10"
      ),
      sabin = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      wild = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    ),
    sc_site(
      "B", "D1", 31.1, c("2024-01-10", "2024-08-15"),
      sabin = FALSE, wild = TRUE
    ),
    sc_site(
      "C", "D3", 32, c("2024-01-20", "2024-11-05", "2024-11-06"),
      sabin = c(FALSE, FALSE, TRUE), wild = c(TRUE, TRUE, FALSE)
    ),
    sc_site("D", "D1", 30, "2023-01-05")
  ) |>
    sc_ids()
}

# the planned round and the fIPV round must both be ignored
sc_measure_sia <- function() {
  tibble::tibble(
    country_iso3code = "AFG",
    adm2_guid = c("{D1}", "D1", "D1", "{d1}"),
    activity_status = c("Done", "Planned", "Done", "Confirmed"),
    activity_vaccine_type = c("bOPV", "bOPV", "fIPV", "tOPV"),
    activity_date_from = as.Date(
      c("2024-01-01", "2024-01-15", "2024-01-20", "2024-06-01")
    )
  )
}

sc_row <- function(tbl, id) {
  dplyr::filter(tbl, site == id)
}

testthat::test_that("campaign response counts samples 0 to 4 weeks after", {
  res <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(),
    as_of = as.Date("2024-12-31"),
    settings = polished::es_score_settings(min_n = 4)
  )
  a <- sc_row(res, "A")
  # weeks 0, 2, 4 and 0.57; week 4.14 and the pre-campaign sample are out
  testthat::expect_equal(a$n_campaign_response, 4L)
  testthat::expect_equal(a$k_campaign_response, 3L)
  testthat::expect_equal(a$campaign_response_pct, 75)
  ci <- stats::binom.test(3, 4)$conf.int
  testthat::expect_equal(a$lo_campaign_response, round(ci[1], 3))
  testthat::expect_equal(a$hi_campaign_response, round(ci[2], 3))
  # one sample between 6 and 12 weeks, without Sabin; none after 24 weeks
  testthat::expect_equal(a$mid_n, 1L)
  testthat::expect_equal(a$retention_pct, 0)
  testthat::expect_true(is.na(a$floor_n))
  testthat::expect_equal(a$judged_basis, "whole record")
  testthat::expect_equal(as.character(a$verdict), "Working")
  testthat::expect_equal(sc_row(res, "B")$n_campaign_response, 1L)
})

testthat::test_that("peak window boundary moves the count", {
  res <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(),
    as_of = as.Date("2024-12-31"),
    settings = polished::es_score_settings(peak_weeks = 3.9)
  )
  testthat::expect_equal(sc_row(res, "A")$n_campaign_response, 3L)
})

testthat::test_that("a site with no campaign in its district is unjudged", {
  res <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31")
  )
  # C has neither measure at 20 km, so it carries no row
  testthat::expect_false("C" %in% res$site)
  # D last sampled 22 months before the latest sample, so it is not judged
  testthat::expect_false("D" %in% res$site)
  wide <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31"),
    settings = polished::es_score_settings(neighbour_km = 120)
  )
  c_site <- sc_row(wide, "C")
  testthat::expect_true(is.na(c_site$n_campaign_response))
  testthat::expect_equal(c_site$n_neighbour_recovery, 1L)
  testthat::expect_equal(c_site$k_neighbour_recovery, 1L)
  testthat::expect_equal(as.character(c_site$verdict), "Too few samples")
})

testthat::test_that("neighbour recovery counts months near a neighbour find", {
  res <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31")
  )
  a <- sc_row(res, "A")
  # Dec, Jan, Mar and Jun fall within 62 days of B's Jan or Aug find
  testthat::expect_equal(a$n_neighbour_recovery, 4L)
  testthat::expect_equal(a$k_neighbour_recovery, 1L)
  testthat::expect_equal(a$neighbour_recovery_pct, 25)
  b <- sc_row(res, "B")
  testthat::expect_equal(b$n_neighbour_recovery, 1L)
  testthat::expect_equal(b$k_neighbour_recovery, 1L)
  # C has no site within 20 km
  testthat::expect_false("C" %in% res$site)
})

testthat::test_that("window and distance boundaries are honoured", {
  run <- function(...) {
    polished::calc_es_scorecard(
      sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31"),
      settings = polished::es_score_settings(...)
    ) |>
      sc_row("A")
  }
  # Mar is 60 days from Jan and Jun 61 days from Aug
  testthat::expect_equal(run(window_days = 60)$n_neighbour_recovery, 3L)
  testthat::expect_equal(run(window_days = 59)$n_neighbour_recovery, 2L)
  # A and B are about 11.1 km apart
  testthat::expect_true(is.na(run(neighbour_km = 11)$n_neighbour_recovery))
  testthat::expect_equal(run(neighbour_km = 12)$n_neighbour_recovery, 4L)
  # at 120 km C counts too, adding A's Nov sample
  testthat::expect_equal(run(neighbour_km = 120)$n_neighbour_recovery, 5L)
})

testthat::test_that("as_of and start_date bound the samples read", {
  res <- polished::calc_es_scorecard(
    sc_measure_es(), sc_measure_sia(),
    start_date = as.Date("2024-01-01"), as_of = as.Date("2024-06-30")
  )
  testthat::expect_setequal(res$site, c("A", "B"))
  testthat::expect_equal(sc_row(res, "A")$panel_samples, 6L)
})

# One district per site with a bOPV round on the first of every month, and
# every sample collected on a round's first day.
sc_verdict_inputs <- function() {
  months <- function(from, n) seq(as.Date(from), by = "month", length.out = n)
  es <- dplyr::bind_rows(
    sc_site(
      "low", "V1", 31, months("2024-03-01", 10),
      rep(c(TRUE, FALSE), c(2, 8))
    ),
    sc_site(
      "fall", "V2", 32,
      c(months("2018-01-01", 20), months("2023-05-01", 20)),
      c(rep(TRUE, 20), rep(c(TRUE, FALSE), c(8, 12)))
    ),
    sc_site("lowiso", "V3", 33, months("2024-03-01", 10)),
    sc_site("ok", "V4", 34, months("2024-03-01", 10)),
    sc_site("few", "V5", 35, months("2024-09-01", 4)),
    sc_site(
      "lowrecent", "V6", 36,
      c(months("2018-01-01", 20), months("2024-03-01", 10)),
      c(rep(TRUE, 20), rep(c(TRUE, FALSE), c(2, 8)))
    )
  ) |>
    sc_ids()
  sia <- tidyr::expand_grid(
    adm2_guid = paste0("V", 1:6),
    activity_date_from = months("2018-01-01", 84)
  ) |>
    dplyr::mutate(
      country_iso3code = "AFG", activity_status = "Done",
      activity_vaccine_type = "bOPV"
    )
  isolation <- tibble::tibble(
    site_guid = c("low", "lowiso", "ok"),
    npev_pct = c(20, 20, 80),
    resolved = c(3L, 10L, 10L),
    months_sampled = 10L
  )
  list(es = es, sia = sia, isolation = isolation)
}

testthat::test_that("each verdict is reached", {
  x <- sc_verdict_inputs()
  res <- polished::calc_es_scorecard(
    x$es, x$sia, isolation = x$isolation, as_of = as.Date("2024-12-31")
  )
  verdict <- function(id) as.character(sc_row(res, id)$verdict)
  testthat::expect_equal(verdict("low"), "Below the floor")
  testthat::expect_equal(verdict("fall"), "Falling")
  testthat::expect_equal(verdict("lowiso"), "Low isolation")
  testthat::expect_equal(verdict("ok"), "Working")
  testthat::expect_equal(verdict("few"), "Too few samples")
  testthat::expect_equal(verdict("lowrecent"), "Below the floor")
  testthat::expect_equal(
    levels(res$verdict),
    c(
      "Below the floor", "Falling", "Low isolation", "Working",
      "Too few samples"
    )
  )
  testthat::expect_equal(as.character(res$verdict[1]), "Below the floor")

  fall <- sc_row(res, "fall")
  testthat::expect_equal(fall$earlier_pct, 100)
  testthat::expect_equal(fall$recent_pct, 40)
  testthat::expect_equal(fall$drop_pts, 60)
  p <- stats::fisher.test(matrix(c(8, 12, 20, 0), nrow = 2))$p.value
  testthat::expect_equal(fall$p_value, p)
  testthat::expect_true(fall$declined)
  testthat::expect_equal(fall$judged_basis, "recent window")

  lr <- sc_row(res, "lowrecent")
  testthat::expect_equal(lr$campaign_response_pct, round(100 * 22 / 30, 1))
  testthat::expect_equal(lr$judged_pct, 20)
})

testthat::test_that("without isolation the low isolation rule is skipped", {
  x <- sc_verdict_inputs()
  res <- polished::calc_es_scorecard(
    x$es, x$sia, as_of = as.Date("2024-12-31")
  )
  testthat::expect_equal(as.character(sc_row(res, "lowiso")$verdict), "Working")
  testthat::expect_true(all(is.na(res$npev_pct)))
})

testthat::test_that("trend thresholds decide a decline", {
  x <- sc_verdict_inputs()
  run <- function(...) {
    polished::calc_es_scorecard(
      x$es, x$sia, as_of = as.Date("2024-12-31"),
      settings = polished::es_score_settings(...)
    ) |>
      sc_row("fall")
  }
  testthat::expect_false(run(trend_drop_pts = 61)$declined)
  testthat::expect_true(run(trend_drop_pts = 60)$declined)
  testthat::expect_true(is.na(run(trend_min_n = 21)$declined))
})

testthat::test_that("network rate pools sites with enough samples", {
  x <- sc_verdict_inputs()
  net <- polished::calc_es_score_network(
    x$es, x$sia, as_of = as.Date("2024-12-31")
  )
  cr <- dplyr::filter(net, axis == "campaign_response")
  # 'few' has 4 samples and is left out
  testthat::expect_equal(cr$sites, 5L)
  testthat::expect_equal(cr$occasions, 100L)
  testthat::expect_equal(cr$network_rate, 0.72)
  # no site has a neighbour, so the second measure has no row
  testthat::expect_false("neighbour_recovery" %in% net$axis)

  small <- polished::calc_es_score_network(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31"),
    settings = polished::es_score_settings(min_n = 1)
  )
  testthat::expect_equal(small$network_rate, c(0.6, 0.4))
  testthat::expect_equal(small$occasions, c(5L, 5L))
})

testthat::test_that("zero denominators give an empty network", {
  net <- polished::calc_es_score_network(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31"),
    settings = polished::es_score_settings(min_n = 50)
  )
  testthat::expect_equal(nrow(net), 0L)
})

testthat::test_that("profile groups samples by weeks since a campaign", {
  prof <- polished::calc_es_score_profile(
    sc_measure_es(), sc_measure_sia(), as_of = as.Date("2024-12-31")
  )
  testthat::expect_equal(
    as.character(prof$bin), c("0-2w", "2-4w", "4-6w", "8-12w", "12-24w")
  )
  testthat::expect_equal(prof$samples, c(4L, 1L, 1L, 2L, 1L))
  testthat::expect_equal(prof$sabin_pct, c(75, 0, 100, 0, 0))
})

testthat::test_that("country_iso reports a subset of the network", {
  es <- sc_measure_es() |>
    dplyr::mutate(
      country_iso3code = dplyr::if_else(site_guid == "B", "PAK", "AFG")
    )
  sia <- sc_measure_sia()
  res <- polished::calc_es_scorecard(
    es, sia, country_iso = "AFG", benchmark_iso = c("AFG", "PAK"),
    as_of = as.Date("2024-12-31")
  )
  testthat::expect_false("B" %in% res$site)
  # B still counts as A's neighbour
  testthat::expect_equal(sc_row(res, "A")$n_neighbour_recovery, 4L)
})

testthat::test_that("cols overrides map renamed columns", {
  es <- dplyr::rename(sc_measure_es(), site_id = site_guid)
  sia <- dplyr::rename(sc_measure_sia(), vtype = activity_vaccine_type)
  res <- polished::calc_es_scorecard(
    es, sia, as_of = as.Date("2024-12-31"),
    cols = list(
      es = list(site_id = "site_id"), sia = list(vaccine_type = "vtype")
    )
  )
  testthat::expect_equal(sc_row(res, "A")$n_campaign_response, 4L)
  testthat::expect_equal(
    polished::es_score_cols()$sia$vaccine_type, "activity_vaccine_type"
  )
})

testthat::test_that("arguments are validated", {
  es <- sc_measure_es()
  sia <- sc_measure_sia()
  testthat::expect_error(
    polished::calc_es_scorecard(es, sia, settings = list()), "es_score_settings"
  )
  testthat::expect_error(
    polished::calc_es_scorecard(es, sia, cols = list(cases = list())),
    "Unknown source"
  )
  testthat::expect_error(
    polished::calc_es_scorecard(dplyr::select(es, -wild1), sia), "wild1"
  )
  testthat::expect_error(
    polished::calc_es_scorecard("es", sia), "data frame"
  )
  testthat::expect_error(
    polished::calc_es_scorecard(
      es, sia, start_date = "2025-01-01", as_of = "2024-01-01"
    ),
    "start_date"
  )
  testthat::expect_error(
    polished::calc_es_scorecard(es, sia, as_of = "not a date"), "as_of"
  )
  testthat::expect_error(
    polished::calc_es_scorecard(es, sia, as_of = "2016-06-01"),
    "No environmental samples"
  )
  testthat::expect_error(
    polished::es_score_settings(neighbour_km = -1), "neighbour_km"
  )
  testthat::expect_error(
    polished::es_score_settings(mid_weeks = c(12, 6)), "mid_weeks"
  )
  testthat::expect_error(
    polished::es_score_settings(trend_alpha = 1), "trend_alpha"
  )
  testthat::expect_error(
    polished::es_score_settings(verdict_labels = c(a = "x")), "verdict_labels"
  )
  testthat::expect_error(
    polished::es_score_settings(profile_labels = "x"), "profile_breaks"
  )
  testthat::expect_error(
    polished::es_score_settings(opv_types = character()), "opv_types"
  )
  iso <- tibble::tibble(
    site_guid = c("A", "A"), npev_pct = 1, resolved = 1L, months_sampled = 1L
  )
  testthat::expect_error(
    polished::calc_es_scorecard(es, sia, isolation = iso), "one row per site"
  )
})
