# Smoke tests: every calc_* function of the indicator modules runs on the
# synthetic tables and returns its documented shape.

sim_polio <- simulate_polio_data()
sim_as_of <- as.Date("2024-12-31")

expect_sim_table <- function(x, columns) {
  expect_s3_class(x, "tbl_df")
  expect_gt(nrow(x), 0)
  expect_true(all(columns %in% names(x)))
}

expect_share <- function(x, upper) {
  x <- x[!is.na(x)]
  expect_gt(length(x), 0)
  expect_true(all(x >= 0 & x <= upper))
}

test_that("AFP case quality indicators run on the synthetic cases", {
  cases <- sim_polio$afp_cases

  timeliness <- calc_afp_timeliness(cases, level = "adm1")
  expect_sim_table(
    timeliness,
    c(
      "level", "guid", "name", "cases", "notify_n", "notify_ok",
      "notify_pct", "invest_n", "invest_ok", "invest_pct", "stool_n",
      "stool_ok", "stool_pct"
    )
  )
  for (step in c("notify_pct", "invest_pct", "stool_pct")) {
    expect_share(timeliness[[step]], 100)
  }

  pathway <- calc_specimen_pathway(cases)
  expect_sim_table(
    pathway, c("level", "guid", "name", "step", "met", "cases", "pct")
  )
  expect_share(pathway$pct, 100)

  reasons <- calc_specimen_reasons(cases)
  expect_sim_table(
    reasons,
    c("level", "guid", "name", "reason", "n", "inadequate", "pct")
  )
  expect_share(reasons$pct, 100)

  followup <- calc_afp_followup(cases, as_of = sim_as_of, window_months = 12)
  expect_sim_table(
    followup,
    c(
      "level", "guid", "name", "onset_month", "records", "inadequate",
      "due", "completed", "mature", "mature_completed", "on_time",
      "no_followup", "overdue", "compatible", "review", "partial"
    )
  )
  expect_true(all(followup$inadequate <= followup$records))
  expect_true(all(followup$completed <= followup$due))
  expect_gt(sum(followup$due), 0)
})

test_that("specimen receipt indicators run on the synthetic specimens", {
  cases <- sim_polio$afp_cases
  specimens <- sim_polio$human_specimens

  monthly <- calc_specimen_receipt_monthly(
    specimens, cases, as_of = sim_as_of
  )
  expect_sim_table(
    monthly,
    c(
      "level", "guid", "name", "collection_month", "specimens", "received",
      "p50", "p75", "p90", "awaiting", "provisional"
    )
  )
  expect_true(all(monthly$received <= monthly$specimens))
  expect_gt(sum(monthly$received), 0)

  by_area <- calc_specimen_receipt_by_area(
    specimens, cases, min_received = 5
  )
  expect_sim_table(
    by_area, c("level", "guid", "name", "received", "p50", "p75", "p90")
  )
  expect_true(all(by_area$p50 >= 0, na.rm = TRUE))
})

test_that("the pending stage ladder runs on the synthetic tables", {
  stages <- calc_pending_stage(
    sim_polio$afp_cases, sim_polio$human_specimens, as_of = sim_as_of
  )
  expect_sim_table(
    stages,
    c(
      "adm0_guid", "adm0", "onset_month", "records", "open", "none_n",
      "collected_n", "at_lab_n", "resulted_n", "none_share",
      "collected_share", "at_lab_share", "resulted_share", "median_days",
      "over_stale", "open_share", "partial"
    )
  )
  expect_share(stages$open_share, 1)
  for (share in c("none_share", "collected_share", "at_lab_share",
                  "resulted_share")) {
    expect_true(all(stages[[share]] >= 0 & stages[[share]] <= 1,
                    na.rm = TRUE))
  }
  expect_gt(sum(stages$open), 0)
  expect_gt(sum(stages$resulted_n), 0)
})

test_that("ES performance indicators run on the synthetic samples", {
  samples <- sim_polio$es_samples

  resolution <- calc_es_resolution(samples, as_of = sim_as_of)
  expect_sim_table(
    resolution, c("month", "samples", "resolved", "pending", "resolved_share")
  )
  expect_share(resolution$resolved_share, 1)

  districts <- calc_es_districts(samples, as_of = sim_as_of)
  expect_sim_table(
    districts,
    c(
      "adm1", "adm2", "guid", "sites", "samples", "resolved", "pending",
      "ev_n", "pv_n", "ev_pct", "pv_pct", "ev_state", "ev_meets_standard"
    )
  )
  expect_share(districts$ev_pct, 100)
  expect_share(districts$pv_pct, 100)

  sites <- calc_es_sites(samples, as_of = sim_as_of)
  expect_sim_table(
    sites,
    c(
      "adm1", "adm2", "site_name", "site_guid", "samples",
      "months_sampled", "resolved", "pending", "ev_n", "pv_n", "x", "y",
      "ev_pct", "pv_pct", "first_month", "months_open", "regularity_pct",
      "regularity_band", "province_ev_pct", "province_regularity_pct",
      "province_sites", "ev_state", "ev_meets_standard"
    )
  )
  for (pct in c("ev_pct", "pv_pct", "regularity_pct", "province_ev_pct",
                "province_regularity_pct")) {
    expect_share(sites[[pct]], 100)
  }
})

test_that("detection windows run on the synthetic detections", {
  detections <- sim_polio$detections

  windows <- calc_detection_windows(detections, as_of = sim_as_of)
  expect_sim_table(
    windows,
    c(
      "window", "from", "to", "window_start", "window_end", "detections",
      "reported_cases", "positive_es", "provinces", "clusters"
    )
  )
  expect_gt(sum(windows$detections), 0)

  comparison <- calc_detection_comparison(detections, as_of = sim_as_of)
  expect_sim_table(
    comparison,
    c(
      "current_window", "previous_window", "metric", "current", "previous",
      "difference", "pct_change", "direction"
    )
  )
  expect_true(all(comparison$direction %in% c("up", "down", "flat")))
})

test_that("the ES site scorecard runs on the synthetic samples", {
  samples <- sim_polio$es_samples
  campaigns <- sim_polio$campaigns

  scorecard <- calc_es_scorecard(samples, campaigns, as_of = sim_as_of)
  expect_sim_table(
    scorecard,
    c(
      "n_campaign_response", "k_campaign_response", "rate_campaign_response",
      "lo_campaign_response", "hi_campaign_response",
      "n_neighbour_recovery", "rate_neighbour_recovery", "judged_pct",
      "judged_basis", "verdict"
    )
  )
  for (rate in grep("^(rate|lo|hi)_", names(scorecard), value = TRUE)) {
    expect_true(all(scorecard[[rate]] >= 0 & scorecard[[rate]] <= 1,
                    na.rm = TRUE))
  }
  expect_share(scorecard$rate_campaign_response, 1)
  # retention is a percentage of campaign response, so it can pass 100
  shares <- setdiff(grep("_pct$", names(scorecard), value = TRUE),
                    "retention_pct")
  for (pct in shares) {
    expect_true(all(scorecard[[pct]] >= 0 & scorecard[[pct]] <= 100,
                    na.rm = TRUE))
  }
  expect_share(scorecard$judged_pct, 100)

  network <- calc_es_score_network(samples, campaigns, as_of = sim_as_of)
  expect_sim_table(network, c("axis", "sites", "occasions", "network_rate"))
  expect_share(network$network_rate, 1)

  profile <- calc_es_score_profile(samples, campaigns, as_of = sim_as_of)
  expect_sim_table(profile, c("iso", "bin", "samples", "sabin_pct"))
  expect_share(profile$sabin_pct, 100)
})
