# AFP case quality module. Every fixture is synthetic and small enough to check
# by hand; the expected values are worked out in the comments.

# ---- fixtures ----------------------------------------------------------------

# Eight cases, one per adequacy situation, in the cleaned column names.
adequacy_cases <- function() {
  tibble::tibble(
    onset_to_stool1 = c(2, 2, 2, 2, 15, 2, 2, 2),
    onset_to_stool2 = c(3, 3, 3, 3, 16, NA, 3, 2),
    stool1_to_stool2 = c(1, 1, 1, 1, 1, NA, 1, 0),
    stool1condition = c(
      "Good", "Poor", NA, "Unknown", "Good", "Good", "Good", "Good"
    ),
    stool2condition = rep("Good", 8),
    onset_date_quality = c(rep("Good", 6), "Missing onset", "Good"),
    adequate_stool_with_condition = c(
      "Yes", "No", "Yes", NA, "No", "No", "Yes", "No"
    )
  )
}

# Six cases in two provinces and three districts.
# notify: 3, 7, 8, NA, 10, 1     -> 5 measured, 3 within 7 days
# invest: 1, 2, 3, 0, NA, NA     -> 4 measured, 3 within 2 days
# stool: -1, 3, 4, NA, 2, 0      -> 5 measured, 4 within 3 days
timeliness_cases <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm0 = "Country",
    adm1_guid = c("P1", "P1", "P1", "P1", "P2", "P2"),
    adm1 = c("Prov A", "Prov A", "Prov A", "Prov A", "Prov B", "Prov B"),
    adm2_guid = c("D1", "D1", "D2", "D2", "D3", "D3"),
    adm2 = c("Dist 1", "Dist 1", "Dist 2", "Dist 2", "Dist 3", "Dist 3"),
    paralysis_onset_date = as.Date("2025-01-05") + c(0, 10, 20, 30, 40, 400),
    onset_to_notify = c(3, 7, 8, NA, 10, 1),
    notify_to_invest = c(1, 2, 3, 0, NA, NA),
    invest_to_stool1 = c(-1, 3, 4, NA, 2, 0)
  )
}

# Four cases with onset on 1 March 2025.
# E1: stools on days 2 and 3, adequate.
# E2: both stools on day 2, so not apart; POLIS says inadequate.
# E3: stools on days 19 and 20, late.
# E4: no stools.
specimen_cases <- function() {
  tibble::tibble(
    epid = c("E1", "E2", "E3", "E4"),
    adm0_guid = "G0",
    adm0 = "Country",
    adm1_guid = c("P1", "P1", "P2", "P2"),
    adm1 = c("Prov A", "Prov A", "Prov B", "Prov B"),
    adm2_guid = c("D1", "D1", "D3", "D3"),
    adm2 = c("Dist 1", "Dist 1", "Dist 3", "Dist 3"),
    paralysis_onset_date = as.Date("2025-03-01"),
    stool1collection_date = as.Date(
      c("2025-03-03", "2025-03-03", "2025-03-20", NA)
    ),
    stool2collection_date = as.Date(
      c("2025-03-04", "2025-03-03", "2025-03-21", NA)
    ),
    adequate_stool_with_condition = c("Yes", "No", "No", "No")
  )
}

# E1: 7 and 2 days to receipt. E2: received before collection, so NA.
# E3: no collection date, dropped. E4: not yet received. E9: not a case.
lab_specimens <- function() {
  tibble::tibble(
    epid = c("E1", "E1", "E2", "E3", "E4", "E9"),
    date_stool_collected = as.Date(c(
      "2025-03-03", "2025-03-04", "2025-03-03", NA, "2025-04-02", "2025-03-03"
    )),
    date_stool_received_in_lab = as.Date(c(
      "2025-03-10", "2025-03-06", "2025-03-01", "2025-03-05", NA, "2025-03-05"
    ))
  )
}

# Six cases, as of 15 July 2025: follow-up is due for onsets up to 16 May,
# so the three-month window runs from March to May.
# C1: inadequate, followed up on day 71 (on time).
# C2: inadequate, never followed up, overdue, no OPV doses: for review.
# C3: adequate.
# C4: inadequate, onset 20 May, not yet due.
# C5: inadequate, compatible, followed up on day 66 with residual weakness.
# C6: inadequate, onset in January, outside the window.
followup_cases <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm0 = "Country",
    adm1_guid = c("P1", "P1", "P1", "P2", "P2", "P2"),
    adm1 = c("Prov A", "Prov A", "Prov A", "Prov B", "Prov B", "Prov B"),
    paralysis_onset_date = as.Date(c(
      "2025-03-10", "2025-03-15", "2025-04-02", "2025-05-20", "2025-04-10",
      "2025-01-01"
    )),
    onset_to_stool1 = c(20, 20, 2, 20, 20, 20),
    onset_to_stool2 = c(21, 21, 3, 21, 21, 21),
    stool1_to_stool2 = 1,
    stool1condition = "Good",
    stool2condition = "Good",
    classification_all = c(
      "NPAFP", "NPAFP", "NPAFP", "PENDING", "COMPATIBLE", "NPAFP"
    ),
    followup_date = as.Date(c(
      "2025-05-20", NA, NA, NA, "2025-06-15", NA
    )),
    followup_findings = c(
      "No residual weakness", "", NA, NA, "Residual weakness/paralysis", NA
    ),
    age_months = 24,
    doses_opv_routine = c(0, 0, 3, 3, 3, 3),
    doses_opvsia = c(0, 0, 3, 3, 3, 3),
    total_number_of_doses = c(0, 0, 6, 6, 6, 6),
    calc_doses_risi = c(0, 0, 6, 6, 6, 6)
  )
}

# ---- stool adequacy ----------------------------------------------------------

testthat::test_that("the polished definition matches the engine's code", {
  cases <- adequacy_cases()
  adq <- polished::afp_stool_adequacy(cases)
  testthat::expect_identical(
    adq, c(TRUE, FALSE, NA, NA, FALSE, FALSE, NA, FALSE)
  )

  canonical <- dplyr::rename(
    cases,
    stool1_condition = stool1condition,
    stool2_condition = stool2condition
  )
  code <- polished:::.afp_adequacy_code(canonical)
  from_code <- dplyr::case_when(code == 1L ~ TRUE, code == 0L ~ FALSE)
  testthat::expect_identical(adq, from_code)
})

testthat::test_that("followup adequacy treats a blank condition as good", {
  adq <- polished::afp_stool_adequacy(
    adequacy_cases(),
    definition = "followup"
  )
  testthat::expect_identical(
    adq, c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
})

testthat::test_that("the polis_field definition reads the POLIS field", {
  adq <- polished::afp_stool_adequacy(
    adequacy_cases(),
    definition = "polis_field"
  )
  testthat::expect_identical(
    adq, c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE, FALSE)
  )
})

testthat::test_that("adequacy windows and columns are arguments", {
  cases <- adequacy_cases()
  wide <- polished::afp_stool_adequacy(
    cases,
    definition = "followup",
    first_stool_days = c(0, 15),
    second_stool_days = c(1, 16)
  )
  testthat::expect_true(wide[5])

  renamed <- dplyr::rename(cases, s1_cond = stool1condition)
  testthat::expect_identical(
    polished::afp_stool_adequacy(
      renamed,
      cols = list(cases = list(stool1_condition = "s1_cond"))
    ),
    polished::afp_stool_adequacy(cases)
  )
})

testthat::test_that("the polished definition runs without onset quality", {
  cases <- dplyr::select(adequacy_cases(), -onset_date_quality)
  testthat::expect_true(polished::afp_stool_adequacy(cases)[7])
})

testthat::test_that("afp_stool_adequacy validates its arguments", {
  cases <- adequacy_cases()
  testthat::expect_error(polished::afp_stool_adequacy(list()), "data.frame")
  testthat::expect_error(
    polished::afp_stool_adequacy(cases, definition = "who"),
    "definition"
  )
  testthat::expect_error(
    polished::afp_stool_adequacy(cases, first_stool_days = c(13, 0)),
    "first_stool_days"
  )
  testthat::expect_error(
    polished::afp_stool_adequacy(dplyr::select(cases, -stool1condition)),
    "stool1condition"
  )
  testthat::expect_error(
    polished::afp_stool_adequacy(cases, cols = list(other = list())),
    "cols"
  )
})

# ---- timeliness --------------------------------------------------------------

testthat::test_that("timeliness gives the hand-worked national figures", {
  res <- polished::calc_afp_timeliness(timeliness_cases())
  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_identical(nrow(res), 1L)
  testthat::expect_identical(res$level, "adm0")
  testthat::expect_identical(res$cases, 6L)
  testthat::expect_identical(
    c(res$notify_n, res$notify_ok, res$invest_n, res$invest_ok),
    c(5L, 3L, 4L, 3L)
  )
  testthat::expect_identical(c(res$stool_n, res$stool_ok), c(5L, 4L))
  testthat::expect_equal(
    c(res$notify_pct, res$invest_pct, res$stool_pct),
    c(60, 75, 80)
  )
})

testthat::test_that("a step with no measured interval has an NA percentage", {
  res <- polished::calc_afp_timeliness(timeliness_cases(), level = "adm1")
  testthat::expect_identical(res$guid, c("P1", "P2"))
  testthat::expect_identical(res$invest_n, c(4L, 0L))
  testthat::expect_identical(res$invest_pct[2], NA_real_)
  testthat::expect_identical(res$notify_n, c(3L, 2L))
  testthat::expect_equal(res$notify_pct, c(200 / 3, 50))
})

testthat::test_that("district level keeps units without cases from the spine", {
  units <- tibble::tibble(
    adm2_guid = c("D1", "D2", "D3", "D4"),
    adm2 = c("Dist 1", "Dist 2", "Dist 3", "Dist 4")
  )
  res <- polished::calc_afp_timeliness(
    timeliness_cases(),
    level = "adm2",
    admin_units = units
  )
  testthat::expect_identical(res$guid, units$adm2_guid)
  testthat::expect_identical(res$cases, c(2L, 2L, 2L, 0L))
  testthat::expect_identical(res$notify_pct[1], 100)
  testthat::expect_identical(res$notify_pct[2], 0)
  testthat::expect_true(is.na(res$notify_pct[4]))
  testthat::expect_identical(res$stool_ok[4], 0L)
})

testthat::test_that("the onset window and thresholds are applied", {
  res <- polished::calc_afp_timeliness(
    timeliness_cases(),
    from = "2025-01-01",
    to = "2025-12-31",
    notify_days = 10
  )
  # the sixth case has onset in 2026 and drops out
  testthat::expect_identical(res$cases, 5L)
  testthat::expect_identical(c(res$notify_n, res$notify_ok), c(4L, 4L))
})

testthat::test_that("calc_afp_timeliness validates its arguments", {
  cases <- timeliness_cases()
  testthat::expect_error(
    polished::calc_afp_timeliness(cases, level = "adm3"),
    "level"
  )
  testthat::expect_error(
    polished::calc_afp_timeliness(cases, notify_days = -1),
    "notify_days"
  )
  testthat::expect_error(
    polished::calc_afp_timeliness(cases, from = "not a date"),
    "from"
  )
  testthat::expect_error(
    polished::calc_afp_timeliness(dplyr::select(cases, -invest_to_stool1)),
    "invest_to_stool1"
  )
})

# ---- specimen pathway --------------------------------------------------------

testthat::test_that("the specimen pathway gives the hand-worked shares", {
  res <- polished::calc_specimen_pathway(specimen_cases())
  testthat::expect_identical(
    res$step,
    c("first_in_window", "both_in_window", "apart", "adequate")
  )
  testthat::expect_identical(res$met, c(2L, 2L, 2L, 1L))
  testthat::expect_identical(unique(res$cases), 4L)
  testthat::expect_equal(res$pct, c(50, 50, 50, 25))
})

testthat::test_that("the pathway adequacy follows the chosen definition", {
  cases <- dplyr::mutate(
    specimen_cases(),
    onset_to_stool1 = c(2, 2, 19, NA),
    onset_to_stool2 = c(3, 2, 20, NA),
    stool1_to_stool2 = c(1, 0, 1, NA),
    stool1condition = "Good",
    stool2condition = "Good"
  )
  res <- polished::calc_specimen_pathway(cases, adequacy = "followup")
  testthat::expect_identical(res$met[4], 1L)
  res <- polished::calc_specimen_pathway(
    cases,
    adequacy = "followup",
    adequacy_args = list(
      first_stool_days = c(0, 20),
      second_stool_days = c(1, 20)
    )
  )
  testthat::expect_identical(res$met[4], 2L)
})

testthat::test_that("the pathway groups by province", {
  res <- polished::calc_specimen_pathway(specimen_cases(), level = "adm1")
  p2 <- dplyr::filter(res, guid == "P2")
  testthat::expect_identical(p2$met, c(0L, 0L, 1L, 0L))
  testthat::expect_identical(unique(p2$cases), 2L)
})

testthat::test_that("specimen reasons count among inadequate cases", {
  res <- polished::calc_specimen_reasons(specimen_cases())
  testthat::expect_identical(res$reason, c("late", "one_or_none"))
  testthat::expect_identical(res$n, c(1L, 1L))
  testthat::expect_identical(unique(res$inadequate), 3L)
  testthat::expect_equal(res$pct, c(100 / 3, 100 / 3))

  none <- dplyr::mutate(
    specimen_cases(),
    adequate_stool_with_condition = "Yes"
  )
  testthat::expect_identical(
    nrow(polished::calc_specimen_reasons(none)),
    0L
  )
})

testthat::test_that("specimen functions validate their arguments", {
  testthat::expect_error(
    polished::calc_specimen_pathway(specimen_cases(), collection_days = "x"),
    "collection_days"
  )
  testthat::expect_error(
    polished::calc_specimen_pathway(specimen_cases(), adequacy_args = 1),
    "adequacy_args"
  )
  testthat::expect_error(
    polished::calc_specimen_reasons(specimen_cases(), level = 2),
    "level"
  )
})

# ---- laboratory receipt ------------------------------------------------------

testthat::test_that("receipt drops undated specimens and negative intervals", {
  res <- polished::afp_specimen_receipt(lab_specimens(), specimen_cases())
  testthat::expect_identical(res$epid, c("E1", "E1", "E2", "E4"))
  testthat::expect_identical(res$days, c(7L, 2L, NA, NA))
  testthat::expect_identical(unique(res$guid), "G0")
})

testthat::test_that("monthly receipt gives counts, percentiles and status", {
  res <- polished::calc_specimen_receipt_monthly(
    lab_specimens(),
    specimen_cases(),
    as_of = "2025-04-10"
  )
  testthat::expect_identical(
    res$collection_month,
    as.Date(c("2025-03-01", "2025-04-01"))
  )
  testthat::expect_identical(res$specimens, c(3L, 1L))
  testthat::expect_identical(res$received, c(2L, 0L))
  testthat::expect_identical(res$awaiting, c(1L, 1L))
  testthat::expect_equal(res$p50, c(4.5, NA))
  testthat::expect_equal(res$p90[1], 6.5)
  # March closes on 31 March; with a tail of 6.5 days it is complete by 10 April
  testthat::expect_identical(res$provisional, c(FALSE, TRUE))
})

testthat::test_that("receipt by area applies the floor and sorts", {
  res <- polished::calc_specimen_receipt_by_area(
    lab_specimens(),
    specimen_cases(),
    min_received = 1
  )
  testthat::expect_identical(res$guid, "P1")
  testthat::expect_identical(res$received, 2L)
  testthat::expect_identical(
    nrow(polished::calc_specimen_receipt_by_area(
      lab_specimens(),
      specimen_cases()
    )),
    0L
  )

  district <- polished::calc_specimen_receipt_by_area(
    lab_specimens(),
    dplyr::mutate(specimen_cases(), adm2_guid = c("D1", "D2", "D3", "D3")),
    level = "adm2",
    min_received = 1,
    probs = c(median = 0.5)
  )
  testthat::expect_identical(names(district)[5], "median")
})

testthat::test_that("receipt functions validate their arguments", {
  testthat::expect_error(
    polished::calc_specimen_receipt_monthly(
      lab_specimens(),
      specimen_cases(),
      probs = c(0.5, 0.9)
    ),
    "probs"
  )
  testthat::expect_error(
    polished::calc_specimen_receipt_monthly(
      lab_specimens(),
      specimen_cases(),
      as_of = NULL
    ),
    "as_of"
  )
  testthat::expect_error(
    polished::afp_specimen_receipt(
      dplyr::select(lab_specimens(), -epid),
      specimen_cases()
    ),
    "epid"
  )
})

# ---- 60-day follow-up --------------------------------------------------------

testthat::test_that("follow-up status flags the hand-worked cases", {
  res <- polished::afp_followup_status(followup_cases(), as_of = "2025-07-15")
  testthat::expect_identical(
    res$adequate,
    c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  testthat::expect_identical(res$due, c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  testthat::expect_identical(
    res$followed,
    c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
  testthat::expect_identical(res$followup_days[c(1, 5)], c(71L, 66L))
  testthat::expect_identical(
    res$on_time,
    c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
  testthat::expect_identical(
    res$overdue,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  )
  testthat::expect_identical(res$dose_reason[1:2], rep("0 OPV doses", 2))
  testthat::expect_identical(
    res$review,
    c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  testthat::expect_identical(res$compatible[5], TRUE)
})

testthat::test_that("the dose screen falls back from OPV counts to totals", {
  cases <- dplyr::slice(followup_cases(), rep(2, 5)) |>
    dplyr::mutate(
      doses_opv_routine = c(2, NA, NA, NA, 0),
      doses_opvsia = c(NA, NA, NA, NA, NA),
      total_number_of_doses = c(NA, 2, 99, NA, NA),
      calc_doses_risi = c(NA, NA, 5, NA, NA)
    )
  res <- polished::afp_followup_status(cases, as_of = "2025-07-15")
  testthat::expect_identical(
    res$dose_reason,
    c(
      NA,
      "Fewer than 3 doses",
      NA,
      "Dose history missing",
      "0 routine OPV doses, SIA doses missing"
    )
  )
  testthat::expect_identical(res$review, c(FALSE, TRUE, FALSE, TRUE, TRUE))
})

testthat::test_that("the follow-up month table gives the hand-worked counts", {
  res <- polished::calc_afp_followup(
    followup_cases(),
    as_of = "2025-07-15",
    window_months = 3
  )
  testthat::expect_identical(
    res$onset_month,
    as.Date(c("2025-03-01", "2025-04-01", "2025-05-01"))
  )
  testthat::expect_identical(res$records, c(2L, 2L, 1L))
  testthat::expect_identical(res$inadequate, c(2L, 1L, 1L))
  testthat::expect_identical(res$due, c(2L, 1L, 0L))
  testthat::expect_identical(res$completed, c(1L, 1L, 0L))
  testthat::expect_identical(res$mature, c(2L, 1L, 0L))
  testthat::expect_identical(res$mature_completed, c(1L, 1L, 0L))
  testthat::expect_identical(res$on_time, c(1L, 1L, 0L))
  testthat::expect_identical(res$no_followup, c(1L, 0L, 0L))
  testthat::expect_identical(res$overdue, c(1L, 0L, 0L))
  testthat::expect_identical(res$compatible, c(0L, 1L, 0L))
  testthat::expect_identical(res$review, c(1L, 0L, 0L))
  testthat::expect_identical(res$partial, c(FALSE, FALSE, TRUE))
})

testthat::test_that("follow-up months with no cases are kept as zeros", {
  res <- polished::calc_afp_followup(
    followup_cases(),
    as_of = "2025-07-15",
    window_months = 3,
    level = "adm1"
  )
  testthat::expect_identical(nrow(res), 6L)
  p2 <- dplyr::filter(res, guid == "P2")
  testthat::expect_identical(p2$records, c(0L, 1L, 1L))
  testthat::expect_identical(p2$due, c(0L, 1L, 0L))
})

testthat::test_that("follow-up adequacy follows the chosen definition", {
  cases <- dplyr::mutate(
    followup_cases(),
    adequate_stool_with_condition = c("Yes", "No", "Yes", "No", "No", "No")
  )
  res <- polished::calc_afp_followup(
    cases,
    as_of = "2025-07-15",
    window_months = 3,
    adequacy = "polis_field"
  )
  testthat::expect_identical(res$inadequate, c(1L, 1L, 1L))
  testthat::expect_identical(res$due, c(1L, 1L, 0L))
})

testthat::test_that("follow-up functions validate their arguments", {
  cases <- followup_cases()
  testthat::expect_error(
    polished::calc_afp_followup(cases, window_months = 0),
    "window_months"
  )
  testthat::expect_error(
    polished::calc_afp_followup(cases, on_time_days = c(90, 60)),
    "on_time_days"
  )
  testthat::expect_error(
    polished::afp_followup_status(cases, review_classes = 1),
    "review_classes"
  )
  testthat::expect_error(
    polished::afp_followup_status(cases, dose_reason_labels = c(a = "x")),
    "dose_reason_labels"
  )
  testthat::expect_error(
    polished::afp_followup_status(dplyr::select(cases, -followup_findings)),
    "followup_findings"
  )
})
