# Pending-classification stage ladder. Synthetic, hand-checkable rows only.
#
# With as_of = 2025-06-30 the kept cases fall in Mar to Jun 2025; the window
# trims back to Mar 2025, the oldest month holding an open case.
#   C1 NPAFP   Mar 05 P1/D1 classified
#   C2 PENDING Mar 01 P1/D1 no stool, no specimen -> none, waited 121 days
#   C3 blank   Mar 02 P1/D2 own stool date only   -> collected, waited 120
#   C4 LAB PEN Apr 10 P2/D3 specimen received     -> at_lab, waited 81
#   C5 NA      May 20 P2/D3 specimen ITD result   -> resulted, waited 41
#   C6 WPV1    May 02 P1/D1 classified
#   C7 PENDING Jun 01 contact sample              -> dropped
#   C8 PENDING no onset                           -> dropped
#   C9 PENDING Jun 12 P2/D3 two specimens, rungs 0 and 1 -> collected, 18

ref <- as.Date("2025-06-30")

pend_cases <- function() {
  tibble::tibble(
    epid = paste0("C", 1:9),
    classification_all = c(
      "NPAFP", "PENDING", "", "Lab Pending", NA, "WPV1", "PENDING",
      "PENDING", "PENDING"
    ),
    paralysis_onset_date = as.Date(c(
      "2025-03-05", "2025-03-01", "2025-03-02", "2025-04-10", "2025-05-20",
      "2025-05-02", "2025-06-01", NA, "2025-06-12"
    )),
    surveillance_type_name = c(rep("AFP", 6), "Contact", "AFP", "AFP"),
    stool1collection_date = as.Date(c(
      "2025-03-07", NA, "2025-03-04", NA, NA, NA, NA, NA, NA
    )),
    stool2collection_date = as.Date(NA),
    adm0_guid = "G0",
    adm0 = "COUNTRY",
    adm1_guid = c("P1", "P1", "P1", "P2", "P2", "P1", "P1", "P1", "P2"),
    adm1 = c("PA", "PA", "PA", "PB", "PB", "PA", "PA", "PA", "PB"),
    adm2_guid = c("D1", "D1", "D2", "D3", "D3", "D1", "D1", "D1", "D3"),
    adm2 = c("DA", "DA", "DB", "DC", "DC", "DA", "DA", "DA", "DC")
  )
}

pend_specimens <- function() {
  tibble::tibble(
    epid = c("C4", "C5", "C9", "C9", NA),
    date_stool_collected = as.Date(c(
      "2025-04-12", "2025-05-22", "2025-06-14", NA, "2025-01-01"
    )),
    date_stool_received_in_lab = as.Date(c(
      "2025-04-15", "2025-05-25", NA, NA, "2025-01-03"
    )),
    date_final_cell_culture_results = as.Date(NA),
    date_itd_result_reported = as.Date(c(NA, "2025-06-10", NA, NA, NA)),
    dateof_sequencing = as.Date(NA)
  )
}

pend_table <- function(...) {
  polished::calc_pending_stage(
    pend_cases(), pend_specimens(), as_of = ref, ...
  )
}

# ---- afp_pending_open --------------------------------------------------------

testthat::test_that("afp_pending_open flags blank and pending labels", {
  testthat::expect_identical(
    polished::afp_pending_open(
      c("NPAFP", "PENDING", " lab pending ", "", "  ", NA, "WPV1", "UNKNOWN")
    ),
    c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  testthat::expect_identical(
    polished::afp_pending_open(c("UNKNOWN", "PENDING"), "unknown"),
    c(TRUE, FALSE)
  )
})

# ---- afp_pending_dates -------------------------------------------------------

pend_date_cases <- function() {
  tibble::tibble(
    classification_all = c("WPV1", "WPV1", "cVDPV2", "NPAFP", "PENDING", NA),
    notification_date = as.Date("2025-01-05"),
    epi_date_itd_results_received = as.Date(c(
      "2025-02-01", NA, NA, "2025-02-09", "2025-02-09", NA
    )),
    date_notificationto_hq = as.Date(c(
      "2025-02-03", "2025-02-04", NA, NA, NA, NA
    )),
    epi_date_isolation_results_received = as.Date(c(
      "2025-01-20", NA, NA, "2025-01-30", "2025-01-30", NA
    ))
  )
}

testthat::test_that("afp_pending_dates sets known and resolved by rule", {
  itd <- polished::afp_pending_dates(pend_date_cases())
  testthat::expect_s3_class(itd, "tbl_df")
  testthat::expect_identical(
    itd$is_open, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  testthat::expect_identical(
    itd$positive, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  testthat::expect_identical(
    itd$known,
    as.Date(c("2025-02-01", "2025-02-04", NA, NA, NA, NA))
  )
  testthat::expect_identical(
    itd$resolved,
    as.Date(c("2025-02-01", "2025-02-04", NA, "2025-01-30", NA, NA))
  )
  testthat::expect_identical(itd$notified, rep(as.Date("2025-01-05"), 6))

  hq <- polished::afp_pending_dates(pend_date_cases(), known_rule = "hq")
  testthat::expect_identical(
    hq$known[1:2], as.Date(c("2025-02-03", "2025-02-04"))
  )
})

# ---- afp_specimen_rung -------------------------------------------------------

testthat::test_that("afp_specimen_rung takes the highest dated rung", {
  spec <- tibble::tibble(
    epid = c("A", "B", "C", "D", "E", "F"),
    date_stool_collected = as.Date(c(
      NA, "2025-01-01", "2025-01-01", NA, "2025-01-01", "2025-01-01"
    )),
    date_stool_received_in_lab = as.Date(c(
      NA, NA, "2025-01-03", "2025-01-03", "2025-01-03", NA
    )),
    date_final_cell_culture_results = as.Date(c(
      NA, NA, NA, NA, "2025-01-20", NA
    )),
    date_itd_result_reported = as.Date(NA),
    dateof_sequencing = as.Date(c(NA, NA, NA, NA, NA, "2025-02-01"))
  )
  out <- polished::afp_specimen_rung(spec)
  testthat::expect_named(
    out, c("epid", "collected", "received", "resulted", "rung")
  )
  testthat::expect_identical(out$rung, c(0L, 1L, 2L, 2L, 3L, 3L))
})

# ---- afp_pending_stage -------------------------------------------------------

testthat::test_that("afp_pending_stage keeps the furthest rung per case", {
  out <- polished::afp_pending_stage(pend_cases(), pend_specimens())
  testthat::expect_identical(
    as.character(out$stage),
    c(
      "collected", "none", "collected", "at_lab", "resulted", "none",
      "none", "none", "collected"
    )
  )
  testthat::expect_identical(
    out$specimens, c(0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 2L)
  )
  testthat::expect_identical(
    levels(out$stage), c("none", "collected", "at_lab", "resulted")
  )

  own_only <- polished::afp_pending_stage(pend_cases())
  testthat::expect_identical(own_only$rung, c(1L, 0L, 1L, rep(0L, 6)))
})

testthat::test_that("afp_pending_stage uses custom stage names", {
  out <- polished::afp_pending_stage(
    pend_cases(), pend_specimens(), stages = c("s0", "s1", "s2", "s3")
  )
  testthat::expect_identical(as.character(out$stage[4]), "s2")
})

# ---- calc_pending_stage: country ---------------------------------------------

testthat::test_that("calc_pending_stage counts each stage by onset month", {
  tab <- pend_table()
  testthat::expect_s3_class(tab, "tbl_df")
  testthat::expect_identical(
    tab$onset_month,
    as.Date(c("2025-03-01", "2025-04-01", "2025-05-01", "2025-06-01"))
  )
  testthat::expect_identical(tab$records, c(3L, 1L, 2L, 1L))
  testthat::expect_identical(tab$open, c(2L, 1L, 1L, 1L))
  testthat::expect_identical(tab$none_n, c(1L, 0L, 0L, 0L))
  testthat::expect_identical(tab$collected_n, c(1L, 0L, 0L, 1L))
  testthat::expect_identical(tab$at_lab_n, c(0L, 1L, 0L, 0L))
  testthat::expect_identical(tab$resulted_n, c(0L, 0L, 1L, 0L))
  testthat::expect_equal(tab$none_share, c(0.5, 0, 0, 0))
  testthat::expect_equal(tab$median_days, c(120.5, 81, 41, 18))
  testthat::expect_identical(tab$over_stale, c(1L, 0L, 0L, 0L))
  testthat::expect_equal(tab$open_share, c(2 / 3, 1, 0.5, 1))
  testthat::expect_identical(tab$partial, c(FALSE, FALSE, FALSE, TRUE))
  testthat::expect_identical(tab$adm0_guid, rep("G0", 4))
})

testthat::test_that("the stale flag counts only waits beyond stale_days", {
  # C2 has waited 121 days and C3 120 days
  testthat::expect_identical(pend_table(stale_days = 119L)$over_stale[1], 2L)
  testthat::expect_identical(pend_table(stale_days = 120L)$over_stale[1], 1L)
  testthat::expect_identical(pend_table(stale_days = 121L)$over_stale[1], 0L)
})

testthat::test_that("the window follows window_months, window_end and trim", {
  untrimmed <- pend_table(trim_window = FALSE)
  testthat::expect_identical(nrow(untrimmed), 12L)
  testthat::expect_identical(min(untrimmed$onset_month), as.Date("2024-07-01"))
  testthat::expect_identical(sum(untrimmed$records), 7L)

  short <- pend_table(window_months = 2L)
  testthat::expect_identical(
    short$onset_month, as.Date(c("2025-05-01", "2025-06-01"))
  )

  ended <- pend_table(window_end = as.Date("2025-04-30"), trim_window = FALSE)
  testthat::expect_identical(max(ended$onset_month), as.Date("2025-04-01"))
})

testthat::test_that("zero open cases gives zero counts and full columns", {
  closed <- pend_cases() |>
    dplyr::mutate(classification_all = "NPAFP")
  tab <- polished::calc_pending_stage(closed, as_of = ref)
  testthat::expect_identical(nrow(tab), 12L)
  testthat::expect_true(all(tab$open == 0L))
  testthat::expect_true(all(tab$over_stale == 0L))
  testthat::expect_true(all(is.na(tab$median_days)))
  share_cols <- paste0(
    c("none", "collected", "at_lab", "resulted"), "_share"
  )
  testthat::expect_true(all(share_cols %in% names(tab)))
  testthat::expect_true(all(tab$resulted_share == 0))
})

testthat::test_that("surveillance_type = NULL keeps every type", {
  tab <- pend_table(surveillance_type = NULL)
  testthat::expect_identical(tab$records, c(3L, 1L, 2L, 2L))
})

# ---- calc_pending_stage: grouping ------------------------------------------

testthat::test_that("province level gives one row per province and month", {
  tab <- pend_table(level = "adm1")
  testthat::expect_identical(nrow(tab), 8L)
  testthat::expect_true(
    all(c("adm0_guid", "adm1_guid", "adm1") %in% names(tab))
  )
  p1 <- dplyr::filter(tab, adm1_guid == "P1")
  testthat::expect_identical(p1$records, c(3L, 0L, 1L, 0L))
  testthat::expect_identical(p1$open, c(2L, 0L, 0L, 0L))
  testthat::expect_true(is.na(p1$open_share[2]))
  p2 <- dplyr::filter(tab, adm1_guid == "P2")
  testthat::expect_identical(p2$at_lab_n, c(0L, 1L, 0L, 0L))
  testthat::expect_identical(sum(tab$open), 5L)
})

testthat::test_that("district level gives one row per district and month", {
  tab <- pend_table(level = "adm2")
  testthat::expect_identical(nrow(tab), 12L)
  d1 <- dplyr::filter(tab, adm2_guid == "D1")
  testthat::expect_identical(d1$records, c(2L, 0L, 1L, 0L))
  testthat::expect_identical(d1$none_n, c(1L, 0L, 0L, 0L))
  d2 <- dplyr::filter(tab, adm2_guid == "D2")
  testthat::expect_identical(d2$collected_n, c(1L, 0L, 0L, 0L))
  testthat::expect_identical(d2$median_days[1], 120)
  testthat::expect_identical(sum(tab$over_stale), 1L)
})

# ---- column mapping ---------------------------------------------------------

testthat::test_that("cols maps renamed columns", {
  renamed <- dplyr::rename(pend_cases(), onset_dt = "paralysis_onset_date")
  tab <- polished::calc_pending_stage(
    renamed, pend_specimens(), as_of = ref,
    cols = list(cases = list(onset_date = "onset_dt"))
  )
  testthat::expect_identical(tab$open, pend_table()$open)
})

# ---- validation -------------------------------------------------------------

testthat::test_that("arguments are validated", {
  testthat::expect_error(polished::calc_pending_stage(list()), "data.frame")
  testthat::expect_error(pend_table(level = "adm3"), "level")
  testthat::expect_error(pend_table(window_months = 0), "window_months")
  testthat::expect_error(pend_table(stale_days = -1), "stale_days")
  testthat::expect_error(pend_table(stale_days = 1.5), "stale_days")
  testthat::expect_error(pend_table(trim_window = NA), "trim_window")
  testthat::expect_error(pend_table(stages = c("a", "b", "c")), "stages")
  testthat::expect_error(
    polished::calc_pending_stage(pend_cases(), as_of = NA), "as_of"
  )
  testthat::expect_error(
    pend_table(cols = list(virus = list())), "Unknown"
  )
  testthat::expect_error(
    polished::calc_pending_stage(
      dplyr::select(pend_cases(), -"adm0"), as_of = ref
    ),
    "adm0"
  )
  testthat::expect_error(
    polished::calc_pending_stage(
      dplyr::filter(pend_cases(), surveillance_type_name == "None"),
      as_of = ref
    ),
    "no case"
  )
  testthat::expect_error(
    polished::afp_pending_dates(pend_date_cases(), known_rule = "lab"),
    "known_rule"
  )
  testthat::expect_error(polished::afp_pending_open(list("a")), "atomic")
  testthat::expect_error(
    polished::afp_specimen_rung(dplyr::select(pend_specimens(), -"epid")),
    "epid"
  )
})
