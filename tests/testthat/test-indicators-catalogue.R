# Indicator-catalogue tests. Synthetic, hand-checkable fixtures only (see
# helper-indicators.R). Each block fully characterises one slice of the engine:
# generators, skip-if-unavailable, composites, validation and parity.

testthat::test_that("full multi-source run computes every family with hand-checked values", {
  cases <- make_indicator_cases()
  res <- polished::calc_polio_indicators(
    cases,
    population = make_indicator_population(),
    es = make_indicator_es(),
    virus = make_indicator_virus(),
    sia = make_indicator_sia(),
    lab = make_indicator_lab(),
    admin_units = make_indicator_admin_units(),
    reference_date = as.Date("2024-01-01"),
    verbose = TRUE
  )

  a0 <- dplyr::filter(res$long, level == "adm0")
  val <- function(code) a0$value[a0$indicator == code]

  # Family A -- counts & rates
  testthat::expect_equal(val("afp_count"), 6)
  testthat::expect_equal(val("npafp_count"), 4)
  testthat::expect_equal(val("npafp_rate"), 0.25)
  testthat::expect_equal(val("npafp_rate_nopending"), 0.2)
  # Family A2 -- stool adequacy (timing / condition / both-good)
  testthat::expect_equal(val("stool_adequacy_pct"), 50)
  testthat::expect_equal(val("stool_adequacy_cond_pct"), 75)
  testthat::expect_equal(val("stool_adequacy_good_pct"), 60)
  # Family A4 -- case quality
  testthat::expect_equal(val("unclass_cases_pct"), 100 / 6)
  testthat::expect_equal(val("fup_insa_cases_pct"), 0)
  testthat::expect_equal(val("case_contacts_avg"), 0.4)
  # Family B -- timeliness
  testthat::expect_equal(val("inv_timeliness_pct"), 100 / 3)
  # Family C -- environmental
  testthat::expect_equal(val("ev_rate"), 200 / 3)
  testthat::expect_equal(val("sites_with_entero_pct"), 100)
  testthat::expect_equal(val("case_es_35days_pct"), 100)
  testthat::expect_equal(val("env_count"), 6)
  testthat::expect_equal(val("env_wpv_count"), 2)
  testthat::expect_equal(val("env_cvdpv_count"), 1)
  # Family D -- virus
  testthat::expect_equal(val("wpv_count"), 2)
  testthat::expect_equal(val("cvdpv_count"), 1)
  # Family E -- SIA
  testthat::expect_equal(val("sia_opvtot"), 4)
  testthat::expect_equal(val("sia_bopv"), 1)
  testthat::expect_equal(val("sia_nopv2"), 1)
  testthat::expect_equal(val("doses_count"), 4750)
  # Lab
  testthat::expect_equal(val("cellc_perf_bylab"), 200 / 3)
  # Family F -- composites
  testthat::expect_equal(val("survindcat"), 1)
  testthat::expect_equal(val("combined_standard"), 0)
  testthat::expect_equal(
    res$long$value[
      res$long$indicator == "silent_districts" & res$long$level == "adm0"
    ],
    20
  )

  # contract: long has the stable typed columns; wide tibbles carry counts
  testthat::expect_true(all(
    c(
      "level",
      "guid",
      "name",
      "year",
      "indicator",
      "family",
      "value",
      "numerator",
      "denominator",
      "confidence",
      "category",
      "text_code"
    ) %in%
      names(res$long)
  ))
  testthat::expect_true(all(
    c("afp_cases", "npafp_cases", "under15_pop") %in% names(res$adm0)
  ))
  testthat::expect_identical(res$adm0$afp_cases[res$adm0$guid == "G0"], 6L)
  # sia_lastcase_count needs an unavailable last-case feed -> the lone skip
  testthat::expect_identical(
    vapply(res$meta$skipped, function(s) s$code, character(1)),
    "sia_lastcase_count"
  )
})

testthat::test_that("bucket and dose families are mutually exclusive and sum to 100% of the denominator", {
  res <- polished::calc_polio_indicators(
    make_indicator_cases(),
    population = make_indicator_population(),
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  a0 <- dplyr::filter(res$long, level == "adm0")

  notif <- a0[grepl("^st_notif_invest", a0$indicator), ]
  testthat::expect_equal(sum(notif$value), 100)
  testthat::expect_true(all(notif$denominator == 6))
  # invest->stool2 bucket family also closes to 100 (derived-date interval)
  inv2 <- a0[grepl("^st_invest_stool2", a0$indicator), ]
  testthat::expect_equal(sum(inv2$value), 100)

  doses <- a0[
    a0$indicator %in% c("afp_dose_0", "afp_dose_1_2", "afp_dose_3plus"),
  ]
  testthat::expect_equal(sum(doses$value), 100)
})

testthat::test_that("skip-if-unavailable: missing source tables and missing columns skip cleanly, never error", {
  # cases + population only: ES/virus/SIA/lab families skip, run still succeeds
  res <- polished::calc_polio_indicators(
    make_indicator_cases(),
    population = make_indicator_population(),
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  skipped <- vapply(res$meta$skipped, function(s) s$code, character(1))
  testthat::expect_true(all(
    c("ev_rate", "wpv_count", "sia_opvtot", "cellc_perf_bylab") %in% skipped
  ))
  testthat::expect_false(any(
    c("ev_rate", "wpv_count") %in% res$meta$indicators
  ))
  testthat::expect_true("npafp_rate" %in% res$meta$indicators)

  # drop a required column -> only the dependent indicators skip
  cases_no_invest <- dplyr::select(make_indicator_cases(), -notify_to_invest)
  res2 <- polished::calc_polio_indicators(
    cases_no_invest,
    population = make_indicator_population(),
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  skipped2 <- vapply(res2$meta$skipped, function(s) s$code, character(1))
  testthat::expect_true(all(
    c("inv_timeliness_pct", "st_notif_invest_0_2") %in% skipped2
  ))
  testthat::expect_true("npafp_rate" %in% res2$meta$indicators)

  # no population -> rate / district indicators skip but percents still compute
  res3 <- polished::calc_polio_indicators(
    make_indicator_cases(),
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  skipped3 <- vapply(res3$meta$skipped, function(s) s$code, character(1))
  testthat::expect_true(all(
    c("npafp_rate", "pct_districts_npafp_ge2") %in% skipped3
  ))
  testthat::expect_true("stool_adequacy_cond_pct" %in% res3$meta$indicators)
})

testthat::test_that("survindcat maps the policy cutoff branches to classes 0/1/2/4", {
  long <- tibble::tibble(
    level = "adm0",
    guid = c("A", "B", "C", "D", "A", "B", "C", "D"),
    name = guid,
    year = 2023L,
    indicator = rep(c("npafp_rate", "stool_adequacy_cond_pct"), each = 4),
    value = c(5, 5, 1, NA, 90, 50, 50, NA),
    family = "AFP"
  )
  cfg <- list(survindcat_rate_cutoff = 2, adequacy_target = 80)
  out <- polished:::.calc_survindcat(long, cfg, "adm0")
  cls <- stats::setNames(out$value, out$guid)
  testthat::expect_identical(cls[["A"]], 4) # both met
  testthat::expect_identical(cls[["B"]], 2) # rate only
  testthat::expect_identical(cls[["C"]], 1) # neither but assessable
  testthat::expect_identical(cls[["D"]], 0) # not assessable
  testthat::expect_identical(out$text_code[out$guid == "D"], "NA")

  # cutoff is overridable: with cutoff 6 even rate 5 fails -> B drops to class 1
  cfg6 <- list(survindcat_rate_cutoff = 6, adequacy_target = 80)
  out6 <- polished:::.calc_survindcat(long, cfg6, "adm0")
  testthat::expect_identical(out6$value[out6$guid == "B"], 1)
})

testthat::test_that("available_indicators mirrors the registry and filters by family", {
  reg <- polished:::.polio_indicator_registry()
  dict <- polished::available_indicators()
  # parity: one dictionary row per registered indicator and vice versa
  testthat::expect_setequal(dict$code, names(reg))
  testthat::expect_identical(nrow(dict), length(reg))
  testthat::expect_true(all(
    c(
      "code",
      "label",
      "family",
      "formula",
      "source",
      "period_basis",
      "levels",
      "requires_pop",
      "polis_fn"
    ) %in%
      names(dict)
  ))
  # every spec carries a compute closure and the documentation fields
  testthat::expect_true(all(vapply(
    reg,
    function(s) is.function(s$compute),
    logical(1)
  )))
  testthat::expect_true(all(nzchar(dict$formula)))

  es_only <- polished::available_indicators(family = "ES")
  testthat::expect_true(all(es_only$family == "ES"))
  testthat::expect_true("ev_rate" %in% es_only$code)
  # list form returns the raw specs
  testthat::expect_type(
    polished::available_indicators(as_tibble = FALSE),
    "list"
  )
})

testthat::test_that("YTD counts keep only the reference year and are not annualised", {
  cases <- make_indicator_cases()
  virus <- dplyr::mutate(
    make_indicator_virus(),
    year_onset = c(2023L, 2023L, 2024L, 2024L),
    virus_date = as.Date(c(
      "2023-02-01",
      "2023-02-02",
      "2024-02-01",
      "2024-02-02"
    ))
  )
  res <- polished::calc_polio_indicators(
    cases,
    population = make_indicator_population(),
    virus = virus,
    reference_date = as.Date("2024-06-01"),
    indicators = c("wpv_count", "wpv_count_ytd"),
    levels = "adm0",
    verbose = FALSE
  )
  a0 <- dplyr::filter(res$long, level == "adm0")
  # full count spans both years (2x WPV per year); YTD keeps only 2024's one WPV
  testthat::expect_equal(sum(a0$value[a0$indicator == "wpv_count"]), 2)
  testthat::expect_equal(
    a0$value[a0$indicator == "wpv_count_ytd" & a0$year == 2024L],
    1
  )
})

testthat::test_that("condition-aware adequacy code and dose-total helpers follow the POLIS coding", {
  d <- tibble::tibble(
    onset_date_quality = c("Good", "Good", "Good", "Missing onset", "Good"),
    stool1_condition = c("Good", "Poor", "Unknown", "Good", "Good"),
    stool2_condition = c("Good", "Good", "Good", "Good", "Good"),
    onset_to_stool1 = c(5, 5, 5, 5, 20), # last row out of window -> inadequate
    onset_to_stool2 = c(8, 8, 8, 8, 8),
    stool1_to_stool2 = c(3, 3, 3, 3, 3)
  )
  testthat::expect_identical(
    polished:::.afp_adequacy_code(d),
    c(1L, 0L, 99L, 77L, 0L)
  )

  # dose total: sum values < 99; all-99 -> 999; all-NA -> NA
  mat <- rbind(
    c(1, 2, 0, 0, 0), # 3
    c(99, 99, 99, 99, 99), # all unknown -> 999
    c(NA, NA, NA, NA, NA) # all missing -> NA
  )
  testthat::expect_identical(polished:::.afp_dose_total(mat), c(3, 999, NA))
})

testthat::test_that("annualisation and yes/no coercion helpers behave", {
  # full elapsed year ~1; half year scales ~2x
  testthat::expect_equal(
    polished:::.polio_annualise_factor(2023L, as.Date("2023-12-31")),
    1,
    tolerance = 1e-3
  )
  testthat::expect_gt(
    polished:::.polio_annualise_factor(2023L, as.Date("2023-07-02")),
    1.9
  )
  testthat::expect_identical(
    polished:::.polis_as_logical(c("Yes", "no", "Good", "Poor", "maybe")),
    c(TRUE, FALSE, TRUE, FALSE, NA)
  )
  testthat::expect_identical(
    polished:::.polis_as_logical(c(1, 0, 1)),
    c(TRUE, FALSE, TRUE)
  )
})

testthat::test_that("input validation aborts on bad arguments and empty data", {
  cases <- make_indicator_cases()
  testthat::expect_error(
    polished::calc_polio_indicators(list(a = 1)),
    "must be a data.frame"
  )
  testthat::expect_error(
    polished::calc_polio_indicators(cases[0, ]),
    "is empty"
  )
  testthat::expect_error(
    polished::calc_polio_indicators(cases, indicators = "not_an_indicator"),
    "Unknown indicator"
  )
  testthat::expect_error(
    polished::calc_polio_indicators(cases, levels = "adm9"),
    "Invalid"
  )
  testthat::expect_error(
    polished::calc_polio_indicators(cases, rate_multiplier = -1),
    "positive number"
  )
  # population missing its key column aborts with a clear message
  bad_pop <- dplyr::rename(make_indicator_population(), wrong = guid)
  testthat::expect_error(
    polished::calc_polio_indicators(
      cases,
      population = bad_pop,
      indicators = "npafp_rate"
    ),
    "Missing column"
  )
})

testthat::test_that("edge branches: all-skip abort, derived/level skips, column maps and helper inputs", {
  cases <- make_indicator_cases()

  # requesting only an unavailable-source indicator -> nothing left to compute
  testthat::expect_error(
    polished::calc_polio_indicators(
      cases,
      indicators = "ev_rate",
      verbose = FALSE
    ),
    "all skipped"
  )

  # derived indicator missing its prerequisite, and a level-mismatch skip
  res <- polished::calc_polio_indicators(
    cases,
    population = make_indicator_population(),
    indicators = c("survindcat", "cellc_perf_bylab", "afp_count"),
    levels = "adm2",
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  skipped <- vapply(res$meta$skipped, function(s) s$code, character(1))
  testthat::expect_true("survindcat" %in% skipped) # needs npafp_rate (not requested)
  testthat::expect_true("cellc_perf_bylab" %in% skipped) # adm0-only, no lab table
  testthat::expect_true("afp_count" %in% res$meta$indicators)

  # dose_total derived from component columns when doses_total is absent
  cases_doses <- dplyr::select(cases, -doses_total)
  cases_doses$doses_opv_routine <- c(0, 1, 2, 99, 0, 1, 2, 0)
  cases_doses$doses_ipv_routine <- 0
  res_d <- polished::calc_polio_indicators(
    cases_doses,
    indicators = c("afp_dose_0", "afp_dose_3plus"),
    levels = "adm0",
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  testthat::expect_true("afp_dose_0" %in% res_d$meta$indicators)

  # per-source column-map override is merged (cols = list(es = ...))
  res_c <- polished::calc_polio_indicators(
    cases,
    es = make_indicator_es(),
    cols = list(es = list(site = "site_name")),
    indicators = "ev_rate",
    levels = "adm0",
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  testthat::expect_equal(
    res_c$long$value[res_c$long$indicator == "ev_rate"],
    200 / 3
  )

  # admin_units must carry adm2_guid; a year column is honoured when present
  testthat::expect_error(
    polished::calc_polio_indicators(
      cases,
      admin_units = tibble::tibble(wrong = "D1"),
      indicators = "silent_districts",
      verbose = FALSE
    ),
    "adm2_guid"
  )
  au_year <- dplyr::mutate(make_indicator_admin_units(), year = 2023L)
  res_s <- polished::calc_polio_indicators(
    cases,
    admin_units = au_year,
    indicators = "silent_districts",
    levels = "adm0",
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  testthat::expect_equal(
    res_s$long$value[res_s$long$indicator == "silent_districts"],
    20
  )

  # .polis_as_logical passes a logical vector straight through
  testthat::expect_identical(
    polished:::.polis_as_logical(c(TRUE, NA, FALSE)),
    c(TRUE, NA, FALSE)
  )

  # silent universe with no resolvable parent -> the only indicator yields no
  # rows, so the run aborts cleanly rather than returning an empty result
  testthat::expect_error(
    polished::calc_polio_indicators(
      cases,
      admin_units = tibble::tibble(adm2_guid = c("D1", "D2")),
      indicators = "silent_districts",
      levels = "adm0",
      reference_date = as.Date("2024-01-01"),
      verbose = FALSE
    ),
    "No indicator values"
  )

  # combined_standard with adm0-only levels has no adm2 inputs -> no rows
  res_comb <- polished::calc_polio_indicators(
    cases,
    population = make_indicator_population(),
    indicators = c(
      "npafp_rate",
      "stool_adequacy_cond_pct",
      "combined_standard"
    ),
    levels = "adm0",
    reference_date = as.Date("2024-01-01"),
    verbose = FALSE
  )
  testthat::expect_false("combined_standard" %in% res_comb$long$indicator)
})
