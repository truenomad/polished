# Tests for the SIA campaign-quality module (LQAS + IM). Synthetic data only,
# except one skip-guarded smoke test against the real preprocessed tables.

# ---- process_lqas -----------------------------------------------------------

testthat::test_that("process_lqas rolls up POLIS and derived classes per district-year", {
  lqas <- data.frame(
    Id = 1:3,
    Admin0Name = "NIGERIA",
    Admin1Name = "KANO",
    Admin2Name = "NASSARAWA",
    Admin2GUID = "{A2}",
    Year = 2024L,
    LqasId = c("L1", "L2", "L3"),
    ChildrenChecked = c(60L, 60L, 50L),
    ChildrenFoundUnvaccinated = c(3L, 12L, 1L),
    Lqas2ClassificationName = c("Pass", "Fail", "Invalid"),
    Lqas3ClassificationName = c("Pass", "Intermediate", "Invalid"),
    stringsAsFactors = FALSE
  )
  out <- polished::process_lqas(lqas, verbose = FALSE)

  testthat::expect_named(out, c("lots", "district", "meta"))
  testthat::expect_equal(nrow(out$lots), 3L)
  testthat::expect_equal(nrow(out$district), 1L)

  d <- out$district
  # shipped POLIS classes
  testthat::expect_equal(d$n_pass_polis, 1L)
  testthat::expect_equal(d$n_fail_polis, 1L)
  testthat::expect_equal(d$n_invalid_polis, 1L)
  testthat::expect_equal(d$pass_pct_polis, 50)
  # derived: 0.95 -> Pass, 0.80 -> Fail (2-level), 50/60 in 2024 -> INVALID
  testthat::expect_equal(d$n_pass_derived, 1L)
  testthat::expect_equal(d$n_fail_derived, 1L)
  testthat::expect_equal(d$n_invalid_derived, 1L)
  testthat::expect_equal(d$pass_pct_derived, 50)
  # both classifications present on the lot table
  testthat::expect_setequal(
    out$lots$lqas2_polis,
    c("Pass", "Fail", "INVALID")
  )
  testthat::expect_true(all(
    c("coverage", "invalid", "lqas2_derived", "lqas3_derived") %in%
      names(out$lots)
  ))
})

testthat::test_that("process_lqas normalises POLIS wording, NA-fills an absent class column, and aborts on missing keys", {
  lqas <- data.frame(
    Id = 1L,
    Admin2GUID = "{A2}",
    Year = 2024L,
    ChildrenChecked = 60L,
    ChildrenFoundUnvaccinated = 3L,
    Lqas2ClassificationName = "Accepted", # POLIS wording variant -> Pass
    stringsAsFactors = FALSE
  )
  out <- polished::process_lqas(lqas, verbose = FALSE)
  testthat::expect_equal(out$lots$lqas2_polis, "Pass")

  # no shipped class column at all -> lqas2_polis all NA -> zero shipped counts
  no_class <- lqas
  no_class$Lqas2ClassificationName <- NULL
  out2 <- polished::process_lqas(no_class, verbose = FALSE)
  testthat::expect_equal(out2$district$n_pass_polis, 0L)
  testthat::expect_true(is.na(out2$district$pass_pct_polis))

  # missing the district GUID aborts with a helpful message
  testthat::expect_error(
    polished::process_lqas(
      data.frame(ChildrenChecked = 60L),
      verbose = FALSE
    ),
    "Missing required column"
  )
})

# ---- process_im -------------------------------------------------------------

testthat::test_that("process_im keeps in-house and out-of-house missed rates separate", {
  im <- data.frame(
    Id = 1:5,
    Admin0 = "NIGERIA",
    Admin1 = "KANO",
    Admin2 = c("A", "A", "B", "B", "C"),
    Admin2GUID = c("{A}", "{A}", "{B}", "{B}", "{C}"),
    ActivityPlannedDateFromYear = 2024L,
    HouseholdsNumberChildrenChecked = c(100L, 100L, 60L, 0L, 10L),
    HouseholdsNumberChildrenMarked = c(90L, 85L, 60L, 0L, 15L),
    HouseholdsResult = c(NA, NA, NA, NA, NA),
    OutOfHouseNumberChildrenChecked = c(50L, 0L, 0L, 0L, 0L),
    OutOfHouseNumberChildrenMarked = c(40L, 0L, 0L, 0L, 0L),
    OutOfHouseResult = c(NA, NA, 0.1, 0.3, NA),
    stringsAsFactors = FALSE
  )
  out <- polished::process_im(im, verbose = FALSE)
  d <- out$district[order(out$district$adm2), ]

  # District A: in-house 1 - 175/200 = 0.125; out-of-house 1 - 40/50 = 0.2
  a <- d[d$adm2 == "A", ]
  testthat::expect_equal(a$missed_frac_inhouse, 0.125)
  testthat::expect_equal(a$missed_frac_outhouse, 0.2)
  testthat::expect_equal(a$im_status_inhouse, "Valid")
  testthat::expect_equal(a$im_status_outhouse, "Valid")

  # District B: no out-of-house children checked -> fallback to mean(result)
  bb <- d[d$adm2 == "B", ]
  testthat::expect_equal(bb$missed_frac_outhouse, 0.2) # mean(0.1, 0.3)
  testthat::expect_equal(bb$missed_frac_inhouse, 0) # 1 - 60/60

  # District C: marked > checked -> negative -> Invalid status
  cc <- d[d$adm2 == "C", ]
  testthat::expect_lt(cc$missed_frac_inhouse, 0)
  testthat::expect_equal(cc$im_status_inhouse, "Invalid")
})

# ---- process_sia_quality ----------------------------------------------------

testthat::test_that("process_sia_quality composes both, tolerates NULLs, and filters dots per processor", {
  lqas <- data.frame(
    Id = 1L,
    Admin2GUID = "{A2}",
    Year = 2024L,
    ChildrenChecked = 60L,
    ChildrenFoundUnvaccinated = 3L,
    Lqas2ClassificationName = "Pass",
    stringsAsFactors = FALSE
  )
  im <- data.frame(
    Id = 1L,
    Admin2GUID = "{A2}",
    Admin2 = "A",
    ActivityPlannedDateFromYear = 2024L,
    HouseholdsNumberChildrenChecked = 100L,
    HouseholdsNumberChildrenMarked = 90L,
    OutOfHouseNumberChildrenChecked = 0L,
    OutOfHouseNumberChildrenMarked = 0L,
    OutOfHouseResult = NA_real_,
    stringsAsFactors = FALSE
  )

  # pass_threshold is an LQAS-only arg; it must not error when im is also run
  both <- polished::process_sia_quality(
    lqas = lqas,
    im = im,
    pass_threshold = 0.5,
    verbose = FALSE
  )
  testthat::expect_named(both, c("lqas", "im"))
  testthat::expect_false(is.null(both$lqas))
  testthat::expect_false(is.null(both$im))

  # lqas only -> im slot is NULL
  lqas_only <- polished::process_sia_quality(lqas = lqas, verbose = FALSE)
  testthat::expect_false(is.null(lqas_only$lqas))
  testthat::expect_null(lqas_only$im)

  # nothing supplied -> abort
  testthat::expect_error(
    polished::process_sia_quality(verbose = FALSE),
    "at least one"
  )
})

# ---- run_pipeline wiring ----------------------------------------------------

testthat::test_that("run_pipeline turns lqas/im inputs into roll-up outputs", {
  lqas <- data.frame(
    Id = 1L,
    Admin2GUID = "{A2}",
    Admin2Name = "A",
    Year = 2024L,
    ChildrenChecked = 60L,
    ChildrenFoundUnvaccinated = 3L,
    Lqas2ClassificationName = "Pass",
    stringsAsFactors = FALSE
  )
  im <- data.frame(
    Id = 1L,
    Admin2GUID = "{A2}",
    Admin2 = "A",
    ActivityPlannedDateFromYear = 2024L,
    HouseholdsNumberChildrenChecked = 100L,
    HouseholdsNumberChildrenMarked = 90L,
    OutOfHouseNumberChildrenChecked = 0L,
    OutOfHouseNumberChildrenMarked = 0L,
    OutOfHouseResult = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(polished::run_pipeline(list(lqas = lqas, im = im)))
  testthat::expect_true(all(c("lqas", "im") %in% names(out)))
  testthat::expect_named(out$lqas, c("lots", "district", "meta"))
  testthat::expect_s3_class(out$im$district, "data.frame")

  # the file writer flattens each list output to one file per data-frame part
  td <- withr::local_tempdir()
  suppressMessages(polished:::.polis_write_outputs(
    out,
    td,
    default_format = "rds"
  ))
  testthat::expect_setequal(
    list.files(td),
    c(
      "polished_lqas_lots.rds",
      "polished_lqas_district.rds",
      "polished_im_district.rds"
    )
  )
})

# ---- spatial reconciliation (clean_afp-style) -------------------------------

testthat::test_that("process_lqas reconciles admin GUIDs against a shape", {
  shape <- make_district_shape() # sf with adm*/guids + year_start/year_end
  lqas <- data.frame(
    Id = 1L,
    Admin0Name = "NIGERIA",
    Admin1Name = "BORNO",
    Admin2Name = "WEST",
    Admin0GUID = "{A0}",
    Admin1GUID = "{A1W}",
    Admin2GUID = NA_character_, # missing district GUID -> recovered from name
    Year = 2020L,
    ChildrenChecked = 60L,
    ChildrenFoundUnvaccinated = 3L,
    Lqas2ClassificationName = "Pass",
    stringsAsFactors = FALSE
  )
  out <- polished::process_lqas(lqas, shape = shape, verbose = FALSE)
  # reconciliation ran (adds geo_source) and filled the district GUID by name
  testthat::expect_true("geo_source" %in% names(out$lots))
  testthat::expect_equal(toupper(out$lots$adm2_guid), "{A2W}")
})

# ---- real-data smoke test ---------------------------------------------------

testthat::test_that("process_sia_quality runs on the real preprocessed lqas/im tables", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  base_dir <- file.path(
    "/Users/mohamedyusuf/WHO-HQ/github/PolioAnalyticsHQ",
    "data_pipeline/01_data/1a_preprocessed_data/data"
  )
  lqas_path <- file.path(base_dir, "lqas.rds")
  im_path <- file.path(base_dir, "im.rds")
  testthat::skip_if_not(
    file.exists(lqas_path) && file.exists(im_path),
    "real preprocessed lqas/im tables not available"
  )

  out <- polished::process_sia_quality(
    lqas = readRDS(lqas_path),
    im = readRDS(im_path),
    verbose = FALSE
  )
  testthat::expect_gt(nrow(out$lqas$district), 0L)
  testthat::expect_gt(nrow(out$im$district), 0L)
  # both LQAS classifications and both IM settings are present
  testthat::expect_true(all(
    c("pass_pct_polis", "pass_pct_derived") %in% names(out$lqas$district)
  ))
  testthat::expect_true(all(
    c("missed_frac_inhouse", "missed_frac_outhouse") %in%
      names(out$im$district)
  ))
})
