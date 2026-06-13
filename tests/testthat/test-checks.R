# Data-quality checks. Fixtures are in *cleaned* shape (snake_case columns the
# cleaners emit), engineered so a single call to each checks_*() trips every
# applicable check at once. A fixed reference_date keeps the future-date checks
# deterministic.

ref <- as.Date("2024-06-01")

# AFP fixture: rows 1-2 duplicate; row 3 trips every single-row AFP check; row 4
# is future-dated; row 5 is clean.
afp_fixture <- function() {
  data.frame(
    id = 1:5,
    epid = c("A-1", "A-1", "C-3", "D-4", "E-5"),
    country_actual = "X",
    adm0 = c("NIGERIA", "NIGERIA", "CHAD", "MALI", "NIGER"),
    adm1 = "p",
    adm2 = "d",
    paralysis_onset_date = c(
      "2024-01-02",
      "2024-01-02",
      NA,
      "2999-01-01",
      "2024-03-03"
    ),
    year_onset = c(2024L, 2024L, NA, 2999L, 2024L),
    classification_all = c("NPAFP", "NPAFP", "", "NPAFP", "NPAFP"),
    adm1_guid = c("g", "g", NA, "g", "g"),
    adm2_guid = c("g", "g", "g", "g", "g"),
    geo_source = "shape",
    latitude = c(9.1, 9.1, 0, 9.1, 9.1),
    longitude = c(7.2, 7.2, 7.2, 7.2, 7.2),
    age_months = c("24", "24", "-5", "30", "36"),
    notify_to_invest = c(1, 1, -1, 2, 1),
    adequate_stool = c("Yes", "Yes", "No", "Yes", "Yes"),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("checks_afp trips every check, orders by severity, trims", {
  res <- polished::checks_afp(afp_fixture(), reference_date = ref)

  # summary covers every applicable AFP check, errors sorted first
  testthat::expect_s3_class(res$summary, "tbl_df")
  expected <- c(
    "afp_duplicates",
    "afp_no_onset",
    "afp_no_classification",
    "afp_missing_guid",
    "afp_empty_coords",
    "afp_future_onset",
    "afp_age_out_of_range",
    "afp_negative_intervals",
    "afp_inadequate_stool"
  )
  testthat::expect_setequal(res$summary$check, expected)
  testthat::expect_true(all(res$summary$n_flagged >= 1L))
  testthat::expect_identical(res$summary$severity[1], "error") # missing_guid

  # one detail tab per check, holding only key + check columns
  testthat::expect_equal(nrow(res$afp_duplicates), 2L)
  testthat::expect_true(all(c("id", "epid") %in% names(res$afp_empty_coords)))
  testthat::expect_true(
    all(c("latitude", "longitude") %in% names(res$afp_empty_coords))
  )
  testthat::expect_false("adequate_stool" %in% names(res$afp_empty_coords))

  # empty (columns present, no rows) -> every check zero, no detail tabs
  empty <- polished::checks_afp(afp_fixture()[0, ], reference_date = ref)
  testthat::expect_true(all(empty$summary$n_flagged == 0L))
  testthat::expect_named(empty, "summary")

  # trimmed input -> checks whose columns are absent are skipped, no error
  trimmed <- polished::checks_afp(
    data.frame(id = 1L, paralysis_onset_date = NA),
    reference_date = ref
  )
  testthat::expect_true("afp_no_onset" %in% trimmed$summary$check)
  testthat::expect_false("afp_missing_guid" %in% trimmed$summary$check)

  # no applicable check at all -> the empty-summary branch
  none <- polished::checks_afp(data.frame(junk = 1L))
  testthat::expect_equal(nrow(none$summary), 0L)
  testthat::expect_named(none, "summary")

  # bad input rejected
  testthat::expect_error(polished::checks_afp(list()), "data.frame")
})

testthat::test_that("checks_es trips every check and both id-column variants", {
  es <- data.frame(
    id = 1:4,
    sample_id = c("S1", "S1", "S2", "S3"),
    adm0 = c("CHAD", "CHAD", "MALI", "NIGER"),
    adm1 = "p",
    adm2 = "d",
    collection_date = c("2024-02-01", "2024-02-01", NA, "2999-01-01"),
    year_collection = c(2024L, 2024L, NA, 2999L),
    adm1_guid = c("g", "g", NA, "g"),
    adm2_guid = "g",
    latitude = c(9, 9, 0, 9),
    longitude = c(7, 7, 7, 7),
    stringsAsFactors = FALSE
  )
  res <- polished::checks_es(es, reference_date = ref)
  testthat::expect_setequal(
    res$summary$check,
    c(
      "es_duplicates",
      "es_no_collection_date",
      "es_missing_guid",
      "es_empty_coords",
      "es_future_collection"
    )
  )
  testthat::expect_equal(nrow(res$es_duplicates), 2L)

  # enviro_sample_id fallback when sample_id is absent
  es2 <- data.frame(
    id = 1:2,
    enviro_sample_id = c("E1", "E1"),
    adm0 = "CHAD",
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(
    nrow(polished::checks_es(es2)$es_duplicates),
    2L
  )

  # applies-predicate false: adm0 present but no id column -> es_duplicates skip
  es3 <- data.frame(adm0 = "CHAD", collection_date = NA)
  testthat::expect_false(
    "es_duplicates" %in% polished::checks_es(es3)$summary$check
  )
})

testthat::test_that("checks_sia / checks_virus / checks_hum_spec trip checks", {
  sia <- data.frame(
    id = 1:2,
    adm0 = "CHAD",
    adm2_guid = c(NA, "g"),
    year_start = c(NA_integer_, 2024L)
  )
  sia_res <- polished::checks_sia(sia)
  testthat::expect_setequal(
    sia_res$summary$check,
    c("sia_missing_guid", "sia_no_start_year")
  )

  virus <- data.frame(
    id = c(1, 1, 2),
    epid = c("v", "v", "w"),
    nt_changes = c(7, 7, 1),
    emergence_group = c(NA, NA, "EMG"),
    classification_all = c("cVDPV2", "cVDPV2", "SABIN"),
    stringsAsFactors = FALSE
  )
  virus_res <- polished::checks_virus(virus)
  testthat::expect_setequal(
    virus_res$summary$check,
    c("virus_duplicates", "virus_large_nt", "virus_missing_emergence")
  )
  testthat::expect_equal(nrow(virus_res$virus_duplicates), 2L)

  hs <- data.frame(
    id = 1:2,
    specimen_id = c("S1", "S1"),
    collection_date = c(NA, "2024-01-01"),
    adm1_guid = c(NA, "g"),
    adm2_guid = "g",
    adequate = c("No", "Yes"),
    stringsAsFactors = FALSE
  )
  hs_res <- polished::checks_hum_spec(hs)
  testthat::expect_setequal(
    hs_res$summary$check,
    c(
      "hum_spec_duplicates",
      "hum_spec_no_collection_date",
      "hum_spec_missing_guid",
      "hum_spec_inadequate"
    )
  )
})

testthat::test_that("write_checks_excel + workbook dispatch write files", {
  testthat::skip_if_not_installed("openxlsx")
  dir <- withr::local_tempdir()

  res <- polished::checks_afp(afp_fixture(), reference_date = ref)
  path <- file.path(dir, "checks_afp.xlsx")
  testthat::expect_identical(polished::write_checks_excel(res, path), path)
  testthat::expect_true(file.exists(path))

  # rejects a non-checks object
  testthat::expect_error(
    polished::write_checks_excel(list(a = 1), path),
    "summary"
  )

  # dispatch: writes one workbook per present, non-empty dataset; skips empties
  cleaned <- list(
    afp = afp_fixture(),
    es = data.frame(id = 1L, sample_id = "S", adm0 = "CHAD"),
    sia = afp_fixture()[0, ] # empty -> skipped
  )
  written <- polished:::.polis_write_check_workbooks(
    cleaned,
    dir,
    reference_date = ref
  )
  testthat::expect_setequal(written, c("checks_afp.xlsx", "checks_es.xlsx"))
  testthat::expect_true(file.exists(file.path(dir, "checks_es.xlsx")))
})

testthat::test_that("workbook dispatch skips gracefully when openxlsx is absent", {
  testthat::local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  testthat::expect_message(
    out <- polished:::.polis_write_check_workbooks(
      list(afp = afp_fixture()),
      withr::local_tempdir(),
      reference_date = ref
    ),
    "openxlsx"
  )
  testthat::expect_identical(out, character(0))
})

testthat::test_that("internal predicates and helpers cover their branches", {
  # .polis_blank: character (NA/blank/value) and non-character (numeric NA)
  testthat::expect_identical(
    polished:::.polis_blank(c(NA, "", "  ", "x")),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  testthat::expect_identical(
    polished:::.polis_blank(c(NA_real_, 1)),
    c(TRUE, FALSE)
  )
  # .polis_zero_coord: NA / zero / non-zero (and character coercion)
  testthat::expect_identical(
    polished:::.polis_zero_coord(c(NA, 0, "0", "5")),
    c(TRUE, TRUE, TRUE, FALSE)
  )
  # .polis_is_false: logical and character forms
  testthat::expect_identical(
    polished:::.polis_is_false(c(TRUE, FALSE, NA)),
    c(FALSE, TRUE, FALSE)
  )
  testthat::expect_identical(
    polished:::.polis_is_false(c("No", "yes", "0")),
    c(TRUE, FALSE, TRUE)
  )
  # .polis_as_date: Date passthrough vs character parse
  d <- as.Date("2024-01-01")
  testthat::expect_identical(polished:::.polis_as_date(d), d)
  testthat::expect_identical(polished:::.polis_as_date("2024-01-01"), d)
  # .polis_dup_rows: missing keys -> zero rows
  testthat::expect_equal(
    nrow(polished:::.polis_dup_rows(data.frame(a = 1:2), "nope")),
    0L
  )
  # .polis_negative_interval_rows: no interval columns -> zero rows
  testthat::expect_equal(
    nrow(polished:::.polis_negative_interval_rows(data.frame(a = 1:2))),
    0L
  )
})
