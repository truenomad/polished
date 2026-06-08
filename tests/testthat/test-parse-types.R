# auto_parse_types() / detect_factors() + the clean_* parse_types toggle.

testthat::test_that("auto_parse_types infers base types and protects codes", {
  df <- tibble::tibble(
    hf_id = c("001", "002", "003"), # id-name -> protected
    code = c("01", "02", "03"), # code-name -> protected
    serial = c("007", "008", "009"), # leading zeros -> protected
    age = c("1", "2", "3"), # -> integer
    score = c("1.5", "2.5", "3.5"), # -> numeric
    when = c("2024-01-01", "2024-01-02", "2024-01-03") # -> Date
  )
  out <- polished::auto_parse_types(df, apply = FALSE)
  testthat::expect_type(out$hf_id, "character")
  testthat::expect_type(out$code, "character")
  testthat::expect_type(out$serial, "character") # leading zeros kept
  testthat::expect_type(out$age, "integer")
  testthat::expect_type(out$score, "double")
  testthat::expect_s3_class(out$when, "Date")
})

testthat::test_that("auto_parse_types factors low-cardinality only when apply=TRUE", {
  df <- tibble::tibble(
    grp = rep(c("A", "B"), 8), # 2 levels / 16 rows -> low ratio -> factor
    uniq = paste0("x", seq_len(16)) # all unique, non-numeric -> stays character
  )
  fac <- polished::auto_parse_types(df, apply = TRUE)
  testthat::expect_s3_class(fac$grp, "factor")
  testthat::expect_type(fac$uniq, "character") # too unique -> not a factor
  base <- polished::auto_parse_types(df, apply = FALSE)
  testthat::expect_type(base$grp, "character") # no factors when apply=FALSE
})

testthat::test_that("detect_factors flags only categorical character columns", {
  df <- tibble::tibble(
    grp = rep(c("A", "B"), 5), # low-cardinality character
    id = as.character(seq_len(10)), # protected by name
    n = seq_len(10) # not character
  )
  plan <- polished::detect_factors(df)
  testthat::expect_setequal(plan$name, "grp")
})

testthat::test_that("clean_afp parse_types toggle controls base-type inference", {
  raw <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    ParalysisOnsetDate = c("2024-01-02", "2024-02-03"),
    Classification = c("Discarded", "Compatible"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  on <- polished::clean_afp(raw, polis_config(parse_types = TRUE))
  off <- polished::clean_afp(raw, polis_config(parse_types = FALSE))
  # last_update_date becomes a parsed Date/datetime when on, stays text when off
  testthat::expect_false(is.character(on$last_update_date))
  testthat::expect_s3_class(on$last_update_date, "Date")
  testthat::expect_type(off$last_update_date, "character")
  # derived analytic columns are identical either way
  testthat::expect_identical(on$classification_all, off$classification_all)
  testthat::expect_identical(on$year_onset, off$year_onset)
})

testthat::test_that("clean_afp drops all-NA columns per drop_empty_cols", {
  raw <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    ParalysisOnsetDate = c("2024-01-02", "2024-02-03"),
    Admin0Name = c("NIGERIA", "CHAD"),
    DiagnosisOther = c(NA, NA), # entirely empty -> dropped by default
    check.names = FALSE
  )
  on <- polished::clean_afp(raw, polis_config(drop_empty_cols = TRUE))
  off <- polished::clean_afp(raw, polis_config(drop_empty_cols = FALSE))
  testthat::expect_false("diagnosis_other" %in% names(on))
  testthat::expect_true("diagnosis_other" %in% names(off))
  # non-empty columns are always kept
  testthat::expect_true(all(c("id", "epid", "adm0") %in% names(on)))
})
