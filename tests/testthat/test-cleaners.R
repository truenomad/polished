# Each cleaner runs standalone on synthetic data with NO archive present.

afp_raw <- function() {
  data.frame(
    Id = c(1, 1, 2),
    Epid = c("A-1", "A-1", "B-2"),
    `Last Update Date` = c("2024-01-01", "2024-03-01", "2024-02-01"),
    `Date Onset` = c("2024-01-02", "2024-01-02", "2024-02-03"),
    `Admin0 Name` = c("NIGERIA", "NIGERIA", "CHAD"),
    junk = c("a", "b", "c"),
    check.names = FALSE
  )
}

test_that("clean_afp dedups by id, derives onset parts, drops raw columns", {
  out <- clean_afp(afp_raw())
  expect_equal(nrow(out), 2L)
  expect_true(all(c("year_onset", "month_onset") %in% names(out)))
  expect_false("junk" %in% names(out))
  expect_equal(names(out)[1:2], c("id", "epid")) # id role first
  expect_equal(out$epid[out$id == 1], "A-1")
})

test_that("clean_afp keeps raw columns when asked", {
  out <- clean_afp(afp_raw(), keep_raw_cols = TRUE)
  expect_true("junk" %in% names(out))
})

test_that("clean_es runs standalone and normalises admin names", {
  raw <- data.frame(
    Id = c(1, 1, 2),
    `Env Sample ID` = c("E1", "E1", "E2"),
    `Last Update Date` = c("2024-01-01", "2024-03-01", "2024-02-01"),
    `Date Collection` = c("2024-01-05", "2024-01-05", "2024-02-09"),
    `Admin0 Name` = c(
      "REPUBLIQUE DE COTE D IVOIRE",
      "REPUBLIQUE DE COTE D IVOIRE",
      "CHAD"
    ),
    check.names = FALSE
  )
  out <- clean_es(raw)
  expect_equal(nrow(out), 2L)
  expect_equal(out$adm0[out$id == 1], "COTE D IVOIRE")
  expect_true(all(c("year_collection", "month_collection") %in% names(out)))
})

test_that("clean_sia runs on activity alone", {
  activity <- data.frame(
    Id = c(1, 2),
    `SIA Sub Activity Code` = c("NGA-2024-001", "TCD-2024-002"),
    `Last Update Date` = c("2024-03-01", "2024-02-01"),
    `Date Start` = c("2024-03-10", "2024-02-12"),
    `Admin0 Name` = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  out <- clean_sia(activity)
  expect_equal(nrow(out), 2L)
  expect_true("sia_code" %in% names(out))
})

test_that("clean_virus runs standalone and integrates linkage when given", {
  raw <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "Z-9"),
    `Last Update Date` = c("2024-03-01", "2024-02-01"),
    `Date Onset` = c("2024-01-02", "2024-02-03"),
    `Virus Type` = c("cVDPV2", "WILD1"),
    `Admin0 Name` = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  expect_equal(nrow(clean_virus(raw)), 2L)

  cases <- clean_afp(afp_raw())
  out <- clean_virus(raw, cases = cases)
  expect_equal(out$surveillance_type[out$epid == "A-1"], "human")
  expect_true(is.na(out$surveillance_type[out$epid == "Z-9"]))
})

test_that("cleaners reject non-data-frame and empty input", {
  expect_error(clean_es(1), "data.frame")
  expect_error(clean_es(data.frame()), "empty")
})
