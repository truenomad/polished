# run_pipeline composes the cleaners; runs on any subset of inputs.

raw_afp <- function() {
  data.frame(
    Id = c(1, 1, 2),
    Epid = c("A-1", "A-1", "B-2"),
    LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
    DateOnset = c("2024-01-02", "2024-01-02", "2024-02-03"),
    Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
    check.names = FALSE
  )
}

raw_virus <- function() {
  data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "Z-9"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    DateOnset = c("2024-01-02", "2024-02-03"),
    VirusTypeName = c("cVDPV2", "WILD1"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
}

testthat::test_that("run_pipeline cleans a single dataset", {
  out <- suppressMessages(polished::run_pipeline(list(afp = raw_afp())))
  testthat::expect_named(out, "afp")
  testthat::expect_equal(nrow(out$afp), 2L)
})

testthat::test_that("run_pipeline links virus to cleaned cases", {
  out <- suppressMessages(
    polished::run_pipeline(list(afp = raw_afp(), virus = raw_virus()))
  )
  testthat::expect_setequal(names(out), c("afp", "virus"))
  testthat::expect_equal(
    out$virus$surveillance_type[out$virus$epid == "A-1"],
    "human"
  )
})

testthat::test_that("run_pipeline rejects a non-list input", {
  testthat::expect_error(
    polished::run_pipeline(data.frame(x = 1)),
    "named list"
  )
})

testthat::test_that("run_pipeline_dir round-trips through disk", {
  src <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  saveRDS(raw_afp(), file.path(src, "Human_Detailed.rds"))

  cleaned <- suppressMessages(
    polished::run_pipeline_dir(src, out_dir, format = "rds")
  )
  testthat::expect_named(cleaned, "afp")
  testthat::expect_true(file.exists(file.path(out_dir, "afp.rds")))
})
