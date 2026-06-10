# Synthetic raw tables in POLIS API-style names, written to a project's raw/
# zone so run_pipeline_project() / load_polis() can be exercised end to end.
.proj_afp_raw <- function() {
  data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-01-01", "2025-02-01"),
    ParalysisOnsetDate = c("2024-01-02", "2025-02-03"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
}
.proj_es_raw <- function() {
  data.frame(
    Id = c(1, 2),
    EnvSampleId = c("E1", "E2"),
    LastUpdateDate = c("2024-01-01", "2025-02-01"),
    CollectionDate = c("2024-01-05", "2025-02-09"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
}
.proj_activity_raw <- function() {
  data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2025-04-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
}
.proj_subactivity_raw <- function() {
  data.frame(
    Id = 10:11,
    SIASubActivityCode = "S1",
    LastModificationDate = "2025-04-01",
    DateFrom = c("2025-01-10", "2025-02-20"),
    Admin0Name = "NIGERIA",
    Admin2Name = "BOSSO",
    Admin2Guid = "abc",
    check.names = FALSE
  )
}
# seed a project's raw/ with all four tables under reader-recognised names
.proj_seed_raw <- function(project) {
  saveRDS(.proj_afp_raw(), file.path(project$raw, "Human.rds"))
  saveRDS(.proj_es_raw(), file.path(project$raw, "EnvSample.rds"))
  saveRDS(.proj_activity_raw(), file.path(project$raw, "Activity.rds"))
  saveRDS(.proj_subactivity_raw(), file.path(project$raw, "SubActivity.rds"))
  invisible(project)
}

testthat::test_that("init_polis_project scaffolds, re-opens, and guards; path/clear helpers work", {
  root <- file.path(withr::local_tempdir(), "proj")
  proj <- polished::init_polis_project(root, quiet = TRUE)

  testthat::expect_s3_class(proj, "polis_project")
  testthat::expect_true(all(dir.exists(unlist(proj))))
  testthat::expect_setequal(
    basename(unlist(proj[c(
      "raw",
      "processed",
      "validation",
      "cache",
      "logs"
    )])),
    c("raw", "processed", "validation", "cache", "logs")
  )
  # gitignore written, ignoring the bulky/regenerable zones
  gi <- readLines(file.path(root, ".gitignore"))
  testthat::expect_true(all(c("raw/", "cache/", "logs/") %in% gi))

  # idempotent re-open: no error, same paths, existing files untouched
  writeLines("keep", file.path(proj$raw, "keep.txt"))
  proj2 <- polished::init_polis_project(root, quiet = TRUE)
  testthat::expect_identical(proj2, proj)
  testthat::expect_true(file.exists(file.path(proj$raw, "keep.txt")))

  # project_path builds under a zone; print returns invisibly
  testthat::expect_identical(
    polished::project_path(proj, "processed", "sia"),
    file.path(proj$processed, "sia")
  )
  testthat::expect_identical(polished::project_path(proj), proj$root)
  testthat::expect_identical(testthat::expect_invisible(print(proj)), proj)

  # clear_cache only ever touches cache/ (verbose path exercises the message)
  writeLines("x", file.path(proj$cache, "c.txt"))
  writeLines("y", file.path(proj$processed, "p.txt"))
  testthat::expect_message(polished::clear_cache(proj), "Cleared")
  testthat::expect_equal(length(list.files(proj$cache)), 0L)
  testthat::expect_true(file.exists(file.path(proj$processed, "p.txt")))

  # a relative root is made absolute against the working directory (verbose)
  withr::with_dir(withr::local_tempdir(), {
    rel <- polished::init_polis_project("rel_proj")
    testthat::expect_true(dir.exists(rel$processed))
  })

  # guards
  testthat::expect_error(polished::init_polis_project(""), "non-empty")
  testthat::expect_error(polished::project_path(list()), "polis_project")
  testthat::expect_error(polished::project_path(proj, "nope"), "zone")
  testthat::expect_error(polished::clear_cache(list()), "polis_project")
})

testthat::test_that("dataset IO round-trips, partitions, resolves keys, and materialises shapes", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("dtplyr")
  dir <- file.path(withr::local_tempdir(), "ds")
  data <- tibble::tibble(
    year_start = c(2024L, 2025L, 2025L),
    adm0 = c("NIGERIA", "NIGERIA", "CHAD"),
    value = 1:3
  )
  # "year" resolves to the real year column; an explicit present key passes
  # through; a missing key is dropped; empty record set -> empty tibble
  testthat::expect_equal(
    polished:::.polis_resolve_partition(data, c("year", "adm0", "nope")),
    c("year_start", "adm0")
  )
  testthat::expect_equal(nrow(polished:::.polis_rbind_rows(list())), 0L)
  polished:::.polis_write_dataset(
    data,
    dir,
    polished:::.polis_resolve_partition(data, "year")
  )
  testthat::expect_true(any(grepl(
    "year_start=2025",
    list.files(dir, recursive = TRUE)
  )))

  q <- polished:::.polis_open_dataset(dir)
  # filter pushes down; absent value -> empty; missing column -> no-op
  one <- polished:::.polis_filter_dataset(q, 2025L, polished:::.polis_year_cols)
  testthat::expect_equal(nrow(dplyr::collect(one)), 2L)
  testthat::expect_identical(
    polished:::.polis_filter_dataset(q, NULL, polished:::.polis_year_cols),
    q
  )

  # materialise shapes
  testthat::expect_s3_class(
    polished:::.polis_materialize(q, "tibble"),
    "tbl_df"
  )
  testthat::expect_s3_class(
    polished:::.polis_materialize(q, "dt"),
    "data.table"
  )
  testthat::expect_s3_class(
    polished:::.polis_materialize(q, "dtplyr"),
    "dtplyr_step"
  )
  testthat::expect_identical(polished:::.polis_materialize(q, "arrow"), q)
})

testthat::test_that("snapshot manifest is content-stable and round-trips", {
  proj <- polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
  testthat::skip_if_not_installed("jsonlite")
  tables <- list(a = tibble::tibble(x = 1:3), b = tibble::tibble(y = 1:2))

  id1 <- polished:::.polis_write_manifest(proj, tables, created = "t0")
  testthat::expect_true(file.exists(file.path(proj$raw, "manifest.json")))
  # same content -> same id; different content -> different id
  id2 <- polished:::.polis_write_manifest(proj, tables, created = "t1")
  testthat::expect_identical(id1, id2)
  id3 <- polished:::.polis_write_manifest(
    proj,
    list(a = tibble::tibble(x = 1:4)),
    created = "t0"
  )
  testthat::expect_false(identical(id1, id3))

  man <- polished:::.polis_read_manifest(proj)
  testthat::expect_identical(man$snapshot_id, id3)
  testthat::expect_equal(man$tables$a$rows, 4L)
  # no manifest -> NULL
  testthat::expect_null(
    polished:::.polis_read_manifest(
      polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
    )
  )
})

testthat::test_that("run_pipeline_project streams raw to partitioned processed and returns a manifest", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("dtplyr")
  proj <- polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
  .proj_seed_raw(proj)

  # first run (verbose) stamps the manifest and writes the datasets
  res <- NULL
  testthat::expect_message(
    res <- polished::run_pipeline_project(proj),
    "Pipeline wrote"
  )
  # afp + es + sia always land; virus only when there are positives (none here)
  testthat::expect_true(all(c("afp", "es", "sia") %in% res$tables$dataset))
  testthat::expect_true(all(res$tables$rows > 0))
  testthat::expect_match(res$snapshot_id, "^[0-9a-f]+$")
  # manifest stamped on first run; sia partitioned by its year column
  testthat::expect_true(file.exists(file.path(proj$raw, "manifest.json")))
  testthat::expect_true(any(grepl(
    "year_start=2025",
    list.files(file.path(proj$processed, "sia"), recursive = TRUE)
  )))
  # SIA cache populated and keyed on the snapshot (one entry)
  testthat::expect_equal(length(list.files(proj$cache)), 1L)

  # second run re-reads the existing manifest (same snapshot) and hits the cache
  res2 <- polished::run_pipeline_project(proj, quiet = TRUE)
  testthat::expect_identical(res2$snapshot_id, res$snapshot_id)
  testthat::expect_equal(length(list.files(proj$cache)), 1L)

  # empty raw aborts
  empty <- polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
  testthat::expect_error(
    polished::run_pipeline_project(empty, quiet = TRUE),
    "No recognised source tables"
  )
})

testthat::test_that("load_polis returns a filtered slice across datasets in the requested shape", {
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("data.table")
  testthat::skip_if_not_installed("dtplyr")
  proj <- polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
  .proj_seed_raw(proj)
  polished::run_pipeline_project(proj, quiet = TRUE)

  # year filter prunes across every dataset; one named element per dataset
  slice <- polished::load_polis(proj, year = 2025)
  testthat::expect_true(all(c("afp", "es", "sia") %in% names(slice)))
  testthat::expect_true(all(slice$sia$year_start == 2025))
  testthat::expect_true(all(slice$afp$year_onset == 2025))

  # country filter (on adm0) + dataset subset + dtplyr shape
  one <- polished::load_polis(
    proj,
    country = "NIGERIA",
    datasets = "sia",
    as = "dtplyr"
  )
  testthat::expect_named(one, "sia")
  testthat::expect_s3_class(one$sia, "dtplyr_step")
  testthat::expect_true(all(dplyr::collect(one$sia)$adm0 == "NIGERIA"))

  # absent year -> empty tables, not an error
  testthat::expect_equal(nrow(polished::load_polis(proj, year = 1990)$sia), 0L)

  # no processed datasets -> warning + empty list
  empty <- polished::init_polis_project(withr::local_tempdir(), quiet = TRUE)
  testthat::expect_warning(
    res <- polished::load_polis(empty),
    "No processed datasets"
  )
  testthat::expect_length(res, 0L)
})
