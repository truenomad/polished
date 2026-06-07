# Tests for get_polis_data() — argument validation and return-type
# handling via mocked HTTP, plus one live skip_on_cran smoke test
# against POLIS at the end.

testthat::test_that("get_polis_data aborts when the API key is empty", {
  withr::local_envvar(POLIS_API_KEY = "")
  testthat::expect_error(
    polished::get_polis_data(tables = "im", polis_api_key = ""),
    "POLIS API key is empty"
  )
})

testthat::test_that("get_polis_data aborts on unknown table names", {
  testthat::expect_error(
    polished::get_polis_data(
      tables = "not_a_table",
      polis_api_key = "dummy",
      polis_folder = withr::local_tempdir()
    ),
    "Unknown table name"
  )
})

testthat::test_that("get_polis_data aborts on `return = \"df\"` multi-table", {
  testthat::expect_error(
    polished::get_polis_data(
      tables = c("im", "lqas"),
      return = "df",
      polis_api_key = "dummy",
      polis_folder = withr::local_tempdir()
    ),
    "single table"
  )
})

testthat::test_that("polis_tables_mapping has the documented rows + columns", {
  testthat::expect_s3_class(polished::polis_tables_mapping, "data.frame")
  testthat::expect_equal(nrow(polished::polis_tables_mapping), 10L)
  testthat::expect_setequal(
    names(polished::polis_tables_mapping),
    c("table_name", "endpoint", "date_field")
  )
  testthat::expect_setequal(
    polished::polis_tables_mapping$table_name,
    c(
      "virus",
      "case",
      "human_specimen",
      "environmental_sample",
      "activity",
      "sub_activity",
      "lqas",
      "im",
      "historized_synonyms",
      "historized_geoplace_names"
    )
  )
})

testthat::test_that("get_polis_data skips a fully-up-to-date table cleanly", {
  # If the existing parts already cover @odata.count, the function
  # should re-merge parts and skip without dispatching workers.
  root <- withr::local_tempdir()
  seed_parts(
    polis_folder = root,
    table_name = "im",
    year = 2024,
    df = data.frame(
      Id = 1:5,
      PublishDate = rep("2024-06-15", 5),
      stringsAsFactors = FALSE
    )
  )

  # Stub the count helper to report exactly what's on disk.
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 5,
    .polis_fetch_id_list = function(...) 1:5,
    .package = "polished"
  )

  res <- polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE,
    return = "df"
  )
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 5L)
})

testthat::test_that("get_polis_data return = \"paths\" returns canonical file", {
  root <- withr::local_tempdir()
  seed_parts(
    polis_folder = root,
    table_name = "im",
    year = 2024,
    df = data.frame(
      Id = 1:3,
      PublishDate = rep("2024-06-15", 3),
      stringsAsFactors = FALSE
    )
  )
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  paths <- polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE,
    return = "paths"
  )
  testthat::expect_named(paths, "im")
  testthat::expect_match(paths[["im"]], "im\\.rds$")
  testthat::expect_true(file.exists(paths[["im"]]))
})

testthat::test_that("get_polis_data return = \"invisible\" yields NULL", {
  root <- withr::local_tempdir()
  seed_parts(
    polis_folder = root,
    table_name = "im",
    year = 2024,
    df = data.frame(
      Id = 1:3,
      PublishDate = rep("2024-06-15", 3),
      stringsAsFactors = FALSE
    )
  )
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  result <- polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE,
    return = "invisible"
  )
  testthat::expect_null(result)
})

testthat::test_that("force = TRUE deletes existing parts before running", {
  root <- withr::local_tempdir()
  part_file <- seed_parts(
    polis_folder = root,
    table_name = "im",
    year = 2024,
    df = data.frame(
      Id = 1:3,
      PublishDate = rep("2024-06-15", 3),
      stringsAsFactors = FALSE
    )
  )
  testthat::expect_true(file.exists(part_file))

  # Stub the workers so force-pull does no real HTTP.
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 0,
    .polis_fetch_id_page = function(...) data.frame(),
    .polis_fetch_id_list = function(...) integer(0),
    .package = "polished"
  )

  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE,
    force = TRUE,
    return = "invisible"
  )
  # parts dir is recreated (empty) but the old part file is gone
  testthat::expect_false(file.exists(part_file))
})

# -------------------------------------------------------------------
# Live smoke test — hits POLIS for one small table.
# -------------------------------------------------------------------

testthat::test_that("get_polis_data smoke test: pulls `im` end-to-end", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()
  key <- Sys.getenv("POLIS_API_KEY")
  testthat::skip_if(!nzchar(key), "POLIS_API_KEY not set")

  root <- withr::local_tempdir()
  df <- tryCatch(
    polished::get_polis_data(
      tables = "im",
      polis_folder = root,
      polis_api_key = key,
      workers = 1L,
      auto_refetch = FALSE,
      quiet = TRUE,
      return = "df"
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      transient <- grepl(
        "timed out|Timeout|Could not resolve|HTTP request|curl",
        msg,
        ignore.case = TRUE
      )
      if (transient) testthat::skip(paste("POLIS unreachable:", msg))
      stop(e)
    }
  )
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_gt(nrow(df), 1000L)
  testthat::expect_true("Id" %in% names(df))
  testthat::expect_false(any(duplicated(df$Id)))
})
