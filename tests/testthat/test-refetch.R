# Auto-refetch path: missing Id detected by Id-list probe gets pulled
# in via .polis_refetch_missing and joined into the canonical file.

testthat::test_that("auto_refetch adds a missing Id to the canonical file", {
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
  seed_parts(
    polis_folder = root,
    table_name = "im",
    year = 2023,
    df = data.frame(
      Id = 4:5,
      PublishDate = rep("2023-06-15", 2),
      stringsAsFactors = FALSE
    )
  )

  testthat::local_mocked_bindings(
    # Declared total is greater than what's on disk so the early
    # "up to date" skip doesn't fire.
    .polis_get_count = function(...) 10,
    # No incremental rows arrive via the year worker.
    .polis_fetch_id_page = function(...) data.frame(),
    # Probe sees one Id (99) we don't have on disk.
    .polis_fetch_id_list = function(...) c(1L, 2L, 3L, 4L, 5L, 99L),
    # Refetch returns one fresh row for that Id.
    .polis_refetch_missing = function(endpoint, ids, polis_api_key, ...) {
      data.frame(
        Id = ids,
        PublishDate = rep("2024-07-01", length(ids)),
        stringsAsFactors = FALSE
      )
    },
    .package = "polished"
  )

  polished::get_polis_data(
    tables = "im",
    min_date = "2023-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    auto_refetch = TRUE,
    quiet = TRUE
  )

  out_file <- file.path(root, "im.rds")
  testthat::expect_true(file.exists(out_file))
  out <- readRDS(out_file)
  testthat::expect_equal(nrow(out), 6L)
  testthat::expect_true(99L %in% out$Id)
})
