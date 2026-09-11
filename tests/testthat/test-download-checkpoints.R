checkpoint_fixture <- function(root, n = 6L) {
  rows <- data.frame(Id = seq_len(n), LastUpdateDate = "2024-06-15")
  spec <- list(
    year = 2024,
    endpoint = "Case",
    date_field = "LastUpdateDate",
    region = "Global",
    country_code = NULL,
    polis_api_key = "test",
    part_file = file.path(root, "year_2024.rds"),
    ext = "rds",
    page_size = 2L
  )
  fetch <- function(last_id = NULL, ...) {
    start <- if (is.null(last_id)) 0 else last_id
    rows[rows$Id > start & rows$Id <= start + 2L, , drop = FALSE]
  }
  list(rows = rows, spec = spec, fetch = fetch)
}

testthat::test_that("page journals resume after network interruption", {
  f <- checkpoint_fixture(withr::local_tempdir())
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(last_id = NULL, ...) {
      if (!is.null(last_id) && last_id >= 4) stop("disconnected")
      f$fetch(last_id)
    },
    .package = "polished"
  )
  testthat::expect_error(
    polished:::.polis_fetch_year_worker(f$spec),
    "disconnected"
  )
  testthat::expect_equal(
    polished:::.polis_read_meta(
      f$spec$part_file,
      "rds",
      "LastUpdateDate"
    )$n_rows,
    4L
  )
  testthat::expect_equal(
    polished:::.polis_checkpoint_read(
      f$spec$part_file,
      "rds",
      "LastUpdateDate"
    )$Id,
    1:4
  )
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = f$fetch,
    .package = "polished"
  )
  result <- polished:::.polis_fetch_year_worker(f$spec)
  testthat::expect_equal(result$new_rows, 2L)
  testthat::expect_equal(readRDS(f$spec$part_file), f$rows)
  testthat::expect_false(dir.exists(polished:::.polis_journal_dir(
    f$spec$part_file
  )))
})

testthat::test_that("uncommitted pages are refetched and compaction can be replayed", {
  f <- checkpoint_fixture(withr::local_tempdir())
  write <- polished:::.polis_io_write_atomic
  commits <- 0L
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = f$fetch,
    .polis_io_write_atomic = function(x, path, fmt) {
      if (basename(path) == "state.rds") {
        commits <<- commits + 1L
        if (commits == 2L) stop("journal failed")
      }
      write(x, path, fmt)
    },
    .package = "polished"
  )
  testthat::expect_error(
    polished:::.polis_fetch_year_worker(f$spec),
    "journal failed"
  )
  testthat::expect_equal(
    polished:::.polis_read_journal(f$spec$part_file)$meta$n_rows,
    0L
  )
  testthat::local_mocked_bindings(
    .polis_io_write_atomic = write,
    .package = "polished"
  )
  write_part <- polished:::.polis_io_write_part
  testthat::local_mocked_bindings(
    .polis_io_write_part = function(...) {
      write_part(...)
      stop("interrupted after rename")
    },
    .package = "polished"
  )
  testthat::expect_error(
    polished:::.polis_fetch_year_worker(f$spec),
    "interrupted after rename"
  )
  testthat::expect_true(
    polished:::.polis_read_journal(f$spec$part_file)$finished
  )
  testthat::local_mocked_bindings(
    .polis_io_write_part = write_part,
    .polis_fetch_id_page = function(...) stop("must not fetch again"),
    .package = "polished"
  )
  polished:::.polis_fetch_year_worker(f$spec)
  testthat::expect_equal(readRDS(f$spec$part_file), f$rows)
})

testthat::test_that("failed rename preserves the previous checkpoint", {
  path <- file.path(withr::local_tempdir(), "part.rds")
  saveRDS(data.frame(Id = 1L), path)
  write <- polished:::.polis_io_write_atomic
  environment(write) <- list2env(
    list(file.rename = function(...) FALSE),
    parent = environment(write)
  )
  testthat::expect_error(
    write(data.frame(Id = 2L), path, "rds"),
    "Failed to commit"
  )
  testthat::expect_equal(readRDS(path)$Id, 1L)
  testthat::expect_false(file.exists(paste0(path, ".tmp.", Sys.getpid())))
})

testthat::test_that("checkpoint data writes grow linearly and progress uses metadata", {
  f <- checkpoint_fixture(withr::local_tempdir(), 20L)
  write <- polished:::.polis_io_write_atomic
  serialized <- 0L
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = f$fetch,
    .polis_io_write_atomic = function(x, path, fmt) {
      if (is.data.frame(x)) serialized <<- serialized + nrow(x)
      write(x, path, fmt)
    },
    .package = "polished"
  )
  polished:::.polis_fetch_year_worker(f$spec)
  testthat::expect_equal(serialized, 40L)
  read <- polished:::.polis_io_read
  testthat::local_mocked_bindings(
    .polis_io_read = function(...) stop("unnecessary read"),
    .package = "polished"
  )
  testthat::expect_equal(
    polished:::.polis_read_meta(
      f$spec$part_file,
      "rds",
      "LastUpdateDate"
    )$n_rows,
    20L
  )
  testthat::local_mocked_bindings(.polis_io_read = read, .package = "polished")
  saveRDS(f$rows[1:2, ], f$spec$part_file)
  testthat::expect_equal(
    polished:::.polis_read_meta(
      f$spec$part_file,
      "rds",
      "LastUpdateDate"
    )$n_rows,
    2L
  )
})
