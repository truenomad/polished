# Download-engine internals in R/utils.R, exercised by mocking the single
# network call (.polis_get_body) or the page fetcher (.polis_fetch_id_page).
# Synthetic OData bodies only; no real HTTP.

testthat::test_that("HTTP primitives build URLs, parse counts and pages off a mocked body", {
  testthat::local_mocked_bindings(
    .polis_get_body = function(url, ...) {
      if (grepl("count=true", url)) {
        return(list(`@odata.count` = "42"))
      }
      fake_polis_body(
        records = list(
          list(Id = 1, V = "a"),
          list(Id = 2, V = "b")
        )
      )
    },
    .package = "polished"
  )

  cnt <- polished:::.polis_get_count(
    "Case",
    "CaseDate",
    "2024-01-01",
    "2024-12-31",
    "AFRO",
    "NGA",
    "key"
  )
  testthat::expect_equal(cnt, 42)

  page <- polished:::.polis_fetch_id_page(
    "Case",
    "CaseDate",
    "2024-01-01",
    "2024-12-31",
    "AFRO",
    "NGA",
    "key",
    last_id = 0,
    select = "Id"
  )
  testthat::expect_equal(nrow(page), 2L)
})

testthat::test_that(".polis_get_count returns NA when the body carries no count", {
  testthat::local_mocked_bindings(
    .polis_get_body = function(url, ...) list(value = list()),
    .package = "polished"
  )
  testthat::expect_true(is.na(polished:::.polis_get_count(
    "Im",
    "PublishDate",
    "2024-01-01",
    "2024-12-31",
    "",
    "",
    "key"
  )))
})

testthat::test_that(".polis_fetch_id_list pages to exhaustion and retries without $select", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(..., select = NULL) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(data.frame()) # empty first page -> triggers no-$select retry
      }
      if (calls == 2L) {
        return(data.frame(Id = 1:3))
      }
      data.frame() # end of data
    },
    .package = "polished"
  )
  ids <- polished:::.polis_fetch_id_list(
    "Case",
    "CaseDate",
    "2024-01-01",
    "2024-12-31",
    "AFRO",
    "NGA",
    "key"
  )
  testthat::expect_equal(ids, 1:3)
})

testthat::test_that(".polis_refetch_missing chunks ids and binds the refetched rows", {
  testthat::local_mocked_bindings(
    .polis_get_body = function(url, ...) {
      fake_polis_body(records = list(list(Id = 1), list(Id = 2)))
    },
    .package = "polished"
  )
  # direct chunk fetch
  chunk <- polished:::.polis_refetch_chunk(c(1, 2), "Im", "key")
  testthat::expect_equal(nrow(chunk), 2L)

  # empty id set -> empty frame, no call
  testthat::expect_equal(
    nrow(polished:::.polis_refetch_missing("Im", integer(0), "key")),
    0L
  )
  # sequential (workers = 1) path binds across chunks
  out <- polished:::.polis_refetch_missing(
    "Im",
    1:4,
    "key",
    chunk_size = 2L,
    workers = 1L
  )
  testthat::expect_true(nrow(out) >= 1L)
})

testthat::test_that(".polis_fetch_year_worker pages into a part file and reports rows", {
  dir <- withr::local_tempdir()
  spec <- list(
    year = 2024,
    part_file = file.path(dir, "year_2024.rds"),
    ext = "rds",
    endpoint = "Im",
    date_field = "PublishDate",
    region = "AFRO",
    country_code = "",
    polis_api_key = "key",
    page_size = 2000L
  )
  n <- 0L
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) {
        data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
      } else {
        data.frame()
      }
    },
    .package = "polished"
  )
  res <- polished:::.polis_fetch_year_worker(spec)
  testthat::expect_equal(res$rows, 3L)
  testthat::expect_true(file.exists(spec$part_file))
})

testthat::test_that(".polis_migrate_to_parts splits a canonical file; .polis_merge_parts dedups", {
  dir <- withr::local_tempdir()
  out_file <- file.path(dir, "im.rds")
  parts_dir <- file.path(dir, ".parts")
  saveRDS(
    data.frame(
      Id = c(1, 2, 3),
      PublishDate = c("2023-06-15", "2024-06-15", "2024-07-01"),
      stringsAsFactors = FALSE
    ),
    out_file
  )
  polished:::.polis_migrate_to_parts(out_file, parts_dir, "rds", "PublishDate")
  testthat::expect_true(dir.exists(parts_dir))
  testthat::expect_gte(length(list.files(parts_dir, pattern = "^year_")), 2L)

  merged_file <- file.path(dir, "merged.rds")
  n <- polished:::.polis_merge_parts(
    parts_dir,
    merged_file,
    "rds",
    "PublishDate"
  )
  testthat::expect_equal(n, 3L)
  testthat::expect_true(file.exists(merged_file))
})

testthat::test_that(".polis_fetch_year_worker resumes from a part, ticks on_batch, and ends on a stalled cursor", {
  dir <- withr::local_tempdir()
  spec <- list(
    year = 2024,
    part_file = file.path(dir, "year_2024.rds"),
    ext = "rds",
    endpoint = "Im",
    date_field = "PublishDate",
    region = "AFRO",
    country_code = "",
    polis_api_key = "key",
    page_size = 2000L
  )
  # resume: a part already on disk -> last_id seeds from it, new rows append
  saveRDS(
    data.frame(Id = 1:2, PublishDate = rep("2024-06-15", 2)),
    spec$part_file
  )
  n <- 0L
  batches <- 0L
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) data.frame(Id = 3:4, PublishDate = rep("2024-06-15", 2)) else
        data.frame()
    },
    .package = "polished"
  )
  res <- polished:::.polis_fetch_year_worker(
    spec,
    on_batch = function(...) batches <<- batches + 1L
  )
  testthat::expect_equal(res$rows, 4L)
  testthat::expect_true(batches >= 1L)

  # stalled cursor: the page never advances past last_id -> the worker ends early
  spec2 <- spec
  spec2$part_file <- file.path(dir, "year_2023.rds")
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(...) {
      data.frame(Id = c(1, 2), PublishDate = rep("2023-06-15", 2))
    },
    .package = "polished"
  )
  stalled <- suppressMessages(polished:::.polis_fetch_year_worker(spec2))
  testthat::expect_equal(stalled$rows, 2L)
})

testthat::test_that(".polis_migrate_to_parts tolerates a corrupt or date-less source", {
  dir <- withr::local_tempdir()
  # corrupt rds -> removed, no parts written, no error
  corrupt <- file.path(dir, "corrupt.rds")
  writeLines("not an rds", corrupt)
  testthat::expect_silent(suppressMessages(
    polished:::.polis_migrate_to_parts(
      corrupt,
      file.path(dir, "p1"),
      "rds",
      "PublishDate"
    )
  ))
  testthat::expect_false(dir.exists(file.path(dir, "p1")))

  # readable but missing the date field -> no-op
  nodate <- file.path(dir, "nodate.rds")
  saveRDS(data.frame(Id = 1:2), nodate)
  polished:::.polis_migrate_to_parts(
    nodate,
    file.path(dir, "p2"),
    "rds",
    "PublishDate"
  )
  testthat::expect_false(dir.exists(file.path(dir, "p2")))
})

testthat::test_that(".polis_merge_parts skips unreadable parts and no-ops on an empty dir", {
  dir <- withr::local_tempdir()
  testthat::expect_equal(
    polished:::.polis_merge_parts(
      file.path(dir, "missing"),
      file.path(dir, "o.rds"),
      "rds",
      "PublishDate"
    ),
    0L
  )
  parts <- file.path(dir, ".parts")
  dir.create(parts)
  saveRDS(
    data.frame(Id = 1:2, PublishDate = "2024-06-15"),
    file.path(parts, "year_2024.rds")
  )
  writeLines("junk", file.path(parts, "year_2023.rds")) # unreadable -> skipped
  out <- file.path(dir, "merged.rds")
  n <- suppressMessages(polished:::.polis_merge_parts(
    parts,
    out,
    "rds",
    "PublishDate"
  ))
  testthat::expect_equal(n, 2L)
})

testthat::test_that("io dispatch + readers reject unsupported types and missing dirs", {
  testthat::expect_error(polished:::.polis_read("x.xml"), "Unsupported")
  testthat::expect_error(
    polished:::.polis_write(
      data.frame(a = 1),
      file.path(withr::local_tempdir(), "x.xml")
    ),
    "Unsupported"
  )
  testthat::expect_error(
    polished:::.polis_require("not_a_real_pkg_zzz", "do x"),
    "needed"
  )
  testthat::expect_error(
    polished:::.polis_read_inputs(file.path(withr::local_tempdir(), "missing")),
    "does not exist"
  )
  # empty dir -> empty list
  testthat::expect_length(
    polished:::.polis_read_inputs(withr::local_tempdir()),
    0L
  )
})

testthat::test_that("part-cache helpers recover from corruption and bad write targets", {
  dir <- withr::local_tempdir()

  # read_meta: a malformed sidecar is ignored, the part re-read (backfill)
  part <- file.path(dir, "year_2024.rds")
  saveRDS(data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3)), part)
  saveRDS("garbage", polished:::.polis_meta_path(part))
  testthat::expect_identical(
    polished:::.polis_read_meta(part, "rds", "PublishDate")$n_rows,
    3L
  )

  # malformed sidecar AND an unreadable part -> empty meta
  bad <- file.path(dir, "year_2023.rds")
  writeLines("not an rds", bad)
  saveRDS("garbage", polished:::.polis_meta_path(bad))
  testthat::expect_identical(
    suppressWarnings(polished:::.polis_read_meta(
      bad,
      "rds",
      "PublishDate"
    ))$n_rows,
    0L
  )

  # io_write_part aborts when the part write target is unwritable
  testthat::expect_error(
    suppressWarnings(polished:::.polis_io_write_part(
      data.frame(Id = 1),
      file.path(dir, "no_such_subdir", "x.rds"),
      "rds",
      "PublishDate"
    ))
  )

  # fetch_year_worker quarantines a corrupt existing part, then pulls fresh
  spec <- list(
    year = 2024,
    part_file = file.path(dir, "y.rds"),
    ext = "rds",
    endpoint = "Im",
    date_field = "PublishDate",
    region = "AFRO",
    country_code = "",
    polis_api_key = "key",
    page_size = 2000L
  )
  writeLines("corrupt", spec$part_file)
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(...) data.frame(),
    .package = "polished"
  )
  res <- suppressWarnings(polished:::.polis_fetch_year_worker(spec))
  testthat::expect_identical(res$rows, 0L)
  testthat::expect_true(
    file.exists(paste0(spec$part_file, ".corrupt.", Sys.getpid()))
  )
})
