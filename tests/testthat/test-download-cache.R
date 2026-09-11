# A changing fake service exercises the public downloader, including its filters.
download_service <- function(root, rows, table = "case") {
  e <- new.env(parent = emptyenv())
  e$rows <- rows
  e$calls <- 0L
  e$refetched <- numeric()
  date_field <- polished::polis_tables_mapping$date_field[
    polished::polis_tables_mapping$table_name == table
  ]
  select_rows <- function(min_date, max_date, region, country_code) {
    x <- e$rows
    if (!is.na(date_field)) {
      years <- as.integer(substr(x[[date_field]], 1, 4))
      x <- x[
        years >= as.integer(format(as.Date(min_date), "%Y")) &
          years <= as.integer(format(as.Date(max_date), "%Y")),
        ,
        drop = FALSE
      ]
    }
    if (!is.null(country_code))
      x <- x[x$CountryISO3Code == country_code, , drop = FALSE]
    if (toupper(region) != "GLOBAL" && "WHORegion" %in% names(x))
      x <- x[x$WHORegion == region, , drop = FALSE]
    x
  }
  testthat::local_mocked_bindings(
    .polis_get_count = function(
      endpoint,
      date_field,
      min_date,
      max_date,
      region,
      country_code,
      ...
    ) {
      nrow(select_rows(min_date, max_date, region, country_code))
    },
    .polis_fetch_id_page = function(
      endpoint,
      date_field,
      min_date,
      max_date,
      region,
      country_code,
      last_id = NULL,
      select = NULL,
      ...
    ) {
      e$calls <- e$calls + 1L
      x <- select_rows(min_date, max_date, region, country_code)
      if (!is.null(last_id)) x <- x[x$Id > last_id, , drop = FALSE]
      x <- x[order(x$Id), , drop = FALSE]
      if (!is.null(select)) x <- x[, select, drop = FALSE]
      utils::head(x, 2L)
    },
    .polis_refetch_missing = function(endpoint, ids, ...) {
      e$refetched <- c(e$refetched, ids)
      e$rows[e$rows$Id %in% ids, , drop = FALSE]
    },
    .package = "polished",
    .env = parent.frame()
  )
  e$run <- function(...) {
    polished::get_polis_data(
      tables = table,
      polis_folder = root,
      polis_api_key = "test",
      quiet = TRUE,
      ...
    )
  }
  e
}

cache_rows <- function() {
  data.frame(
    Id = 1:3,
    LastUpdateDate = "2024-06-15T08:00:00Z",
    CountryISO3Code = c("NGA", "PAK", "NGA"),
    WHORegion = c("AFRO", "EMRO", "AFRO"),
    value = 1:3
  )
}

testthat::test_that("unchanged counts cannot hide revisions, replacements or deletions", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  e$rows$value[1] <- 99L
  e$rows$LastUpdateDate[1] <- "2024-06-15T09:00:00Z"
  e$rows$Id[2] <- 4L
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  x <- readRDS(file.path(root, "raw_afp.rds"))
  testthat::expect_setequal(x$Id, c(1, 3, 4))
  testthat::expect_equal(x$value[x$Id == 1], 99L)
  testthat::expect_setequal(e$refetched, c(1, 4))
  e$rows <- e$rows[-1, ]
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(readRDS(file.path(root, "raw_afp.rds"))$Id, c(3, 4))
})

testthat::test_that("country, region and year scope changes replace the snapshot", {
  root <- withr::local_tempdir()
  rows <- cache_rows()
  rows$LastUpdateDate[3] <- "2023-06-15T08:00:00Z"
  e <- download_service(root, rows)
  read_ids <- function() readRDS(file.path(root, "raw_afp.rds"))$Id
  e$run(min_date = "2023-01-01", max_date = "2024-12-31", country_code = "NGA")
  testthat::expect_setequal(read_ids(), c(1, 3))
  e$run(min_date = "2023-01-01", max_date = "2024-12-31", country_code = "PAK")
  testthat::expect_equal(read_ids(), 2L)
  e$run(min_date = "2023-01-01", max_date = "2024-12-31", region = "AFRO")
  testthat::expect_setequal(read_ids(), c(1, 3))
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(read_ids(), c(1, 2))
  e$run(min_date = "2023-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(read_ids(), 1:3)
})

testthat::test_that("unchanged completed snapshots avoid canonical reads and partition rebuilding", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::local_mocked_bindings(
    .polis_io_read = function(...) stop("unnecessary data read"),
    .polis_io_write_part = function(...) stop("unnecessary split"),
    .package = "polished"
  )
  testthat::expect_no_error(e$run(
    min_date = "2024-01-01",
    max_date = "2024-12-31"
  ))
  testthat::expect_false(dir.exists(file.path(root, ".parts", "raw_afp")))
})

testthat::test_that("reference snapshots use an explicit expiry and force bypasses it", {
  root <- withr::local_tempdir()
  e <- download_service(root, data.frame(Id = 1:3, Pop = 100:102), "population")
  e$run()
  calls <- e$calls
  e$rows$Pop[1] <- 999L
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_equal(e$calls, calls)
  testthat::expect_equal(
    readRDS(file.path(root, "raw_population.rds"))$Pop[1],
    100L
  )
  e$run(reference_refresh_days = 0)
  testthat::expect_equal(
    readRDS(file.path(root, "raw_population.rds"))$Pop[1],
    999L
  )
  e$rows$Pop[1] <- 1000L
  e$run(force = TRUE)
  testthat::expect_equal(
    readRDS(file.path(root, "raw_population.rds"))$Pop[1],
    1000L
  )
  manifest <- polished:::.polis_download_manifest(root, "raw_population", "rds")
  state <- readRDS(manifest)
  state$fetched_at <- Sys.time() - 2 * 86400
  saveRDS(state, manifest)
  e$rows$Pop[1] <- 1001L
  e$run()
  testthat::expect_equal(
    readRDS(file.path(root, "raw_population.rds"))$Pop[1],
    1001L
  )
})

testthat::test_that("failed refresh or changed scope preserves the last complete file", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  path <- file.path(root, "raw_afp.rds")
  original <- readRDS(path)
  e$rows$LastUpdateDate[1] <- "2024-06-16T08:00:00Z"
  testthat::local_mocked_bindings(
    .polis_refetch_missing = function(...) data.frame(),
    .package = "polished"
  )
  testthat::expect_error(
    e$run(min_date = "2024-01-01", max_date = "2024-12-31"),
    "previous snapshot retained"
  )
  testthat::expect_identical(readRDS(path), original)
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(...) stop("disconnected"),
    .package = "polished"
  )
  testthat::expect_error(
    e$run(
      min_date = "2024-01-01",
      max_date = "2024-12-31",
      country_code = "PAK"
    ),
    "checkpointed"
  )
  testthat::expect_identical(readRDS(path), original)
  testthat::expect_false(
    readRDS(polished:::.polis_download_manifest(
      root,
      "raw_afp",
      "rds"
    ))$complete
  )
})

testthat::test_that("legacy and externally modified caches are not certified by row count", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  path <- file.path(root, "raw_afp.rds")
  saveRDS(data.frame(Id = 7:9), path)
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(readRDS(path)$Id, 1:3)
  saveRDS(data.frame(Id = 100L), path)
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(readRDS(path)$Id, 1:3)
})

testthat::test_that("failed initial verification retains resumable parts and can be retried", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  fetch_ids <- polished:::.polis_fetch_id_list
  testthat::local_mocked_bindings(
    .polis_fetch_id_list = function(...) stop("verification unavailable"),
    .package = "polished"
  )
  testthat::expect_error(
    e$run(min_date = "2024-01-01", max_date = "2024-12-31"),
    "Checkpoints retained"
  )
  testthat::expect_false(file.exists(file.path(root, "raw_afp.rds")))
  testthat::expect_true(dir.exists(file.path(root, ".parts", "raw_afp")))
  testthat::local_mocked_bindings(
    .polis_fetch_id_list = fetch_ids,
    .package = "polished"
  )
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_setequal(readRDS(file.path(root, "raw_afp.rds"))$Id, 1:3)
})

testthat::test_that("invalid reference expiry is rejected", {
  for (bad in list(-1, NA_real_, Inf, c(1, 2), "1")) {
    testthat::expect_error(
      polished::get_polis_data(reference_refresh_days = bad),
      "reference_refresh_days"
    )
  }
})

testthat::test_that("resuming an interrupted pull reconciles edits below its cursor", {
  root <- withr::local_tempdir()
  e <- download_service(root, cache_rows())
  fetch <- polished:::.polis_fetch_id_page
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = function(last_id = NULL, ...) {
      if (!is.null(last_id) && last_id >= 2) stop("disconnected")
      fetch(last_id = last_id, ...)
    },
    .package = "polished"
  )
  testthat::expect_error(
    e$run(min_date = "2024-01-01", max_date = "2024-12-31"),
    "checkpointed"
  )
  e$rows$value[1] <- 77L
  e$rows$LastUpdateDate[1] <- "2024-08-01T08:00:00Z"
  testthat::local_mocked_bindings(
    .polis_fetch_id_page = fetch,
    .package = "polished"
  )
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  x <- readRDS(file.path(root, "raw_afp.rds"))
  testthat::expect_equal(x$value[x$Id == 1], 77L)
})

testthat::test_that("event-date tables refresh edits even when their dates are unchanged", {
  root <- withr::local_tempdir()
  e <- download_service(
    root,
    data.frame(Id = 1:3, PublishDate = "2024-06-15", value = 1:3),
    "im"
  )
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  e$rows$value[1] <- 88L
  e$run(min_date = "2024-01-01", max_date = "2024-12-31")
  testthat::expect_equal(readRDS(file.path(root, "raw_im.rds"))$value[1], 88L)
})
