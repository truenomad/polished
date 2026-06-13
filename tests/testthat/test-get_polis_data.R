# Tests for get_polis_data() — argument validation and the on-disk /
# return-paths contract via mocked HTTP, plus one live skip_on_cran smoke
# test against POLIS at the end.

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

testthat::test_that("polis_tables_mapping has the documented rows + columns", {
  testthat::expect_s3_class(polished::polis_tables_mapping, "data.frame")
  testthat::expect_equal(nrow(polished::polis_tables_mapping), 11L)
  testthat::expect_setequal(
    names(polished::polis_tables_mapping),
    c("table_name", "endpoint", "date_field", "file_stem")
  )
  # file_stem is the raw_* on-disk name (e.g. case -> raw_afp)
  testthat::expect_identical(
    polished::polis_tables_mapping$file_stem[
      polished::polis_tables_mapping$table_name == "case"
    ],
    "raw_afp"
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
      "historized_geoplace_names",
      "population"
    )
  )
  # population is the lone reference table: NA date_field -> pulled whole
  pop <- polished::polis_tables_mapping[
    polished::polis_tables_mapping$table_name == "population",
  ]
  testthat::expect_identical(pop$endpoint, "Population")
  testthat::expect_true(is.na(pop$date_field))
})

testthat::test_that("population (NA date_field) is pulled whole, Id-only, with no date filter", {
  root <- withr::local_tempdir()
  urls <- character(0)
  testthat::local_mocked_bindings(
    # count short-circuits to disk-vs-total; the page fetch drives the rest
    .polis_get_count = function(...) 3,
    .polis_get_body = function(url, polis_api_key, ...) {
      urls[[length(urls) + 1L]] <<- url
      # subsequent pages (Id gt N) and the verification re-page return empty
      if (grepl("Id%20gt", url)) {
        return(list(value = list()))
      }
      list(
        value = list(
          list(Id = 1L, Pop = 100L),
          list(Id = 2L, Pop = 200L),
          list(Id = 3L, Pop = 300L)
        )
      )
    },
    .package = "polished"
  )

  result <- polished::get_polis_data(
    tables = "population",
    polis_folder = root,
    polis_api_key = "fake",
    # keep the resume cache so the part layout can be inspected below
    prune_parts = FALSE,
    quiet = TRUE
  )

  # whole table landed in the canonical file
  pop <- readRDS(file.path(root, "raw_population.rds"))
  testthat::expect_identical(nrow(pop), 3L)
  # a single un-partitioned part (year_0), not one per calendar year
  parts <- list.files(file.path(root, ".parts", "raw_population"))
  testthat::expect_true(any(grepl("^year_0\\.", parts)))
  testthat::expect_false(any(grepl("^year_20", parts)))
  # no date-range clause on any request -- it is a reference table
  testthat::expect_false(any(grepl("%20ge%20|%20le%20", urls)))
  testthat::expect_true(any(grepl("Population", urls)))
})

testthat::test_that("get_polis_data writes to disk and returns paths only", {
  # The data is never returned into memory: the value is the named file
  # path(s), and the table is read back from disk.
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

  result <- polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )

  # Nothing is returned -- pure side effect.
  testthat::expect_null(result)

  # The data is on disk at the canonical path and reads back with every row.
  out_file <- file.path(root, "raw_im.rds")
  testthat::expect_true(file.exists(out_file))
  on_disk <- readRDS(out_file)
  testthat::expect_s3_class(on_disk, "data.frame")
  testthat::expect_equal(nrow(on_disk), 5L)
})

testthat::test_that("get_polis_data returns NULL invisibly", {
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
  # withVisible reports FALSE for an invisibly-returned value.
  vis <- withVisible(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  ))
  testthat::expect_false(vis$visible)
  testthat::expect_null(vis$value)
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
    force = TRUE
  )
  # parts dir is recreated (empty) but the old part file is gone
  testthat::expect_false(file.exists(part_file))
})

# -------------------------------------------------------------------
# prune_parts — the resume cache is dropped once the canonical is a
# complete, deduped checkpoint, and rebuilt from it on the next run.
# -------------------------------------------------------------------

testthat::test_that("prune_parts = TRUE (default) deletes the .parts cache after a successful pull", {
  root <- withr::local_tempdir()
  n <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) {
        data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
      } else {
        data.frame()
      }
    },
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )
  # canonical is complete; the resume cache is gone
  testthat::expect_equal(nrow(readRDS(file.path(root, "raw_im.rds"))), 3L)
  testthat::expect_false(dir.exists(file.path(root, ".parts", "raw_im")))
})

testthat::test_that("prune_parts = FALSE retains the resume cache", {
  root <- withr::local_tempdir()
  n <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) {
        data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
      } else {
        data.frame()
      }
    },
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    prune_parts = FALSE,
    quiet = TRUE
  )
  parts_dir <- file.path(root, ".parts", "raw_im")
  testthat::expect_true(dir.exists(parts_dir))
  testthat::expect_true(file.exists(file.path(parts_dir, "year_2024.rds")))
})

testthat::test_that("a pruned run rebuilds parts from the canonical and resumes incrementally", {
  root <- withr::local_tempdir()

  # Run 1: fresh pull of Id 1:3; parts pruned afterwards (default).
  n1 <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      n1 <<- n1 + 1L
      if (n1 == 1L) {
        data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
      } else {
        data.frame()
      }
    },
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )
  testthat::expect_false(dir.exists(file.path(root, ".parts", "raw_im")))

  # Run 2: POLIS now has a 4th row. The part rebuilt from the canonical seeds
  # last_id = 3, so the worker fetches ONLY Id 4 -- proving resume survives the
  # prune rather than re-pulling 1:3.
  seen_last <- NULL
  n2 <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 4,
    .polis_fetch_id_page = function(..., last_id = NULL) {
      n2 <<- n2 + 1L
      if (n2 == 1L) {
        seen_last <<- last_id
        data.frame(Id = 4L, PublishDate = "2024-06-16")
      } else {
        data.frame()
      }
    },
    .polis_fetch_id_list = function(...) 1:4,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )
  out <- readRDS(file.path(root, "raw_im.rds"))
  testthat::expect_setequal(out$Id, 1:4)
  # resumed from the rebuilt part's max(Id), not from scratch
  testthat::expect_equal(seen_last, 3)
})

testthat::test_that("the up-to-date skip prunes the cache by default", {
  root <- withr::local_tempdir()
  seed_parts(
    root,
    "raw_im",
    2024,
    data.frame(Id = 1:5, PublishDate = rep("2024-06-15", 5))
  )
  saveRDS(
    data.frame(Id = 1:5, PublishDate = rep("2024-06-15", 5)),
    file.path(root, "raw_im.rds")
  )
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 5,
    .polis_fetch_id_list = function(...) 1:5,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )
  testthat::expect_false(dir.exists(file.path(root, ".parts", "raw_im")))
  testthat::expect_equal(nrow(readRDS(file.path(root, "raw_im.rds"))), 5L)
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
  tryCatch(
    polished::get_polis_data(
      tables = "im",
      polis_folder = root,
      polis_api_key = key,
      workers = 1L,
      auto_refetch = FALSE,
      quiet = TRUE
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
  df <- readRDS(file.path(root, "raw_im.rds"))
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_gt(nrow(df), 1000L)
  testthat::expect_true("Id" %in% names(df))
  testthat::expect_false(any(duplicated(df$Id)))
})

testthat::test_that("get_polis_data fresh pull fetches, merges, verifies and refetches a missing id", {
  root <- withr::local_tempdir()
  page_calls <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      page_calls <<- page_calls + 1L
      if (page_calls == 1L) {
        data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
      } else {
        data.frame()
      }
    },
    .polis_fetch_id_list = function(...) 1:4, # POLIS reports one more than on disk
    .polis_refetch_missing = function(...) {
      data.frame(Id = 4, PublishDate = "2024-06-15")
    },
    .package = "polished"
  )
  res <- polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  )
  testthat::expect_null(res)
  df <- readRDS(file.path(root, "raw_im.rds"))
  testthat::expect_true(all(1:4 %in% df$Id)) # the missing id was refetched in
})

testthat::test_that("get_polis_data reports an up-to-date table (verbose) without re-fetching", {
  root <- withr::local_tempdir()
  seed_parts(
    root,
    "im",
    2024,
    data.frame(Id = 1:5, PublishDate = rep("2024-06-15", 5))
  )
  dir.create(file.path(root), recursive = TRUE, showWarnings = FALSE)
  saveRDS(
    data.frame(Id = 1:5, PublishDate = rep("2024-06-15", 5)),
    file.path(root, "raw_im.rds")
  )
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 5,
    .polis_fetch_id_list = function(...) 1:5,
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))
})

testthat::test_that("get_polis_data rebuilds a corrupt canonical from parts when up to date", {
  root <- withr::local_tempdir()
  seed_parts(
    root,
    "raw_im",
    2024,
    data.frame(Id = 1:5, PublishDate = rep("2024-06-15", 5))
  )
  # a canonical that cannot be read (corrupt / torn write)
  out_file <- file.path(root, "raw_im.rds")
  writeLines("not an rds file", out_file)
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 5,
    .polis_fetch_id_list = function(...) 1:5,
    .package = "polished"
  )
  polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = TRUE
  )
  # canonical was rebuilt from the intact parts and is readable again
  rebuilt <- readRDS(out_file)
  testthat::expect_equal(nrow(rebuilt), 5L)
})

testthat::test_that("get_polis_data tolerates a failed count query (unbounded progress)", {
  root <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) stop("count boom"),
    .polis_fetch_id_page = function(...) data.frame(),
    .polis_fetch_id_list = function(...) integer(0),
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))
})

testthat::test_that("get_polis_data force=TRUE clears both the canonical file and parts", {
  root <- withr::local_tempdir()
  dir.create(file.path(root), recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(root, "raw_im.rds")
  saveRDS(data.frame(Id = 1, PublishDate = "2024-06-15"), out_file)
  pf <- seed_parts(
    root,
    "raw_im",
    2024,
    data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3))
  )
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
    force = TRUE
  )
  testthat::expect_false(file.exists(pf))
  testthat::expect_false(file.exists(out_file))
})

testthat::test_that("get_polis_data verification passes cleanly when nothing is missing", {
  root <- withr::local_tempdir()
  n <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3)) else
        data.frame()
    },
    .polis_fetch_id_list = function(...) 1:3,
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))
})

testthat::test_that("get_polis_data warns and skips when the verification id-list fetch fails", {
  root <- withr::local_tempdir()
  n <- 0L
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = function(...) {
      n <<- n + 1L
      if (n == 1L) data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3)) else
        data.frame()
    },
    .polis_fetch_id_list = function(...) stop("list boom"),
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = root,
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))
})

testthat::test_that("get_polis_data warns when refetch fails, and when ids are still missing after it", {
  fetch_three <- local({
    function() {
      n <- 0L
      function(...) {
        n <<- n + 1L
        if (n == 1L)
          data.frame(Id = 1:3, PublishDate = rep("2024-06-15", 3)) else
          data.frame()
      }
    }
  })

  # refetch itself errors -> warned, nothing added
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = fetch_three(),
    .polis_fetch_id_list = function(...) 1:5,
    .polis_refetch_missing = function(...) stop("refetch boom"),
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = withr::local_tempdir(),
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))

  # refetch returns only some of the missing ids -> "still missing" warning
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 3,
    .polis_fetch_id_page = fetch_three(),
    .polis_fetch_id_list = function(...) 1:5,
    .polis_refetch_missing = function(...)
      data.frame(Id = 4, PublishDate = "2024-06-15"),
    .package = "polished"
  )
  testthat::expect_null(polished::get_polis_data(
    tables = "im",
    min_date = "2024-01-01",
    max_date = "2024-12-31",
    polis_folder = withr::local_tempdir(),
    polis_api_key = "dummy",
    workers = 1L,
    quiet = FALSE
  ))
})

testthat::test_that("get_polis_data aborts with a checkpoint when a year worker fails", {
  root <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .polis_get_count = function(...) 10,
    .polis_fetch_id_page = function(...) stop("page boom"),
    .package = "polished"
  )
  testthat::expect_error(
    polished::get_polis_data(
      tables = "im",
      min_date = "2024-01-01",
      max_date = "2024-12-31",
      polis_folder = root,
      polis_api_key = "dummy",
      workers = 1L,
      quiet = TRUE
    ),
    "fetch failed"
  )
})
