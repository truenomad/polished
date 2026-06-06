# Tests for the internal helpers — purely unit-level, no network.

test_that(".polis_check_format accepts the supported formats", {
  for (fmt in c("rds", "rda", "csv", "parquet", "qs2")) {
    expect_silent(polisapi2:::.polis_check_format(fmt))
  }
})

test_that(".polis_check_format aborts on unsupported formats", {
  expect_error(
    polisapi2:::.polis_check_format("xlsx"),
    "Unsupported"
  )
  expect_error(
    polisapi2:::.polis_check_format("json"),
    "Unsupported"
  )
})

test_that(".polis_pretty_num formats numbers with thousands separators", {
  expect_identical(polisapi2:::.polis_pretty_num(0), "0")
  expect_identical(polisapi2:::.polis_pretty_num(123), "123")
  expect_identical(polisapi2:::.polis_pretty_num(1234), "1,234")
  expect_identical(polisapi2:::.polis_pretty_num(1991076), "1,991,076")
  expect_identical(polisapi2:::.polis_pretty_num(NA), "0")
  expect_identical(polisapi2:::.polis_pretty_num(Inf), "0")
})

test_that(".polis_build_id_filter year-aligns the date bounds", {
  flt <- polisapi2:::.polis_build_id_filter(
    date_field = "LastUpdateDate",
    min_date = "2016-04-06",
    max_date = "2020-09-30",
    endpoint = "Case",
    region = "Global",
    country_code = NULL
  )
  expect_match(flt, "LastUpdateDate ge 2016-01-01", fixed = TRUE)
  expect_match(flt, "LastUpdateDate le 2020-12-31", fixed = TRUE)
})

test_that(".polis_build_id_filter adds region for non-Global", {
  flt <- polisapi2:::.polis_build_id_filter(
    date_field = "LastUpdateDate",
    min_date = "2020-01-01",
    max_date = "2020-12-31",
    endpoint = "Case",
    region = "AFRO",
    country_code = NULL
  )
  expect_match(flt, "WHORegion eq 'AFRO'", fixed = TRUE)

  # Virus uses RegionName, not WHORegion
  flt_v <- polisapi2:::.polis_build_id_filter(
    date_field = "UpdatedDate",
    min_date = "2020-01-01",
    max_date = "2020-12-31",
    endpoint = "Virus",
    region = "EMRO",
    country_code = NULL
  )
  expect_match(flt_v, "RegionName eq 'EMRO'", fixed = TRUE)

  # Global stays unfiltered
  flt_g <- polisapi2:::.polis_build_id_filter(
    date_field = "LastUpdateDate",
    min_date = "2020-01-01",
    max_date = "2020-12-31",
    endpoint = "Case",
    region = "Global",
    country_code = NULL
  )
  expect_false(grepl("WHORegion", flt_g))
})

test_that(".polis_build_id_filter adds country and Id-range clauses", {
  flt <- polisapi2:::.polis_build_id_filter(
    date_field = "LastUpdateDate",
    min_date = "2020-01-01",
    max_date = "2020-12-31",
    endpoint = "Case",
    region = "Global",
    country_code = "NGA",
    last_id = 12345
  )
  expect_match(flt, "CountryISO3Code eq 'NGA'", fixed = TRUE)
  expect_match(flt, "Id gt 12345", fixed = TRUE)
})

test_that(".polis_build_id_filter doesn't add region for Im or LabSpecimen", {
  for (ep in c("Im", "LabSpecimen")) {
    flt <- polisapi2:::.polis_build_id_filter(
      date_field = "PublishDate",
      min_date = "2020-01-01",
      max_date = "2020-12-31",
      endpoint = ep,
      region = "AFRO",
      country_code = NULL
    )
    expect_false(grepl("WHORegion|RegionName", flt))
  }
})

test_that(".polis_dedup keeps the row with the highest date per Id", {
  df <- data.frame(
    Id = c(1, 1, 2, 2, 3),
    LastUpdateDate = c(
      "2024-01-01",
      "2024-06-01",
      "2024-03-01",
      "2024-09-01",
      "2024-12-01"
    ),
    payload = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )
  out <- polisapi2:::.polis_dedup(
    df,
    id_col = "Id",
    date_col = "LastUpdateDate"
  )
  expect_equal(nrow(out), 3L)
  expect_setequal(out$Id, c(1, 2, 3))
  expect_identical(out$payload[out$Id == 1], "b")
  expect_identical(out$payload[out$Id == 2], "d")
  expect_identical(out$payload[out$Id == 3], "e")
})

test_that(".polis_dedup is a no-op on empty / missing-column input", {
  expect_identical(
    polisapi2:::.polis_dedup(data.frame(), date_col = "LastUpdateDate"),
    data.frame()
  )
  df_no_id <- data.frame(x = 1:3, LastUpdateDate = c("a", "b", "c"))
  expect_equal(
    nrow(polisapi2:::.polis_dedup(df_no_id, date_col = "LastUpdateDate")),
    3L
  )
})

test_that(".polis_io_read/write round-trips through rds", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  df <- data.frame(Id = 1:3, x = letters[1:3])
  polisapi2:::.polis_io_write(df, tmp, "rds")
  out <- polisapi2:::.polis_io_read(tmp, "rds")
  expect_equal(out, df)
})

test_that(".polis_io_read/write round-trips through rda", {
  tmp <- withr::local_tempfile(fileext = ".rda")
  df <- data.frame(Id = 1:3, x = letters[1:3])
  polisapi2:::.polis_io_write(df, tmp, "rda")
  out <- polisapi2:::.polis_io_read(tmp, "rda")
  expect_equal(out, df)
})

test_that(".polis_io_read/write round-trips through csv", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  df <- data.frame(Id = 1:3, x = c("a", "b", "c"), stringsAsFactors = FALSE)
  polisapi2:::.polis_io_write(df, tmp, "csv")
  out <- polisapi2:::.polis_io_read(tmp, "csv")
  expect_equal(out$Id, df$Id)
  expect_equal(out$x, df$x)
})

test_that(".polis_log_window appends rows to an rds log", {
  log_file <- withr::local_tempfile(fileext = ".rds")
  row1 <- data.frame(
    table = "im",
    cumulative_rows = 100,
    timestamp = Sys.time()
  )
  row2 <- data.frame(
    table = "im",
    cumulative_rows = 200,
    timestamp = Sys.time()
  )
  polisapi2:::.polis_log_window(log_file, row1)
  polisapi2:::.polis_log_window(log_file, row2)
  out <- readRDS(log_file)
  expect_equal(nrow(out), 2L)
  expect_equal(out$cumulative_rows, c(100, 200))
})

test_that(".polis_log_window is a no-op when log_file is NULL", {
  expect_silent(
    polisapi2:::.polis_log_window(
      NULL,
      data.frame(table = "x", cumulative_rows = 1)
    )
  )
})

test_that(".polis_archive copies into data/archive and prunes", {
  root <- withr::local_tempdir()
  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE)
  out_file <- file.path(data_dir, "case.rds")
  saveRDS(data.frame(Id = 1:3), out_file)

  for (i in seq_len(3L)) {
    polisapi2:::.polis_archive(
      out_file,
      root,
      "case",
      "rds",
      keep_n = 2L
    )
    Sys.sleep(1.01) # ensure distinct mtimes
  }
  arc_dir <- file.path(root, "data", "archive")
  arcs <- list.files(arc_dir, pattern = "^case_")
  expect_lte(length(arcs), 2L)
})

test_that(".polis_archive is a no-op when keep_n <= 0", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"), recursive = TRUE)
  out_file <- file.path(root, "data", "case.rds")
  saveRDS(data.frame(Id = 1:3), out_file)
  polisapi2:::.polis_archive(
    out_file,
    root,
    "case",
    "rds",
    keep_n = 0L
  )
  expect_false(dir.exists(file.path(root, "data", "archive")))
})

test_that(".polis_migrate_to_parts splits an existing file by year", {
  root <- withr::local_tempdir()
  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE)
  out_file <- file.path(data_dir, "case.rds")

  df <- data.frame(
    Id = 1:6,
    LastUpdateDate = c(
      "2018-01-01",
      "2018-06-15",
      "2019-01-01",
      "2019-12-31",
      "2020-05-05",
      "2020-11-30"
    ),
    stringsAsFactors = FALSE
  )
  saveRDS(df, out_file)

  parts_dir <- file.path(data_dir, ".parts", "case")
  polisapi2:::.polis_migrate_to_parts(
    out_file,
    parts_dir,
    "rds",
    "LastUpdateDate"
  )

  expect_true(dir.exists(parts_dir))
  # Migration writes both the part file and its meta sidecar.
  expect_setequal(
    list.files(parts_dir, pattern = "^year_\\d+\\.rds$"),
    c("year_2018.rds", "year_2019.rds", "year_2020.rds")
  )
  expect_setequal(
    list.files(parts_dir, pattern = "^year_\\d+\\.meta\\.rds$"),
    c("year_2018.meta.rds", "year_2019.meta.rds", "year_2020.meta.rds")
  )
  y2018 <- readRDS(file.path(parts_dir, "year_2018.rds"))
  expect_equal(nrow(y2018), 2L)
  expect_setequal(y2018$Id, c(1L, 2L))
})

test_that(".polis_compute_part_meta extracts shape + Id range + max date", {
  df <- data.frame(
    Id = c(10L, 30L, 20L, NA_integer_),
    LastUpdateDate = c("2024-01-01", "2024-06-15", "2024-03-01", NA),
    stringsAsFactors = FALSE
  )
  meta <- polisapi2:::.polis_compute_part_meta(df, "LastUpdateDate")
  expect_equal(meta$n_rows, 4L)
  expect_equal(meta$min_id, 10)
  expect_equal(meta$max_id, 30)
  expect_equal(meta$max_date, as.Date("2024-06-15"))
})

test_that(".polis_compute_part_meta handles empty input safely", {
  meta <- polisapi2:::.polis_compute_part_meta(
    data.frame(),
    "LastUpdateDate"
  )
  expect_equal(meta$n_rows, 0L)
  expect_true(is.na(meta$min_id))
  expect_true(is.na(meta$max_id))
})

test_that(".polis_io_write_part writes both part + meta sidecar", {
  tmp_dir <- withr::local_tempdir()
  part_file <- file.path(tmp_dir, "year_2024.rds")
  df <- data.frame(
    Id = 1:3,
    LastUpdateDate = c("2024-01-01", "2024-06-01", "2024-12-01"),
    stringsAsFactors = FALSE
  )
  polisapi2:::.polis_io_write_part(
    df,
    part_file,
    "rds",
    "LastUpdateDate"
  )
  meta_file <- polisapi2:::.polis_meta_path(part_file)
  expect_true(file.exists(part_file))
  expect_true(file.exists(meta_file))
  m <- readRDS(meta_file)
  expect_equal(m$n_rows, 3L)
  expect_equal(m$min_id, 1)
  expect_equal(m$max_id, 3)
})

test_that(".polis_read_meta backfills when sidecar is missing", {
  tmp_dir <- withr::local_tempdir()
  part_file <- file.path(tmp_dir, "year_2024.rds")
  meta_file <- polisapi2:::.polis_meta_path(part_file)
  saveRDS(
    data.frame(
      Id = 1:5,
      LastUpdateDate = rep("2024-06-15", 5),
      stringsAsFactors = FALSE
    ),
    part_file
  )
  expect_false(file.exists(meta_file))

  m <- polisapi2:::.polis_read_meta(part_file, "rds", "LastUpdateDate")
  expect_equal(m$n_rows, 5L)
  # Backfill should have created the sidecar.
  expect_true(file.exists(meta_file))
})

test_that(".polis_read_meta returns empty meta when nothing exists", {
  tmp_dir <- withr::local_tempdir()
  part_file <- file.path(tmp_dir, "year_2099.rds")
  m <- polisapi2:::.polis_read_meta(part_file, "rds", "LastUpdateDate")
  expect_equal(m$n_rows, 0L)
})

test_that(".polis_id_ranges_overlap detects pairwise overlap", {
  no_overlap <- list(
    list(min_id = 1, max_id = 100),
    list(min_id = 200, max_id = 300),
    list(min_id = 500, max_id = 800)
  )
  expect_false(polisapi2:::.polis_id_ranges_overlap(no_overlap))

  yes_overlap <- list(
    list(min_id = 1, max_id = 250),
    list(min_id = 200, max_id = 300)
  )
  expect_true(polisapi2:::.polis_id_ranges_overlap(yes_overlap))

  # Touching at a single Id counts as overlap.
  touching <- list(
    list(min_id = 1, max_id = 100),
    list(min_id = 100, max_id = 200)
  )
  expect_true(polisapi2:::.polis_id_ranges_overlap(touching))

  # Singleton list -> false (nothing to overlap with).
  expect_false(polisapi2:::.polis_id_ranges_overlap(
    list(list(min_id = 1, max_id = 5))
  ))
})

test_that(".polis_merge_parts skips dedup when ranges are disjoint", {
  root <- withr::local_tempdir()
  data_dir <- file.path(root, "data")
  parts_dir <- file.path(data_dir, ".parts", "case")
  dir.create(parts_dir, recursive = TRUE)

  # Write two disjoint year parts via the meta-aware writer.
  polisapi2:::.polis_io_write_part(
    data.frame(
      Id = 1:3,
      LastUpdateDate = "2020-01-01",
      stringsAsFactors = FALSE
    ),
    file.path(parts_dir, "year_2020.rds"),
    "rds",
    "LastUpdateDate"
  )
  polisapi2:::.polis_io_write_part(
    data.frame(
      Id = 4:6,
      LastUpdateDate = "2021-01-01",
      stringsAsFactors = FALSE
    ),
    file.path(parts_dir, "year_2021.rds"),
    "rds",
    "LastUpdateDate"
  )

  out_file <- file.path(data_dir, "case.rds")
  polisapi2:::.polis_merge_parts(
    parts_dir,
    out_file,
    "rds",
    "LastUpdateDate"
  )
  out <- readRDS(out_file)
  expect_equal(nrow(out), 6L)
  expect_setequal(out$Id, 1:6)
})

test_that(".polis_migrate_to_parts handles a corrupt file gracefully", {
  root <- withr::local_tempdir()
  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE)
  out_file <- file.path(data_dir, "case.rds")
  # Write garbage that readRDS won't understand
  writeLines("not an rds file", out_file)

  parts_dir <- file.path(data_dir, ".parts", "case")
  # cli::cli_alert_warning emits via `message()`, not `warning()`.
  expect_message(
    polisapi2:::.polis_migrate_to_parts(
      out_file,
      parts_dir,
      "rds",
      "LastUpdateDate"
    ),
    "unreadable"
  )
  expect_false(file.exists(out_file))
})

test_that(".polis_merge_parts binds parts and dedups by Id", {
  root <- withr::local_tempdir()
  data_dir <- file.path(root, "data")
  parts_dir <- file.path(data_dir, ".parts", "case")
  dir.create(parts_dir, recursive = TRUE)

  saveRDS(
    data.frame(Id = c(1L, 2L), LastUpdateDate = c("2020-01-01", "2020-02-01")),
    file.path(parts_dir, "year_2020.rds")
  )
  saveRDS(
    data.frame(Id = c(2L, 3L), LastUpdateDate = c("2021-01-01", "2021-02-01")),
    file.path(parts_dir, "year_2021.rds")
  )
  out_file <- file.path(data_dir, "case.rds")
  polisapi2:::.polis_merge_parts(
    parts_dir,
    out_file,
    "rds",
    "LastUpdateDate"
  )
  out <- readRDS(out_file)
  expect_equal(nrow(out), 3L)
  expect_setequal(out$Id, c(1L, 2L, 3L))
  # Dedup keeps the later update for Id == 2
  expect_identical(
    out$LastUpdateDate[out$Id == 2L],
    "2021-01-01"
  )
})
