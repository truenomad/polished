# Internal utility primitives in R/utils.R. Each test below is a single
# full characterisation of one helper (or a cohesive group): happy path,
# edge cases and error paths together, rather than one assertion apiece.
# Synthetic data only.

testthat::test_that("number + GUID formatters cover magnitudes, NA guards, brace forms", {
  # .polis_big_num: millions, thousands, bare units, and the non-scalar/NA guard
  testthat::expect_identical(polished:::.polis_big_num(1991076), "1.99M")
  testthat::expect_identical(polished:::.polis_big_num(142641), "142.6K")
  testthat::expect_identical(polished:::.polis_big_num(42), "42")
  testthat::expect_identical(polished:::.polis_big_num(NA), "0")
  testthat::expect_identical(polished:::.polis_big_num(c(1, 2)), "0")

  # .polis_pretty_num: thousands separators + NA guard
  testthat::expect_identical(polished:::.polis_pretty_num(1991076), "1,991,076")
  testthat::expect_identical(polished:::.polis_pretty_num(NA), "0")

  # .polis_pb_set: clamp to total, pass through under total / NA total
  testthat::expect_identical(polished:::.polis_pb_set(120, 100), 100L)
  testthat::expect_identical(polished:::.polis_pb_set(40, 100), 40L)
  testthat::expect_identical(polished:::.polis_pb_set(50, NA), 50)

  # GUID key/canon/display round-trips + NA/empty -> NA, and column reformat
  testthat::expect_identical(polished:::.geo_guid_key("{abc}"), "ABC")
  testthat::expect_identical(polished:::.geo_guid_canon("{ABC}"), "abc")
  testthat::expect_identical(polished:::.geo_guid_display("abc"), "{ABC}")
  testthat::expect_identical(
    polished:::.geo_guid_display(c(NA, "")),
    c(NA_character_, NA_character_)
  )
  reformatted <- polished:::.geo_guid_display_cols(
    data.frame(adm0_guid = "a0", other = "x", stringsAsFactors = FALSE)
  )
  testthat::expect_identical(reformatted$adm0_guid, "{A0}")
  testthat::expect_identical(reformatted$other, "x")
  no_guid <- data.frame(x = 1)
  testthat::expect_identical(
    polished:::.geo_guid_display_cols(no_guid),
    no_guid
  )
})

testthat::test_that("format-dispatched I/O round-trips rds/csv/rda, checks formats, requires pkgs", {
  testthat::expect_no_error(polished:::.polis_check_format("rds"))
  testthat::expect_error(polished:::.polis_check_format("xml"), "Unsupported")

  dir <- withr::local_tempdir()
  df <- data.frame(Id = 1:3, v = c("a", "b", "c"), stringsAsFactors = FALSE)
  for (fmt in c("rds", "csv", "rda")) {
    path <- file.path(dir, paste0("t.", fmt))
    polished:::.polis_io_write(df, path, fmt)
    back <- polished:::.polis_io_read(path, fmt)
    testthat::expect_equal(back$Id, df$Id, info = fmt)
    testthat::expect_equal(as.character(back$v), df$v, info = fmt)
  }

  testthat::expect_true(isNamespace(polished:::.polis_require("stats", "x")))
  testthat::expect_error(
    polished:::.polis_require("not_a_real_pkg_9xyz", "do the thing"),
    "is needed"
  )
})

testthat::test_that("part-meta sidecars compute, backfill, and detect id overlaps", {
  testthat::expect_identical(
    polished:::.polis_meta_path("a/year_2024.rds"),
    "a/year_2024.meta.rds"
  )

  df <- data.frame(
    Id = c(3, 1, 2),
    LastUpdateDate = c("2024-03-01", "2024-01-01", "2024-02-01"),
    stringsAsFactors = FALSE
  )
  meta <- polished:::.polis_compute_part_meta(df, date_field = "LastUpdateDate")
  testthat::expect_identical(meta$n_rows, 3L)
  testthat::expect_identical(meta$min_id, 1)
  testthat::expect_identical(meta$max_id, 3)
  testthat::expect_identical(meta$max_date, as.Date("2024-03-01"))

  empty <- polished:::.polis_compute_part_meta(df[0, ], date_field = NULL)
  testthat::expect_identical(empty$n_rows, 0L)
  testthat::expect_true(is.na(empty$min_id))
  testthat::expect_identical(polished:::.polis_empty_meta()$n_rows, 0L)

  # .polis_read_meta: lazy backfill writes the sidecar; both absent -> empty
  dir <- withr::local_tempdir()
  part <- file.path(dir, "year_2024.rds")
  saveRDS(df, part)
  read_back <- polished:::.polis_read_meta(part, "rds", "LastUpdateDate")
  testthat::expect_identical(read_back$n_rows, 3L)
  testthat::expect_true(file.exists(polished:::.polis_meta_path(part)))
  testthat::expect_identical(
    polished:::.polis_read_meta(
      file.path(dir, "year_1999.rds"),
      "rds",
      "LastUpdateDate"
    )$n_rows,
    0L
  )

  # .polis_id_ranges_overlap: overlap, touching, disjoint, single, all-NA
  mk <- function(lo, hi) list(min_id = lo, max_id = hi)
  testthat::expect_true(
    polished:::.polis_id_ranges_overlap(list(mk(1, 5), mk(4, 9)))
  )
  testthat::expect_true(
    polished:::.polis_id_ranges_overlap(list(mk(1, 5), mk(5, 9)))
  )
  testthat::expect_false(
    polished:::.polis_id_ranges_overlap(list(mk(1, 5), mk(6, 9)))
  )
  testthat::expect_false(polished:::.polis_id_ranges_overlap(list(mk(1, 5))))
  testthat::expect_false(
    polished:::.polis_id_ranges_overlap(list(mk(NA, NA), mk(1, 5)))
  )
})

testthat::test_that(".polis_build_id_filter aligns years and toggles region/country/id clauses", {
  # Case endpoint: year-aligned bounds + WHORegion + country
  flt <- polished:::.polis_build_id_filter(
    "CaseDate",
    "2020-03-01",
    "2021-05-15",
    "Case",
    "AFRO",
    "NGA"
  )
  testthat::expect_match(flt, "CaseDate ge 2020-01-01")
  testthat::expect_match(flt, "CaseDate le 2021-12-31")
  testthat::expect_match(flt, "WHORegion eq 'AFRO'")
  testthat::expect_match(flt, "CountryISO3Code eq 'NGA'")

  # Virus uses RegionName; empty country code drops the country clause
  virus <- polished:::.polis_build_id_filter(
    "DateOfOnset",
    "2022-01-01",
    "2022-12-31",
    "Virus",
    "AFRO",
    ""
  )
  testthat::expect_match(virus, "RegionName eq 'AFRO'")
  testthat::expect_no_match(virus, "CountryISO3Code")

  # Region omitted for global, and for the Im / LabSpecimen endpoints
  testthat::expect_no_match(
    polished:::.polis_build_id_filter(
      "CaseDate",
      "2022-01-01",
      "2022-12-31",
      "Case",
      "Global",
      ""
    ),
    "WHORegion"
  )
  testthat::expect_no_match(
    polished:::.polis_build_id_filter(
      "PublishDate",
      "2022-01-01",
      "2022-12-31",
      "Im",
      "AFRO",
      ""
    ),
    "WHORegion"
  )

  # last_id appends the keyset clause
  testthat::expect_match(
    polished:::.polis_build_id_filter(
      "CaseDate",
      "2022-01-01",
      "2022-12-31",
      "Case",
      "Global",
      "",
      last_id = 500
    ),
    "Id gt 500$"
  )
})

testthat::test_that(".polis_dedup keeps latest per id, plus archive/log housekeeping", {
  # keep-latest by date per Id
  df <- data.frame(
    Id = c(1, 1, 2),
    d = c("2024-01-01", "2024-03-01", "2024-02-01"),
    v = c("old", "new", "x"),
    stringsAsFactors = FALSE
  )
  out <- polished:::.polis_dedup(df, id_col = "Id", date_col = "d")
  testthat::expect_identical(nrow(out), 2L)
  testthat::expect_identical(out$v[out$Id == 1], "new")

  # no Id column -> drop fully identical rows
  no_id <- data.frame(
    a = c(1, 1, 2),
    b = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )
  testthat::expect_identical(
    nrow(polished:::.polis_dedup(no_id, id_col = "Id", date_col = "missing")),
    2L
  )

  # .polis_log_window appends rows; NULL log file is a silent no-op
  log_file <- file.path(withr::local_tempdir(), "log.rds")
  polished:::.polis_log_window(log_file, data.frame(a = 1))
  polished:::.polis_log_window(log_file, data.frame(a = 2))
  testthat::expect_identical(nrow(readRDS(log_file)), 2L)
  testthat::expect_invisible(
    polished:::.polis_log_window(NULL, data.frame(a = 3))
  )

  # .polis_archive copies a timestamped backup; keep_n = 0 is a no-op
  root <- withr::local_tempdir()
  out_file <- file.path(root, "im.rds")
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(a = 1), out_file)
  polished:::.polis_archive(out_file, root, "im", "rds", keep_n = 2L)
  testthat::expect_identical(
    length(list.files(file.path(root, "archive"))),
    1L
  )

  root2 <- withr::local_tempdir()
  out2 <- file.path(root2, "im.rds")
  dir.create(dirname(out2), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(a = 1), out2)
  polished:::.polis_archive(out2, root2, "im", "rds", keep_n = 0L)
  testthat::expect_false(dir.exists(file.path(root2, "archive")))
})

testthat::test_that("utils misc: extdata abort, write_outputs, archive prune, no-id meta, merge dedup", {
  testthat::expect_error(
    polished:::.polis_extdata_path("definitely_not_here.csv"),
    "Could not locate"
  )

  # write a named list of cleaned tables to disk as polished_*, format per source
  dir <- withr::local_tempdir()
  polished:::.polis_write_outputs(
    list(afp = data.frame(a = 1), es = data.frame(b = 2)),
    dir,
    formats = list(afp = "rds", es = "rds")
  )
  testthat::expect_true(all(file.exists(file.path(
    dir,
    c("polished_afp.rds", "polished_es.rds")
  ))))

  # archive copies the canonical file and prunes older copies beyond keep_n
  root <- withr::local_tempdir()
  arc <- file.path(root, "archive")
  dir.create(arc, recursive = TRUE)
  out_file <- file.path(root, "im.rds")
  saveRDS(data.frame(Id = 1), out_file)
  file.create(file.path(
    arc,
    c("im_20240101_000000.rds", "im_20240102_000000.rds")
  ))
  polished:::.polis_archive(out_file, root, "im", "rds", keep_n = 1L)
  testthat::expect_equal(length(list.files(arc, pattern = "^im_")), 1L)

  # part-meta with no Id column -> NA id bounds
  m <- polished:::.polis_compute_part_meta(
    data.frame(x = 1:2),
    date_field = NULL
  )
  testthat::expect_true(is.na(m$min_id) && is.na(m$max_id))
  testthat::expect_identical(m$n_rows, 2L)

  # merge dedups overlapping ids across parts, keeping the latest by date
  parts <- file.path(withr::local_tempdir(), ".parts")
  dir.create(parts)
  saveRDS(
    data.frame(Id = c(1, 2), PublishDate = c("2024-01-01", "2024-01-01")),
    file.path(parts, "year_2023.rds")
  )
  saveRDS(
    data.frame(Id = c(2, 3), PublishDate = c("2024-06-01", "2024-06-01")),
    file.path(parts, "year_2024.rds")
  )
  merged_file <- file.path(withr::local_tempdir(), "m.rds")
  n <- polished:::.polis_merge_parts(parts, merged_file, "rds", "PublishDate")
  testthat::expect_equal(n, 3L)
  out <- readRDS(merged_file)
  testthat::expect_equal(out$PublishDate[out$Id == 2], "2024-06-01")
})

testthat::test_that(".polis_polio_type uses the fallback column; .polis_dedup no-ops on empty", {
  pt <- polished:::.polis_polio_type(
    data.frame(polio_virus_types = "WILD1", stringsAsFactors = FALSE)
  )
  testthat::expect_identical(pt$polio_type, "Type 1")
  testthat::expect_identical(nrow(polished:::.polis_dedup(data.frame())), 0L)
})

testthat::test_that("legacy-name migration renames canonical, parts and archives", {
  dir <- withr::local_tempdir()
  # no-op when the stem already matches the table name
  testthat::expect_invisible(
    polished:::.polis_migrate_legacy_names(dir, "raw_im", "raw_im", "rds")
  )
  # seed an old-convention canonical file, parts dir and archive copy
  saveRDS(data.frame(a = 1), file.path(dir, "case.rds"))
  dir.create(file.path(dir, ".parts", "case"), recursive = TRUE)
  writeLines("x", file.path(dir, ".parts", "case", "year_2024.rds"))
  dir.create(file.path(dir, "archive"))
  saveRDS(
    data.frame(a = 1),
    file.path(dir, "archive", "case_20240101_120000.rds")
  )

  suppressMessages(
    polished:::.polis_migrate_legacy_names(dir, "case", "raw_afp", "rds")
  )
  testthat::expect_true(file.exists(file.path(dir, "raw_afp.rds")))
  testthat::expect_false(file.exists(file.path(dir, "case.rds")))
  testthat::expect_true(dir.exists(file.path(dir, ".parts", "raw_afp")))
  testthat::expect_true(file.exists(
    file.path(dir, "archive", "raw_afp_20240101_120000.rds")
  ))
})

testthat::test_that("io helpers round-trip qs2, reject unknown formats; resolve_ref reads paths", {
  dir <- withr::local_tempdir()
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)

  # csv round-trip
  csv_path <- file.path(dir, "t.csv")
  polished:::.polis_write(df, csv_path)
  testthat::expect_equal(nrow(polished:::.polis_read(csv_path)), 2L)

  # qs2 round-trip (optional dep)
  if (requireNamespace("qs2", quietly = TRUE)) {
    qs_path <- file.path(dir, "t.qs2")
    polished:::.polis_write(df, qs_path)
    testthat::expect_equal(nrow(polished:::.polis_read(qs_path)), 2L)
  }

  # unsupported extension aborts on both read and write
  testthat::expect_error(
    polished:::.polis_write(df, file.path(dir, "t.json")),
    "Unsupported file type"
  )
  writeLines("x", file.path(dir, "t.json"))
  testthat::expect_error(
    polished:::.polis_read(file.path(dir, "t.json")),
    "Unsupported file type"
  )

  # .polis_resolve_ref: path -> read, missing -> abort, non-path -> passthrough
  ref_path <- file.path(dir, "ref.rds")
  saveRDS(df, ref_path)
  testthat::expect_s3_class(
    polished:::.polis_resolve_ref(ref_path),
    "data.frame"
  )
  testthat::expect_error(
    polished:::.polis_resolve_ref(file.path(dir, "missing.rds")),
    "does not exist"
  )
  testthat::expect_null(polished:::.polis_resolve_ref(NULL))
  testthat::expect_s3_class(polished:::.polis_resolve_ref(df), "data.frame")
})

testthat::test_that(".polis_write_outputs expands a list value to one file per frame", {
  dir <- withr::local_tempdir()
  cleaned <- list(
    afp = data.frame(a = 1),
    indicators = list(adm0 = data.frame(b = 1), meta = list(x = 1))
  )
  polished:::.polis_write_outputs(
    cleaned,
    dir,
    formats = list(afp = "rds"),
    default_format = "rds"
  )
  testthat::expect_true(file.exists(file.path(dir, "polished_afp.rds")))
  # data-frame component written, non-frame (meta list) skipped
  testthat::expect_true(file.exists(file.path(
    dir,
    "polished_indicators_adm0.rds"
  )))
  testthat::expect_false(file.exists(file.path(
    dir,
    "polished_indicators_meta.rds"
  )))
})

testthat::test_that("excel helpers: utf8, sheet names, prepare, widths", {
  # .polis_to_utf8: factor coerced; bytes that don't decode in the locale
  # (encoding left "unknown") fall back through latin1 to valid UTF-8;
  # non-data.frame passes through
  raw_bytes <- "caf\xe9" # invalid UTF-8, undeclared encoding
  u <- polished:::.polis_to_utf8(
    data.frame(f = factor("a"), c = raw_bytes, n = 1, stringsAsFactors = FALSE)
  )
  testthat::expect_true(is.character(u$f))
  testthat::expect_true(validUTF8(u$c))
  testthat::expect_identical(polished:::.polis_to_utf8(1:3), 1:3)

  # .polis_excel_sheet_names: blank/NA -> Sheet, illegal stripped, trunc + dedupe
  nm <- polished:::.polis_excel_sheet_names(
    c("", NA, "bad:name*", paste0(strrep("x", 40), c("a", "b")))
  )
  testthat::expect_true(all(nchar(nm) <= 31L))
  testthat::expect_equal(length(unique(nm)), length(nm))
  testthat::expect_false(any(grepl("[:*]", nm)))

  # .polis_prepare_for_excel: data.frame path (dates/list-cols stringified,
  # duplicate names made unique)
  df <- data.frame(x = 1, check.names = FALSE)
  df$d <- as.Date("2024-01-01")
  df$lst <- I(list(1:2))
  names(df) <- c("z", "z", "z")
  prepped <- polished:::.polis_prepare_for_excel(df)
  testthat::expect_true(is.character(prepped[[2]])) # date -> character
  testthat::expect_equal(length(unique(names(prepped))), 3L)
  # list input keeps only data frames and sanitises names
  lst <- polished:::.polis_prepare_for_excel(
    list(a = data.frame(x = 1), not_df = 1L)
  )
  testthat::expect_named(lst, "a")
  # non-df, non-list coercible input -> coerced
  testthat::expect_s3_class(
    polished:::.polis_prepare_for_excel(matrix(1:4, 2)),
    "data.frame"
  )
  # non-coercible input (a function) -> returned unchanged
  testthat::expect_identical(polished:::.polis_prepare_for_excel(sum), sum)

  # .polis_excel_col_width: clamps to [12, 60]; no candidates -> 12
  testthat::expect_equal(
    polished:::.polis_excel_col_width(integer(0), character(0)),
    12
  )
  testthat::expect_equal(
    polished:::.polis_excel_col_width(strrep("y", 80), "h"),
    60
  )
  # .polis_is_integerish: empty TRUE, integers TRUE, decimals FALSE
  testthat::expect_true(polished:::.polis_is_integerish(numeric(0)))
  testthat::expect_true(polished:::.polis_is_integerish(c(1, 2, NA)))
  testthat::expect_false(polished:::.polis_is_integerish(c(1.5, 2)))
})

testthat::test_that("formatted xlsx writer styles every column type", {
  testthat::skip_if_not_installed("openxlsx")
  dir <- withr::local_tempdir()

  data <- data.frame(
    name = c("a", "b", "c"),
    count = c(1L, 2L, 3L), # integer format
    rate = c(1.5, 2.5, 3.5), # decimal format
    pct_pos = c(50, 60, 70), # percent, > 1 -> rescaled
    year_onset = c(2024L, 2024L, 2024L), # "year" -> left unformatted
    stringsAsFactors = FALSE
  )
  # an sf sheet (geometry dropped), an empty-rows sheet, and duplicate long names
  sf_sheet <- sf::st_sf(
    a = 1L,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)))
  )
  long <- strrep("s", 40)
  sheets <- stats::setNames(
    list(data, sf_sheet, data[0, ]),
    c(long, long, "empty")
  )

  path <- file.path(dir, "wb.xlsx")
  testthat::expect_identical(polished:::.polis_write_xlsx(sheets, path), path)
  testthat::expect_true(file.exists(path))
  got <- openxlsx::getSheetNames(path)
  testthat::expect_equal(length(got), 3L)
  testthat::expect_true(all(nchar(got) <= 31L))

  # data.frame input writes a single "Data" sheet
  path2 <- file.path(dir, "single.xlsx")
  polished:::.polis_write_xlsx(data, path2)
  testthat::expect_identical(openxlsx::getSheetNames(path2), "Data")

  # direct writer covers the structural edges prepare() would otherwise smooth
  # over: a non-data.frame sheet, a zero-column sheet, and a column-free-numeric
  # (character-only) sheet
  path3 <- file.path(dir, "edges.xlsx")
  edges <- list(
    chars = data.frame(a = c("x", "y"), stringsAsFactors = FALSE),
    no_cols = data.frame(),
    mat = matrix(1:4, 2)
  )
  polished:::.polis_write_excel_formatted(edges, path3)
  testthat::expect_true(file.exists(path3))
})
