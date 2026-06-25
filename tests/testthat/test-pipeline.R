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

raw_afp_positive <- function() {
  data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    DateOnset = c("2024-01-02", "2024-02-03"),
    NotificationDate = c("2024-01-09", "2024-02-10"),
    PolioVirusTypes = c("WILD1", NA),
    Classification = c("Confirmed (wild)", "Discarded"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
}

testthat::test_that("run_pipeline cleans a single dataset", {
  out <- suppressMessages(polished::run_pipeline(list(afp = raw_afp())))
  testthat::expect_named(out, "afp")
  testthat::expect_equal(nrow(out$afp), 2L)
})

testthat::test_that("run_pipeline builds virus positives from cleaned cases", {
  out <- suppressMessages(
    polished::run_pipeline(list(afp = raw_afp_positive()))
  )
  testthat::expect_setequal(names(out), c("afp", "virus"))
  # only the WPV case becomes a positive; it is tagged human
  testthat::expect_equal(nrow(out$virus), 1L)
  testthat::expect_equal(out$virus$measurement, "WPV 1")
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
  saveRDS(raw_afp(), file.path(src, "raw_afp.rds"))

  cleaned <- suppressMessages(
    polished::run_pipeline_dir(src, out_dir)
  )
  testthat::expect_true("afp" %in% names(cleaned))
  # output is polished_<key> with the format of its raw source (rds here),
  # written into the configured data/ sub-directory of output_dir
  testthat::expect_true(
    file.exists(file.path(out_dir, "data", "polished_afp.rds"))
  )
})

testthat::test_that("polis_config validates its scalar args and prints a summary", {
  testthat::expect_error(
    polished::polis_config(start_year = "x"),
    "single number"
  )
  testthat::expect_error(
    polished::polis_config(parse_types = "x"),
    "single logical"
  )
  testthat::expect_error(
    polished::polis_config(drop_empty_cols = 1),
    "single logical"
  )
  cfg <- polished::polis_config()
  testthat::expect_s3_class(cfg, "polis_config")
  testthat::expect_invisible(print(cfg))
})

testthat::test_that("run_pipeline scopes outputs by start_year and regions", {
  # two cases in different years (2018, 2024) and regions (AFRO, EMRO); the
  # cleaner derives year_onset + who_region, on which scoping then keys.
  afp <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-03-01", "2019-02-01"),
    ParalysisOnsetDate = c("2024-01-02", "2018-02-03"),
    Admin0Name = c("NIGERIA", "CHAD"),
    WHORegion = c("EMRO", "AFRO"),
    check.names = FALSE
  )

  # default start_year (2020) drops the 2018 case; lowering it keeps both
  default_run <- suppressMessages(polished::run_pipeline(list(afp = afp)))
  testthat::expect_equal(nrow(default_run$afp), 1L)
  testthat::expect_equal(default_run$afp$year_onset, 2024)

  wide_run <- suppressMessages(
    polished::run_pipeline(
      list(afp = afp),
      polished::polis_config(start_year = 2017)
    )
  )
  testthat::expect_equal(nrow(wide_run$afp), 2L)

  # region scope keeps only the EMRO case; the default six-region set is a no-op
  emro_run <- suppressMessages(
    polished::run_pipeline(
      list(afp = afp),
      polished::polis_config(regions = "EMRO")
    )
  )
  testthat::expect_equal(emro_run$afp$who_region, "EMRO")
})

testthat::test_that("run_pipeline_dir reads a directory and writes the data/ layout", {
  src <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  saveRDS(raw_afp(), file.path(src, "raw_afp.rds"))

  suppressMessages(
    polished::run_pipeline_dir(src, out_dir, polished::polis_config())
  )
  # output inherits the source format (rds) and lands in data/
  testthat::expect_true(
    file.exists(file.path(out_dir, "data", "polished_afp.rds"))
  )
})

testthat::test_that("run_pipeline output_dir writes data + checks sub-directories", {
  out_dir <- withr::local_tempdir()
  # explicit default config so the session-active config can't leak folder
  # overrides from an earlier test into this run
  suppressMessages(
    polished::run_pipeline(
      list(afp = raw_afp()),
      cfg = polished::polis_config(),
      output_dir = out_dir
    )
  )
  # data files in data/, check workbooks in checks/ (when openxlsx is present)
  testthat::expect_true(file.exists(file.path(
    out_dir,
    "data",
    "polished_afp.qs2"
  )))
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    testthat::expect_true(
      file.exists(file.path(out_dir, "checks", "checks_afp.xlsx"))
    )
  }
})

testthat::test_that("run_pipeline does not rewrite unchanged output files", {
  raw_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  saveRDS(raw_afp(), file.path(raw_dir, "raw_afp.rds"))

  cfg <- polished::polis_config(inputs = raw_dir, cache_dir = cache_dir)
  suppressMessages(polished::run_pipeline(cfg = cfg, output_dir = out_dir))
  afp_out <- file.path(out_dir, "data", "polished_afp.rds")
  testthat::expect_true(file.exists(afp_out))
  mtime_before <- file.mtime(afp_out)

  # an unchanged re-run leaves the identical output file untouched
  suppressMessages(polished::run_pipeline(cfg = cfg, output_dir = out_dir))
  testthat::expect_identical(file.mtime(afp_out), mtime_before)
})

testthat::test_that("cfg$cache_dir threads the SIA cache through run_pipeline", {
  cache_dir <- withr::local_tempdir()
  inputs <- list(
    activity = data.frame(
      Id = 1L,
      Admin0Name = "NIGERIA",
      ActivityDateFrom = "2024-01-05",
      check.names = FALSE
    )
  )
  cfg <- polished::polis_config(cache_dir = cache_dir)
  suppressMessages(polished::run_pipeline(inputs, cfg))
  # a cleaned-SIA cache file is materialised in the configured cache dir
  testthat::expect_true(
    length(list.files(cache_dir, pattern = "^clean_sia_.*\\.qs2$")) >= 1L
  )
})

testthat::test_that("region scope reaches the IM/LQAS roll-ups via who_region attached from surveillance", {
  afp <- data.frame(
    Id = 1:2,
    Epid = c("A-1", "B-2"),
    LastUpdateDate = "2024-03-01",
    ParalysisOnsetDate = "2024-01-02",
    Admin0Name = c("AFGHANISTAN", "ALGERIA"),
    CountryISO3Code = c("AFG", "DZA"),
    WHORegion = c("EMRO", "AFRO"),
    check.names = FALSE
  )
  # IM carries adm0 (Title case) but no who_region; the roll-up should inherit it
  im <- data.frame(
    Id = 1:2,
    Admin0 = c("Afghanistan", "Algeria"),
    Admin1 = "x",
    Admin2 = c("d1", "d2"),
    Admin2GUID = c("{a}", "{b}"),
    ActivityPlannedDateFromYear = 2024L,
    HouseholdsNumberChildrenChecked = 20L,
    HouseholdsNumberChildrenMarked = 18L,
    HouseholdsResult = NA_real_,
    OutOfHouseNumberChildrenChecked = 0L,
    OutOfHouseNumberChildrenMarked = 0L,
    OutOfHouseResult = NA_real_,
    check.names = FALSE
  )
  out <- suppressMessages(
    polished::run_pipeline(
      list(afp = afp, im = im),
      cfg = polished::polis_config(regions = "EMRO")
    )
  )
  # AFRO (Algeria) dropped, EMRO (Afghanistan) kept, region column attached
  testthat::expect_equal(out$im$district$adm0, "Afghanistan")
  testthat::expect_equal(unique(out$im$district$who_region), "EMRO")
})

testthat::test_that("run_pipeline reuses the clean cache when the source is unchanged", {
  raw_dir <- withr::local_tempdir()
  cache_dir <- withr::local_tempdir()
  saveRDS(raw_afp(), file.path(raw_dir, "raw_afp.rds"))

  cfg <- polished::polis_config(inputs = raw_dir, cache_dir = cache_dir)
  first <- suppressMessages(polished::run_pipeline(cfg = cfg))
  cache_file <- list.files(
    cache_dir,
    pattern = "^clean_afp_.*\\.qs2$",
    full.names = TRUE
  )
  testthat::expect_length(cache_file, 1L)
  # the derived virus step is cached too (not just the cleaners)
  virus_cache <- list.files(
    cache_dir,
    pattern = "^clean_virus_.*\\.qs2$",
    full.names = TRUE
  )
  testthat::expect_length(virus_cache, 1L)
  mtime_before <- file.mtime(c(cache_file, virus_cache))

  # a second run hits the caches: it reads the files rather than recomputing, so
  # they are never rewritten (mtime unchanged) and the result matches
  second <- suppressMessages(polished::run_pipeline(cfg = cfg))
  testthat::expect_identical(
    file.mtime(c(cache_file, virus_cache)),
    mtime_before
  )
  testthat::expect_equal(second$afp, first$afp)

  # refresh = TRUE ignores the cache and rewrites it (mtime advances)
  Sys.sleep(1.1)
  suppressMessages(polished::run_pipeline(cfg = cfg, refresh = TRUE))
  testthat::expect_false(
    any(file.mtime(c(cache_file, virus_cache)) == mtime_before)
  )
})

testthat::test_that(".polis_backfill_guids fills blanks by admin name then shape id", {
  # afp carries the GUID; sia has the same district but a blank GUID -- once by a
  # matching name, once only by a matching shape id (name misspelled)
  afp <- data.frame(
    adm0 = "AFGHANISTAN",
    adm1 = "BADAKHSHAN",
    adm2 = "ZEBAK",
    adm1_guid = "{A1}",
    adm2_guid = "{A2}",
    admin1shape_id = "s1",
    admin2shape_id = "s2",
    stringsAsFactors = FALSE
  )
  sia <- data.frame(
    adm0 = "AFGHANISTAN",
    adm1 = "BADAKHSHAN",
    adm2 = c("ZEBAK", "ZEBAK MISSPELLED"),
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    admin1shape_id = "s1",
    admin2shape_id = "s2",
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(
    polished:::.polis_backfill_guids(list(afp = afp, sia = sia))
  )
  testthat::expect_equal(out$sia$adm2_guid, c("{A2}", "{A2}")) # name, then shape
  testthat::expect_equal(out$sia$adm1_guid, c("{A1}", "{A1}"))
  testthat::expect_equal(out$afp$adm2_guid, "{A2}") # existing GUID untouched
})

testthat::test_that(".polis_lazy_ref defers the read and memoises it", {
  # constructing the loader does not read; a missing path errors only on call
  absent <- polished:::.polis_lazy_ref(
    file.path(withr::local_tempdir(), "absent.rds")
  )
  testthat::expect_error(absent(), "does not exist")

  # first call reads; the file can then vanish and the value is still returned
  p <- withr::local_tempfile(fileext = ".rds")
  saveRDS(1:3, p)
  load_once <- polished:::.polis_lazy_ref(p)
  v1 <- load_once()
  unlink(p)
  testthat::expect_identical(load_once(), v1)
})

testthat::test_that("load_polished reads and filters the written outputs by country", {
  out_dir <- withr::local_tempdir()
  afp <- raw_afp()
  afp$CountryISO3Code <- c("NGA", "NGA", "TCD")
  suppressMessages(
    polished::run_pipeline(
      list(afp = afp),
      cfg = polished::polis_config(),
      output_dir = out_dir
    )
  )
  slice <- polished::load_polished(country = "TCD", output_dir = out_dir)
  testthat::expect_named(slice, "afp")
  testthat::expect_true(all(slice$afp$country_iso3code == "TCD"))
})

testthat::test_that("load_polished resolves an ISO3 country filter to name-keyed roll-ups", {
  out_dir <- withr::local_tempdir()
  d <- file.path(out_dir, "data")
  dir.create(d, recursive = TRUE)
  # afp carries ISO3 + adm0 name; im_district is name-keyed only (no ISO3)
  polished:::.polis_write(
    data.frame(
      country_iso3code = c("AFG", "DZA"),
      adm0 = c("AFGHANISTAN", "ALGERIA"),
      year_onset = 2024L
    ),
    file.path(d, "polished_afp.qs2")
  )
  polished:::.polis_write(
    # adm0 in a different case than the afp stream -- the match must be
    # case-insensitive or this would silently come back empty
    data.frame(
      adm0 = c("Afghanistan", "Algeria", "Angola"),
      adm2_guid = c("{a}", "{b}", "{c}"),
      year = 2024L
    ),
    file.path(d, "polished_im_district.qs2")
  )

  afg <- polished::load_polished(country = "AFG", output_dir = out_dir)
  testthat::expect_equal(afg$afp$country_iso3code, "AFG")
  # name-keyed roll-up resolved AFG -> AFGHANISTAN, matched case-insensitively
  testthat::expect_equal(afg$im_district$adm0, "Afghanistan")

  # even when only the name-keyed dataset is requested, it reads afp for the map
  only_im <- polished::load_polished(
    country = "AFG",
    datasets = "im_district",
    output_dir = out_dir
  )
  testthat::expect_equal(only_im$im_district$adm0, "Afghanistan")
})

testthat::test_that(".polis_attach_indicator_iso3 keys guid-only indicators by country", {
  cleaned <- list(
    afp = tibble::tibble(
      country_iso3code = c("AFG", "PAK"),
      adm0_guid = c("{C-AFG}", "{C-PAK}"),
      adm1_guid = c("{P-AFG}", "{P-PAK}"),
      adm2_guid = c("{D-AFG}", "{D-PAK}")
    )
  )
  indicators <- list(
    adm0 = tibble::tibble(
      guid = c("{C-AFG}", "{C-PAK}"),
      name = c("A", "P"),
      value = 1:2
    ),
    long = tibble::tibble(level = "adm2", guid = "{D-PAK}", value = 9),
    meta = list(levels = "adm0")
  )
  out <- polished:::.polis_attach_indicator_iso3(indicators, cleaned)
  testthat::expect_equal(out$adm0$country_iso3code, c("AFG", "PAK"))
  testthat::expect_identical(names(out$adm0)[[1]], "country_iso3code")
  testthat::expect_equal(out$long$country_iso3code, "PAK")
  # meta (a non-data-frame component) passes through untouched
  testthat::expect_identical(out$meta, indicators$meta)
})

testthat::test_that("country filter skips an adm0-name-only table instead of emptying it", {
  # adm0 holds the country NAME, never an ISO3, so it must not be a filter key.
  testthat::expect_false("adm0" %in% polished:::.polis_country_cols)
  name_only <- tibble::tibble(adm0 = c("AFGHANISTAN", "PAKISTAN"), x = 1:2)
  kept <- polished:::.polis_filter_dataset(
    name_only,
    "AFG",
    polished:::.polis_country_cols
  )
  testthat::expect_identical(nrow(kept), 2L)
  iso_keyed <- tibble::tibble(country_iso3code = c("AFG", "PAK"), x = 1:2)
  afg <- polished:::.polis_filter_dataset(
    iso_keyed,
    "AFG",
    polished:::.polis_country_cols
  )
  testthat::expect_identical(afg$country_iso3code, "AFG")
})

testthat::test_that("run_pipeline cleans es + sia and reconciles against a full pull", {
  inputs <- list(
    es = data.frame(
      Id = 1:2,
      SampleId = c("E1", "E2"),
      LastUpdateDate = rep("2024-03-01", 2),
      CollectionDate = rep("2024-01-05", 2),
      Admin0Name = rep("NIGERIA", 2),
      check.names = FALSE
    ),
    activity = data.frame(
      Id = 1,
      SubActivityId = "S1",
      LastUpdateDate = "2024-03-01",
      DateFrom = "2024-03-10",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    )
  )
  out <- suppressMessages(polished::run_pipeline(inputs))
  testthat::expect_true(all(c("es", "sia") %in% names(out)))

  recon <- suppressMessages(polished::run_pipeline(
    list(afp = raw_afp_positive()),
    reconcile_with = list(afp = data.frame(id = c(1, 2)))
  ))
  testthat::expect_true("afp" %in% names(recon))
})

testthat::test_that("run_pipeline_dir aborts on an empty dir and returns without writing", {
  testthat::expect_error(
    suppressMessages(polished::run_pipeline_dir(withr::local_tempdir())),
    "No recognised raw_"
  )
  src <- withr::local_tempdir()
  saveRDS(raw_afp(), file.path(src, "raw_afp.rds"))
  cleaned <- suppressMessages(polished::run_pipeline_dir(src))
  testthat::expect_true("afp" %in% names(cleaned))
})
