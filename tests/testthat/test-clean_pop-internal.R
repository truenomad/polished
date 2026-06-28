# High-coverage tests for clean_pop()'s internals and the pipeline wiring.
# Behavioural end-to-end tests live in test-clean_pop.R; this file drives the
# individual helpers and every branch (ladder rungs, age-order, validity,
# raster extraction, checks, config + run_pipeline wiring).

# ---- .pop_pick -------------------------------------------------------------

test_that(".pop_pick matches exactly, then case/punctuation-insensitively", {
  df <- data.frame(`Place.Id` = 1, Value = 2, check.names = FALSE)
  # exact candidate wins
  expect_equal(polished:::.pop_pick(df, c("Value", "value")), "Value")
  # normalised match: "Place.Id" -> candidate "PlaceId"
  expect_equal(polished:::.pop_pick(df, c("PlaceId", "place_id")), "Place.Id")
  # required (default) aborts when nothing matches
  expect_error(polished:::.pop_pick(df, c("Nope")), "no")
  # not-required returns NA
  expect_true(is.na(polished:::.pop_pick(df, "Nope", required = FALSE)))
})

# ---- .pop_brace_guid -------------------------------------------------------

test_that(".pop_brace_guid braces + upper-cases idempotently", {
  expect_equal(polished:::.pop_brace_guid("g1"), "{G1}")
  expect_equal(polished:::.pop_brace_guid("{g1}"), "{G1}")
  expect_equal(
    polished:::.pop_brace_guid(c("a", "{b}", NA, "")),
    c("{A}", "{B}", NA, NA)
  )
})

# ---- .pop_normalise_raw ----------------------------------------------------

test_that(".pop_normalise_raw accepts snake_case and scopes age + year", {
  age_map <- c(u5 = "0 to 5 years", u15 = "0 to 15 years", all = "All ages")
  raw <- tibble::tibble(
    place_id = c("g1", "g1", "g1", "g1"),
    place_display_name = "X",
    year = c(2020, 2020, 1990, 2020),
    age_group_name = c(
      "0 to 15 years",
      "Unknown band",
      "0 to 15 years",
      "0 to 5 years"
    ),
    value = c(100, 100, 100, 50)
  )
  out <- polished:::.pop_normalise_raw(raw, age_map, years = 2010:2027)
  # unknown age dropped, out-of-window year dropped -> 2 rows kept
  expect_equal(nrow(out), 2L)
  expect_setequal(out$age, c("u15", "u5"))
  expect_true(all(out$adm2_guid == "{G1}"))
})

# ---- .pop_dup_conflicts ----------------------------------------------------

test_that(".pop_dup_conflicts flags only multi-valued place-years", {
  long <- tibble::tibble(
    adm2_guid = "{G1}",
    place_name = "X",
    year = c(2020, 2020, 2021),
    age = "u15",
    pop_polis = c(1000, 1200, 900)
  )
  out <- polished:::.pop_dup_conflicts(long)
  expect_equal(nrow(out), 1L)
  expect_equal(out$chosen_median, 1100)
  expect_equal(out$n_values, 2L)
})

# ---- .pop_shape_geo --------------------------------------------------------

test_that(".pop_shape_geo drops geometry and renames the ISO column", {
  expect_null(polished:::.pop_shape_geo(NULL))
  df <- tibble::tibble(iso_3_code = "AFG", adm2_guid = "{G1}")
  expect_named(
    polished:::.pop_shape_geo(df),
    c("country_iso3code", "adm2_guid")
  )

  skip_if_not_installed("sf")
  poly <- sf::st_sf(
    iso_3_code = "AFG",
    adm2_guid = "{G1}",
    geometry = sf::st_sfc(sf::st_point(c(0, 0)))
  )
  geo <- polished:::.pop_shape_geo(poly)
  expect_false("geometry" %in% names(geo))
  expect_true("country_iso3code" %in% names(geo))
})

# ---- .pop_orphan_xwalk -----------------------------------------------------

test_that(".pop_orphan_xwalk resolves / flags ambiguous / no-match guids", {
  shp <- tibble::tibble(
    adm2 = c("DISTRICT C", "DUP NAME", "DUP NAME"),
    adm2_guid = c("{G3}", "{G4}", "{G5}"),
    enddate = as.Date(c("2030-01-01", "2030-01-01", "2030-01-01"))
  )
  long <- tibble::tibble(
    adm2_guid = c("{G9}", "{G8}", "{G7}"),
    place_name = c("DISTRICT C", "DUP NAME", "NOWHERE")
  )
  xw <- polished:::.pop_orphan_xwalk(
    long,
    shp,
    reference_date = as.Date("2026-01-01")
  )
  expect_equal(xw$xwalk_status[xw$polis_guid == "{G9}"], "resolved")
  expect_equal(xw$current_guid[xw$polis_guid == "{G9}"], "{G3}")
  expect_equal(xw$xwalk_status[xw$polis_guid == "{G8}"], "ambiguous")
  expect_true(is.na(xw$current_guid[xw$polis_guid == "{G8}"]))
  expect_equal(xw$xwalk_status[xw$polis_guid == "{G7}"], "no_match")
})

test_that(".pop_orphan_xwalk only crosswalks to CURRENT boundaries", {
  # a name that is unique among current districts but duplicated by a retired
  # boundary should still resolve (the retired one is filtered by enddate)
  shp <- tibble::tibble(
    adm2 = c("DISTRICT C", "DISTRICT C"),
    adm2_guid = c("{G3}", "{G3OLD}"),
    enddate = as.Date(c("2030-01-01", "2018-01-01"))
  )
  long <- tibble::tibble(adm2_guid = "{G9}", place_name = "DISTRICT C")
  xw <- polished:::.pop_orphan_xwalk(
    long,
    shp,
    reference_date = as.Date("2026-01-01")
  )
  expect_equal(xw$xwalk_status, "resolved")
  expect_equal(xw$current_guid, "{G3}")
})

# ---- .pop_universe ---------------------------------------------------------

test_that(".pop_universe crosses districts x years (shape or POLIS guids)", {
  shp <- tibble::tibble(
    who_region = "EMRO",
    country_iso3code = "AFG",
    adm0 = "A",
    adm0_guid = "{A0}",
    adm1 = "P",
    adm1_guid = "{A1}",
    adm2 = c("DA", "DB"),
    adm2_guid = c("{G1}", "{G2}")
  )
  u <- polished:::.pop_universe(shp, NULL, years = 2020:2021)
  expect_equal(nrow(u), 4L) # 2 districts x 2 years
  expect_true(all(c("adm1_guid", "year") %in% names(u)))

  # no shape -> universe from POLIS guids
  pd <- tibble::tibble(
    adm2_guid = c("{G9}"),
    year = 2020,
    age = "u15",
    pop_polis = 1
  )
  u2 <- polished:::.pop_universe(NULL, pd, years = 2020:2021)
  expect_setequal(names(u2), c("adm2_guid", "year"))
  expect_equal(nrow(u2), 2L)
})

# ---- .pop_wp_element -------------------------------------------------------

test_that(".pop_wp_element keys by short label or full AgeGroupName", {
  age_map <- c(u5 = "0 to 5 years", u15 = "0 to 15 years", all = "All ages")
  wp_short <- list(u15 = "short")
  wp_full <- stats::setNames(list("full"), "0 to 15 years")
  expect_equal(polished:::.pop_wp_element(wp_short, "u15", age_map), "short")
  expect_equal(polished:::.pop_wp_element(wp_full, "u15", age_map), "full")
  expect_null(polished:::.pop_wp_element(NULL, "u15", age_map))
  expect_null(polished:::.pop_wp_element(list(u5 = "x"), "u15", age_map))
})

# ---- .pop_worldpop_table ---------------------------------------------------

test_that(".pop_worldpop_table normalises a pre-extracted table", {
  df <- tibble::tibble(
    adm2_guid = c("g1", "g1"),
    year = c(2020, 2020),
    u15_pop = c(900, 910)
  )
  out <- polished:::.pop_worldpop_table(
    df,
    "u15",
    shape = NULL,
    reference_date = Sys.Date()
  )
  expect_named(out, c("adm2_guid", "year", "pop_wp"))
  expect_equal(out$adm2_guid, "{G1}") # braced
  expect_equal(out$pop_wp, 905) # duplicate (guid,year) collapsed to median
})

test_that(".pop_worldpop_table falls back to the lone numeric column", {
  df <- tibble::tibble(adm2_guid = "{G1}", year = 2020, whatever = 777)
  out <- polished:::.pop_worldpop_table(
    df,
    "all",
    shape = NULL,
    reference_date = Sys.Date()
  )
  expect_equal(out$pop_wp, 777)
})

test_that(".pop_worldpop_table errors on an ambiguous population column", {
  df <- tibble::tibble(adm2_guid = "{G1}", year = 2020, foo = 1, bar = 2)
  expect_error(
    polished:::.pop_worldpop_table(
      df,
      "u15",
      shape = NULL,
      reference_date = Sys.Date()
    ),
    "population column"
  )
})

test_that(".pop_worldpop_table reads a table from a file path", {
  df <- tibble::tibble(adm2_guid = "{G1}", year = 2020L, u5_pop = 300L)
  p <- file.path(tempdir(), "wp_u5.csv")
  readr::write_csv(df, p)
  out <- polished:::.pop_worldpop_table(
    p,
    "u5",
    shape = NULL,
    reference_date = Sys.Date()
  )
  expect_equal(out$pop_wp, 300)
  expect_null(polished:::.pop_worldpop_table(NULL, "u5", NULL, Sys.Date()))
})

test_that(".pop_worldpop_table needs a shape to read a raster directory", {
  d <- file.path(tempdir(), "wp_needs_shape")
  dir.create(d, showWarnings = FALSE)
  expect_error(
    polished:::.pop_worldpop_table(
      d,
      "u15",
      shape = NULL,
      reference_date = Sys.Date()
    ),
    "shape"
  )
})

# ---- .polis_extract_worldpop (raster path) ---------------------------------

test_that(".polis_extract_worldpop zonal-sums annual GeoTIFFs to the shape", {
  skip_if_not_installed("terra")
  skip_if_not_installed("exactextractr")
  skip_if_not_installed("sf")

  r <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(r) <- 1
  d <- file.path(tempdir(), "wp_rast")
  unlink(d, recursive = TRUE)
  dir.create(d)
  terra::writeRaster(
    r,
    file.path(d, "global_total_00_14_2021.tif"),
    overwrite = TRUE
  )

  poly <- sf::st_sf(
    adm2_guid = "{G1}",
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)))),
      crs = "EPSG:4326"
    )
  )
  out <- suppressWarnings(polished:::.polis_extract_worldpop(d, poly))
  expect_equal(out$year, 2021L)
  expect_equal(out$adm2_guid, "{G1}")
  expect_equal(out$pop_wp, 16) # 16 cells of value 1, fully covered

  # empty directory and a shape without adm2_guid both abort
  empty <- file.path(tempdir(), "wp_empty")
  dir.create(empty, showWarnings = FALSE)
  expect_error(polished:::.polis_extract_worldpop(empty, poly), "GeoTIFF")
  bad_poly <- poly
  bad_poly$adm2_guid <- NULL
  expect_error(polished:::.polis_extract_worldpop(d, bad_poly), "adm2_guid")

  # .pop_worldpop_table routes a directory (+ shape) through the extractor
  via_table <- suppressWarnings(
    polished:::.pop_worldpop_table(
      d,
      "u15",
      shape = poly,
      reference_date = Sys.Date()
    )
  )
  expect_equal(via_table$pop_wp, 16)
})

test_that(".polis_extract_worldpop aborts when the raster backend is absent", {
  testthat::local_mocked_bindings(.pop_has_raster_backend = function() FALSE)
  expect_error(
    polished:::.polis_extract_worldpop("/any/dir", shape = NULL),
    "terra"
  )
})

# ---- .pop_impute_age ladder ------------------------------------------------

impute_base <- function(...) {
  tibble::tibble(adm1_guid = "{A1}", adm0_guid = "{A0}", pop_wp = NA_real_, ...)
}

test_that(".pop_impute_age fills a gap from the district's own trend", {
  base <- impute_base(
    adm2_guid = "{G1}",
    year = 2018:2021,
    pop_polis = c(1000, 1010, 1020, 0) # last year zero -> missing
  )
  d <- polished:::.pop_impute_age(
    base,
    list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
    "u15",
    TRUE
  )
  last <- d[d$year == 2021, ]
  expect_equal(last$source, "district_trend")
  expect_true(last$imputed)
  expect_equal(last$pop, as.integer(round(stats::median(c(1000, 1010, 1020)))))
})

test_that(".pop_impute_age flags a value that jumps from its own history", {
  base <- impute_base(
    adm2_guid = "{G1}",
    year = 2018:2021,
    pop_polis = c(1000, 1010, 1020, 5000)
  )
  d <- polished:::.pop_impute_age(
    base,
    list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
    "u15",
    TRUE
  )
  out <- d[d$year == 2021, ]
  expect_true(out$bad_vs_history)
  expect_equal(out$source, "district_trend") # no worldpop -> own median
})

test_that(".pop_impute_age climbs to adm1 then adm0 when a district is empty", {
  base <- tibble::tibble(
    adm2_guid = c("{G1}", "{G2}", "{G3}"),
    year = 2020L,
    adm0_guid = "{A0}",
    adm1_guid = c("{A1}", "{A1}", "{A2}"), # G2 shares G1's province; G3 alone
    pop_polis = c(1000, NA, NA),
    pop_wp = NA_real_
  )
  d <- polished:::.pop_impute_age(
    base,
    list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
    "u15",
    TRUE
  )
  expect_equal(d$source[d$adm2_guid == "{G2}"], "adm1") # province median (1000)
  expect_equal(d$source[d$adm2_guid == "{G3}"], "adm0") # country median (1000)
})

test_that(".pop_impute_age without parents only uses polis / district trend", {
  base <- tibble::tibble(
    adm2_guid = "{G1}",
    year = 2020,
    pop_polis = NA_real_,
    pop_wp = NA_real_
  )
  d <- polished:::.pop_impute_age(
    base,
    list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
    "u15",
    FALSE
  )
  # no parents, no history, no wp -> nothing to impute from
  expect_true(is.na(d$source))
  expect_true(is.na(d$pop))
})

# ---- age-order reconciliation (via clean_pop) ------------------------------

test_that("clean_pop reconciles an age-order breach down to WorldPop", {
  shp <- tibble::tibble(
    who_region = "EMRO",
    iso_3_code = "AFG",
    adm0 = "A",
    adm0_guid = "{A0}",
    adm1 = "P",
    adm1_guid = "{A1}",
    adm2 = "DA",
    adm2_guid = "{G1}",
    startdate = as.Date("2015-01-01"),
    enddate = as.Date("2030-01-01")
  )
  raw <- tibble::tibble(
    PlaceId = "g1",
    PlaceDisplayName = "DA",
    Year = 2020,
    AgeGroupName = c("0 to 5 years", "0 to 15 years", "All ages"),
    Value = c(850, 800, 2000) # u5 > u15 breach, both within WorldPop tolerance
  )
  grid <- tibble::tibble(adm2_guid = "{G1}", year = 2020L)
  wp <- list(
    u5 = dplyr::mutate(grid, u5_pop = 300L),
    u15 = dplyr::mutate(grid, u15_pop = 900L),
    all = dplyr::mutate(grid, all_pop = 2000L)
  )
  res <- clean_pop(raw, shape = shp, worldpop = wp, years = 2020)
  row <- res$adm2
  expect_true(row$age_order_bad)
  expect_equal(row$u5_pop_source, "worldpop")
  expect_equal(row$u5_pop, 300L)
  expect_equal(row$u15_pop, 900L)
})

# ---- .pop_apply_validity ---------------------------------------------------

test_that(".pop_apply_validity keeps only the boundary valid each year", {
  wide <- tibble::tibble(
    adm2_guid = c("{G1A}", "{G1B}"),
    year = 2020L,
    u15_pop = c(100L, 200L)
  )
  shp <- tibble::tibble(
    adm2_guid = c("{G1A}", "{G1B}"),
    startdate = as.Date(c("2010-01-01", "2019-07-01")),
    enddate = as.Date(c("2019-06-30", "2030-01-01"))
  )
  out <- polished:::.pop_apply_validity(wide, shp, reference_date = Sys.Date())
  expect_equal(out$adm2_guid, "{G1B}") # retired boundary dropped for 2020
})

test_that(".pop_apply_validity is a no-op without validity columns or shape", {
  wide <- tibble::tibble(adm2_guid = "{G1}", year = 2020L, u15_pop = 1L)
  shp_no_dates <- tibble::tibble(adm2_guid = "{G1}")
  expect_equal(
    nrow(polished:::.pop_apply_validity(wide, shp_no_dates, Sys.Date())),
    1L
  )
  expect_identical(polished:::.pop_apply_validity(wide, NULL, Sys.Date()), wide)
})

# ---- .pop_sum_or_na / .pop_rollup ------------------------------------------

test_that(".pop_sum_or_na sums but preserves all-missing as NA", {
  expect_true(is.na(polished:::.pop_sum_or_na(c(NA, NA))))
  expect_equal(polished:::.pop_sum_or_na(c(1, NA, 2)), 3L)
})

test_that(".pop_rollup sums pop columns over an admin level", {
  adm2 <- tibble::tibble(
    who_region = "EMRO",
    country_iso3code = "AFG",
    adm0 = "A",
    adm0_guid = "{A0}",
    adm1 = "P",
    adm1_guid = "{A1}",
    year = 2020L,
    u15_pop = c(100L, 200L),
    all_pop = c(NA_integer_, NA_integer_)
  )
  out <- polished:::.pop_rollup(adm2, polished:::.pop_adm1_by)
  expect_equal(out$u15_pop, 300L)
  expect_true(is.na(out$all_pop)) # all-missing band stays NA
})

# ---- checks_pop ------------------------------------------------------------

test_that("checks_pop validates its input", {
  expect_error(checks_pop(data.frame(x = 1)), "clean_pop")
  expect_error(checks_pop(list(adm2 = NULL, meta = list())), "clean_pop")
})

test_that("checks_pop drops empty detail sheets but always returns a summary", {
  # a single clean district, no dups/orphans/outliers
  raw <- tibble::tibble(
    PlaceId = "g1",
    PlaceDisplayName = "DA",
    Year = 2020,
    AgeGroupName = c("0 to 5 years", "0 to 15 years", "All ages"),
    Value = c(300, 900, 2000)
  )
  shp <- tibble::tibble(
    who_region = "EMRO",
    iso_3_code = "AFG",
    adm0 = "A",
    adm0_guid = "{A0}",
    adm1 = "P",
    adm1_guid = "{A1}",
    adm2 = "DA",
    adm2_guid = "{G1}",
    startdate = as.Date("2015-01-01"),
    enddate = as.Date("2030-01-01")
  )
  ck <- checks_pop(clean_pop(raw, shape = shp, years = 2020))
  expect_true(is.data.frame(ck$summary))
  expect_equal(ck$summary$domain[[1]], "population")
  expect_false("conflicting_dups" %in% names(ck)) # empty -> dropped
  expect_true("source_mix" %in% names(ck)) # always populated
  # summary still records the (zero) conflicting-dup count
  cd <- ck$summary[ck$summary$check == "conflicting_dups", ]
  expect_equal(cd$n_flagged, 0L)
})

test_that(".polis_write_check_workbooks writes a pop workbook", {
  skip_if_not_installed("openxlsx")
  raw <- tibble::tibble(
    PlaceId = "g1",
    PlaceDisplayName = "DA",
    Year = 2020,
    AgeGroupName = "0 to 15 years",
    Value = 1000
  )
  pop <- clean_pop(raw, years = 2020)
  d <- file.path(tempdir(), "ckdir")
  unlink(d, recursive = TRUE)
  dir.create(d)
  polished:::.polis_write_check_workbooks(
    list(pop = pop),
    d,
    reference_date = Sys.Date()
  )
  expect_true(file.exists(file.path(d, "checks_pop.xlsx")))
})

# ---- config + pipeline wiring ----------------------------------------------

test_that("polis_config carries worldpop + pop_years and prints WorldPop", {
  cfg <- polis_config(
    worldpop = list(u15 = data.frame()),
    pop_years = 2010:2020
  )
  expect_false(is.null(cfg$worldpop))
  expect_equal(cfg$pop_years, 2010:2020)
  # print.polis_config emits via cli (to the message stream)
  printed <- paste(
    capture.output(print(cfg), type = "message"),
    collapse = "\n"
  )
  expect_match(printed, "WorldPop")
})

test_that("population is a recognised input stream with a cache version", {
  expect_equal(
    unname(polished:::.polis_input_stems[["population"]]),
    "raw_population"
  )
  expect_equal(unname(polished:::.polis_clean_versions[["pop"]]), 1L)
})

test_that(".polis_pop_denominator extracts the u15 denominator (or NULL)", {
  expect_null(polished:::.polis_pop_denominator(NULL))
  expect_null(polished:::.polis_pop_denominator(list(
    adm2 = tibble::tibble(x = 1)
  )))
  pop <- list(
    adm2 = tibble::tibble(
      adm2_guid = c("{G1}", "{G1}"),
      year = c(2020L, 2020L),
      u15_pop = c(100L, 100L),
      extra = 1
    )
  )
  den <- polished:::.polis_pop_denominator(pop)
  expect_named(den, c("adm2_guid", "year", "u15_pop"))
  expect_equal(nrow(den), 1L) # distinct
})

test_that("run_pipeline feeds the produced pop as the indicators denominator", {
  raw_afp <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "B-2"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    DateOnset = c("2024-01-02", "2024-02-03"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  raw_pop <- tibble::tibble(
    PlaceId = "g1",
    PlaceDisplayName = "DA",
    Year = 2024,
    AgeGroupName = "0 to 15 years",
    Value = 1000
  )
  # No shape (keeps clean_afp simple) and no cfg$population, so the indicators
  # step's denominator must fall back to the pop the pipeline just produced.
  cfg <- polis_config(pop_years = 2024L)
  out <- suppressMessages(
    run_pipeline(list(afp = raw_afp, population = raw_pop), cfg = cfg)
  )
  expect_true("pop" %in% names(out))
  expect_true("u15_pop" %in% names(out$pop$adm2))
})

test_that("checks_pop tolerates a minimal pop with optional columns absent", {
  pop_min <- list(
    adm2 = tibble::tibble(adm2_guid = "{G1}", year = 2020L, u15_pop = 1L),
    meta = list(
      audit = tibble::tibble(),
      dup_conflicts = tibble::tibble(),
      orphan_xwalk = NULL
    )
  )
  ck <- checks_pop(pop_min)
  expect_equal(nrow(ck$summary), 5L) # summary always has the five checks
  expect_equal(names(ck), "summary") # every detail sheet empty -> dropped
})
