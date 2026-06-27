# Spatial helpers in R/spatial.R. Each test is a single full characterisation
# of one helper group: happy path, edges and error paths together. Synthetic
# data only; no real boundary files are read.

testthat::test_that("source-resolution helpers detect containers, pick layers, rename guids", {
  # .spatial_is_container: .gdb dir and .gpkg file are containers; a plain
  # folder and a .shp file are not
  tmp <- withr::local_tempdir()
  gdb <- file.path(tmp, "who.gdb")
  dir.create(gdb)
  testthat::expect_true(polished:::.spatial_is_container(gdb))
  testthat::expect_false(polished:::.spatial_is_container(tmp))
  testthat::expect_true(polished:::.spatial_is_container("boundaries.gpkg"))
  testthat::expect_false(polished:::.spatial_is_container("adm0.shp"))

  # .spatial_pick: exact match wins, else substring; multiple hits -> last in
  # decreasing sort; no match aborts
  choices <- c("GLOBAL_ADM0", "GLOBAL_ADM1", "GLOBAL_ADM2")
  testthat::expect_identical(
    polished:::.spatial_pick(choices, "global_adm1", "layer", "src"),
    "GLOBAL_ADM1"
  )
  testthat::expect_identical(
    polished:::.spatial_pick(c("a_adm2_b", "c_adm0"), "adm2", "layer", "src"),
    "a_adm2_b"
  )
  testthat::expect_identical(
    polished:::.spatial_pick(c("adm2_v1", "adm2_v2"), "adm2", "layer", "src"),
    "adm2_v2"
  )
  testthat::expect_error(
    polished:::.spatial_pick(choices, "adm9", "layer", "src"),
    "matches"
  )

  # .spatial_rename_guid: renames `guid` per level, no-op when absent
  renamed <- polished:::.spatial_rename_guid(
    data.frame(guid = "g", x = 1, stringsAsFactors = FALSE),
    "adm2"
  )
  testthat::expect_true("adm2_guid" %in% names(renamed))
  testthat::expect_false("guid" %in% names(renamed))
  untouched <- data.frame(x = 1)
  testthat::expect_identical(
    polished:::.spatial_rename_guid(untouched, "adm0"),
    untouched
  )
})

testthat::test_that("geometry helpers count holes, test CRS equivalence, cap Somalia end-date", {
  # .spatial_feature_holes: a polygon with an interior ring -> 1; solid -> 0;
  # non-polygon -> 0
  outer <- rbind(c(0, 0), c(0, 10), c(10, 10), c(10, 0), c(0, 0))
  hole <- rbind(c(3, 3), c(3, 6), c(6, 6), c(6, 3), c(3, 3))
  testthat::expect_identical(
    polished:::.spatial_feature_holes(sf::st_polygon(list(outer, hole))),
    1L
  )
  testthat::expect_identical(
    polished:::.spatial_feature_holes(sf::st_polygon(list(outer))),
    0L
  )
  testthat::expect_identical(
    polished:::.spatial_feature_holes(sf::st_point(c(0, 0))),
    0L
  )

  # .spatial_crs_equivalent: identical CRS -> TRUE, different -> FALSE
  pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326))
  testthat::expect_true(polished:::.spatial_crs_equivalent(pt, 4326))
  testthat::expect_false(polished:::.spatial_crs_equivalent(pt, 3857))

  # .spatial_somalia_fix: caps only the flagged province to 2021; no-op without
  # the guid columns
  flagged <- data.frame(
    adm0_guid = c("B5FF48B9-7282-445C-8CD2-BEFCE4E0BDA7", "OTHER"),
    adm1_guid = c("EE73F3EA-DD35-480F-8FEA-5904274087C4", "OTHER"),
    year_end = c(9999, 2010),
    stringsAsFactors = FALSE
  )
  testthat::expect_identical(
    polished:::.spatial_somalia_fix(flagged)$year_end,
    c(2021, 2010)
  )
  bare <- data.frame(year_end = 9999)
  testthat::expect_identical(polished:::.spatial_somalia_fix(bare), bare)
})

testthat::test_that("create_long_shape expands to active years and validates inputs", {
  shapes <- tibble::tibble(
    adm0_guid = "g0",
    adm0 = "NIGERIA",
    adm1_guid = "g1",
    adm1 = "BORNO",
    year_start = 2018,
    year_end = 2020
  )
  long <- polished::create_long_shape(shapes, "adm1")
  current_year <- lubridate::year(Sys.Date())
  testthat::expect_s3_class(long, "tbl_df")
  testthat::expect_true("active_year" %in% names(long))
  testthat::expect_true(9999 %in% long$active_year)
  real_years <- sort(long$active_year[long$active_year != 9999])
  testthat::expect_identical(real_years, 2018:as.integer(current_year))

  testthat::expect_error(polished::create_long_shape(1L, "adm1"), "data frame")
  testthat::expect_error(
    polished::create_long_shape(shapes[0, ], "adm1"),
    "zero rows"
  )
  testthat::expect_error(polished::create_long_shape(shapes, "adm9"), "level")
  testthat::expect_error(
    polished::create_long_shape(dplyr::select(shapes, -year_end), "adm1"),
    "missing required"
  )
})

testthat::test_that("process_spatial reads a container, repairs, transforms and writes every level", {
  src_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  gpkg <- write_spatial_gpkg(src_dir)

  ret <- testthat::expect_invisible(
    polished::process_spatial(
      gpkg,
      out_dir,
      transform = TRUE,
      crs = 4326,
      fix_issues = TRUE,
      output_format = "rds",
      verbose = FALSE
    )
  )
  testthat::expect_identical(ret, out_dir)

  # cleaned per-level shapes + year-expanded long shapes are all written
  for (lvl in c("adm0", "adm1", "adm2")) {
    testthat::expect_true(
      file.exists(file.path(out_dir, paste0("spatial_global_", lvl, ".rds")))
    )
  }
  testthat::expect_true(
    file.exists(file.path(out_dir, "spatial_adm1_long_shape.rds"))
  )
  testthat::expect_true(
    file.exists(file.path(out_dir, "spatial_adm2_long_shape.rds"))
  )

  # the Somalia province end-date cap fired in-pipeline
  adm1 <- readRDS(file.path(out_dir, "spatial_global_adm1.rds"))
  testthat::expect_identical(adm1$year_end[adm1$adm0 == "SOMALIA"], 2021)

  # transform actually reprojected the source (3857) to the target WGS84
  testthat::expect_identical(
    sf::st_crs(readRDS(file.path(out_dir, "spatial_global_adm0.rds")))$epsg,
    4326L
  )
})

testthat::test_that("process_spatial reads a folder source and rejects bad arguments", {
  src_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  adm1_sf <- sf::st_sf(
    Adm0_Name = "NIGERIA",
    Adm1_Name = "BORNO",
    Adm0_Guid = "{A0}",
    Guid = "{A1}",
    Startdate = "2015-01-01",
    Enddate = "9999-12-31",
    geometry = sf::st_sfc(test_square(0, 0, size = 1), crs = 4326)
  )
  sf::st_write(adm1_sf, file.path(src_dir, "adm1.geojson"), quiet = TRUE)

  # folder-of-files branch of .spatial_locate; source already WGS84 so the
  # transform is skipped via .spatial_crs_equivalent()
  polished::process_spatial(
    src_dir,
    out_dir,
    layers = c(adm1 = "adm1"),
    transform = TRUE,
    crs = 4326,
    fix_issues = TRUE,
    verbose = FALSE
  )
  testthat::expect_true(
    file.exists(file.path(out_dir, "spatial_global_adm1.rds"))
  )

  # argument validation
  testthat::expect_error(polished::process_spatial(123, out_dir), "single path")
  testthat::expect_error(
    polished::process_spatial(tempfile(), out_dir),
    "does not exist"
  )
  testthat::expect_error(
    polished::process_spatial(src_dir, 123),
    "single path"
  )
  testthat::expect_error(
    polished::process_spatial(src_dir, out_dir, layers = c(bad = "x")),
    "unknown level"
  )
  testthat::expect_error(
    polished::process_spatial(src_dir, out_dir, layers = character(0)),
    "named vector"
  )
  testthat::expect_error(
    polished::process_spatial(src_dir, out_dir, output_format = "xml"),
    "output_format"
  )
  testthat::expect_error(
    polished::process_spatial(src_dir, out_dir, sliver_area = -1),
    "non-negative"
  )
})

testthat::test_that(".spatial_check writes issue CSVs and .spatial_repair fixes geometries", {
  checks_dir <- withr::local_tempdir()
  data <- make_pathological_adm2()

  polished:::.spatial_check(data, "adm2", checks_dir)
  written <- list.files(checks_dir)
  testthat::expect_true(any(grepl("invalid_adm2", written)))
  testthat::expect_true(any(grepl("empty_adm2", written)))
  testthat::expect_true(any(grepl("duplicate_adm2_guid", written)))
  testthat::expect_true(any(grepl("duplicate_adm2_name", written)))

  repaired <- polished:::.spatial_repair(
    data,
    "adm2",
    sliver_area = 1e4,
    verbose = FALSE
  )
  testthat::expect_s3_class(repaired, "sf")
  testthat::expect_identical(nrow(repaired), nrow(data))
  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  testthat::expect_false(any(!sf::st_is_valid(repaired), na.rm = TRUE))
})

testthat::test_that("get_admin_info_from_coords imputes admin, drops ambiguous, validates", {
  shp <- make_district_shape()
  cases <- data.frame(
    longitude = c(0.5, 5, NA),
    latitude = c(0.5, 5, 0.5),
    adm0 = "NIGERIA",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    year_onset = 2024,
    stringsAsFactors = FALSE
  )

  # temporally-filtered join: only the in-district point survives, fully imputed
  out <- polished::get_admin_info_from_coords(
    cases,
    shp,
    year_col = "year_onset"
  )
  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_identical(out$adm1, "BORNO")
  testthat::expect_identical(out$adm2, "WEST")

  # untemporal join path
  out_no_year <- polished::get_admin_info_from_coords(cases[1, ], shp)
  testthat::expect_identical(out_no_year$adm2, "WEST")

  # a point on the shared edge matches both districts -> dropped as ambiguous
  ambiguous <- data.frame(
    longitude = 1,
    latitude = 0.5,
    adm0 = "NIGERIA",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::expect_identical(
    nrow(polished::get_admin_info_from_coords(ambiguous, shp)),
    0L
  )

  # validation
  testthat::expect_error(
    polished::get_admin_info_from_coords(1L, shp),
    "data frame"
  )
  testthat::expect_error(
    polished::get_admin_info_from_coords(cases[0, ], shp),
    "zero rows"
  )
  testthat::expect_error(
    polished::get_admin_info_from_coords(cases, data.frame(x = 1)),
    "sf"
  )
  testthat::expect_error(
    polished::get_admin_info_from_coords(cases, dplyr::select(shp, -adm1)),
    "missing column"
  )
  testthat::expect_error(
    polished::get_admin_info_from_coords(
      data.frame(adm0 = "NIGERIA"),
      shp
    ),
    "coordinate column"
  )
})

testthat::test_that("impute_geo_from_coords fills missing admin, stamps source, handles no-ops", {
  shp <- make_district_shape()
  cases <- data.frame(
    longitude = c(0.5, 9),
    latitude = c(0.5, 9),
    adm0_guid = "{A0}",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    year_onset = 2024,
    geo_source = "unresolved",
    stringsAsFactors = FALSE
  )
  out <- polished::impute_geo_from_coords(cases, shp, verbose = TRUE)
  testthat::expect_identical(out$adm2[1], "WEST")
  # imputed GUIDs are written in the shape's native form (here "{A2W}"), the
  # same as get_admin_info_from_coords() -- not re-cased -- so they stay
  # consistent with the data's existing GUIDs ("{A0}") and the shape.
  testthat::expect_identical(out$adm2_guid[1], "{A2W}")
  testthat::expect_identical(out$geo_source[1], "coord_match")
  testthat::expect_true(is.na(out$adm2[2]))
  testthat::expect_identical(out$geo_source[2], "unresolved")

  # all admin present -> nothing to recover -> returned unchanged
  complete <- data.frame(
    longitude = 0.5,
    latitude = 0.5,
    adm1 = "X",
    adm2 = "Y",
    adm1_guid = "g1",
    adm2_guid = "g2",
    year_onset = 2024,
    stringsAsFactors = FALSE
  )
  testthat::expect_identical(
    polished::impute_geo_from_coords(complete, shp, verbose = TRUE),
    complete
  )

  # coords that resolve no district -> returned unchanged
  faraway <- data.frame(
    longitude = 50,
    latitude = 50,
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    year_onset = 2024,
    stringsAsFactors = FALSE
  )
  testthat::expect_true(is.na(
    polished::impute_geo_from_coords(faraway, shp, verbose = TRUE)$adm2
  ))

  # no coordinate columns -> early return, plus validation
  no_coords <- data.frame(adm1 = NA, adm2 = NA, adm1_guid = NA, adm2_guid = NA)
  testthat::expect_identical(
    polished::impute_geo_from_coords(no_coords, shp, verbose = FALSE),
    no_coords
  )
  testthat::expect_error(
    polished::impute_geo_from_coords(1L, shp),
    "non-empty data frame"
  )
  testthat::expect_error(
    polished::impute_geo_from_coords(
      data.frame(longitude = 1, latitude = 1),
      1L
    ),
    "sf"
  )
})

testthat::test_that("geo-name fixes apply contains/exact/substr rules and validate inputs", {
  fixes <- tibble::tibble(
    field = c("adm0_name", "adm0_name", "adm1_name"),
    pattern = c("IVOIRE", "EXACTNAME", "OLD"),
    replacement = c("COTE D IVOIRE", "FIXED", "NEW"),
    match_type = c("contains", "exact", "substr")
  )

  testthat::expect_identical(
    polished::polis_fix_geo_names(
      c("REPUBLIC OF IVOIRE", NA),
      "adm0_name",
      fixes
    ),
    c("COTE D IVOIRE", NA)
  )
  testthat::expect_identical(
    polished::polis_fix_geo_names(
      c("EXACTNAME", "EXACTNAME SUFFIX"),
      "adm0_name",
      fixes
    ),
    c("FIXED", "EXACTNAME SUFFIX")
  )
  testthat::expect_identical(
    polished::polis_fix_geo_names("OLD TOWN", "adm1_name", fixes),
    "NEW TOWN"
  )
  # no rules for a valid-but-absent field -> passthrough
  testthat::expect_identical(
    polished::polis_fix_geo_names("zzz", "location", fixes),
    "zzz"
  )
  testthat::expect_error(
    polished::polis_fix_geo_names("x", "bogus", fixes),
    "Unknown"
  )
  testthat::expect_error(
    polished::polis_fix_geo_names(
      "A",
      "adm0_name",
      tibble::tibble(
        field = "adm0_name",
        pattern = "A",
        replacement = "B",
        match_type = "regex"
      )
    ),
    "match_type"
  )

  out <- polished::fix_geo_names(
    data.frame(
      adm0 = "REPUBLIC OF IVOIRE",
      adm1 = "OLD TOWN",
      other = "Z",
      stringsAsFactors = FALSE
    ),
    fixes = fixes
  )
  testthat::expect_identical(out$adm0, "COTE D IVOIRE")
  testthat::expect_identical(out$adm1, "NEW TOWN")
  testthat::expect_identical(out$other, "Z")

  # the shipped lookup table loads
  testthat::expect_s3_class(polished::polis_geo_name_fixes(), "tbl_df")
})

testthat::test_that("impute_missing_coords samples points inside the district, validates inputs", {
  shp <- make_district_shape()
  cases <- data.frame(
    adm2_guid = c("{A2W}", "{A2E}", "{A2W}", "{NOPE}"),
    longitude = c(NA, 0, 1.5, NA),
    latitude = c(NA, 0, 0.5, NA),
    stringsAsFactors = FALSE
  )
  out <- polished::impute_missing_coords(cases, shp, verbose = TRUE)
  testthat::expect_true("coord_imputed" %in% names(out))
  testthat::expect_true(out$coord_imputed[1]) # NA coords -> imputed
  testthat::expect_true(out$coord_imputed[2]) # (0, 0) -> imputed
  testthat::expect_false(out$coord_imputed[3]) # already had coords
  testthat::expect_false(out$coord_imputed[4]) # GUID absent from shape
  testthat::expect_false(is.na(out$longitude[1]))
  testthat::expect_true(out$longitude[1] >= 0 && out$longitude[1] <= 1)

  ok <- data.frame(
    adm2_guid = "{A2W}",
    longitude = 0.5,
    latitude = 0.5,
    stringsAsFactors = FALSE
  )
  testthat::expect_false(any(
    polished::impute_missing_coords(ok, shp, verbose = TRUE)$coord_imputed
  ))

  testthat::expect_error(
    polished::impute_missing_coords(1L, shp),
    "non-empty"
  )
  testthat::expect_error(
    polished::impute_missing_coords(cases, 1L),
    "sf"
  )
  testthat::expect_error(
    polished::impute_missing_coords(data.frame(x = 1), shp),
    "missing column"
  )
  testthat::expect_error(
    polished::impute_missing_coords(cases, shp, shape_guid_var = "nope"),
    "no .* column"
  )
})

testthat::test_that("process_spatial reports progress when verbose and rejects bad sources", {
  src <- withr::local_tempdir()
  out <- withr::local_tempdir()
  gpkg <- write_spatial_gpkg(src)
  testthat::expect_invisible(
    polished::process_spatial(
      gpkg,
      out,
      layers = c(adm2 = "GLOBAL_ADM2"),
      fix_issues = TRUE,
      verbose = TRUE
    )
  )

  # a folder with no spatial files
  empty <- withr::local_tempdir()
  testthat::expect_error(
    polished::process_spatial(
      empty,
      out,
      layers = c(adm1 = "adm1"),
      verbose = FALSE
    ),
    "No spatial files"
  )

  # an existing plain file that is neither a container nor a directory
  flat <- file.path(withr::local_tempdir(), "notes.txt")
  writeLines("x", flat)
  testthat::expect_error(
    polished::process_spatial(
      flat,
      out,
      layers = c(adm1 = "adm1"),
      verbose = FALSE
    ),
    "not a directory or dataset"
  )
})

testthat::test_that(".spatial_drop_sliver_holes/parts drop slivers, keep real, honour threshold", {
  poly_sliver <- test_square_hole(0, 0, hole = 10) # 100 m^2 hole -> dropped
  mpoly_hole <- sf::st_multipolygon(list(unclass(
    test_square_hole(5000, 0, hole = 10)
  )))
  solid <- test_square(10000, 0)
  holes_sf <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(poly_sliver, mpoly_hole, solid, crs = 3857)
  )
  dropped <- polished:::.spatial_drop_sliver_holes(holes_sf, 1e4)
  testthat::expect_gt(dropped$n, 0L)
  testthat::expect_identical(
    polished:::.spatial_drop_sliver_holes(holes_sf, 0)$n,
    0L
  )

  multipart <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(test_multipart(0, 0, small = 10), crs = 3857)
  )
  parts <- polished:::.spatial_drop_sliver_parts(multipart, 1e4)
  testthat::expect_identical(parts$n, 1L)
  single <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(test_square(0, 0), crs = 3857)
  )
  testthat::expect_identical(
    polished:::.spatial_drop_sliver_parts(single, 1e4)$n,
    0L
  )
})

testthat::test_that("epid_split rejects bad sep/extra/fill arguments", {
  testthat::expect_error(polished::epid_split("A-B", sep = ""), "sep")
  testthat::expect_error(
    polished::epid_split("A-B", sep = c("-", "_")),
    "sep"
  )
  testthat::expect_error(
    polished::epid_split("A-B", extra = "explode"),
    "extra"
  )
  testthat::expect_error(polished::epid_split("A-B", fill = "left"), "fill")
})

testthat::test_that("process_spatial selects an inner layer from a multilayer GeoPackage in a folder", {
  src <- withr::local_tempdir()
  out <- withr::local_tempdir()
  adm2 <- sf::st_sf(
    Adm0_Name = "NIGERIA",
    Adm1_Name = "BORNO",
    Adm2_Name = "BOSSO",
    Adm0_Guid = "{A0}",
    Adm1_Guid = "{A1}",
    Guid = "{A2}",
    Startdate = "2015-01-01",
    Enddate = "9999-12-31",
    geometry = sf::st_sfc(test_square(0, 0, size = 1), crs = 4326)
  )
  gpkg <- file.path(src, "adm2.gpkg")
  sf::st_write(adm2, gpkg, layer = "adm2", quiet = TRUE)
  sf::st_write(adm2, gpkg, layer = "other", quiet = TRUE, append = TRUE)

  polished::process_spatial(
    src,
    out,
    layers = c(adm2 = "adm2"),
    transform = FALSE,
    fix_issues = FALSE,
    verbose = FALSE
  )
  testthat::expect_true(
    file.exists(file.path(out, "spatial_global_adm2.rds"))
  )
})

testthat::test_that("impute_geo_from_coords works without a year column or an adm2 column", {
  shp <- make_district_shape()
  no_year <- data.frame(
    longitude = 0.5,
    latitude = 0.5,
    adm0_guid = "{A0}",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::expect_identical(
    polished::impute_geo_from_coords(no_year, shp, verbose = FALSE)$adm2,
    "WEST"
  )

  no_adm2 <- data.frame(
    longitude = 0.5,
    latitude = 0.5,
    adm0_guid = "{A0}",
    adm1 = NA_character_,
    adm1_guid = NA_character_,
    adm2_guid = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- polished::impute_geo_from_coords(no_adm2, shp, verbose = FALSE)
  testthat::expect_false(is.na(out$adm2_guid))
})

testthat::test_that("create_long_shape flags multiple shapes active in the same year", {
  overlapping <- tibble::tibble(
    adm0_guid = c("g0", "g0"),
    adm0 = c("NIGERIA", "NIGERIA"),
    adm1_guid = c("g1a", "g1b"),
    adm1 = c("BORNO", "BORNO"),
    year_start = c(2018, 2018),
    year_end = c(2020, 2020)
  )
  checks_dir <- withr::local_tempdir()
  long <- polished::create_long_shape(
    overlapping,
    "adm1",
    checks_dir = checks_dir
  )
  testthat::expect_s3_class(long, "tbl_df")
  testthat::expect_true(
    any(grepl("spatial_shape_multiple_adm1", list.files(checks_dir)))
  )
})
