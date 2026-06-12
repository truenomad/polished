# Test helpers for the POLIS download engine. Synthetic data only.

# Build a fake POLIS OData JSON body for a list of records.
fake_polis_body <- function(records = list(), count = NULL) {
  out <- list(value = records)
  if (!is.null(count)) {
    out[["@odata.count"]] <- count
  }
  out
}

# Make a fake "case row" with all the columns the workers depend on.
fake_case_row <- function(id, date = "2024-06-15") {
  list(
    Id = id,
    EPID = paste0("TEST-", id),
    CaseDate = date,
    LastUpdateDate = date,
    WHORegion = "AFRO",
    CountryISO3Code = "NGA"
  )
}

# Wrap a function so it returns a saved data.frame regardless of input.
make_stub_body_response <- function(records, count = length(records)) {
  function(url, polis_api_key, ...) {
    fake_polis_body(records = records, count = count)
  }
}

# Make a temporary polis_folder with a few preexisting part files
# (helper for resume-from-disk tests).
seed_parts <- function(polis_folder, table_name, year, df, ext = "rds") {
  parts_dir <- file.path(polis_folder, ".parts", table_name)
  dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
  part_file <- file.path(parts_dir, sprintf("year_%d.%s", year, ext))
  saveRDS(df, part_file)
  part_file
}

# --- Synthetic spatial geometry builders (all in projected metres) --------
# A simple axis-aligned square POLYGON anchored at (x0, y0).
test_square <- function(x0 = 0, y0 = 0, size = 1000) {
  ring <- rbind(
    c(x0, y0),
    c(x0, y0 + size),
    c(x0 + size, y0 + size),
    c(x0 + size, y0),
    c(x0, y0)
  )
  sf::st_polygon(list(ring))
}

# A self-intersecting "bowtie" POLYGON (st_is_valid() -> FALSE).
test_bowtie <- function(x0 = 0, y0 = 0, size = 1000) {
  ring <- rbind(
    c(x0, y0),
    c(x0 + size, y0 + size),
    c(x0 + size, y0),
    c(x0, y0 + size),
    c(x0, y0)
  )
  sf::st_polygon(list(ring))
}

# A square POLYGON with one interior hole of side `hole` metres.
test_square_hole <- function(x0 = 0, y0 = 0, size = 1000, hole = 10) {
  outer <- rbind(
    c(x0, y0),
    c(x0, y0 + size),
    c(x0 + size, y0 + size),
    c(x0 + size, y0),
    c(x0, y0)
  )
  inner <- rbind(
    c(x0 + 1, y0 + 1),
    c(x0 + 1, y0 + 1 + hole),
    c(x0 + 1 + hole, y0 + 1 + hole),
    c(x0 + 1 + hole, y0 + 1),
    c(x0 + 1, y0 + 1)
  )
  sf::st_polygon(list(outer, inner))
}

# A MULTIPOLYGON: one large part plus one detached `small`-metre sliver part.
test_multipart <- function(x0 = 0, y0 = 0, size = 1000, small = 10) {
  big <- list(rbind(
    c(x0, y0),
    c(x0, y0 + size),
    c(x0 + size, y0 + size),
    c(x0 + size, y0),
    c(x0, y0)
  ))
  sliver_x <- x0 + size + 100
  tiny <- list(rbind(
    c(sliver_x, y0),
    c(sliver_x, y0 + small),
    c(sliver_x + small, y0 + small),
    c(sliver_x + small, y0),
    c(sliver_x, y0)
  ))
  sf::st_multipolygon(list(big, tiny))
}

# Write a minimal multi-layer GeoPackage with GLOBAL_ADM0/1/2 layers (projected
# EPSG:3857 so geometry areas are in metres) and return its path. The adm1
# layer carries the Somalia province GUID pair so the in-pipeline end-date fix
# fires. Raw column names mimic the WHO export (clean_names + crosswalk land on
# the canonical adm0/adm1/adm2 + *_guid columns).
write_spatial_gpkg <- function(dir) {
  crs <- 3857
  somalia_country <- "B5FF48B9-7282-445C-8CD2-BEFCE4E0BDA7"
  somalia_province <- "EE73F3EA-DD35-480F-8FEA-5904274087C4"

  adm0 <- sf::st_sf(
    Adm0_Name = c("NIGERIA", "SOMALIA"),
    Guid = c("{A0-NGA}", "{A0-SOM}"),
    Startdate = c("2015-01-01", "2015-01-01"),
    Enddate = c("9999-12-31", "9999-12-31"),
    geometry = sf::st_sfc(test_square(0, 0), test_square(3000, 0), crs = crs)
  )
  adm1 <- sf::st_sf(
    Adm0_Name = c("NIGERIA", "SOMALIA"),
    Adm1_Name = c("BORNO", "BANADIR"),
    Adm0_Guid = c("{A0-NGA}", somalia_country),
    Guid = c("{A1-BOR}", somalia_province),
    Startdate = c("2015-01-01", "2015-01-01"),
    Enddate = c("9999-12-31", "9999-12-31"),
    geometry = sf::st_sfc(test_square(0, 0), test_square(3000, 0), crs = crs)
  )
  adm2 <- sf::st_sf(
    Adm0_Name = "NIGERIA",
    Adm1_Name = "BORNO",
    Adm2_Name = "BOSSO",
    Adm0_Guid = "{A0-NGA}",
    Adm1_Guid = "{A1-BOR}",
    Guid = "{A2-BOS}",
    Startdate = "2015-01-01",
    Enddate = "9999-12-31",
    geometry = sf::st_sfc(test_square(0, 0), crs = crs)
  )

  gpkg <- file.path(dir, "who.gpkg")
  sf::st_write(adm0, gpkg, layer = "GLOBAL_ADM0", quiet = TRUE, append = FALSE)
  sf::st_write(adm1, gpkg, layer = "GLOBAL_ADM1", quiet = TRUE, append = TRUE)
  sf::st_write(adm2, gpkg, layer = "GLOBAL_ADM2", quiet = TRUE, append = TRUE)
  gpkg
}

# An in-memory adm2 `sf` carrying every issue .spatial_check flags and every
# repair .spatial_repair performs: an invalid bowtie, an empty geometry,
# duplicate GUIDs, duplicate names, a sliver hole + a kept hole, and a sliver
# multipart + kept part. Projected EPSG:3857 (areas in metres).
make_pathological_adm2 <- function() {
  geoms <- sf::st_sfc(
    test_square(0, 0), # valid, unique
    test_bowtie(5000, 0), # invalid
    sf::st_polygon(), # empty
    test_square(10000, 0), # dup guid (a)
    test_square(11000, 0), # dup guid (b)
    test_square(20000, 0), # dup name (a)
    test_square(21000, 0), # dup name (b)
    test_square_hole(30000, 0, hole = 10), # sliver hole (dropped)
    test_square_hole(40000, 0, hole = 200), # real hole (kept)
    test_multipart(50000, 0, small = 10), # sliver part (dropped)
    crs = 3857
  )
  sf::st_sf(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = c(
      "A",
      "B",
      "C",
      "D",
      "E",
      "DUPNAME",
      "DUPNAME",
      "H",
      "I",
      "J"
    ),
    adm0_guid = "{A0}",
    adm1_guid = "{A1}",
    adm2_guid = c(
      "{G1}",
      "{G2}",
      "{G3}",
      "{DUP}",
      "{DUP}",
      "{G6}",
      "{G7}",
      "{G8}",
      "{G9}",
      "{G10}"
    ),
    year_start = 2015,
    year_end = 2020,
    geometry = geoms
  )
}

# A two-district adm2 `sf` (WGS84) sharing an edge, with validity windows, for
# the coordinate-join recovery tests.
make_district_shape <- function() {
  west <- test_square(0, 0, size = 1)
  east <- test_square(1, 0, size = 1)
  sf::st_sf(
    adm0 = c("NIGERIA", "NIGERIA"),
    adm1 = c("BORNO", "YOBE"),
    adm2 = c("WEST", "EAST"),
    adm0_guid = c("{A0}", "{A0}"),
    adm1_guid = c("{A1W}", "{A1E}"),
    adm2_guid = c("{A2W}", "{A2E}"),
    year_start = c(2010, 2010),
    year_end = c(2025, 2025),
    geometry = sf::st_sfc(west, east, crs = 4326)
  )
}
