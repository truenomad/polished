# reconcile_admin_guids() / impute_missing_coords() on synthetic data.

shape_long <- function() {
  data.frame(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm0_guid = "{A0}",
    adm1_guid = "{A1}",
    adm2_guid = "{A2}",
    active_year = c(2023, 2024, 9999),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("reconcile fills missing names from a valid adm2 GUID", {
  cases <- data.frame(
    adm0 = "NIGERIA",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm0_guid = "a0",
    adm1_guid = NA_character_,
    adm2_guid = "a2", # lower-case, unbraced -> matches {A2}
    year_onset = 2024,
    stringsAsFactors = FALSE
  )
  out <- polished::reconcile_admin_guids(cases, shape_long(), verbose = FALSE)
  testthat::expect_equal(out$adm1, "BORNO")
  testthat::expect_equal(out$adm2, "BOSSO")
  testthat::expect_equal(out$adm1_guid, "a1") # filled, canonical lower-case
  testthat::expect_equal(out$geo_source, "guid_match")
})

testthat::test_that("reconcile corrects a wrong GUID from unambiguous names", {
  cases <- data.frame(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm0_guid = "a0",
    adm1_guid = "a1",
    adm2_guid = "wrongguid",
    year_onset = 2024,
    stringsAsFactors = FALSE
  )
  out <- polished::reconcile_admin_guids(cases, shape_long(), verbose = FALSE)
  testthat::expect_equal(out$adm2_guid, "a2")
  testthat::expect_equal(out$geo_source, "guid_corrected_from_name")
})

testthat::test_that("reconcile resolves a valid GUID in a non-active year", {
  cases <- data.frame(
    adm0 = "NIGERIA",
    adm1 = NA_character_,
    adm2 = NA_character_,
    adm0_guid = "a0",
    adm1_guid = NA_character_,
    adm2_guid = "a2",
    year_onset = 1999, # outside 2023/2024 active window
    stringsAsFactors = FALSE
  )
  out <- polished::reconcile_admin_guids(cases, shape_long(), verbose = FALSE)
  testthat::expect_equal(out$adm2, "BOSSO")
  testthat::expect_equal(out$geo_source, "guid_match_other_year")
})

testthat::test_that("reconcile flags an unknown GUID as unresolved", {
  cases <- data.frame(
    adm0 = "NIGERIA",
    adm1 = "ZZZ",
    adm2 = "ZZZ",
    adm0_guid = "a0",
    adm1_guid = "x1",
    adm2_guid = "nope",
    year_onset = 2024,
    stringsAsFactors = FALSE
  )
  out <- polished::reconcile_admin_guids(cases, shape_long(), verbose = FALSE)
  testthat::expect_equal(out$geo_source, "unresolved")
  testthat::expect_s3_class(attr(out, "reconcile_qa"), "tbl_df")
})

testthat::test_that("impute_missing_coords samples a point inside the polygon", {
  testthat::skip_if_not_installed("sf")
  # a 10x10 square district at the origin
  sq <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(10, 0),
    c(10, 10),
    c(0, 10),
    c(0, 0)
  )))
  shp <- sf::st_sf(
    adm2_guid = "{D1}",
    geometry = sf::st_sfc(sq, crs = 4326)
  )
  cases <- data.frame(
    adm2_guid = c("d1", "d1"),
    longitude = c(NA, 0),
    latitude = c(NA, 0), # one NA, one (0,0) -> both need imputation
    stringsAsFactors = FALSE
  )
  out <- polished::impute_missing_coords(cases, shp, verbose = FALSE)
  testthat::expect_true(all(out$coord_imputed))
  testthat::expect_true(all(out$longitude >= 0 & out$longitude <= 10))
  testthat::expect_true(all(out$latitude >= 0 & out$latitude <= 10))
})

testthat::test_that("impute_geo_from_coords fills admin in place from coords", {
  testthat::skip_if_not_installed("sf")
  sq <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(10, 0),
    c(10, 10),
    c(0, 10),
    c(0, 0)
  )))
  shp <- sf::st_sf(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm0_guid = "g0",
    adm1_guid = "g1",
    adm2_guid = "g2",
    year_start = 2000,
    year_end = 2030,
    geometry = sf::st_sfc(sq, crs = 4326)
  )
  cases <- data.frame(
    adm0 = c("NIGERIA", "NIGERIA"),
    adm1 = c("BORNO", NA),
    adm2 = c("BOSSO", NA),
    adm1_guid = c("g1", NA),
    adm2_guid = c("g2", NA), # row 2 missing, inside the polygon
    year_onset = c(2024, 2024),
    longitude = c(5, 5),
    latitude = c(5, 5),
    stringsAsFactors = FALSE
  )
  out <- polished::impute_geo_from_coords(cases, shp, verbose = FALSE)
  testthat::expect_equal(nrow(out), 2L) # every row kept
  testthat::expect_equal(out$adm2[2], "BOSSO") # filled from coords
  testthat::expect_equal(out$adm2_guid[2], "g2")
  testthat::expect_equal(out$adm1[2], "BORNO")
})

testthat::test_that("clean_afp accepts a polygon shape (derives long + coords)", {
  testthat::skip_if_not_installed("sf")
  sq <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(10, 0),
    c(10, 10),
    c(0, 10),
    c(0, 0)
  )))
  # geometry column named `shape`, mirroring spatial_global_adm2
  poly <- sf::st_sf(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm0_guid = "{G0}",
    adm1_guid = "{G1}",
    adm2_guid = "{G2}",
    year_start = 2000,
    year_end = 2030,
    shape = sf::st_sfc(sq, crs = 4326)
  )
  raw <- data.frame(
    Id = 1:2,
    Epid = c("NIE-BOR-AAA-24-001", "NIE-BOR-AAA-24-002"),
    LastUpdateDate = c("2024-01-01", "2024-01-01"),
    ParalysisOnsetDate = c("2024-02-01", "2024-02-01"),
    Admin0Name = c("NIGERIA", "NIGERIA"),
    Admin1Name = c("BORNO", NA),
    Admin2Name = c("BOSSO", NA), # row 2 missing district
    Admin0GUID = c("{G0}", "{G0}"),
    Admin1GUID = c("{G1}", NA),
    Admin2GUID = c("{G2}", NA),
    Latitude = c(5, 5),
    Longitude = c(5, 5), # both inside the polygon
    check.names = FALSE
  )
  # impute_geo = FALSE so the fill must come from coordinates, not the EPID
  out <- polished::clean_afp(
    raw,
    shape = poly,
    impute_geo = FALSE,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true("geo_source" %in% names(out))
  testthat::expect_equal(out$adm2[out$epid == "NIE-BOR-AAA-24-002"], "BOSSO")
  testthat::expect_equal(
    out$adm2_guid[out$epid == "NIE-BOR-AAA-24-002"],
    "{G2}"
  )
})
