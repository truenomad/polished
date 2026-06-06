# Tests for the EPID-driven geography cleaner. Synthetic data only.

# ---- epid_split ------------------------------------------------------

test_that("epid_split parses canonical, short, and NA EPIDs", {
  split_result <- epid_split(c("NIE-BOS-XYZ-24-001", "AGO-LUA", NA))

  expect_s3_class(split_result, "tbl_df")
  expect_named(
    split_result,
    c("country", "province", "district", "year", "serial")
  )
  expect_identical(split_result$country, c("NIE", "AGO", NA))
  expect_identical(split_result$district, c("XYZ", NA, NA))
  expect_identical(split_result$serial, c("001", NA, NA))
})

test_that("epid_split honours extra = 'merge' and never errors on junk", {
  merged <- epid_split("A-B-C-D-E-F-G", extra = "merge")
  expect_identical(merged$serial, "E-F-G")

  expect_no_error(epid_split(c("", "   ", "-", "A--B")))
})

# ---- epid_country_code ----------------------------------------------

test_that("epid_country_code returns the 3-char prefix and respects upper", {
  expect_identical(
    epid_country_code(c("NIE-BOS-1", "ago-lua-1", NA)),
    c("NIE", "AGO", NA)
  )
  expect_identical(epid_country_code("ago-lua", upper = FALSE), "ago")
  expect_identical(epid_country_code("  NIE-1"), "NIE")
})

# ---- epid_prefix -----------------------------------------------------

test_that("epid_prefix takes the leading characters and NA-pads blanks", {
  expect_identical(
    epid_prefix("NIE-BOS-XYZ-24-001", length = 11),
    "NIE-BOS-XYZ"
  )
  expect_identical(epid_prefix(c("  ", NA)), c(NA_character_, NA_character_))
})

# ---- epid_strip_contact ---------------------------------------------

test_that("epid_strip_contact splits contact markers off the base EPID", {
  contact_split <- epid_strip_contact(c(
    "NIE-BOS-XYZ-24-001",
    "NIE-BOS-XYZ-24-001CC",
    "NIE-BOS-XYZ-24-001-HC",
    "NIE-BOS-XYZ-24-001C2"
  ))
  expect_identical(contact_split$epid_base, rep("NIE-BOS-XYZ-24-001", 4))
  expect_identical(contact_split$contact_code, c(NA, "CC", "HC", "C2"))
})

# ---- build_admin_ref -------------------------------------------------

test_that("build_admin_ref keeps the most-recent non-NA value per EPID", {
  cases <- tibble::tibble(
    epid = c("A-1", "A-1", "A-1", "B-2"),
    year = c(2022, 2024, 2023, 2024),
    district = c("OLD", NA, "MID", "LUANDA")
  )
  ref <- build_admin_ref(cases, "district")

  expect_setequal(ref$epid, c("A-1", "B-2"))
  expect_identical(ref$district[ref$epid == "A-1"], "MID")
})

test_that("build_admin_ref aborts on a missing column", {
  expect_error(
    build_admin_ref(tibble::tibble(epid = "A-1"), "district"),
    "Missing column"
  )
})

# ---- build_prefix_ref ------------------------------------------------

test_that("build_prefix_ref is unique-or-NA with an n_candidates count", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2", "NIE-BOS-BBB-1"),
    year = c(2024, 2024, 2024),
    district = c("BOSSO", "BOSSO", "BIRNI")
  )
  ref <- build_prefix_ref(cases, "district", prefix_length = 11)

  unique_row <- ref[ref$prefix == "NIE-BOS-AAA", ]
  expect_identical(unique_row$n_candidates, 1L)
  expect_identical(unique_row$district, "BOSSO")
})

test_that("build_prefix_ref returns NA when a prefix has rival values", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2"),
    year = c(2024, 2024),
    district = c("BOSSO", "KUKAWA")
  )
  ref <- build_prefix_ref(cases, "district", prefix_length = 11)
  expect_identical(ref$n_candidates, 2L)
  expect_true(is.na(ref$district))
})

# ---- resolve_epid_country -------------------------------------------

test_that("resolve_epid_country returns the raw code when ref is NULL", {
  country_result <- resolve_epid_country(c("NIE-1", "AGO-1"))
  expect_identical(country_result$code, c("NIE", "AGO"))
  expect_false(any(country_result$resolved))
})

test_that("resolve_epid_country matches on code or ISO3", {
  crosswalk <- tibble::tibble(
    code = c("NIE", "AGO"),
    name = c("NIGERIA", "ANGOLA"),
    iso3 = c("NGA", "AGO")
  )
  by_code <- resolve_epid_country("NIE-BOS-1", ref = crosswalk)
  expect_identical(by_code$name, "NIGERIA")
  expect_true(by_code$resolved)

  by_iso3 <- resolve_epid_country("NGA-BOS-1", ref = crosswalk)
  expect_identical(by_iso3$name, "NIGERIA")
})

test_that("resolve_epid_country flags codes that map to >1 name", {
  crosswalk <- tibble::tibble(
    code = c("XYZ", "XYZ"),
    name = c("LAND A", "LAND B"),
    iso3 = c("AAA", "BBB")
  )
  ambiguous_result <- resolve_epid_country("XYZ-1", ref = crosswalk)
  expect_true(ambiguous_result$ambiguous)
  expect_true(is.na(ambiguous_result$name))
  expect_identical(ambiguous_result$n_matches, 2L)
})

# ---- impute_geo_from_epid: strategies -------------------------------

test_that("self_ref fills a missing value from the same exact EPID", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-001"),
    yronset = c(2023, 2024),
    place.admin.0 = c("NIGERIA", "NIGERIA"),
    place.admin.1 = c("BORNO", "BORNO"),
    place.admin.2 = c("BOSSO", NA)
  )
  res <- impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "self_ref",
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2, c("BOSSO", "BOSSO"))
  expect_identical(
    as.character(res$data$place.admin.2_source),
    c("original", "self_ref")
  )
})

test_that("prefix_match fills via the prefix + year for a different serial", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    yronset = c(2024, 2024),
    place.admin.0 = c("NIGERIA", "NIGERIA"),
    place.admin.1 = c("BORNO", "BORNO"),
    place.admin.2 = c("BOSSO", NA)
  )
  res <- impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2[2], "BOSSO")
  expect_identical(
    as.character(res$data$place.admin.2_source[2]),
    "prefix_match"
  )
})

test_that("prefix_match refuses to fill on an ambiguous prefix", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "NIE-BOS-AAA-24-003"
    ),
    yronset = c(2024, 2024, 2024),
    place.admin.0 = c("NIGERIA", "NIGERIA", "NIGERIA"),
    place.admin.1 = c("BORNO", "YOBE", NA),
    place.admin.2 = c("BOSSO", "KUKAWA", NA)
  )
  res <- impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_true(is.na(res$data$place.admin.2[3]))
  admin2_qa <- res$qa[res$qa$column == "place.admin.2", ]
  expect_true(admin2_qa$n_ambiguous >= 1L)
  expect_identical(
    as.character(res$data$place.admin.2_source[3]),
    "unresolved"
  )
})

test_that("prefix_match parent tie-break resolves an otherwise ambiguous prefix", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "NIE-BOS-AAA-24-003"
    ),
    yronset = c(2024, 2024, 2024),
    place.admin.1 = c("BORNO", "YOBE", "BORNO"),
    place.admin.2 = c("BOSSO", "KUKAWA", NA)
  )
  res <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2[3], "BOSSO")
})

test_that("reference fills a dataset with no names of its own", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    place.admin.2 = c(NA_character_, NA_character_)
  )
  reference <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    place.admin.2 = c("BOSSO", "LUANDA")
  )
  res <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    reference = reference,
    strategies = "reference",
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2, c("BOSSO", "LUANDA"))
  expect_identical(
    as.character(res$data$place.admin.2_source),
    c("reference", "reference")
  )
})

test_that("reference can be keyed on prefix", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    place.admin.2 = c(NA_character_, NA_character_)
  )
  reference <- tibble::tibble(
    prefix = "NIE-BOS-AAA",
    place.admin.2 = "BOSSO"
  )
  res <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    reference = reference,
    strategies = "reference",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2, c("BOSSO", "BOSSO"))
})

test_that("country_prefix resolves Admin0 from a crosswalk", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    place.admin.0 = c(NA_character_, NA_character_)
  )
  country_ref <- tibble::tibble(
    code = c("NIE", "AGO"),
    name = c("NIGERIA", "ANGOLA"),
    iso3 = c("NGA", "AGO")
  )
  res <- impute_geo_from_epid(
    cases,
    admin1_var = NULL,
    admin2_var = NULL,
    guid_vars = NULL,
    country_ref = country_ref,
    strategies = "country_prefix",
    canonicalise = FALSE,
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.0, c("NIGERIA", "ANGOLA"))
  expect_identical(
    as.character(res$data$place.admin.0_source),
    c("country_prefix", "country_prefix")
  )
})

# ---- impute_geo_from_epid: provenance, GUIDs, QA --------------------

test_that("original values are never overwritten", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    yronset = c(2024, 2024),
    place.admin.2 = c("REALPLACE", "OTHERPLACE")
  )
  res <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    verbose = FALSE
  )
  expect_identical(res$data$place.admin.2, c("REALPLACE", "OTHERPLACE"))
  expect_true(all(res$data$place.admin.2_source == "original"))
})

test_that("GUID columns are filled like admin values", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    yronset = c(2024, 2024),
    place.admin.2 = c("BOSSO", "BOSSO"),
    Admin2GUID = c("g-bosso", NA)
  )
  res <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = c(adm2 = "Admin2GUID"),
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_identical(res$data$Admin2GUID, c("g-bosso", "g-bosso"))
})

test_that("QA counts reconcile: missing_before == filled + unresolved", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "AGO-LUA-BBB-24-001"
    ),
    yronset = c(2024, 2024, 2024),
    place.admin.0 = c("NIGERIA", NA, "ANGOLA"),
    place.admin.1 = c("BORNO", NA, "LUANDA"),
    place.admin.2 = c("BOSSO", NA, "LUANDA")
  )
  res <- impute_geo_from_epid(cases, guid_vars = NULL, verbose = FALSE)
  filled <- with(
    res$qa,
    n_filled_self_ref +
      n_filled_prefix_match +
      n_filled_reference +
      n_filled_country_prefix
  )
  expect_equal(res$qa$n_missing_before, filled + res$qa$n_unresolved)
})

# ---- robustness ------------------------------------------------------

test_that("running twice changes nothing on the second pass", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    yronset = c(2024, 2024),
    place.admin.2 = c("BOSSO", NA)
  )
  first <- impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  second <- impute_geo_from_epid(
    first$data,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  expect_identical(second$data$place.admin.2, first$data$place.admin.2)
})

test_that("blank, lowercase, and whitespace EPIDs do not error", {
  cases <- tibble::tibble(
    epid = c("nie-bos-aaa-24-001", "  ", NA, "NIE-BOS-AAA-24-002"),
    yronset = c(2024, 2024, 2024, 2024),
    place.admin.2 = c("BOSSO", NA, NA, NA)
  )
  expect_no_error(
    impute_geo_from_epid(
      cases,
      admin0_var = NULL,
      admin1_var = NULL,
      guid_vars = NULL,
      verbose = FALSE
    )
  )
})

# ---- validation ------------------------------------------------------

test_that("impute_geo_from_epid validates inputs", {
  good <- tibble::tibble(
    epid = "NIE-BOS-AAA-24-001",
    yronset = 2024,
    place.admin.2 = "BOSSO"
  )
  expect_error(
    impute_geo_from_epid(good[0, ], verbose = FALSE),
    "non-empty data frame"
  )
  expect_error(
    impute_geo_from_epid(good, strategies = "made_up", verbose = FALSE),
    "Unknown"
  )
  expect_error(
    impute_geo_from_epid(good, epid_var = "nope", verbose = FALSE),
    "Missing required column"
  )
  expect_error(
    impute_geo_from_epid(good, prefix_length = -1, verbose = FALSE),
    "positive whole number"
  )
})
