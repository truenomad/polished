# Tests for the EPID-driven geography cleaner. Synthetic data only.

# ---- epid_split ------------------------------------------------------

testthat::test_that("epid_split parses canonical, short, and NA EPIDs", {
  split_result <- polished::epid_split(c("NIE-BOS-XYZ-24-001", "AGO-LUA", NA))

  testthat::expect_s3_class(split_result, "tbl_df")
  testthat::expect_named(
    split_result,
    c("country", "province", "district", "year", "serial")
  )
  testthat::expect_identical(split_result$country, c("NIE", "AGO", NA))
  testthat::expect_identical(split_result$district, c("XYZ", NA, NA))
  testthat::expect_identical(split_result$serial, c("001", NA, NA))
})

testthat::test_that("epid_split honours extra = 'merge' and never errors on junk", {
  merged <- polished::epid_split("A-B-C-D-E-F-G", extra = "merge")
  testthat::expect_identical(merged$serial, "E-F-G")

  testthat::expect_no_error(polished::epid_split(c("", "   ", "-", "A--B")))
})

# ---- epid_country_code ----------------------------------------------

testthat::test_that("epid_country_code returns the 3-char prefix and respects upper", {
  testthat::expect_identical(
    polished::epid_country_code(c("NIE-BOS-1", "ago-lua-1", NA)),
    c("NIE", "AGO", NA)
  )
  testthat::expect_identical(
    polished::epid_country_code("ago-lua", upper = FALSE),
    "ago"
  )
  testthat::expect_identical(polished::epid_country_code("  NIE-1"), "NIE")
})

# ---- epid_prefix -----------------------------------------------------

testthat::test_that("epid_prefix takes the leading characters and NA-pads blanks", {
  testthat::expect_identical(
    polished::epid_prefix("NIE-BOS-XYZ-24-001", length = 11),
    "NIE-BOS-XYZ"
  )
  testthat::expect_identical(
    polished::epid_prefix(c("  ", NA)),
    c(NA_character_, NA_character_)
  )
})

# ---- epid_strip_contact ---------------------------------------------

testthat::test_that("epid_strip_contact splits contact markers off the base EPID", {
  contact_split <- polished::epid_strip_contact(c(
    "NIE-BOS-XYZ-24-001",
    "NIE-BOS-XYZ-24-001CC",
    "NIE-BOS-XYZ-24-001-HC",
    "NIE-BOS-XYZ-24-001C2"
  ))
  testthat::expect_identical(
    contact_split$epid_base,
    rep("NIE-BOS-XYZ-24-001", 4)
  )
  testthat::expect_identical(
    contact_split$contact_code,
    c(NA, "CC", "HC", "C2")
  )
})

# ---- build_admin_ref -------------------------------------------------

testthat::test_that("build_admin_ref keeps the most-recent non-NA value per EPID", {
  cases <- tibble::tibble(
    epid = c("A-1", "A-1", "A-1", "B-2"),
    year = c(2022, 2024, 2023, 2024),
    district = c("OLD", NA, "MID", "LUANDA")
  )
  ref <- polished::build_admin_ref(cases, "district")

  testthat::expect_setequal(ref$epid, c("A-1", "B-2"))
  testthat::expect_identical(ref$district[ref$epid == "A-1"], "MID")
})

testthat::test_that("build_admin_ref aborts on a missing column", {
  testthat::expect_error(
    polished::build_admin_ref(tibble::tibble(epid = "A-1"), "district"),
    "Missing column"
  )
})

# ---- build_prefix_ref ------------------------------------------------

testthat::test_that("build_prefix_ref is unique-or-NA with an n_candidates count", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2", "NIE-BOS-BBB-1"),
    year = c(2024, 2024, 2024),
    district = c("BOSSO", "BOSSO", "BIRNI")
  )
  ref <- polished::build_prefix_ref(cases, "district", prefix_length = 11)

  unique_row <- ref[ref$prefix == "NIE-BOS-AAA", ]
  testthat::expect_identical(unique_row$n_candidates, 1L)
  testthat::expect_identical(unique_row$district, "BOSSO")
})

testthat::test_that("build_prefix_ref returns NA when a prefix has rival values", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2"),
    year = c(2024, 2024),
    district = c("BOSSO", "KUKAWA")
  )
  ref <- polished::build_prefix_ref(cases, "district", prefix_length = 11)
  testthat::expect_identical(ref$n_candidates, 2L)
  testthat::expect_true(is.na(ref$district))
})

# ---- resolve_epid_country -------------------------------------------

testthat::test_that("resolve_epid_country returns the raw code when ref is NULL", {
  country_result <- polished::resolve_epid_country(c("NIE-1", "AGO-1"))
  testthat::expect_identical(country_result$code, c("NIE", "AGO"))
  testthat::expect_false(any(country_result$resolved))
})

testthat::test_that("resolve_epid_country matches on code or ISO3", {
  crosswalk <- tibble::tibble(
    code = c("NIE", "AGO"),
    name = c("NIGERIA", "ANGOLA"),
    iso3 = c("NGA", "AGO")
  )
  by_code <- polished::resolve_epid_country("NIE-BOS-1", ref = crosswalk)
  testthat::expect_identical(by_code$name, "NIGERIA")
  testthat::expect_true(by_code$resolved)

  by_iso3 <- polished::resolve_epid_country("NGA-BOS-1", ref = crosswalk)
  testthat::expect_identical(by_iso3$name, "NIGERIA")
})

testthat::test_that("resolve_epid_country flags codes that map to >1 name", {
  crosswalk <- tibble::tibble(
    code = c("XYZ", "XYZ"),
    name = c("LAND A", "LAND B"),
    iso3 = c("AAA", "BBB")
  )
  ambiguous_result <- polished::resolve_epid_country("XYZ-1", ref = crosswalk)
  testthat::expect_true(ambiguous_result$ambiguous)
  testthat::expect_true(is.na(ambiguous_result$name))
  testthat::expect_identical(ambiguous_result$n_matches, 2L)
})

# ---- impute_geo_from_epid: strategies -------------------------------

testthat::test_that("self_ref fills a missing value from the same exact EPID", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-001"),
    year_onset = c(2023, 2024),
    adm0 = c("NIGERIA", "NIGERIA"),
    adm1 = c("BORNO", "BORNO"),
    adm2 = c("BOSSO", NA)
  )
  res <- polished::impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "self_ref",
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2, c("BOSSO", "BOSSO"))
  testthat::expect_identical(
    as.character(res$data$adm2_source),
    c("original", "self_ref")
  )
})

testthat::test_that("prefix_match fills via the prefix + year for a different serial", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024),
    adm0 = c("NIGERIA", "NIGERIA"),
    adm1 = c("BORNO", "BORNO"),
    adm2 = c("BOSSO", NA)
  )
  res <- polished::impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2[2], "BOSSO")
  testthat::expect_identical(
    as.character(res$data$adm2_source[2]),
    "prefix_match"
  )
})

testthat::test_that("prefix_match refuses to fill on an ambiguous prefix", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "NIE-BOS-AAA-24-003"
    ),
    year_onset = c(2024, 2024, 2024),
    adm0 = c("NIGERIA", "NIGERIA", "NIGERIA"),
    adm1 = c("BORNO", "YOBE", NA),
    adm2 = c("BOSSO", "KUKAWA", NA)
  )
  res <- polished::impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_true(is.na(res$data$adm2[3]))
  admin2_qa <- res$qa[res$qa$column == "adm2", ]
  testthat::expect_true(admin2_qa$n_ambiguous >= 1L)
  testthat::expect_identical(
    as.character(res$data$adm2_source[3]),
    "unresolved"
  )
})

testthat::test_that("prefix_match parent tie-break resolves an otherwise ambiguous prefix", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "NIE-BOS-AAA-24-003"
    ),
    year_onset = c(2024, 2024, 2024),
    adm1 = c("BORNO", "YOBE", "BORNO"),
    adm2 = c("BOSSO", "KUKAWA", NA)
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2[3], "BOSSO")
})

testthat::test_that("reference fills a dataset with no names of its own", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    adm2 = c(NA_character_, NA_character_)
  )
  reference <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    adm2 = c("BOSSO", "LUANDA")
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    reference = reference,
    strategies = "reference",
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2, c("BOSSO", "LUANDA"))
  testthat::expect_identical(
    as.character(res$data$adm2_source),
    c("reference", "reference")
  )
})

testthat::test_that("reference can be keyed on prefix", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    adm2 = c(NA_character_, NA_character_)
  )
  reference <- tibble::tibble(
    prefix = "NIE-BOS-AAA",
    adm2 = "BOSSO"
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    reference = reference,
    strategies = "reference",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2, c("BOSSO", "BOSSO"))
})

testthat::test_that("country_prefix resolves Admin0 from a crosswalk", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "AGO-LUA-BBB-24-001"),
    adm0 = c(NA_character_, NA_character_)
  )
  country_ref <- tibble::tibble(
    code = c("NIE", "AGO"),
    name = c("NIGERIA", "ANGOLA"),
    iso3 = c("NGA", "AGO")
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin1_var = NULL,
    admin2_var = NULL,
    guid_vars = NULL,
    country_ref = country_ref,
    strategies = "country_prefix",
    canonicalise = FALSE,
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm0, c("NIGERIA", "ANGOLA"))
  testthat::expect_identical(
    as.character(res$data$adm0_source),
    c("country_prefix", "country_prefix")
  )
})

# ---- impute_geo_from_epid: provenance, GUIDs, QA --------------------

testthat::test_that("original values are never overwritten", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024),
    adm2 = c("REALPLACE", "OTHERPLACE")
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    verbose = FALSE
  )
  testthat::expect_identical(
    res$data$adm2,
    c("REALPLACE", "OTHERPLACE")
  )
  testthat::expect_true(all(res$data$adm2_source == "original"))
})

testthat::test_that("GUID columns are filled like admin values", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024),
    adm2 = c("BOSSO", "BOSSO"),
    adm2_guid = c("g-bosso", NA)
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = c(adm2 = "adm2_guid"),
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_identical(res$data$adm2_guid, c("g-bosso", "g-bosso"))
})

testthat::test_that("QA counts reconcile: missing_before == filled + unresolved", {
  cases <- tibble::tibble(
    epid = c(
      "NIE-BOS-AAA-24-001",
      "NIE-BOS-AAA-24-002",
      "AGO-LUA-BBB-24-001"
    ),
    year_onset = c(2024, 2024, 2024),
    adm0 = c("NIGERIA", NA, "ANGOLA"),
    adm1 = c("BORNO", NA, "LUANDA"),
    adm2 = c("BOSSO", NA, "LUANDA")
  )
  res <- polished::impute_geo_from_epid(
    cases,
    guid_vars = NULL,
    verbose = FALSE
  )
  filled <- with(
    res$qa,
    n_filled_self_ref +
      n_filled_prefix_match +
      n_filled_reference +
      n_filled_country_prefix
  )
  testthat::expect_equal(res$qa$n_missing_before, filled + res$qa$n_unresolved)
})

# ---- robustness ------------------------------------------------------

testthat::test_that("running twice changes nothing on the second pass", {
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024),
    adm2 = c("BOSSO", NA)
  )
  first <- polished::impute_geo_from_epid(
    cases,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  second <- polished::impute_geo_from_epid(
    first$data,
    admin0_var = NULL,
    admin1_var = NULL,
    guid_vars = NULL,
    strategies = "prefix_match",
    prefix_length = 11,
    verbose = FALSE
  )
  testthat::expect_identical(
    second$data$adm2,
    first$data$adm2
  )
})

testthat::test_that("blank, lowercase, and whitespace EPIDs do not error", {
  cases <- tibble::tibble(
    epid = c("nie-bos-aaa-24-001", "  ", NA, "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024, 2024, 2024),
    adm2 = c("BOSSO", NA, NA, NA)
  )
  testthat::expect_no_error(
    polished::impute_geo_from_epid(
      cases,
      admin0_var = NULL,
      admin1_var = NULL,
      guid_vars = NULL,
      verbose = FALSE
    )
  )
})

# ---- validation ------------------------------------------------------

testthat::test_that("impute_geo_from_epid validates inputs", {
  good <- tibble::tibble(
    epid = "NIE-BOS-AAA-24-001",
    year_onset = 2024,
    adm2 = "BOSSO"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(good[0, ], verbose = FALSE),
    "non-empty data frame"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      strategies = "made_up",
      verbose = FALSE
    ),
    "Unknown"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(good, epid_var = "nope", verbose = FALSE),
    "Missing required column"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(good, prefix_length = -1, verbose = FALSE),
    "positive whole number"
  )
})

testthat::test_that("EPID reference helpers cover aborts, region filter, empty candidates", {
  testthat::expect_error(
    polished::build_prefix_ref(tibble::tibble(epid = "A-1"), "district"),
    "Missing column"
  )
  testthat::expect_error(
    polished::resolve_epid_country("NIE-1", ref = tibble::tibble(code = "NIE")),
    "missing column"
  )
  ref <- tibble::tibble(
    code = c("NIE", "NIE"),
    name = c("NIGERIA", "NIGERIA"),
    iso3 = c("NGA", "NGA"),
    region = c("AFRO", "EMRO")
  )
  reg <- polished::resolve_epid_country(
    "NIE-1",
    ref = ref,
    region = "AFRO",
    region_var = "region"
  )
  testthat::expect_identical(reg$name, "NIGERIA")
  testthat::expect_true(is.na(
    polished:::.epid_resolve_candidates(
      c(NA_character_, NA_character_),
      c(NA, NA),
      NA_character_
    )
  ))
})

testthat::test_that("impute_geo_from_epid prints a verbose report and validates every argument", {
  good <- tibble::tibble(
    epid = "NIE-BOS-AAA-24-001",
    year_onset = 2024,
    adm2 = "BOSSO"
  )

  # verbose run that leaves a cell unresolved and hits an ambiguous country code
  cases <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002", "ZZZ-XX-1"),
    year_onset = c(2024, 2024, 2024),
    adm0 = c("NIGERIA", NA, NA),
    adm2 = c("BOSSO", NA, NA)
  )
  country_ref <- tibble::tibble(
    code = c("NIE", "ZZZ", "ZZZ"),
    name = c("NIGERIA", "LAND A", "LAND B"),
    iso3 = c("NGA", "AAA", "BBB")
  )
  res <- polished::impute_geo_from_epid(
    cases,
    admin1_var = NULL,
    guid_vars = NULL,
    country_ref = country_ref,
    strategies = c("self_ref", "prefix_match", "country_prefix"),
    verbose = TRUE
  )
  testthat::expect_s3_class(res$qa, "tbl_df")

  # verbose run where everything resolves -> the "all resolved" success branch
  full <- tibble::tibble(
    epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    year_onset = c(2024, 2024),
    adm2 = c("BOSSO", NA)
  )
  testthat::expect_no_error(
    polished::impute_geo_from_epid(
      full,
      admin0_var = NULL,
      admin1_var = NULL,
      guid_vars = NULL,
      strategies = "prefix_match",
      verbose = TRUE
    )
  )

  # ---- remaining validation aborts ----
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      strategies = character(0),
      verbose = FALSE
    ),
    "at least one strategy"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(good, year_window = -1, verbose = FALSE),
    "non-negative"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(good, sep = "", verbose = FALSE),
    "sep"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      guid_vars = c(bad = "g"),
      verbose = FALSE
    ),
    "named with"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      admin0_var = NULL,
      admin1_var = NULL,
      admin2_var = NULL,
      guid_vars = NULL,
      verbose = FALSE
    ),
    "No target columns"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      admin0_var = NULL,
      admin1_var = NULL,
      guid_vars = NULL,
      reference = 1L,
      verbose = FALSE
    ),
    "data frame or"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      good,
      admin0_var = NULL,
      admin1_var = NULL,
      guid_vars = NULL,
      reference = tibble::tibble(x = 1),
      verbose = FALSE
    ),
    "epid"
  )
  c0 <- tibble::tibble(epid = "NIE-1", adm0 = NA_character_)
  testthat::expect_error(
    polished::impute_geo_from_epid(
      c0,
      admin1_var = NULL,
      admin2_var = NULL,
      guid_vars = NULL,
      country_ref = 1L,
      strategies = "country_prefix",
      verbose = FALSE
    ),
    "data frame or"
  )
  testthat::expect_error(
    polished::impute_geo_from_epid(
      c0,
      admin1_var = NULL,
      admin2_var = NULL,
      guid_vars = NULL,
      country_ref = tibble::tibble(code = "X"),
      strategies = "country_prefix",
      verbose = FALSE
    ),
    "missing column"
  )
})
