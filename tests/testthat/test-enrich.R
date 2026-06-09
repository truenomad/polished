# enrich_afp(): country groupings, polio_type and AFP flags.

testthat::test_that("polis_country_lookup ships the country reference", {
  lk <- polished::polis_country_lookup()
  testthat::expect_s3_class(lk, "tbl_df")
  testthat::expect_true(all(
    c("iso3", "country_actual", "risk_group", "epi_zones") %in% names(lk)
  ))
  testthat::expect_equal(lk$risk_group[lk$iso3 == "AFG"], "Endemic")
})

testthat::test_that("enrich_afp derives country groupings, polio_type and flags", {
  df <- data.frame(
    country_iso3code = c("NGA", "PAK"),
    adm0 = c("NIGERIA", "PAKISTAN"),
    classification = c("Discarded", "Confirmed (wild)"),
    classification_all = c("NPAFP", "WPV 1"),
    polio_virus_types = c(NA, "WILD1"),
    surveillance_type_name = c("AFP", "AFP"),
    stringsAsFactors = FALSE
  )
  out <- polished::enrich_afp(df)
  testthat::expect_equal(out$country_actual, c("Nigeria", "Pakistan"))
  testthat::expect_equal(out$risk_group, c("Very High Risk", "Endemic"))
  testthat::expect_equal(out$epi_zones, c("Lake Chad Basin", "Other"))
  testthat::expect_equal(out$polio_type, c(NA, "Type 1"))
  testthat::expect_equal(out$afp_class, c("Non-polio AFP", "AFP-Positive"))
  testthat::expect_equal(out$afp, c(0L, 1L))
  testthat::expect_equal(out$npafp, c(1L, 0L))
  testthat::expect_equal(out$pending_results, c(FALSE, FALSE))
})

testthat::test_that("clean_afp(enrich = TRUE) appends the enrichment columns", {
  raw <- data.frame(
    Id = 1:2,
    Epid = c("A-1", "B-2"),
    LastUpdateDate = rep("2024-01-01", 2),
    ParalysisOnsetDate = rep("2024-02-01", 2),
    Admin0Name = c("NIGERIA", "PAKISTAN"),
    CountryISO3Code = c("NGA", "PAK"),
    Classification = c("Discarded", "Confirmed (wild)"),
    SurveillanceTypeName = c("AFP", "AFP"),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw, enrich = TRUE, verbose = FALSE)
  testthat::expect_true(all(
    c(
      "country_actual",
      "risk_group",
      "epi_zones",
      "polio_type",
      "afp",
      "npafp"
    ) %in%
      names(out)
  ))
  testthat::expect_equal(out$risk_group[out$epid == "A-1"], "Very High Risk")
})
