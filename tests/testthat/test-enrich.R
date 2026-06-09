# Country enrichment is an always-on step of clean_afp() and clean_es().

testthat::test_that("polis_country_lookup ships the country reference", {
  lk <- polished::polis_country_lookup()
  testthat::expect_s3_class(lk, "tbl_df")
  testthat::expect_true(all(
    c("iso3", "country_actual", "risk_group", "epi_zones") %in% names(lk)
  ))
  testthat::expect_equal(lk$risk_group[lk$iso3 == "AFG"], "Endemic")
})

testthat::test_that("clean_afp always enriches with groupings, polio_type, flags", {
  raw <- data.frame(
    Id = 1:2,
    Epid = c("A-1", "B-2"),
    LastUpdateDate = rep("2024-01-01", 2),
    ParalysisOnsetDate = rep("2024-02-01", 2),
    Admin0Name = c("NIGERIA", "PAKISTAN"),
    CountryISO3Code = c("NGA", "PAK"),
    Classification = c("Discarded", "Confirmed (wild)"),
    PolioVirusTypes = c(NA, "WILD1"),
    SurveillanceTypeName = c("AFP", "AFP"),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw, verbose = FALSE)
  testthat::expect_true(all(
    c(
      "country_actual",
      "risk_group",
      "epi_zones",
      "epi_zones_v2",
      "polio_type",
      "afp_class",
      "afp",
      "npafp",
      "pending_results"
    ) %in%
      names(out)
  ))
  testthat::expect_equal(out$country_actual[out$epid == "A-1"], "Nigeria")
  testthat::expect_equal(out$risk_group[out$epid == "A-1"], "Very High Risk")
  testthat::expect_equal(out$epi_zones[out$epid == "A-1"], "Lake Chad Basin")
  testthat::expect_equal(out$afp_class[out$epid == "A-1"], "Non-polio AFP")
  testthat::expect_equal(out$afp[out$epid == "A-1"], 0L)
  testthat::expect_equal(out$npafp[out$epid == "A-1"], 1L)
  # the WPV1 case reads Type 1 off its classification
  testthat::expect_equal(out$polio_type[out$epid == "B-2"], "Type 1")
  testthat::expect_equal(out$afp_class[out$epid == "B-2"], "AFP-Positive")
})

testthat::test_that("clean_es always enriches with country groupings + polio_type", {
  raw <- data.frame(
    Id = 1:2,
    SampleId = c("E-1", "E-2"),
    LastUpdateDate = rep("2024-03-01", 2),
    CollectionDate = rep("2024-01-05", 2),
    VirusTypes = c("cVDPV2", "NPEV"),
    VdpvClassifications = c("Circulating", NA),
    Admin0Name = c("NIGERIA", "CHAD"),
    CountryISO3Code = c("NGA", "TCD"),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  testthat::expect_true(all(
    c("country_actual", "risk_group", "epi_zones", "polio_type") %in% names(out)
  ))
  testthat::expect_equal(out$country_actual[out$sample_id == "E-1"], "Nigeria")
  testthat::expect_equal(
    out$risk_group[out$sample_id == "E-1"],
    "Very High Risk"
  )
  # polio_type read off classification_all (cVDPV 2 -> Type 2)
  testthat::expect_equal(out$polio_type[out$sample_id == "E-1"], "Type 2")
  # ES has no AFP case flags
  testthat::expect_false("afp_class" %in% names(out))
})
