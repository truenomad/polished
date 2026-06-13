# clean_human_spec(): the specimen-level (LabSpecimen) cleaner.

testthat::test_that("clean_human_spec dedups, dates, classifies, adequacy", {
  raw <- data.frame(
    Id = c(1, 1, 2),
    SpecimenId = c("S1", "S1", "S2"),
    Epid = c("A-1", "A-1", "B-2"),
    LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
    DateStoolCollected = c("2024-01-05", "2024-01-05", "2024-02-09"),
    VirusTypes = c("cVDPV2", "cVDPV2", NA),
    VdpvClassification = c("Circulating", "Circulating", NA),
    SpecimenStoolConditionName = c("Good", "Good", "Poor"),
    AdequateSpecimen = c("Yes", "Yes", "No"),
    Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
    CountryISO3Code = c("NGA", "NGA", "TCD"),
    check.names = FALSE
  )
  out <- polished::clean_human_spec(raw, verbose = FALSE)
  # dedup by id keeps the latest
  testthat::expect_equal(nrow(out), 2L)
  # collection year/month from the sanitised stool date
  testthat::expect_s3_class(out$date_stool_collected, "Date")
  testthat::expect_equal(out$year_collection[out$id == 1], 2024)
  # singular vdpv_classification still drives the circulating prefix
  testthat::expect_equal(out$classification_all[out$id == 1], "cVDPV 2")
  testthat::expect_equal(out$vtype[out$id == 1], "cVDPV 2")
  # adequacy flag from AdequateSpecimen
  testthat::expect_equal(out$adequate[out$id == 1], 1L)
  testthat::expect_equal(out$adequate[out$id == 2], 0L)
  # always-on country enrichment
  testthat::expect_equal(out$country_actual[out$id == 1], "Nigeria")
})

testthat::test_that("clean_human_spec sanitises implausible lab dates", {
  raw <- data.frame(
    Id = 1:2,
    SpecimenId = c("S1", "S2"),
    LastUpdateDate = rep("2024-03-01", 2),
    DateStoolCollected = c("2024-01-05", "1850-01-01"),
    DateofSequencing = c("2099-01-01", "2024-02-01"),
    Admin0Name = rep("NIGERIA", 2),
    check.names = FALSE
  )
  out <- polished::clean_human_spec(raw, verbose = FALSE)
  out <- out[order(out$id), ]
  # pre-1980 collection and future sequencing date both NA'd
  testthat::expect_true(is.na(out$date_stool_collected[out$id == 2]))
  testthat::expect_true(is.na(out$dateof_sequencing[out$id == 1]))
})

testthat::test_that("clean_human_spec derives lab-turnaround intervals", {
  raw <- data.frame(
    Id = 1:2,
    SpecimenId = c("S1", "S2"),
    LastUpdateDate = rep("2024-03-01", 2),
    DateStoolCollected = c("2024-01-05", "2024-02-01"),
    DateStoolReceivedInLab = c("2024-01-10", NA),
    DateFinalCellCultureResults = c("2024-01-20", NA),
    DateofSequencing = c("2024-02-04", NA),
    Admin0Name = rep("NIGERIA", 2),
    check.names = FALSE
  )
  out <- polished::clean_human_spec(raw, verbose = FALSE)
  out <- out[order(out$id), ]
  testthat::expect_equal(out$collect_to_lab[out$id == 1], 5)
  testthat::expect_equal(out$lab_to_culture[out$id == 1], 10)
  testthat::expect_equal(out$collect_to_seq[out$id == 1], 30)
  # a missing endpoint yields NA, not a fabricated interval
  testthat::expect_true(is.na(out$collect_to_lab[out$id == 2]))
})

testthat::test_that("clean_human_spec reconciles admin GUIDs from a shape", {
  shape <- data.frame(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm0_guid = "{A0}",
    adm1_guid = "{A1}",
    adm2_guid = "{A2}",
    active_year = c(2024, 9999),
    stringsAsFactors = FALSE
  )
  raw <- data.frame(
    Id = 1,
    SpecimenId = "S1",
    Epid = "NIE-BOR-BOS-24-001",
    LastUpdateDate = "2024-03-01",
    DateStoolCollected = "2024-01-05",
    Admin0Name = "NIGERIA",
    Admin1Name = NA_character_,
    Admin2Name = NA_character_,
    Admin0GUID = "{A0}",
    Admin1GUID = NA_character_,
    Admin2GUID = "{A2}",
    check.names = FALSE
  )
  out <- polished::clean_human_spec(raw, shape = shape, verbose = FALSE)
  testthat::expect_equal(out$adm1, "BORNO")
  testthat::expect_equal(out$adm2, "BOSSO")
  testthat::expect_true("geo_source" %in% names(out))
})

testthat::test_that("clean_human_spec recovers district from the parent case by EPID", {
  # a specimen with no district of its own, whose EPID matches a cleaned case
  raw <- data.frame(
    Id = 1,
    SpecimenId = "S1",
    Epid = "NIE-BOR-BOS-24-001",
    LastUpdateDate = "2024-03-01",
    DateStoolCollected = "2024-01-05",
    Admin0Name = "NIGERIA",
    Admin1Name = "BORNO",
    Admin2Name = NA_character_,
    Admin0GUID = "{A0}",
    Admin1GUID = "{A1}",
    Admin2GUID = NA_character_,
    check.names = FALSE
  )
  cases <- tibble::tibble(
    epid = "NIE-BOR-BOS-24-001",
    adm1 = "BORNO",
    adm2 = "BOSSO",
    adm1_guid = "{A1}",
    adm2_guid = "{A2}"
  )
  out <- polished::clean_human_spec(raw, cases = cases, verbose = FALSE)
  testthat::expect_equal(out$adm2, "BOSSO")
  testthat::expect_equal(out$adm2_guid, "{A2}")

  # without the case reference the district stays missing (nothing to borrow);
  # the all-NA adm2 column may be dropped entirely by drop_empty_cols
  out_nocase <- polished::clean_human_spec(raw, verbose = FALSE)
  testthat::expect_true(
    !"adm2" %in% names(out_nocase) || is.na(out_nocase[["adm2"]])
  )
})

testthat::test_that("clean_human_spec verbose run reconciles via sf shape + fills from cases; guards", {
  spec <- data.frame(
    Id = 1:2,
    SpecimenId = c("S1", "S2"),
    Epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    LastUpdateDate = rep("2024-03-01", 2),
    DateStoolCollected = rep("2024-01-05", 2),
    Admin0Name = rep("NIGERIA", 2),
    Admin1Name = c("BORNO", NA),
    Admin2Name = c("WEST", NA),
    Admin0GUID = "{A0}",
    Admin1GUID = c("{A1W}", NA),
    Admin2GUID = c("{A2W}", NA),
    AdequateSpecimen = c("Yes", "No"),
    check.names = FALSE
  )
  cases <- data.frame(
    epid = "NIE-BOS-AAA-24-002",
    adm1 = "BORNO",
    adm2 = "WEST",
    stringsAsFactors = FALSE
  )
  out <- polished::clean_human_spec(
    spec,
    shape = make_district_shape(),
    cases = cases,
    verbose = TRUE
  )
  testthat::expect_true(all(c("geo_source", "adequate") %in% names(out)))

  # guard helpers: no date cols, no collection date, no overlap, all ambiguous
  testthat::expect_identical(
    polished:::.spec_parse_dates(data.frame(x = 1)),
    data.frame(x = 1)
  )
  testthat::expect_identical(
    polished:::.spec_add_collection_vars(data.frame(x = 1)),
    data.frame(x = 1)
  )
  d <- data.frame(epid = "A-1", x = 1, stringsAsFactors = FALSE)
  testthat::expect_identical(
    polished:::.spec_fill_from_cases(d, data.frame(epid = "A-1")),
    d
  )
  ambiguous <- data.frame(
    epid = c("d1", "d1"),
    adm1 = c("X", "Y"),
    adm2 = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  spec2 <- data.frame(
    epid = "d1",
    adm1 = NA_character_,
    adm2 = NA_character_,
    stringsAsFactors = FALSE
  )
  testthat::expect_true(
    is.na(polished:::.spec_fill_from_cases(spec2, ambiguous)$adm2)
  )
})
