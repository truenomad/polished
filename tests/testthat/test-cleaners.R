# Each cleaner runs standalone on synthetic data with NO archive present.
# Fixtures use raw POLIS API-style names (no spaces) so janitor + crosswalk
# produce the canonical Snake_Name (e.g. Admin0Name -> admin0name).

afp_raw <- function() {
  data.frame(
    Id = c(1, 1, 2),
    Epid = c("A-1", "A-1", "B-2"),
    LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
    ParalysisOnsetDate = c("2024-01-02", "2024-01-02", "2024-02-03"),
    NotificationDate = c("2024-01-05", "2024-01-05", "2024-02-06"),
    InvestigationDate = c("2024-01-06", "2024-01-06", "2024-02-08"),
    Stool1CollectionDate = c("2024-01-08", "2024-01-08", "2024-02-10"),
    Stool2CollectionDate = c("2024-01-10", "2024-01-10", "2024-02-13"),
    CalculatedAgeInMonth = c("24", "24", "36"),
    Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
    RandomCol = c("a", "b", "c"),
    check.names = FALSE
  )
}

testthat::test_that("clean_afp dedups by id, derives onset, canonicalises", {
  out <- polished::clean_afp(afp_raw())
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(c("year_onset", "month_onset") %in% names(out)))
  testthat::expect_true("adm0" %in% names(out))
  testthat::expect_true("random_col" %in% names(out)) # all columns kept
  testthat::expect_equal(names(out)[1:2], c("id", "epid")) # id role first
  testthat::expect_equal(out$epid[out$id == 1], "A-1")
  testthat::expect_equal(out$year_onset[out$id == 1], 2024)
})

testthat::test_that("clean_afp derives onset year from the real onset column", {
  # ParalysisOnsetDate (-> paralysis_onset_date) is the canonical onset field;
  # year_onset must be derived from it, not the legacy date_onset name.
  out <- polished::clean_afp(afp_raw())
  testthat::expect_true("paralysis_onset_date" %in% names(out))
  testthat::expect_s3_class(out$paralysis_onset_date, "Date")
  testthat::expect_false(any(is.na(out$year_onset)))
})

testthat::test_that("clean_afp derives age, intervals and timeliness", {
  out <- polished::clean_afp(afp_raw())
  testthat::expect_equal(out$age_months[out$id == 1], 24)
  testthat::expect_equal(out$onset_to_notify[out$id == 1], 3)
  testthat::expect_equal(out$notify_to_invest[out$id == 1], 1)
  testthat::expect_equal(out$onset_to_stool1[out$id == 1], 6)
  testthat::expect_equal(out$stool1_to_stool2[out$id == 1], 2)
  testthat::expect_equal(out$timeliness[out$id == 1], "Timely")
  testthat::expect_false(out$needs_60day_followup[out$id == 1])
})

testthat::test_that("clean_afp trims blanks to NA and flags missing stools", {
  raw <- data.frame(
    Id = c(1, 2),
    Epid = c(" A-1 ", "B-2"), # padded
    LastUpdateDate = c("2024-01-01", "2024-01-01"),
    ParalysisOnsetDate = c("2024-02-01", "2024-02-01"),
    Stool1CollectionDate = c("2024-02-05", ""), # empty -> NA
    Stool1Condition = c("Good", ""), # empty -> NA, so stool 1 missing
    Stool2CollectionDate = c("", ""),
    Stool2Condition = c("", ""),
    Admin0Name = c("NIGERIA", "NIGERIA"),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw)
  testthat::expect_equal(out$epid[out$id == 1], "A-1") # trimmed
  testthat::expect_true(out$stool1_missing[out$id == 2])
  testthat::expect_false(out$stool1_missing[out$id == 1])
  testthat::expect_true(all(out$stool2_missing))
  testthat::expect_true(out$stool_missing[out$id == 2])
})

testthat::test_that("clean_afp fuses virus type and classification", {
  raw <- data.frame(
    Id = 1:4,
    Epid = c("A", "B", "C", "D"),
    LastUpdateDate = rep("2024-01-01", 4),
    ParalysisOnsetDate = rep("2024-02-01", 4),
    Admin0Name = rep("NIGERIA", 4),
    Classification = c("Discarded", "Confirmed", "Compatible", "Not an AFP"),
    PolioVirusTypes = c(NA, "VDPV2", NA, NA),
    VdpvClassifications = c(NA, "Circulating", NA, NA),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw)
  # raw classification is preserved untouched
  testthat::expect_equal(out$classification[out$epid == "A"], "Discarded")
  # discarded -> NPAFP, compatible -> COMPATIBLE, not-an-afp -> NOT-AFP
  testthat::expect_equal(out$classification_all[out$epid == "A"], "NPAFP")
  testthat::expect_equal(out$classification_all[out$epid == "C"], "COMPATIBLE")
  testthat::expect_equal(out$classification_all[out$epid == "D"], "NOT-AFP")
  # virus-positive carries the decoded circulating VDPV type
  testthat::expect_equal(out$vtype[out$epid == "B"], "cVDPV 2")
  testthat::expect_equal(out$classification_all[out$epid == "B"], "cVDPV 2")
  testthat::expect_true(all(
    c("vtype_fixed", "sabin1", "sabin2", "sabin3") %in% names(out)
  ))
})

testthat::test_that("classification uses WPV nomenclature, not WILD", {
  raw <- data.frame(
    classification = c("Confirmed (wild)", "Confirmed (wild)", "Discarded"),
    polio_virus_types = c("WILD1", "WILD3", NA),
    vdpv_classifications = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )
  out <- polished::clean_afp_classification(raw)
  testthat::expect_equal(out$classification_all, c("WPV 1", "WPV 3", "NPAFP"))
  testthat::expect_false(any(grepl("WILD", out$classification_all)))
  # the documented WPV1 filter actually matches
  testthat::expect_equal(
    sum(grepl("^WPV 1|^WPV1and", out$classification_all)),
    1
  )
})

testthat::test_that("clean_afp sanitises garbage dates, ages and intervals", {
  raw <- data.frame(
    Id = c(10, 11, 12),
    Epid = c("X-1", "X-2", "X-3"),
    LastUpdateDate = c("2024-01-01", "2024-01-01", "2024-01-01"),
    # X-1: impossible onset year; X-2: onset after notification; X-3: clean
    ParalysisOnsetDate = c("1850-02-01", "2024-02-10", "2024-02-01"),
    NotificationDate = c("2024-02-05", "2024-02-01", "2024-02-04"),
    InvestigationDate = c("2024-02-06", "2024-02-12", "2024-02-05"),
    Stool1CollectionDate = c("2024-02-08", "2024-02-14", "2024-02-06"),
    Stool2CollectionDate = c("2024-02-10", "2024-02-16", "2024-02-08"),
    CalculatedAgeInMonth = c("22841", "30", "40"), # X-1 age ~1900 years
    Admin0Name = c("NIGERIA", "NIGERIA", "NIGERIA"),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw)

  # impossible onset date -> NA, so no interval is fabricated from it
  testthat::expect_true(is.na(out$paralysis_onset_date[out$epid == "X-1"]))
  testthat::expect_true(is.na(out$onset_to_notify[out$epid == "X-1"]))
  # impossible age -> NA, row retained
  testthat::expect_true(is.na(out$age_months[out$epid == "X-1"]))
  testthat::expect_equal(nrow(out), 3L)
  # onset-after-notification is flagged and excluded from timeliness
  testthat::expect_equal(
    out$onset_date_quality[out$epid == "X-2"],
    "Onset after notification"
  )
  testthat::expect_equal(out$timeliness[out$epid == "X-2"], "Unable to Assess")
  # negative interval from the incoherent pair is NA'd, not kept
  testthat::expect_true(is.na(out$onset_to_notify[out$epid == "X-2"]))
  # the clean case is assessed normally
  testthat::expect_equal(out$onset_date_quality[out$epid == "X-3"], "Good")
  testthat::expect_equal(out$timeliness[out$epid == "X-3"], "Timely")
})

testthat::test_that("clean_es runs standalone and normalises admin names", {
  raw <- data.frame(
    Id = c(1, 1, 2),
    EnvSampleId = c("E1", "E1", "E2"),
    LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
    CollectionDate = c("2024-01-05", "2024-01-05", "2024-02-09"),
    Admin0Name = c(
      "REPUBLIQUE DE COTE D IVOIRE",
      "REPUBLIQUE DE COTE D IVOIRE",
      "CHAD"
    ),
    check.names = FALSE
  )
  out <- polished::clean_es(raw)
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$adm0[out$id == 1], "COTE D IVOIRE")
  testthat::expect_true(
    all(c("year_collection", "month_collection") %in% names(out))
  )
  # derived from the real CollectionDate -> collection_date column
  testthat::expect_s3_class(out$collection_date, "Date")
  testthat::expect_false(any(is.na(out$year_collection)))
})

testthat::test_that("clean_sia runs on activity alone", {
  activity <- data.frame(
    Id = c(1, 2),
    SubActivityId = c("S1", "S2"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    DateFrom = c("2024-03-10", "2024-02-12"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  out <- polished::clean_sia(activity)
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true("sub_activity_id" %in% names(out))
  # year_start derived from the real DateFrom -> date_from column
  testthat::expect_true("year_start" %in% names(out))
  testthat::expect_equal(sort(out$year_start), c(2024, 2024))
})

testthat::test_that("clean_virus runs standalone and integrates linkage", {
  raw <- data.frame(
    Id = c(1, 2),
    Epid = c("A-1", "Z-9"),
    LastUpdateDate = c("2024-03-01", "2024-02-01"),
    VirusDate = c("2024-01-02", "2024-02-03"),
    VirusTypeName = c("cVDPV2", "WILD1"),
    Admin0Name = c("NIGERIA", "CHAD"),
    check.names = FALSE
  )
  vout <- polished::clean_virus(raw)
  testthat::expect_equal(nrow(vout), 2L)
  # year_onset derived from the real VirusDate -> virus_date column
  testthat::expect_true("year_onset" %in% names(vout))
  testthat::expect_false(any(is.na(vout$year_onset)))

  cases <- polished::clean_afp(afp_raw())
  out <- polished::clean_virus(raw, cases = cases)
  testthat::expect_equal(out$surveillance_type[out$epid == "A-1"], "human")
  testthat::expect_true(is.na(out$surveillance_type[out$epid == "Z-9"]))
})

testthat::test_that("cleaners reject non-data-frame and empty input", {
  testthat::expect_error(polished::clean_es(1), "data.frame")
  testthat::expect_error(polished::clean_es(data.frame()), "empty")
})
