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

testthat::test_that("clean_es classification uses AFP WPV/cVDPV vocabulary", {
  raw <- data.frame(
    Id = 1:5,
    EnviroSampleManualEditId = 1:5,
    LastUpdateDate = rep("2024-03-01", 5),
    CollectionDate = rep("2024-01-05", 5),
    VirusTypes = c(
      "cVDPV2",
      "WILD1",
      "NPEV, VACCINE3",
      "VACCINE1, VACCINE3, WILD1",
      NA
    ),
    VdpvClassifications = c("Circulating", NA, NA, NA, NA),
    IsNPEV = c(NA, NA, TRUE, NA, NA),
    IsNegative = c(NA, NA, NA, NA, TRUE),
    Admin0Name = rep("NIGERIA", 5),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  out <- out[order(out$id), ]
  # AFP-identical vocabulary: a circulating VDPV2 -> "cVDPV 2", wild -> "WPV 1"
  testthat::expect_equal(out$classification_all[out$id == 1], "cVDPV 2")
  testthat::expect_equal(out$classification_all[out$id == 2], "WPV 1")
  # vaccine-only -> SABIN; NPEV present too but Sabin takes precedence
  testthat::expect_equal(out$classification_all[out$id == 3], "SABIN")
  testthat::expect_equal(out$sabin3[out$id == 3], 1L)
  testthat::expect_equal(out$npev[out$id == 3], 1L)
  # wild + vaccine co-detection -> the wild label wins for classification
  testthat::expect_equal(out$classification_all[out$id == 4], "WPV 1")
  testthat::expect_equal(out$sabin1[out$id == 4], 1L)
  # tested negative, never typed -> NEGATIVE, and sabin is NA (untyped)
  testthat::expect_equal(out$classification_all[out$id == 5], "NEGATIVE")
  testthat::expect_true(is.na(out$sabin1[out$id == 5]))
  # one filter works across both streams
  testthat::expect_equal(
    sum(grepl("WPV|cVDPV", out$classification_all)),
    3L
  )
})

testthat::test_that("clean_es reads logical Yes/No detection columns", {
  raw <- data.frame(
    Id = 1:2,
    LastUpdateDate = rep("2024-03-01", 2),
    CollectionDate = rep("2024-01-05", 2),
    VirusTypes = c("NPEV", NA),
    IsNPEV = c(TRUE, NA),
    nVACCINE2 = c(TRUE, NA),
    Admin0Name = rep("CHAD", 2),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  out <- out[order(out$id), ]
  testthat::expect_equal(out$npev, c(1L, 0L))
  testthat::expect_equal(out$nvaccine, c(1L, 0L))
  testthat::expect_equal(out$ev_detect, c(1L, 0L))
})

testthat::test_that("clean_es reconciles admin GUIDs from a shape (like clean_afp)", {
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
    Id = 1:2,
    LastUpdateDate = rep("2024-03-01", 2),
    CollectionDate = rep("2024-01-05", 2),
    Admin0Name = c("NIGERIA", "NIGERIA"),
    Admin1Name = c(NA, NA),
    Admin2Name = c(NA, NA),
    Admin0GUID = c("{A0}", "{A0}"),
    Admin1GUID = c(NA, NA),
    Admin2GUID = c("{A2}", "{A2}"),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, shape = shape, verbose = FALSE)
  # reconciliation keyed on year_collection filled the names from the GUID
  testthat::expect_equal(unique(out$adm1), "BORNO")
  testthat::expect_equal(unique(out$adm2), "BOSSO")
  testthat::expect_true("geo_source" %in% names(out))
})

testthat::test_that("clean_es fills missing admin from unambiguous same-site samples", {
  raw <- data.frame(
    Id = 1:4,
    SampleId = paste0("S", 1:4),
    SiteId = c(10, 10, 10, 20),
    LastUpdateDate = rep("2024-03-01", 4),
    CollectionDate = rep("2024-01-05", 4),
    Admin0Name = rep("NIGERIA", 4),
    Admin2Name = c("BOSSO", NA, NA, NA),
    Admin2GUID = c("{A2}", NA, NA, NA),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, impute_geo = TRUE, verbose = FALSE)
  site10 <- out[out$site_id == 10, ]
  # site 10 maps unambiguously to {A2} -> its missing rows are recovered
  testthat::expect_true(all(site10$adm2_guid == "a2"))
  testthat::expect_true(all(site10$adm2 == "BOSSO"))
  testthat::expect_equal(
    out$geo_source[out$site_id == 10 & out$id %in% 2:3],
    c("site_match", "site_match")
  )
  # site 20 never carries a GUID anywhere -> left missing, not guessed
  testthat::expect_true(is.na(out$adm2_guid[out$site_id == 20]))
})

testthat::test_that("clean_es does not fill from an ambiguous site (two GUIDs)", {
  raw <- data.frame(
    Id = 1:3,
    SiteId = c(10, 10, 10),
    LastUpdateDate = rep("2024-03-01", 3),
    CollectionDate = rep("2024-01-05", 3),
    Admin0Name = rep("NIGERIA", 3),
    Admin2GUID = c("{A2}", "{B2}", NA), # site 10 maps to two districts
    check.names = FALSE
  )
  out <- polished::clean_es(raw, impute_geo = TRUE, verbose = FALSE)
  # the ambiguous site is left untouched rather than guessed
  testthat::expect_true(is.na(out$adm2_guid[out$id == 3]))
})

testthat::test_that("clean_es canonicalises admin GUIDs", {
  raw <- data.frame(
    Id = 1:2,
    LastUpdateDate = rep("2024-03-01", 2),
    CollectionDate = rep("2024-01-05", 2),
    Admin0Name = rep("CHAD", 2),
    Admin2GUID = c("{ABC-123}", "{DEF-456}"),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  testthat::expect_equal(sort(out$adm2_guid), c("abc-123", "def-456"))
})

testthat::test_that("validate_es_sites flags unknown sites without dropping", {
  es <- data.frame(
    site_name = c("SITE A", "SITE B", "SITE C"),
    site_y_coordinate = c(6.5, NA, 7.1)
  )
  out <- polished::validate_es_sites(es, sites = "SITE A", verbose = FALSE)
  flagged <- attr(out, "polis_new_sites")
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_setequal(flagged$site_name, c("SITE B", "SITE C"))
  testthat::expect_true(flagged$no_coords[flagged$site_name == "SITE B"])
})

testthat::test_that("es_missingness summarises missingness, most-missing first", {
  es <- data.frame(
    collection_date = as.Date(c("2024-01-01", NA, NA)),
    adm0 = c("CHAD", "CHAD", "CHAD"),
    classification_all = c(NA, "NEGATIVE", "NEGATIVE")
  )
  mm <- polished::es_missingness(es)
  testthat::expect_s3_class(mm, "tbl_df")
  testthat::expect_equal(mm$variable[1], "collection_date")
  testthat::expect_equal(
    mm$pct_missing[mm$variable == "collection_date"],
    round(200 / 3, 2)
  )
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

testthat::test_that("clean_virus needs at least one cleaned stream", {
  testthat::expect_error(polished::clean_virus(), "cases.*es|es.*cases")
})

testthat::test_that("clean_virus builds positives from cleaned cases and ES", {
  cases <- polished::clean_afp(
    data.frame(
      Id = 1:2,
      Epid = c("A-1", "B-2"),
      LastUpdateDate = rep("2024-03-01", 2),
      ParalysisOnsetDate = c("2024-01-02", "2024-02-03"),
      NotificationDate = c("2024-01-09", "2024-02-10"),
      PolioVirusTypes = c("WILD1", NA),
      Classification = c("Confirmed (wild)", "Discarded"),
      Admin0Name = c("NIGERIA", "NIGERIA"),
      check.names = FALSE
    ),
    verbose = FALSE
  )
  es <- polished::clean_es(
    data.frame(
      Id = 1:2,
      SampleId = c("E-1", "E-2"),
      LastUpdateDate = rep("2024-03-01", 2),
      CollectionDate = c("2024-01-05", "2024-02-09"),
      VirusTypes = c("cVDPV2", "NPEV"),
      VdpvClassifications = c("Circulating", NA),
      VdpvClassificationChangeDate = c("2024-02-01", NA),
      Admin0Name = c("NIGERIA", "CHAD"),
      check.names = FALSE
    ),
    verbose = FALSE
  )
  out <- polished::clean_virus(cases = cases, es = es, verbose = FALSE)

  # only the poliovirus positives survive: WPV1 case + cVDPV2 sample (the
  # Discarded case and the NPEV-only sample are not positives)
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$measurement, c("WPV 1", "cVDPV 2"))
  testthat::expect_setequal(
    out$surveillance_type,
    c("human", "environmental")
  )
  # report_date: WPV -> notification date, VDPV -> classification-change date
  wpv <- out[out$measurement == "WPV 1", ]
  vdpv <- out[out$measurement == "cVDPV 2", ]
  testthat::expect_equal(wpv$report_date, as.Date("2024-01-09"))
  testthat::expect_equal(vdpv$report_date, as.Date("2024-02-01"))
  # ES epid is the sample id; human epid is the case epid
  testthat::expect_true("E-1" %in% out$epid && "A-1" %in% out$epid)
})

testthat::test_that("clean_virus separate_rows splits co-detections per serotype", {
  cases <- polished::clean_afp(
    data.frame(
      Id = 1,
      Epid = "A-1",
      LastUpdateDate = "2024-03-01",
      ParalysisOnsetDate = "2024-01-02",
      PolioVirusTypes = "WILD1, VDPV2",
      VdpvClassifications = "Circulating",
      Classification = "Confirmed (wild)",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  # the case is a WPV1 + cVDPV2 co-detection -> one fused row by default
  fused <- polished::clean_virus(cases = cases, verbose = FALSE)
  testthat::expect_equal(nrow(fused), 1L)
  testthat::expect_equal(fused$measurement, "WPV1andcVDPV 2")
  # split -> one row per serotype, both for the same epid
  split <- polished::clean_virus(
    cases = cases,
    separate_rows = TRUE,
    verbose = FALSE
  )
  testthat::expect_equal(nrow(split), 2L)
  testthat::expect_setequal(split$measurement, c("WPV 1", "cVDPV 2"))
  testthat::expect_true(all(split$epid == "A-1"))
  testthat::expect_equal(split$classification_all, split$measurement)
})

testthat::test_that("clean_virus flags nOPV2 from an emergence reference", {
  es <- polished::clean_es(
    data.frame(
      Id = 1:2,
      SampleId = c("E-1", "E-2"),
      LastUpdateDate = rep("2024-03-01", 2),
      CollectionDate = rep("2024-01-05", 2),
      VirusTypes = c("cVDPV2", "cVDPV2"),
      VdpvClassifications = c("Circulating", "Circulating"),
      VdpvEmergenceGroupNames = c("NIE-XYZ-1", "OTHER-1"),
      Admin0Name = rep("NIGERIA", 2),
      check.names = FALSE
    ),
    verbose = FALSE
  )
  out <- polished::clean_virus(
    es = es,
    nopv_emergence = "NIE-XYZ-1",
    verbose = FALSE
  )
  testthat::expect_equal(out$nopv2[out$emergence_group == "NIE-XYZ-1"], 1L)
  testthat::expect_equal(out$nopv2[out$emergence_group == "OTHER-1"], 0L)
})

testthat::test_that("cleaners reject non-data-frame and empty input", {
  testthat::expect_error(polished::clean_es(1), "data.frame")
  testthat::expect_error(polished::clean_es(data.frame()), "empty")
})
