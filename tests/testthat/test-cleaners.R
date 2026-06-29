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
  testthat::expect_true(all(site10$adm2_guid == "{A2}"))
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

testthat::test_that("clean_es emits admin GUIDs in the braced upper-case form", {
  raw <- data.frame(
    Id = 1:2,
    LastUpdateDate = rep("2024-03-01", 2),
    CollectionDate = rep("2024-01-05", 2),
    Admin0Name = rep("CHAD", 2),
    Admin2GUID = c("{abc-123}", "DEF-456"),
    check.names = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  testthat::expect_equal(sort(out$adm2_guid), c("{ABC-123}", "{DEF-456}"))
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

testthat::test_that("clean_sia combines the sub-activity grain with the parent activity", {
  activity <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2024-03-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
  subactivity <- data.frame(
    Id = c(10, 11),
    SIASubActivityCode = c("S1", "S1"),
    LastModificationDate = c("2024-03-01", "2024-03-01"),
    DateFrom = c("2024-03-10", "2024-03-11"),
    Admin0Name = c("NIGERIA", "NIGERIA"),
    Admin2Name = c("BOSSO", "KUKAWA"),
    check.names = FALSE
  )
  out <- polished::clean_sia(activity, subactivity, verbose = FALSE)
  # the sub-activity is the grain (2 rows), each carrying its parent's vaccine
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_true(all(out$vaccine_type == "bOPV"))
  testthat::expect_setequal(out$adm2, c("BOSSO", "KUKAWA"))
  testthat::expect_true(all(out$year_start == 2024))
})

testthat::test_that("clean_sia bins campaigns into rounds and flags the latest", {
  activity <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2024-04-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
  # one district (same Admin2Guid), three campaigns: two within a few days (one
  # round) then one a month later (a second round)
  subactivity <- data.frame(
    Id = 10:12,
    SIASubActivityCode = "S1",
    LastModificationDate = "2024-04-01",
    DateFrom = c("2024-01-10", "2024-01-12", "2024-02-20"),
    Admin0Name = "NIGERIA",
    Admin2Name = "BOSSO",
    Admin2Guid = "abc",
    check.names = FALSE
  )
  out <- polished::clean_sia(activity, subactivity, verbose = FALSE) |>
    dplyr::arrange(date_from)
  testthat::expect_equal(out$round_num, c(1L, 1L, 2L))
  # last_campaign flags only the most recent campaign in the district
  testthat::expect_equal(out$last_campaign, c(0L, 0L, 1L))
  testthat::expect_true(all(out$max_round_date == as.Date("2024-02-20")))

  # a wide gap threshold collapses the same dates into a single round
  one_round <- polished::clean_sia(
    activity,
    subactivity,
    round_gap_days = 90L,
    verbose = FALSE
  )
  testthat::expect_true(all(one_round$round_num == 1L))
})

testthat::test_that("clean_sia caches by content and reuses on identical calls", {
  activity <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2024-04-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
  subactivity <- data.frame(
    Id = 10:11,
    SIASubActivityCode = "S1",
    LastModificationDate = "2024-04-01",
    DateFrom = c("2024-01-10", "2024-02-20"),
    Admin0Name = "NIGERIA",
    Admin2Name = "BOSSO",
    Admin2Guid = "abc",
    check.names = FALSE
  )
  # a not-yet-existing nested dir exercises the cache-dir creation path
  cache <- file.path(withr::local_tempdir(), "nested")

  fresh <- polished::clean_sia(activity, subactivity, verbose = FALSE)
  # cold miss: computes, creates the dir, writes, and announces the write
  testthat::expect_message(
    cold <- polished::clean_sia(
      activity,
      subactivity,
      cache_dir = cache,
      verbose = TRUE
    ),
    "Cached SIA result"
  )
  testthat::expect_equal(length(list.files(cache)), 1L)
  # a hit returns the same table the fresh run produced and announces the load
  testthat::expect_message(
    warm <- polished::clean_sia(
      activity,
      subactivity,
      cache_dir = cache,
      verbose = TRUE
    ),
    "Loaded cached SIA result"
  )
  testthat::expect_equal(warm, fresh)
  testthat::expect_equal(warm, cold)

  # a different round threshold is a different key -> a second entry, not a stale hit
  polished::clean_sia(
    activity,
    subactivity,
    cache_dir = cache,
    round_gap_days = 90L,
    verbose = FALSE
  )
  testthat::expect_equal(length(list.files(cache)), 2L)
})

testthat::test_that("sia cache write surfaces a failed rename and cleans up", {
  cleaned <- tibble::tibble(id = 1L)
  cache_path <- file.path(withr::local_tempdir(), "x.qs2")
  # a cross-device rename returns FALSE; we should warn and leave no stray files
  testthat::local_mocked_bindings(
    file.rename = function(...) FALSE,
    .package = "base"
  )
  testthat::expect_warning(
    polished:::.sia_cache_write(cleaned, cache_path),
    "Could not write SIA cache"
  )
  testthat::expect_false(file.exists(cache_path))
  testthat::expect_false(file.exists(paste0(cache_path, ".tmp")))
})

testthat::test_that("sia round assignment handles missing dates and absent keys", {
  # a district whose campaigns are all undated: round/flag are NA/0 and the
  # district max is a typed NA date rather than -Inf
  undated <- tibble::tibble(
    adm2_guid = "{Z}",
    vaccine_type = "bOPV",
    date_from = as.Date(c(NA, NA))
  )
  rounds <- polished:::.sia_assign_rounds(undated, gap_days = 21L)
  testthat::expect_true(all(is.na(rounds$round_num)))
  testthat::expect_true(all(is.na(rounds$max_round_date)))
  testthat::expect_true(all(rounds$last_campaign == 0L))

  # a mix of dated and undated rows in one district: the dated row is round 1 and
  # the latest campaign; the undated row gets NA round and is not the latest
  mixed <- tibble::tibble(
    adm2_guid = "{Z}",
    vaccine_type = "bOPV",
    date_from = as.Date(c("2024-01-01", NA))
  )
  mr <- polished:::.sia_assign_rounds(mixed, gap_days = 21L)
  testthat::expect_equal(mr$round_num, c(1L, NA_integer_))
  testthat::expect_equal(mr$last_campaign, c(1L, 0L))
  testthat::expect_equal(
    mr$max_round_date,
    as.Date(c("2024-01-01", "2024-01-01"))
  )

  # absent key columns (and empty data) make round assignment a no-op
  testthat::expect_identical(
    polished:::.sia_assign_rounds(tibble::tibble(x = 1L)),
    tibble::tibble(x = 1L)
  )
  testthat::expect_identical(
    polished:::.sia_assign_rounds(tibble::tibble(
      adm2_guid = character(0),
      vaccine_type = character(0),
      date_from = as.Date(character(0))
    )),
    tibble::tibble(
      adm2_guid = character(0),
      vaccine_type = character(0),
      date_from = as.Date(character(0))
    )
  )
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
      CountryISO3Code = c("NGA", "NGA"),
      VirusIsOrphan = c(TRUE, FALSE),
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
      CountryISO3Code = c("NGA", "TCD"),
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
  # the orphan flag survives from clean_afp into the positives table; ES rows
  # have no orphan source column, so they carry NA
  testthat::expect_true("virus_is_orphan" %in% names(out))
  testthat::expect_equal(wpv$virus_is_orphan, TRUE)
  testthat::expect_true(is.na(vdpv$virus_is_orphan))
  # country_iso3code carries through so the positives slice by country like the
  # other streams (both surviving positives are Nigerian)
  testthat::expect_true("country_iso3code" %in% names(out))
  testthat::expect_equal(wpv$country_iso3code, "NGA")
  testthat::expect_equal(vdpv$country_iso3code, "NGA")
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

testthat::test_that("clean_afp_classification names the real wild serotype in a co-detection", {
  out <- polished::clean_afp_classification(data.frame(
    polio_virus_types = c("WILD1 VDPV2", "WILD3 VDPV2"),
    vdpv_classifications = c("Circulating", "Circulating"),
    classification = c("Confirmed (wild)", "Confirmed (wild)"),
    stringsAsFactors = FALSE
  ))
  testthat::expect_equal(
    out$classification_all,
    c("WPV1andcVDPV 2", "WPV3andcVDPV 2")
  )
})

testthat::test_that("clean_virus keeps two distinct positives with the same projected values", {
  es <- polished::clean_es(
    data.frame(
      Id = 1:2,
      SampleId = c("S1", "S1"),
      LastUpdateDate = rep("2024-03-01", 2),
      CollectionDate = rep("2024-01-05", 2),
      Admin0Name = rep("NIGERIA", 2),
      VirusTypes = rep("cVDPV2", 2),
      VirusTypeName = rep("cVDPV2", 2),
      check.names = FALSE
    ),
    verbose = FALSE
  )
  testthat::expect_equal(
    nrow(polished::clean_virus(es = es, verbose = FALSE)),
    2L
  )
})

testthat::test_that("clean_virus report_date is per-serotype after a co-detection split", {
  cases <- polished::clean_afp(
    data.frame(
      Id = 1,
      Epid = "A-1",
      LastUpdateDate = "2024-03-01",
      ParalysisOnsetDate = "2024-01-02",
      NotificationDate = "2024-01-09",
      PolioVirusTypes = "WILD1 VDPV2",
      VdpvClassification = "Circulating",
      VdpvClassificationChangeDate = "2024-02-15",
      Classification = "Confirmed (wild)",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  out <- polished::clean_virus(
    cases = cases,
    separate_rows = TRUE,
    verbose = FALSE
  )
  testthat::expect_equal(
    out$report_date[out$measurement == "WPV 1"],
    as.Date("2024-01-09")
  )
  testthat::expect_equal(
    out$report_date[out$measurement == "VDPV 2"],
    as.Date("2024-02-15")
  )
})

testthat::test_that(".polis_detections projects positives to core columns, in order", {
  # scrambled order, with two non-core columns that must be dropped
  virus <- tibble::tibble(
    notification_date = as.Date("2024-01-09"),
    adm2 = "MMC",
    epid = "NIE-BOR-MMC-24-001",
    virus_cluster = "C1",
    adm0 = "NIGERIA",
    vtype = "WPV 1",
    surveillance_type = "human",
    adm2_guid = "{G2}",
    latitude = 11.8,
    longitude = 13.1,
    emergence_group = "NIE-BOS-1"
  )
  det <- polished:::.polis_detections(virus)
  # only core columns survive, in .polis_detection_cols order
  expected <- intersect(polished:::.polis_detection_cols, names(virus))
  testthat::expect_identical(names(det), expected)
  # non-core columns are dropped
  testthat::expect_false(any(
    c("notification_date", "virus_cluster") %in% names(det)
  ))
  # a pure projection: rows and values are unchanged (nothing recomputed)
  testthat::expect_equal(nrow(det), nrow(virus))
  testthat::expect_identical(det$epid, virus$epid)
  testthat::expect_identical(det$vtype, virus$vtype)
  testthat::expect_identical(det$adm2_guid, virus$adm2_guid)
})

testthat::test_that(".polis_detections keeps only the columns present", {
  det <- polished:::.polis_detections(tibble::tibble(
    epid = "A-1",
    vtype = "WPV 1"
  ))
  testthat::expect_identical(names(det), c("epid", "vtype"))
})

testthat::test_that(".polis_detections composes on a real clean_virus() table", {
  cases <- polished::clean_afp(
    data.frame(
      Id = 1,
      Epid = "A-1",
      LastUpdateDate = "2024-03-01",
      ParalysisOnsetDate = "2024-01-02",
      NotificationDate = "2024-01-09",
      PolioVirusTypes = "WILD1",
      Classification = "Confirmed (wild)",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  v <- polished::clean_virus(cases = cases, verbose = FALSE)
  det <- polished:::.polis_detections(v)
  testthat::expect_equal(nrow(det), nrow(v))
  testthat::expect_true(all(
    c("epid", "adm0", "surveillance_type", "vtype") %in% names(det)
  ))
  # every detection column reuses an existing positives-table name
  testthat::expect_true(all(names(det) %in% names(v)))
})

testthat::test_that("clean_virus covers no-positive streams, df nOPV ref, and label/guard helpers", {
  # one stream has no positives -> its side collapses to NULL, the other survives
  cases_neg <- polished::clean_afp(
    data.frame(
      Id = 1,
      Epid = "A-1",
      LastUpdateDate = "2024-03-01",
      ParalysisOnsetDate = "2024-01-02",
      Classification = "Discarded",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  es_pos <- polished::clean_es(
    data.frame(
      Id = 1,
      SampleId = "E-1",
      LastUpdateDate = "2024-03-01",
      CollectionDate = "2024-01-05",
      VirusTypes = "cVDPV2",
      VdpvClassifications = "Circulating",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  testthat::expect_setequal(
    polished::clean_virus(
      cases = cases_neg,
      es = es_pos,
      verbose = FALSE
    )$surveillance_type,
    "environmental"
  )

  es_neg <- polished::clean_es(
    data.frame(
      Id = 1,
      SampleId = "E-1",
      LastUpdateDate = "2024-03-01",
      CollectionDate = "2024-01-05",
      VirusTypes = "NPEV",
      IsNPEV = TRUE,
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  cases_pos <- polished::clean_afp(
    data.frame(
      Id = 1,
      Epid = "A-1",
      LastUpdateDate = "2024-03-01",
      ParalysisOnsetDate = "2024-01-02",
      PolioVirusTypes = "WILD1",
      Classification = "Confirmed (wild)",
      Admin0Name = "NIGERIA",
      check.names = FALSE
    ),
    verbose = FALSE
  )
  testthat::expect_setequal(
    polished::clean_virus(
      cases = cases_pos,
      es = es_neg,
      verbose = FALSE
    )$surveillance_type,
    "human"
  )

  # nOPV2 reference supplied as a data frame (not a bare vector)
  flagged <- polished::clean_virus(
    es = polished::clean_es(
      data.frame(
        Id = 1,
        SampleId = "E-1",
        LastUpdateDate = "2024-03-01",
        CollectionDate = "2024-01-05",
        VirusTypes = "cVDPV2",
        VdpvClassifications = "Circulating",
        VdpvEmergenceGroupNames = "NIE-XYZ-1",
        Admin0Name = "NIGERIA",
        check.names = FALSE
      ),
      verbose = FALSE
    ),
    nopv_emergence = data.frame(emergence_group = "NIE-XYZ-1"),
    verbose = FALSE
  )
  testthat::expect_equal(
    flagged$nopv2[flagged$emergence_group == "NIE-XYZ-1"],
    1L
  )

  # pure label/guard helpers (the public path can't supply these states)
  testthat::expect_identical(
    polished:::.virus_split_label(NA_character_),
    NA_character_
  )
  testthat::expect_identical(
    polished:::.virus_split_label("VDPV12and3"),
    c("VDPV 1", "VDPV 2", "VDPV 3")
  )
  testthat::expect_identical(
    nrow(polished:::.virus_keep_positive(data.frame(x = 1))),
    0L
  )
  testthat::expect_identical(
    polished:::.virus_add_report_date(data.frame(x = 1)),
    data.frame(x = 1)
  )
  testthat::expect_identical(
    nrow(polished:::.virus_separate(data.frame(measurement = character(0)))),
    0L
  )
})

testthat::test_that("clean_sia reconciles against an sf shape and guards its date helpers", {
  act <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2024-03-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
  sub <- data.frame(
    Id = 1:2,
    SIASubActivityCode = c("S1", "S1"),
    LastModificationDate = rep("2024-03-01", 2),
    DateFrom = c("2024-03-10", "2024-03-11"),
    Admin0Name = "NIGERIA",
    Admin1Name = "BORNO",
    Admin2Name = c("WEST", "EAST"),
    Admin0GUID = "{A0}",
    Admin1GUID = c("{A1W}", "{A1E}"),
    Admin2GUID = c("{A2W}", "{A2E}"),
    check.names = FALSE
  )
  out <- polished::clean_sia(
    act,
    sub,
    shape = make_district_shape(),
    verbose = FALSE
  )
  testthat::expect_true("geo_source" %in% names(out))
  testthat::expect_equal(nrow(out), 2L)

  # guard branches in the helpers
  testthat::expect_identical(
    polished:::.sia_combine(data.frame(x = 1), data.frame(y = 2)),
    data.frame(y = 2)
  )
  testthat::expect_identical(
    polished:::.sia_parse_dates(data.frame(x = 1)),
    data.frame(x = 1)
  )
  testthat::expect_identical(
    polished:::.sia_add_start_vars(data.frame(x = 1)),
    data.frame(x = 1)
  )
})

testthat::test_that("clean_afp recovers admin from an sf shape (coords + EPID) and a plain long shape", {
  shape <- make_district_shape()
  raw <- data.frame(
    Id = 1:2,
    Epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002"),
    LastUpdateDate = rep("2024-03-01", 2),
    ParalysisOnsetDate = rep("2024-01-02", 2),
    Admin0Name = "NIGERIA",
    Admin1Name = c("BORNO", NA),
    Admin2Name = c("WEST", NA),
    Admin0GUID = "{A0}",
    Admin1GUID = c("{A1W}", NA),
    Admin2GUID = c("{A2W}", NA),
    Longitude = c(0.5, 1.5),
    Latitude = c(0.5, 0.5),
    check.names = FALSE
  )
  testthat::expect_true(
    "geo_source" %in%
      names(polished::clean_afp(raw, shape = shape, verbose = FALSE))
  )

  long <- data.frame(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "WEST",
    adm0_guid = "{A0}",
    adm1_guid = "{A1W}",
    adm2_guid = "{A2W}",
    active_year = c(2024, 9999),
    stringsAsFactors = FALSE
  )
  testthat::expect_true(
    "geo_source" %in%
      names(polished::clean_afp(raw, shape = long, verbose = FALSE))
  )
})

testthat::test_that("clean_afp derives lab-pending, hot case, dual-age and 60-day followup; guards", {
  raw <- data.frame(
    Id = 1,
    Epid = "A-1",
    LastUpdateDate = "2024-03-01",
    ParalysisOnsetDate = "2024-01-02",
    NotificationDate = "2024-01-20",
    Stool1CollectionDate = "2024-01-25",
    Stool2CollectionDate = "2024-01-27",
    FollowupDate = "2024-03-08",
    CalculatedAgeInMonth = NA,
    PersonAgeInMonths = "24",
    Classification = "Pending",
    FinalCultureResult = "Not received in lab",
    ParalysisAsymmetric = "Yes",
    ParalysisOnsetFever = "Yes",
    ParalysisRapidProgress = "Yes",
    Admin0Name = "NIGERIA",
    check.names = FALSE
  )
  out <- polished::clean_afp(raw, verbose = FALSE)
  testthat::expect_equal(out$classification_all, "LAB PENDING")
  testthat::expect_equal(out$hot_case, 1L)
  testthat::expect_equal(out$age_months, 24)
  testthat::expect_true(out$needs_60day_followup)
  testthat::expect_true(out$got_60day_followup)

  # defensive guards reached directly
  testthat::expect_identical(
    polished:::.afp_parse_dates(data.frame(x = 1)),
    data.frame(x = 1)
  )
  timed <- polished:::.afp_add_timeliness(
    data.frame(onset_to_stool1 = 5, onset_to_stool2 = 3, stool1_to_stool2 = 2)
  )
  testthat::expect_true("timeliness" %in% names(timed))
  flags <- polished:::.afp_enrich_flags(
    data.frame(
      classification = "Discarded",
      surveillance_type_name = "AFP",
      polio_virus_types = "cVDPV2",
      stringsAsFactors = FALSE
    )
  )
  testthat::expect_true("npafp" %in% names(flags))
})

testthat::test_that("clean_es runs the full geo recovery pipeline (sf shape, coords, site label, site validation)", {
  raw <- data.frame(
    Id = 1:3,
    SampleId = c("E1", "E2", "E3"),
    SiteId = c(10, 10, 20),
    SiteName = c("Site A", "Site A", "Site B"),
    LastUpdateDate = rep("2024-03-01", 3),
    CollectionDate = rep("2024-01-05", 3),
    Admin0Name = "NIGERIA",
    Admin1Name = c("BORNO", NA, NA),
    Admin2Name = c("WEST", NA, NA),
    Admin0GUID = "{A0}",
    Admin1GUID = c("{A1W}", NA, NA),
    Admin2GUID = c("{A2W}", NA, NA),
    SiteXCoordinate = c(0.5, 0.5, 1.5),
    SiteYCoordinate = c(0.5, 0.5, 0.5),
    check.names = FALSE
  )
  out <- polished::clean_es(
    raw,
    shape = make_district_shape(),
    sites = "Site A",
    verbose = TRUE
  )
  testthat::expect_true(all(c("geo_source", "site") %in% names(out)))
})

testthat::test_that("clean_es reads character truthy flags, nOPV nvaccine, and culture/RT-PCR detection", {
  raw <- data.frame(
    id = 1:2,
    last_update_date = rep("2024-03-01", 2),
    collection_date = rep("2024-01-05", 2),
    adm0 = "NIGERIA",
    virus_types = c(NA, NA),
    is_npev = c("Yes", "No"),
    final_combinedr_rtpc_rresults = c("nOPV2", "PV1"),
    final_cell_culture_result = c("Poliovirus", "L20B"),
    stringsAsFactors = FALSE
  )
  out <- polished::clean_es(raw, verbose = FALSE)
  out <- out[order(out$id), ]
  testthat::expect_equal(out$npev, c(1L, 0L))
  testthat::expect_equal(out$nvaccine, c(1L, 0L))
  testthat::expect_equal(out$ev_detect, c(1L, 1L))

  # date helper guards
  testthat::expect_identical(
    polished:::.es_parse_dates(data.frame(x = 1)),
    data.frame(x = 1)
  )
  testthat::expect_identical(
    polished:::.es_add_collection_vars(data.frame(x = 1)),
    data.frame(x = 1)
  )
})

testthat::test_that("validate_es_sites and es_missingness validate inputs, report, and guard", {
  es <- data.frame(
    site_name = c("SITE A", "SITE B"),
    site_y_coordinate = c(6.5, NA)
  )
  out <- polished::validate_es_sites(es, sites = "SITE A", verbose = TRUE)
  testthat::expect_equal(nrow(attr(out, "polis_new_sites")), 1L)
  testthat::expect_s3_class(
    attr(
      polished::validate_es_sites(
        es,
        sites = data.frame(site_name = "SITE A"),
        verbose = FALSE
      ),
      "polis_new_sites"
    ),
    "tbl_df"
  )
  testthat::expect_identical(
    polished::validate_es_sites(
      data.frame(x = 1),
      sites = "A",
      verbose = FALSE
    ),
    data.frame(x = 1)
  )
  testthat::expect_error(polished::validate_es_sites(1, "A"), "data frame")
  testthat::expect_error(
    polished::validate_es_sites(es, "A", site_col = 1),
    "single character"
  )
  testthat::expect_error(
    polished::validate_es_sites(es, sites = 1),
    "data frame or character"
  )

  testthat::expect_error(polished::es_missingness(1), "data frame")
  testthat::expect_error(polished::es_missingness(data.frame()), "empty")
})

testthat::test_that("shared geo helpers: .geo_miss_admin counts, plain long shape, .es_impute_geo guard", {
  testthat::expect_identical(polished:::.geo_miss_admin(data.frame(x = 1)), 0L)
  testthat::expect_identical(
    polished:::.geo_miss_admin(data.frame(
      adm1 = c(NA, "x"),
      adm2 = c("y", NA)
    )),
    2L
  )

  # clean_sia with a plain (non-sf) long-shape data frame exercises the else branch
  act <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastUpdateDate = "2024-03-01",
    VaccineType = "bOPV",
    check.names = FALSE
  )
  sub <- data.frame(
    Id = 1,
    SIASubActivityCode = "S1",
    LastModificationDate = "2024-03-01",
    DateFrom = "2024-03-10",
    Admin0Name = "NIGERIA",
    Admin1Name = "BORNO",
    Admin2Name = "WEST",
    Admin0GUID = "{A0}",
    Admin1GUID = "{A1W}",
    Admin2GUID = "{A2W}",
    check.names = FALSE
  )
  long <- data.frame(
    adm0 = "NIGERIA",
    adm1 = "BORNO",
    adm2 = "WEST",
    adm0_guid = "{A0}",
    adm1_guid = "{A1W}",
    adm2_guid = "{A2W}",
    active_year = c(2024, 9999),
    stringsAsFactors = FALSE
  )
  testthat::expect_true(
    "geo_source" %in%
      names(polished::clean_sia(act, sub, shape = long, verbose = FALSE))
  )

  # .es_impute_geo is a no-op without the site coordinate columns
  d <- data.frame(year_collection = 2024, adm2_guid = "g")
  testthat::expect_identical(
    polished:::.es_impute_geo(d, make_district_shape()),
    d
  )
})
