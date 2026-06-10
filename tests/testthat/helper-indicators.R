# Synthetic fixtures for the indicator-catalogue tests. Hand-checkable fake rows
# only -- never real line-lists. Built so a single calc_polio_indicators() run
# exercises every generator and indicator family at once.

# AFP cases: one country (G0) with two provinces (P1/P2) and four districts
# (D1..D4). Classifications, stool conditions, doses, timeliness intervals and
# surveillance types are chosen so each indicator has a hand-checkable answer.
make_indicator_cases <- function() {
  tibble::tibble(
    classification_all = c(
      "NPAFP",
      "NPAFP",
      "NPAFP",
      "NPAFP", # adequate, inadequate, missing, bad
      "PENDING",
      "COMPATIBLE",
      "NOT-AFP",
      "NPAFP"
    ),
    age_months = c(12, 30, 48, 24, 36, 24, 24, 24),
    year_onset = c(rep(2023L, 7), NA_integer_),
    paralysis_onset_date = as.Date("2023-02-01") + (1:8),
    adm0_guid = "G0",
    adm0 = "Country",
    adm1_guid = c("P1", "P1", "P2", "P2", "P1", "P2", "P1", "P1"),
    adm1 = c(
      "ProvA",
      "ProvA",
      "ProvB",
      "ProvB",
      "ProvA",
      "ProvB",
      "ProvA",
      "ProvA"
    ),
    adm2_guid = c("D1", "D2", "D3", "D4", "D1", "D3", "D2", "D1"),
    adm2 = c("Da", "Db", "Dc", "Dd", "Da", "Dc", "Db", "Da"),
    adequate_stool = c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No"),
    # conditions -> adq_code: r1=1(Good/Good), r2=0(Poor), r3=99(Unknown),
    # r4=77(bad onset quality), others good
    stool1condition = c(
      "Good",
      "Poor",
      "Unknown",
      "Good",
      "Good",
      "Good",
      "Good",
      "Good"
    ),
    stool2condition = c(
      "Good",
      "Good",
      "Good",
      "Good",
      "Good",
      "Good",
      "Good",
      "Good"
    ),
    onset_to_stool1 = c(5, 5, 5, 5, 5, 5, 5, 5),
    onset_to_stool2 = c(8, 8, 8, 8, 8, 8, 8, 8),
    stool1_to_stool2 = c(3, 3, 3, 3, 3, 3, 3, 3),
    onset_date_quality = c(
      "Good",
      "Good",
      "Good",
      "Missing onset",
      "Good",
      "Good",
      "Good",
      "Good"
    ),
    notify_to_invest = c(1, 5, 12, -1, 2, 8, 1, 1),
    onset_to_followup = c(70, NA, NA, NA, NA, NA, NA, NA),
    followup_date = as.Date(c("2023-05-01", NA, NA, NA, NA, NA, NA, NA)),
    surveillance_type_name = c(
      "AFP",
      "AFP",
      "AFP",
      "AFP",
      "Contact",
      "Contact",
      "AFP",
      "AFP"
    ),
    doses_total = c(0, 2, 4, 99, 1, 3, 5, 0),
    investigation_date = as.Date("2023-02-05") + (1:8),
    stool2collection_date = as.Date("2023-02-09") + (1:8),
    stool_date_sent_to_lab = as.Date("2023-02-12") + (1:8),
    spec_date_received_by_nat_lab = as.Date("2023-02-15") + (1:8)
  )
}

make_indicator_population <- function() {
  tibble::tibble(
    guid = c("G0", "P1", "P2", "D1", "D2", "D3", "D4"),
    year = 2023L,
    pop = c(2e6, 1e6, 1e6, 5e5, 5e5, 5e5, 5e5)
  )
}

make_indicator_es <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm1_guid = "P1",
    adm2_guid = "D1",
    adm0 = "Country",
    adm1 = "ProvA",
    adm2 = "Da",
    classification_all = c(
      "WPV 1",
      "NEGATIVE",
      "CVDPV 2",
      "NPEV",
      "NEGATIVE",
      "WPV 1"
    ),
    year_collection = 2023L,
    collection_date = as.Date("2023-01-01") + (1:6),
    site_name = c("S1", "S1", "S1", "S2", "S2", "S2"),
    ev_detect = c(1, 0, 1, 1, 0, 1),
    date_final_combined_result = as.Date("2023-01-10") + (1:6)
  )
}

make_indicator_virus <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm1_guid = "P1",
    adm2_guid = "D1",
    adm0 = "Country",
    adm1 = "ProvA",
    adm2 = "Da",
    classification_all = c("WPV 1", "CVDPV 2", "VDPV 1", "WPV 1"),
    surveillance_type = "human",
    year_onset = 2023L,
    virus_date = as.Date("2023-02-01") + (1:4),
    report_date = as.Date("2023-03-01") + (1:4)
  )
}

make_indicator_sia <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm1_guid = "P1",
    adm2_guid = "D1",
    adm0 = "Country",
    adm1 = "ProvA",
    adm2 = "Da",
    vaccine_type = c("bOPV", "nOPV2", "bOPV", "tOPV", "mOPV1"),
    sia_sub_activity_code = c("R1", "R2", "R1", "R3", "R4"),
    year_start = 2023L,
    calculated_dosages = c(1000, 2000, 1000, 500, 250)
  )
}

make_indicator_lab <- function() {
  tibble::tibble(
    adm0_guid = "G0",
    adm1_guid = "P1",
    adm2_guid = "D1",
    adm0 = "Country",
    adm1 = "ProvA",
    adm2 = "Da",
    year_collection = 2023L,
    collect_to_lab = c(2, 3, 4),
    lab_to_culture = c(10, 20, 14),
    adequate = c(1, 0, 1)
  )
}

make_indicator_admin_units <- function() {
  tibble::tibble(
    adm2_guid = c("D1", "D2", "D3", "D4", "D5"),
    adm0_guid = "G0",
    adm1_guid = c("P1", "P1", "P2", "P2", "P1")
  )
}
