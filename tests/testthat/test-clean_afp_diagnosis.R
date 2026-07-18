# clean_afp_diagnosis() harmonises the four scattered POLIS diagnosis fields
# into one canonical label and separates reported non-AFP illness from the
# acute-flaccid-paralysis differentials.

testthat::test_that("the three diagnosis reference tables ship and agree", {
  lookup <- polished::polis_afp_diagnosis_lookup()
  icd <- polished::polis_afp_icd10()
  cls <- polished::polis_afp_diagnosis_class()

  testthat::expect_true(all(c("pattern", "diagnosis") %in% names(lookup)))
  testthat::expect_true(all(c("pattern", "diagnosis") %in% names(icd)))
  testthat::expect_true(all(
    c("diagnosis", "diagnosis_class", "is_non_afp") %in% names(cls)
  ))
  testthat::expect_type(cls$is_non_afp, "logical")

  # referential integrity: every mapped diagnosis is classified.
  mapped <- unique(c(lookup$diagnosis, icd$diagnosis))
  testthat::expect_true(all(mapped %in% cls$diagnosis))

  # is_non_afp is exactly the non_afp class.
  testthat::expect_setequal(
    cls$diagnosis[cls$is_non_afp],
    cls$diagnosis[cls$diagnosis_class == "non_afp"]
  )
})

testthat::test_that("the source priority coalesce resolves in order", {
  data <- data.frame(
    classification = c(
      "Confirmed (wild)",
      "Discarded",
      "Discarded",
      "Discarded"
    ),
    diagnosis_final = c("Other", "Guillain Barre Syndrom", "Other", "Other"),
    diagnosis_other = c(NA, NA, "B54", NA),
    diagnosis_other_specified = c(NA, NA, NA, "malaria"),
    provisional_diagnosis = c(NA, NA, NA, NA)
  )
  out <- polished::clean_afp_diagnosis(data)

  # confirmed polio overrides even a coded final.
  testthat::expect_equal(out$diagnosis_harmonised[1], "Poliomyelitis")
  testthat::expect_equal(out$diagnosis_source[1], "classification (polio)")
  # coded final beats the (absent) lower sources.
  testthat::expect_equal(out$diagnosis_harmonised[2], "Guillain-Barre syndrome")
  testthat::expect_equal(out$diagnosis_source[2], "diagnosis_final")
  # ICD (B54 malaria) resolves when final is only "Other".
  testthat::expect_equal(out$diagnosis_harmonised[3], "Malaria")
  testthat::expect_equal(out$diagnosis_source[3], "diagnosis_other (ICD)")
  # free text resolves last.
  testthat::expect_equal(out$diagnosis_harmonised[4], "Malaria")
  testthat::expect_equal(out$diagnosis_source[4], "diagnosis_other_specified")
})

testthat::test_that("is_non_afp separates reported non-AFP illness", {
  data <- data.frame(
    classification = rep("Discarded", 4),
    diagnosis_other_specified = c(
      "malaria",
      "severe sepsis",
      "guillain barre syndrome",
      "transverse myelitis"
    )
  )
  out <- polished::clean_afp_diagnosis(data)
  testthat::expect_equal(out$diagnosis_harmonised[1], "Malaria")
  testthat::expect_equal(out$diagnosis_harmonised[2], "Sepsis")
  testthat::expect_equal(out$is_non_afp, c(TRUE, TRUE, FALSE, FALSE))
  testthat::expect_equal(
    out$diagnosis_class,
    c("non_afp", "non_afp", "afp_compatible", "afp_compatible")
  )
})

testthat::test_that("free text matches across languages and misspellings", {
  data <- data.frame(
    classification = rep("Discarded", 6),
    diagnosis_other_specified = c(
      "MONOPRESIS", # misspelled monoparesis
      "GIZI BURUK", # Indonesian: malnutrition
      "MYELITE", # French, accent-free
      "C.V.A", # dotted abbreviation
      "T.N.", # traumatic neuritis abbreviation
      "post diptheric palatal palsy" # diphtheritic neuropathy
    )
  )
  out <- polished::clean_afp_diagnosis(data)
  testthat::expect_equal(
    out$diagnosis_harmonised,
    c(
      "Paralysis, pattern only",
      "Malnutrition",
      "Transverse myelitis",
      "Hemiplegia or stroke",
      "Traumatic neuritis",
      "Diphtheritic neuropathy"
    )
  )
})

testthat::test_that("fall-through labels are explicit and never NA", {
  data <- data.frame(
    classification = rep("Discarded", 3),
    diagnosis_final = c("Other", "Unknown", NA),
    diagnosis_other_specified = c("something not in the dictionary xyz", NA, NA)
  )
  out <- polished::clean_afp_diagnosis(data)
  testthat::expect_equal(out$diagnosis_harmonised[1], "Other (non-specific)")
  testthat::expect_equal(out$diagnosis_source[1], "non-specific text")
  testthat::expect_equal(out$diagnosis_harmonised[2], "Unknown")
  testthat::expect_equal(out$diagnosis_source[2], "recorded unknown")
  testthat::expect_equal(out$diagnosis_harmonised[3], "Not recorded")
  testthat::expect_equal(out$diagnosis_source[3], "none")
  testthat::expect_false(any(is.na(out$diagnosis_harmonised)))
  testthat::expect_false(any(is.na(out$diagnosis_class)))
})

testthat::test_that("uninformative tokens fall through rather than matching", {
  data <- data.frame(
    classification = rep("Discarded", 3),
    diagnosis_other_specified = c("AFP", "999", "unknown")
  )
  out <- polished::clean_afp_diagnosis(data)
  testthat::expect_equal(out$diagnosis_harmonised, rep("Not recorded", 3))
})

testthat::test_that("residual paralysis and presentation flags derive", {
  data <- data.frame(
    classification = rep("Discarded", 3),
    diagnosis_other_specified = rep("guillain barre", 3),
    followup_findings = c(
      "Residual weakness/paralysis",
      "No residual weakness/paralysis",
      "Lost to follow-up"
    ),
    paralysis_asymmetric = c("Yes", "No", "Yes"),
    paralysis_onset_fever = c("Yes", "Yes", "No")
  )
  out <- polished::clean_afp_diagnosis(data)
  testthat::expect_equal(
    out$residual_paralysis,
    c("residual", "recovered", NA_character_)
  )
  testthat::expect_equal(out$febrile_asymmetric_onset, c(TRUE, FALSE, FALSE))
})

testthat::test_that("clean_afp_diagnosis is a no-op without source columns", {
  data <- data.frame(id = 1:2, epid = c("A-1", "B-2"))
  testthat::expect_identical(polished::clean_afp_diagnosis(data), data)
})

testthat::test_that("clean_afp wires the harmonised diagnosis into its output", {
  raw <- data.frame(
    Id = 1:2,
    Epid = c("A-1", "B-2"),
    LastUpdateDate = rep("2024-01-01", 2),
    ParalysisOnsetDate = rep("2024-02-01", 2),
    Admin0Name = c("NIGERIA", "CHAD"),
    Classification = c("Discarded", "Discarded"),
    DiagnosisOtherSpecified = c("malaria", "guillain barre syndrome"),
    check.names = FALSE
  )
  out <- polished::clean_afp(raw, verbose = FALSE)
  testthat::expect_true(all(
    c(
      "diagnosis_harmonised",
      "diagnosis_source",
      "diagnosis_class",
      "is_non_afp"
    ) %in%
      names(out)
  ))
  testthat::expect_equal(out$diagnosis_harmonised[out$epid == "A-1"], "Malaria")
  testthat::expect_true(out$is_non_afp[out$epid == "A-1"])
  testthat::expect_false(out$is_non_afp[out$epid == "B-2"])
})
