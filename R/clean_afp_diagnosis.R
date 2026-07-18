# =============================================================================
# Harmonise the AFP clinical diagnosis
#
# POLIS scatters the clinical cause of an AFP case across four fields --
# `diagnosis_final` (coded), `diagnosis_other` (ICD-10), a bilingual
# `diagnosis_other_specified` free text and a bilingual `provisional_diagnosis`
# free text. This module coalesces them into one canonical
# `diagnosis_harmonised` (with a confirmed-polio override from `classification`),
# driven by three packaged reference tables loaded from `inst/extdata/dict/`:
# a free-text keyword lookup, an ICD-10 prefix list, and a diagnosis -> class
# map. From that it derives the coarse `diagnosis_class` and the `is_non_afp`
# flag that separates reported non-AFP illness (malaria, sepsis, ...) from the
# genuine acute-flaccid-paralysis differentials, plus the 60-day
# `residual_paralysis` outcome and the `febrile_asymmetric_onset` presentation
# flag. Every step is a no-op when its source columns are absent, so a trimmed
# case table still passes through.
# =============================================================================

#' AFP free-text diagnosis lookup shipped with the package
#'
#' Returns the packaged free-text keyword lookup that maps a normalised
#' diagnosis string (English, French, Portuguese and other national-language
#' variants, plus common misspellings) to a canonical diagnosis label. The
#' first matching `pattern` wins, so the rows are ordered specific-before-broad.
#' Consumed by [clean_afp_diagnosis()].
#'
#' @return A tibble with columns `pattern` (a case-insensitive regular
#'   expression, matched against normalised text) and `diagnosis` (the canonical
#'   label, itself a key of [polis_afp_diagnosis_class()]).
#'
#' @seealso [polis_afp_icd10()], [polis_afp_diagnosis_class()],
#'   [clean_afp_diagnosis()].
#'
#' @examples
#' head(polis_afp_diagnosis_lookup())
#'
#' @export
polis_afp_diagnosis_lookup <- function() {
  readr::read_csv(
    .polis_extdata_path(file.path("dict", "afp_diagnosis_lookup.csv")),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' AFP ICD-10 reference list shipped with the package
#'
#' Returns the packaged ICD-10 reference that maps an ICD-10 code prefix to a
#' canonical diagnosis label. The first matching `pattern` wins, so the rows are
#' ordered specific-before-broad. Consumed by [clean_afp_diagnosis()].
#'
#' @return A tibble with columns `pattern` (an anchored regular expression
#'   matched against the upper-cased ICD-10 code) and `diagnosis` (the canonical
#'   label, itself a key of [polis_afp_diagnosis_class()]).
#'
#' @seealso [polis_afp_diagnosis_lookup()], [polis_afp_diagnosis_class()],
#'   [clean_afp_diagnosis()].
#'
#' @examples
#' head(polis_afp_icd10())
#'
#' @export
polis_afp_icd10 <- function() {
  readr::read_csv(
    .polis_extdata_path(file.path("dict", "afp_icd10.csv")),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' AFP diagnosis-to-class map shipped with the package
#'
#' Returns the packaged map from a canonical diagnosis label to its coarse AFP
#' class and the `is_non_afp` flag it implies. Every diagnosis emitted by
#' [polis_afp_diagnosis_lookup()] and [polis_afp_icd10()] is a key here.
#' Consumed by [clean_afp_diagnosis()].
#'
#' @return A tibble with columns `diagnosis` (the canonical label),
#'   `diagnosis_class` (one of `polio`, `afp_compatible`, `non_flaccid`,
#'   `non_afp`, `unspecified`, `not_recorded`) and `is_non_afp` (logical; `TRUE`
#'   for reported illness that is not acute flaccid paralysis, i.e. the
#'   `non_afp` class).
#'
#' @seealso [polis_afp_diagnosis_lookup()], [polis_afp_icd10()],
#'   [clean_afp_diagnosis()].
#'
#' @examples
#' head(polis_afp_diagnosis_class())
#'
#' @export
polis_afp_diagnosis_class <- function() {
  readr::read_csv(
    .polis_extdata_path(file.path("dict", "afp_diagnosis_class.csv")),
    col_types = readr::cols(
      diagnosis = readr::col_character(),
      diagnosis_class = readr::col_character(),
      is_non_afp = readr::col_logical()
    ),
    progress = FALSE
  )
}

#' Harmonise the AFP clinical diagnosis
#'
#' Coalesces the four scattered POLIS diagnosis fields into one canonical
#' `diagnosis_harmonised` and derives the analytic variables that separate true
#' non-AFP illness from the acute-flaccid-paralysis differentials. The four
#' sources are read in priority order -- a confirmed-polio override from
#' `classification`, then coded `diagnosis_final`, then the ICD-10
#' `diagnosis_other`, then the free-text `diagnosis_other_specified`, then the
#' free-text `provisional_diagnosis` -- and the first that resolves to a
#' specific diagnosis wins. Free text is normalised (accent-stripped,
#' lower-cased, punctuation collapsed) and matched against
#' [polis_afp_diagnosis_lookup()]; ICD-10 codes against [polis_afp_icd10()];
#' the resulting label is classified via [polis_afp_diagnosis_class()].
#'
#' Cases that carry text but resolve to nothing specific are labelled
#' `"Other (non-specific)"`, an explicit `"Unknown"` is preserved, and a case
#' with no diagnostic information at all becomes `"Not recorded"`, so every row
#' receives a definite label and a `diagnosis_source` provenance.
#'
#' @param data A cleaned AFP data frame carrying any of the canonical diagnosis
#'   columns (`diagnosis_final`, `diagnosis_other`, `diagnosis_other_specified`,
#'   `provisional_diagnosis`) and ideally `classification`. When none are
#'   present the input is returned unchanged.
#'
#' @return `data` with the harmonised diagnosis columns added:
#'   \itemize{
#'     \item `diagnosis_harmonised` -- the single canonical diagnosis label;
#'     \item `diagnosis_source` -- which field supplied it (`classification
#'       (polio)`, `diagnosis_final`, `diagnosis_other (ICD)`,
#'       `diagnosis_other_specified`, `provisional_diagnosis`, `non-specific
#'       text`, `recorded unknown` or `none`);
#'     \item `diagnosis_class` -- the coarse class from
#'       [polis_afp_diagnosis_class()];
#'     \item `is_non_afp` -- `TRUE` for reported illness that is not acute
#'       flaccid paralysis (malaria, sepsis, malnutrition, ...);
#'     \item `residual_paralysis` -- the 60-day outcome from `followup_findings`
#'       (`residual` / `recovered` / `died` / `pending`), when present;
#'     \item `febrile_asymmetric_onset` -- `TRUE` when paralysis was asymmetric
#'       with fever at onset, when both source columns are present.
#'   }
#'   The raw `diagnosis_*` and `provisional_diagnosis` columns are left
#'   untouched.
#'
#' @seealso [polis_afp_diagnosis_lookup()], [polis_afp_icd10()],
#'   [polis_afp_diagnosis_class()], [clean_afp_classification()].
#'
#' @examples
#' clean_afp_diagnosis(data.frame(
#'   classification = c("Confirmed (wild)", "Discarded", "Discarded"),
#'   diagnosis_final = c(NA, "Guillain Barre Syndrom", "Other"),
#'   diagnosis_other = c(NA, NA, "B54"),
#'   diagnosis_other_specified = c(NA, NA, "malaria"),
#'   provisional_diagnosis = c(NA, NA, NA)
#' ))
#'
#' @export
clean_afp_diagnosis <- function(data) {
  dx_cols <- c(
    "diagnosis_final",
    "diagnosis_other",
    "diagnosis_other_specified",
    "provisional_diagnosis"
  )
  if (!"classification" %in% names(data) && !any(dx_cols %in% names(data))) {
    return(data)
  }
  data |>
    .afp_dx_harmonise() |>
    .afp_dx_add_class() |>
    .afp_dx_add_flags()
}

#' Classification values that mark a confirmed poliovirus case.
#' @noRd
.afp_dx_polio_set <- c(
  "Confirmed (wild)",
  "VDPV",
  "Compatible",
  "VAPP",
  "iVDPV"
)

#' Coded `diagnosis_final` labels mapped to their canonical diagnosis.
#'
#' Only the specific coded causes; `"Other"` and `"Unknown"` are handled by the
#' fall-through in [.afp_dx_harmonise()].
#' @noRd
.afp_dx_final_map <- c(
  "Traumatic Neuritis" = "Traumatic neuritis",
  "Guillain Barre Syndrom" = "Guillain-Barre syndrome",
  "Transverse myelitis" = "Transverse myelitis",
  "Non-polio enterovirus" = "Non-polio enterovirus",
  "Facial palsy" = "Facial palsy",
  "Transient paralysis" = "Transient paralysis"
)

#' Regex for free text that carries no diagnostic information.
#'
#' Matched against normalised text (see [.afp_dx_normalise()]); a hit blanks the
#' field so the coalesce falls through to the next source -- AFP/PFA restated,
#' administrative notes, or pure numeric placeholder codes.
#' @noRd
.afp_dx_uninform_pattern <- paste0(
  "^(afp|pfa|susp(ected)? ?(afp|polio)?|two stool.*|not ?known|unknown|",
  "inconnu|ras|nil|none|n ?a|[0-9]+|)$"
)

#' Normalise a free-text diagnosis string for matching
#'
#' Strips the Excel carriage-return artifact, transliterates accents to ASCII,
#' lower-cases, collapses every run of non-alphanumeric characters to a single
#' space and trims. Empty results become `NA` so they are treated as missing.
#' @noRd
.afp_dx_normalise <- function(x) {
  v <- as.character(x)
  v <- gsub("_x000d_", " ", v, ignore.case = TRUE)
  v <- iconv(v, to = "ASCII//TRANSLIT")
  v <- tolower(v)
  v <- gsub("[^a-z0-9]+", " ", v)
  v <- trimws(gsub("\\s+", " ", v))
  dplyr::if_else(v == "" | is.na(v), NA_character_, v)
}

#' First-match-wins mapping of a key vector against a pattern table
#'
#' Resolves on the distinct keys then indexes back, so the regex sweep costs one
#' pass per pattern over the vocabulary rather than over every case.
#' @noRd
.afp_dx_match <- function(key, patterns) {
  uni <- unique(key)
  out <- rep(NA_character_, length(uni))
  for (i in seq_len(nrow(patterns))) {
    pending <- is.na(out) & !is.na(uni)
    if (!any(pending)) {
      break
    }
    hit <- pending & grepl(patterns$pattern[[i]], uni, perl = TRUE)
    out[hit] <- patterns$diagnosis[[i]]
  }
  out[match(key, uni)]
}

#' Pull a column as character, or an all-NA vector when it is absent
#' @noRd
.afp_dx_col <- function(data, name) {
  if (name %in% names(data)) {
    as.character(data[[name]])
  } else {
    rep(NA_character_, nrow(data))
  }
}

#' Build `diagnosis_harmonised` and `diagnosis_source` from the four sources
#' @noRd
.afp_dx_harmonise <- function(data) {
  lookup <- polis_afp_diagnosis_lookup()
  icd <- polis_afp_icd10()

  classification <- .afp_dx_col(data, "classification")
  final <- .afp_dx_col(data, "diagnosis_final")

  # confirmed polio overrides everything; coded final is trusted next.
  dx_polio <- dplyr::if_else(
    classification %in% .afp_dx_polio_set,
    "Poliomyelitis",
    NA_character_
  )
  dx_final <- unname(.afp_dx_final_map[final])

  # ICD-10: upper-case the code, drop bare-numeric placeholders, then match.
  icd_key <- toupper(trimws(.afp_dx_col(data, "diagnosis_other")))
  icd_key <- dplyr::if_else(
    icd_key == "" | icd_key == "NA" | grepl("^[0-9]+$", icd_key),
    NA_character_,
    icd_key
  )
  dx_icd <- .afp_dx_match(icd_key, icd)

  # free text: normalise, blank the uninformative, then match the lookup.
  os_key <- .afp_dx_blank_uninform(
    .afp_dx_normalise(.afp_dx_col(data, "diagnosis_other_specified"))
  )
  pv_key <- .afp_dx_blank_uninform(
    .afp_dx_normalise(.afp_dx_col(data, "provisional_diagnosis"))
  )
  dx_os <- .afp_dx_match(os_key, lookup)
  dx_pv <- .afp_dx_match(pv_key, lookup)

  harmonised <- dplyr::coalesce(dx_polio, dx_final, dx_icd, dx_os, dx_pv)
  source <- dplyr::case_when(
    !is.na(dx_polio) ~ "classification (polio)",
    !is.na(dx_final) ~ "diagnosis_final",
    !is.na(dx_icd) ~ "diagnosis_other (ICD)",
    !is.na(dx_os) ~ "diagnosis_other_specified",
    !is.na(dx_pv) ~ "provisional_diagnosis",
    .default = NA_character_
  )

  # nothing specific: label as non-specific text, explicit unknown, or absent.
  had_text <- (final %in% "Other") |
    !is.na(icd_key) |
    !is.na(os_key) |
    !is.na(pv_key)
  harmonised <- dplyr::case_when(
    !is.na(harmonised) ~ harmonised,
    final %in% "Unknown" ~ "Unknown",
    had_text ~ "Other (non-specific)",
    .default = "Not recorded"
  )
  source <- dplyr::case_when(
    !is.na(source) ~ source,
    harmonised == "Other (non-specific)" ~ "non-specific text",
    harmonised == "Unknown" ~ "recorded unknown",
    .default = "none"
  )

  data$diagnosis_harmonised <- harmonised
  data$diagnosis_source <- source
  data
}

#' Blank the free-text keys that carry no diagnostic information
#' @noRd
.afp_dx_blank_uninform <- function(key) {
  dplyr::if_else(
    !is.na(key) & grepl(.afp_dx_uninform_pattern, key, perl = TRUE),
    NA_character_,
    key
  )
}

#' Attach the coarse `diagnosis_class` and the `is_non_afp` flag
#' @noRd
.afp_dx_add_class <- function(data) {
  if (!"diagnosis_harmonised" %in% names(data)) {
    return(data)
  }
  cls <- polis_afp_diagnosis_class()
  dplyr::left_join(
    data,
    cls,
    by = dplyr::join_by(diagnosis_harmonised == diagnosis),
    relationship = "many-to-one"
  )
}

#' Derive the 60-day residual-paralysis outcome and the presentation flag
#' @noRd
.afp_dx_add_flags <- function(data) {
  if ("followup_findings" %in% names(data)) {
    data$residual_paralysis <- dplyr::case_when(
      data$followup_findings == "Residual weakness/paralysis" ~ "residual",
      data$followup_findings == "No residual weakness/paralysis" ~ "recovered",
      data$followup_findings == "Died before follow-up" ~ "died",
      data$followup_findings == "Pending" ~ "pending",
      .default = NA_character_
    )
  }
  if (
    all(c("paralysis_asymmetric", "paralysis_onset_fever") %in% names(data))
  ) {
    data$febrile_asymmetric_onset <- data$paralysis_asymmetric == "Yes" &
      data$paralysis_onset_fever == "Yes"
  }
  data
}
