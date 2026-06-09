# =============================================================================
# Optional AFP enrichment
#
# A thin, opinionated layer on top of clean_afp(): joins country-keyed reference
# attributes (standardised country name, polio risk tier, epidemiological zone),
# derives the poliovirus serotype, and computes the surveillance AFP flags. Kept
# separate so the core cleaner stays free of policy lookups; opt in via
# clean_afp(enrich = TRUE) or call enrich_afp() directly.
# =============================================================================

#' Country reference lookup shipped with the package
#'
#' Maps an ISO3 country code to a standardised display name (`country_actual`),
#' the polio `risk_group` tier and the `epi_zones` grouping used by
#' [enrich_afp()].
#'
#' @return A tibble with columns `iso3`, `country_actual`, `risk_group`,
#'   `epi_zones`, `epi_zones_v2`.
#'
#' @examples
#' head(polis_country_lookup())
#'
#' @export
polis_country_lookup <- function() {
  readr::read_csv(
    .polis_extdata_path("country_lookup.csv"),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' Enrich cleaned AFP data with country groupings and AFP flags
#'
#' An optional layer over [clean_afp()] that adds, where derivable:
#' \itemize{
#'   \item country-keyed reference attributes from [polis_country_lookup()] --
#'     `country_actual` (standardised name, falling back to the title-cased
#'     admin-0 name), `risk_group` (Endemic / Very High / High / Medium Risk),
#'     and `epi_zones` / `epi_zones_v2`;
#'   \item `polio_type` (Type 1 / 2 / 3) read off the serotype in the
#'     classification;
#'   \item the surveillance AFP flags `afp_class` (AFP-Positive / Non-polio AFP /
#'     Not an AFP / VAPP / Others), the binary `afp` and `npafp`, and
#'     `pending_results`.
#' }
#'
#' @param data A cleaned AFP data frame (e.g. the output of [clean_afp()]).
#' @param iso3_var Country ISO3 column. Default `"country_iso3code"`.
#' @param class_var Raw classification column. Default `"classification"`.
#' @param virus_var Poliovirus-types column. Default `"polio_virus_types"`.
#' @param surv_var Surveillance-type column. Default `"surveillance_type_name"`.
#' @param adm0_var Admin-0 name column, used as the `country_actual` fallback.
#'   Default `"adm0"`.
#' @param type_var Column the serotype is read from for `polio_type` (default
#'   `"classification_all"`, falling back to `virus_var`).
#' @param lookup Optional country lookup table (same columns as
#'   [polis_country_lookup()]); the packaged lookup is used when `NULL`.
#'
#' @return `data` with `country_actual`, `risk_group`, `epi_zones`,
#'   `epi_zones_v2`, `polio_type`, `afp_class`, `afp`, `npafp` and
#'   `pending_results` added where their inputs are present.
#'
#' @examples
#' enrich_afp(data.frame(
#'   country_iso3code = c("NGA", "PAK"),
#'   classification = c("Discarded", "Confirmed (wild)"),
#'   classification_all = c("NPAFP", "WPV 1"),
#'   polio_virus_types = c(NA, "WILD1"),
#'   surveillance_type_name = c("AFP", "AFP")
#' ))
#'
#' @export
enrich_afp <- function(
  data,
  iso3_var = "country_iso3code",
  class_var = "classification",
  virus_var = "polio_virus_types",
  surv_var = "surveillance_type_name",
  adm0_var = "adm0",
  type_var = "classification_all",
  lookup = NULL
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  lookup <- lookup %||% polis_country_lookup()
  data <- .enrich_country(data, lookup, iso3_var, adm0_var)
  data <- .enrich_polio_type(data, type_var, virus_var)
  .enrich_afp_flags(data, class_var, virus_var, surv_var)
}

#' Join the country lookup by ISO3 and fill the country fields
#' @noRd
.enrich_country <- function(data, lookup, iso3_var, adm0_var) {
  if (!iso3_var %in% names(data)) {
    return(data)
  }
  lookup <- dplyr::distinct(lookup, iso3, .keep_all = TRUE)
  pos <- match(toupper(trimws(data[[iso3_var]])), lookup$iso3)
  data$country_actual <- lookup$country_actual[pos]
  data$risk_group <- lookup$risk_group[pos]
  data$epi_zones <- dplyr::coalesce(lookup$epi_zones[pos], "Other")
  data$epi_zones_v2 <- dplyr::coalesce(lookup$epi_zones_v2[pos], "Other")
  if (adm0_var %in% names(data)) {
    data$country_actual <- dplyr::coalesce(
      data$country_actual,
      stringr::str_to_title(data[[adm0_var]])
    )
  }
  data
}

#' Read the poliovirus serotype (Type 1/2/3) off the classification
#' @noRd
.enrich_polio_type <- function(data, type_var, virus_var) {
  src <- if (type_var %in% names(data)) {
    data[[type_var]]
  } else if (virus_var %in% names(data)) {
    data[[virus_var]]
  } else {
    return(data)
  }
  data$polio_type <- dplyr::case_when(
    stringr::str_detect(src, "1") ~ "Type 1",
    stringr::str_detect(src, "2") ~ "Type 2",
    stringr::str_detect(src, "3") ~ "Type 3",
    TRUE ~ NA_character_
  )
  data
}

#' Derive the surveillance AFP flags
#' @noRd
.enrich_afp_flags <- function(data, class_var, virus_var, surv_var) {
  if (!class_var %in% names(data)) {
    return(data)
  }
  cls <- data[[class_var]]
  surv <- if (surv_var %in% names(data)) {
    data[[surv_var]]
  } else {
    rep(NA_character_, nrow(data))
  }
  virus <- if (virus_var %in% names(data)) {
    data[[virus_var]]
  } else {
    rep(NA_character_, nrow(data))
  }
  is_afp <- !is.na(surv) & surv == "AFP"
  data$afp_class <- dplyr::case_when(
    is_afp & cls %in% c("Compatible", "Confirmed (wild)") ~ "AFP-Positive",
    is_afp & cls == "Discarded" ~ "Non-polio AFP",
    is_afp & cls == "Not an AFP" ~ "Not an AFP",
    is_afp & cls == "VAPP" ~ "VAPP",
    TRUE ~ "Others"
  )
  data$afp <- dplyr::if_else(data$afp_class == "AFP-Positive", 1L, 0L)
  # non-polio AFP: discarded/pending and not a circulating VDPV type 1/2
  data$npafp <- dplyr::if_else(
    cls %in%
      c("Discarded", "Pending") &
      !grepl("cVDPV1|cVDPV2", dplyr::coalesce(virus, "")),
    1L,
    0L
  )
  data$pending_results <- cls %in% "Pending"
  data
}
