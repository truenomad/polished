# =============================================================================
# Clean environmental surveillance (ES) data
#
# Reads one POLIS EnvSamples table and returns a canonically-named, deduped,
# ordered analytic tibble. Like the other cleaners this is one linear recipe
# that runs standalone -- no archive, no other dataset, no files. On top of
# naming and dedup it sanitises every collection/laboratory date and derives the
# sample-level virus-detection variables ES surveillance needs (the per-class
# sabin / vdpv / wpv / npev / nvaccine flags, the fused `ev_detect` "anything
# detected" flag and a normalised `virus_type` label) directly from the
# canonical columns -- the ES analogue of what clean_afp() derives for cases.
# =============================================================================

#' Clean POLIS environmental surveillance data
#'
#' Standardises one raw POLIS environmental-samples table and derives the
#' sample-level analytic variables ES surveillance relies on:
#' \itemize{
#'   \item canonical snake_case names (via the crosswalk + janitor);
#'   \item every collection/laboratory date parsed to `Date` and sanitised with
#'     the same "sensible date" rule clean_afp() uses -- a value before the dawn
#'     of surveillance (`min_year`) or in the future is a data-entry error and is
#'     set to `NA`, never dropped (audit timestamps such as `last_update_date`
#'     stay ISO strings for the keep-latest dedup);
#'   \item `year_collection` / `month_collection` from the sanitised
#'     `collection_date`;
#'   \item the AFP-style virus classification (via [clean_es_classification()]):
#'     a normalised `virus_type` list, `vtype` and the fused `classification_all`
#'     label in the **same** `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary the AFP
#'     cleaner emits (so one `grepl("WPV|cVDPV", classification_all)` works across
#'     both streams), the per-serotype Sabin flags `sabin1`/`sabin2`/`sabin3`,
#'     the `npev` and `nvaccine` (nOPV2) flags and the fused `ev_detect`
#'     "anything detected" flag;
#'   \item normalised admin names (and a title-cased `site` label) and one row
#'     per POLIS `id` (latest by `last_update_date`).
#' }
#' The raw POLIS `virus_types`, `vdpv_classifications`, the per-serotype
#' `vaccine*`/`vdpv*`/`wild*` fields and `sample_condition` are kept as-is
#' alongside the derived columns. The business key `sample_id` + `adm0` (the ES
#' analogue of the AFP `epid` + `adm0` key, `sample_id` being the EPID-equivalent
#' sample identifier) is asserted as a tripwire: violations are flagged to QA,
#' never dropped.
#'
#' @param data A raw POLIS environmental-samples data frame.
#' @param cfg A [polis_config()] object (default `polis_config()`). Supply
#'   `cfg$qa` to route ambiguity flags.
#' @param verbose Emit cli progress messages for each phase. Default `TRUE`.
#'
#' @return A tibble of cleaned ES records, one row per POLIS `id`, with columns
#'   ordered identically to [clean_afp()] (id -> location -> time ->
#'   classification -> dates -> other). The derived columns (`year_collection`,
#'   `month_collection`, `vtype`, `classification_all`, `sabin1`/`sabin2`/
#'   `sabin3`, `npev`, `nvaccine`, `ev_detect`) are added only when their
#'   prerequisite source columns are present in `data`, so a trimmed input yields
#'   a correspondingly trimmed output rather than an error.
#'
#' @examples
#' raw <- data.frame(
#'   Id = c(1, 1, 2),
#'   EnviroSampleId = c("E1", "E1", "E2"),
#'   LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
#'   CollectionDate = c("2024-01-05", "2024-01-05", "2024-02-09"),
#'   VirusTypes = c("cVDPV2", "cVDPV2", NA),
#'   VACCINE1 = c("No", "No", "Yes"),
#'   IsNPEV = c("No", "No", "Yes"),
#'   Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
#'   check.names = FALSE
#' )
#' clean_es(raw)
#'
#' @export
clean_es <- function(data, cfg = polis_config(), verbose = TRUE) {
  step <- function(msg, done) {
    if (isTRUE(verbose)) {
      cli::cli_progress_step(msg, msg_done = done, .envir = parent.frame())
    }
  }
  n_in <- .polis_big_num(if (is.data.frame(data)) nrow(data) else 0L)

  # ---- validate & standardise names -----------------------------------------
  .polis_check_input(data, "es")
  step(
    "Standardising names on {n_in} rows",
    "Standardised names on {n_in} rows"
  )
  data <- standardise_names(data, cfg$crosswalk) |>
    .polis_clean_strings()

  # ---- standardise dates ----------------------------------------------------
  # parse + sanitise every collection/laboratory date, then derive
  # year/month from the cleaned collection date. last_update_date (and the other
  # audit timestamps) stay ISO-string for the dedup sort.
  step(
    "Parsing dates and deriving year/month of collection",
    "Parsed dates and derived year/month of collection"
  )
  data <- data |>
    .es_parse_dates() |>
    .es_add_collection_vars()

  # ---- classify virus detection ---------------------------------------------
  step("Deriving virus-detection flags", "Derived virus-detection flags")
  data <- clean_es_classification(data)

  # ---- standardise geography ------------------------------------------------
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data)
  # a title-cased site label for display, alongside the raw upper-case site_name
  if ("site_name" %in% names(data)) {
    data$site <- stringr::str_to_title(data$site_name)
  }

  # ---- finalise: dedup by id, infer types, assert business key, order -------
  step("Deduplicating by id and finalising", "Deduplicated by id and finalised")
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    flag_ambiguous(key = c("sample_id", "adm0"), sink = cfg$qa) |>
    order_columns(cfg$column_roles)
  if (isTRUE(verbose)) {
    cli::cli_progress_done()
    out_fmt <- .polis_big_num(nrow(out))
    cli::cli_alert_success("Cleaned {out_fmt} ES samples.")
  }
  out
}

#' Garbage floor for the ES "sensible date" test (pre-surveillance era).
#' @noRd
.es_min_sensible_year <- 1980L

#' POLIS ES date columns to sanitise (audit timestamps excluded)
#'
#' Every genuine collection/laboratory date is `^date_*` or `*_date` -- the lone
#' exception being `collection_date`, which is included explicitly. The audit
#' timestamps are held out: in particular `last_update_date` must stay a sortable
#' ISO string for the keep-latest dedup.
#' @noRd
.es_date_cols <- function(data) {
  audit <- c(
    "last_update_date",
    "created_date",
    "publish_date",
    "uploaded_date"
  )
  cols <- union(
    grep("^date_", names(data), value = TRUE),
    grep("_date$", names(data), value = TRUE)
  )
  setdiff(cols, audit)
}

#' Parse ES date columns to `Date`, NA-ing implausible values
#'
#' Parses each date column with `as_date` (tolerant of ISO date and datetime
#' strings) then nulls any value outside the plausible window
#' `[min_year-01-01, reference_date]` -- a date before the dawn of surveillance
#' or in the future is a data-entry error, not a real observation, so it is set
#' to `NA` before any year/month is derived from it. The same rule clean_afp()
#' applies to case dates.
#' @noRd
.es_parse_dates <- function(
  data,
  min_year = .es_min_sensible_year,
  reference_date = Sys.Date()
) {
  cols <- .es_date_cols(data)
  if (length(cols) == 0) {
    return(data)
  }
  floor_date <- lubridate::make_date(min_year, 1L, 1L)
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      \(x) {
        parsed <- suppressWarnings(lubridate::as_date(x))
        dplyr::if_else(
          parsed >= floor_date & parsed <= reference_date,
          parsed,
          lubridate::NA_Date_
        )
      }
    )
  )
}

#' Derive year/month of collection from the sanitised collection date
#' @noRd
.es_add_collection_vars <- function(data) {
  if (!"collection_date" %in% names(data)) {
    return(data)
  }
  dplyr::mutate(
    data,
    year_collection = lubridate::year(collection_date),
    month_collection = lubridate::month(collection_date)
  )
}

#' Derive the AFP-style virus classification and detection flags for ES
#'
#' The environmental analogue of [clean_afp_classification()]: it decodes the
#' same poliovirus vocabulary so a single downstream filter
#' (`grepl("WPV|cVDPV", classification_all)`) works identically across the human
#' (AFP) and environmental streams. Detection is read from the combined
#' `virus_types` string and the `vdpv_classifications` field -- the ES
#' equivalents of POLIS `polio_virus_types` / `vdpv_classifications` -- using
#' standard **WPV** (wild poliovirus) nomenclature, *not* the legacy `WILD n`
#' strings.
#'
#' Two layers, mirroring the AFP cleaner:
#' \enumerate{
#'   \item `vtype` decodes the specific poliovirus. A VDPV always carries an
#'     explicit kind prefix from `vdpv_classifications` -- `cVDPV` (circulating),
#'     `aVDPV` (ambiguous), `iVDPV` (immune-deficient) -- so the three are never
#'     merged; an untyped `VDPV n` only remains when the kind is unknown. Samples
#'     with no poliovirus are `none` (and `NA` when the sample was never typed).
#'   \item `classification_all` is the single analysis label: the `vtype` virus
#'     string for poliovirus-positive samples, otherwise the sample outcome --
#'     `SABIN` (Sabin vaccine virus only), `NPEV` (non-polio enterovirus only),
#'     `NEGATIVE` (tested negative) or `PENDING` (classification pending);
#'     samples matching none stay `none`/`NA`.
#' }
#'
#' @section Classification vocabulary (match on these prefixes, not free text):
#' \describe{
#'   \item{Wild}{`WPV 1`, `WPV 2`, `WPV 3`, `WPV1andWPV3` -- prefix `WPV`.}
#'   \item{Circulating VDPV}{`cVDPV 1/2/3` and combinations -- prefix `cVDPV`.}
#'   \item{Ambiguous VDPV}{`aVDPV 1/2/3` -- prefix `aVDPV`. Labelled, never
#'     folded into `cVDPV`.}
#'   \item{Immune-deficient VDPV}{`iVDPV 1/2/3` -- prefix `iVDPV`.}
#'   \item{Untyped VDPV}{`VDPV 1/2/3` -- kind unknown.}
#'   \item{Wild + VDPV co-detection}{`WPV1and...` (e.g. `WPV1andcVDPV 2`).}
#'   \item{Sample outcome}{`SABIN`, `NPEV`, `NEGATIVE`, `PENDING`, `none`.}
#' }
#'
#' Alongside the labels it derives the Sabin-detection flags
#' `sabin1` / `sabin2` / `sabin3` (per serotype, exactly as the AFP cleaner), the
#' non-polio-enterovirus flag `npev`, the novel-OPV2 flag `nvaccine` and the
#' fused `ev_detect` ("any poliovirus or enterovirus detected"). Every condition
#' is NA-safe: a missing source value leaves the prior value intact rather than
#' nulling it. The raw `virus_types` and `vdpv_classifications` columns are kept.
#'
#' @param data A cleaned ES data frame carrying at least `virus_types` (and
#'   ideally `vdpv_classifications`). Any source column may be absent -- each
#'   derived column is added only when its inputs are present.
#'
#' @return `data` with `virus_type` (the normalised full virus-type list),
#'   `vtype`, `classification_all`, `sabin1`/`sabin2`/`sabin3`, `npev`,
#'   `nvaccine` and `ev_detect` added where derivable; the raw POLIS columns are
#'   left untouched.
#'
#' @examples
#' clean_es_classification(data.frame(
#'   virus_types = c("cVDPV2", "WILD1", "NPEV, VACCINE3", NA),
#'   vdpv_classifications = c("Circulating", NA, NA, NA),
#'   is_npev = c(NA, NA, TRUE, NA)
#' ))
#'
#' @export
clean_es_classification <- function(data) {
  if (!"virus_types" %in% names(data)) {
    return(data)
  }
  n <- nrow(data)
  pvt <- data$virus_types
  vdpv <- if ("vdpv_classifications" %in% names(data)) {
    data$vdpv_classifications
  } else {
    rep(NA_character_, n)
  }
  vdpv <- dplyr::if_else(is.na(vdpv), "", vdpv)
  # str_detect propagates NA, so an un-typed sample (NA virus_types) yields an
  # NA vtype rather than a spurious "none"; a missing column is treated as NA.
  has <- function(pattern) stringr::str_detect(pvt, pattern)

  # ---- normalised full virus-type string -----------------------------------
  # space each serotype digit off its prefix ("cVDPV2" -> "cVDPV 2") across the
  # whole comma-separated list, preserving every token; the raw column is kept.
  data$virus_type <- stringr::str_squish(
    stringr::str_replace_all(
      pvt,
      "\\b(cVDPV|aVDPV|iVDPV|VDPV|WILD|VACCINE|NPEV)\\s*([123])\\b",
      "\\1 \\2"
    )
  )

  # ---- layer 1: specific virus type (WPV nomenclature) ---------------------
  # detection runs on the POLIS tokens ("WILD1", "VDPV2", ...), but the emitted
  # label uses WPV so it matches the standard surveillance vocabulary. This is
  # the same cascade as clean_afp_classification(), kept token-for-token in step.
  vtype <- dplyr::if_else(has("WILD1"), "WPV 1", "none")
  vtype <- dplyr::if_else(has("WILD2"), "WPV 2", vtype)
  vtype <- dplyr::if_else(has("WILD3"), "WPV 3", vtype)
  vtype <- dplyr::if_else(has("WILD1") & has("WILD3"), "WPV1andWPV3", vtype)
  vtype <- dplyr::if_else(has("VDPV1"), "VDPV 1", vtype)
  vtype <- dplyr::if_else(has("VDPV2"), "VDPV 2", vtype)
  vtype <- dplyr::if_else(has("VDPV3"), "VDPV 3", vtype)
  vtype <- dplyr::if_else(has("VDPV1") & has("VDPV2"), "VDPV1andVDPV2", vtype)
  vtype <- dplyr::if_else(has("VDPV1") & has("VDPV3"), "VDPV1andVDPV3", vtype)
  vtype <- dplyr::if_else(has("VDPV2") & has("VDPV3"), "VDPV2andVDPV3", vtype)
  vtype <- dplyr::if_else(
    has("VDPV1") & has("VDPV2") & has("VDPV3"),
    "VDPV12and3",
    vtype
  )
  # a combined wild + VDPV detection -> "WPV1and<vdpv>"
  vtype <- dplyr::if_else(
    has("WILD") & has("VDPV"),
    paste0("WPV1and", vtype),
    vtype
  )
  # circulating / ambiguous / immune-deficient prefix from the classification
  vtype <- dplyr::if_else(
    vdpv == "Ambiguous" & !is.na(vtype),
    paste0("a", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    stringr::str_detect(vdpv, "Circulating") & !is.na(vtype),
    paste0("c", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    vdpv == "Immune Deficient" & !is.na(vtype),
    paste0("i", vtype),
    vtype
  )
  vtype <- dplyr::if_else(
    vtype %in% c("cnone", "anone", "inone"),
    "none",
    vtype
  )
  # move the kind prefix onto the VDPV component of a co-detection
  vtype <- dplyr::if_else(vtype == "cWPV1andVDPV 2", "WPV1andcVDPV 2", vtype)
  vtype <- dplyr::if_else(vtype == "cWPV1andVDPV 3", "WPV1andcVDPV 3", vtype)
  vtype <- dplyr::if_else(vtype == "cWPV1andVDPV 1", "WPV1andcVDPV 1", vtype)
  vtype <- dplyr::if_else(vtype == "cVDPV2andVDPV3", "cVDPV2andcVDPV3", vtype)
  data$vtype <- vtype

  # ---- per-serotype Sabin flags (exactly as clean_afp) ---------------------
  data$sabin1 <- dplyr::if_else(has("VACCINE1"), 1L, 0L)
  data$sabin2 <- dplyr::if_else(has("VACCINE2"), 1L, 0L)
  data$sabin3 <- dplyr::if_else(has("VACCINE3"), 1L, 0L)

  # ---- non-polio enterovirus + novel-OPV2 flags ----------------------------
  # NPEV appears as a token in virus_types and/or the logical `is_npev`; POLIS
  # leaves the field empty rather than "negative", so absence reads as 0 not NA.
  npev <- .es_safe(has("NPEV"))
  if ("is_npev" %in% names(data)) {
    npev <- npev | .es_truthy(data$is_npev)
  }
  data$npev <- as.integer(npev)
  nvaccine <- rep(FALSE, n)
  if ("n_vaccine2" %in% names(data)) {
    nvaccine <- nvaccine | .es_truthy(data$n_vaccine2)
  }
  if ("final_combinedr_rtpc_rresults" %in% names(data)) {
    nvaccine <- nvaccine |
      .es_safe(stringr::str_detect(data$final_combinedr_rtpc_rresults, "nOPV"))
  }
  data$nvaccine <- as.integer(nvaccine)

  # ---- layer 2: fuse virus type with the sample outcome --------------------
  # a poliovirus vtype always wins; otherwise the sample outcome is assigned in
  # priority order (SABIN > NPEV > NEGATIVE > PENDING), each only filling a label
  # still unset ("none" or NA) so an earlier outcome is never overwritten.
  sabin_any <- .es_safe(has("VACCINE1") | has("VACCINE2") | has("VACCINE3"))
  is_neg <- if ("is_negative" %in% names(data)) {
    .es_truthy(data$is_negative)
  } else {
    rep(FALSE, n)
  }
  classification_all <- .es_relabel(vtype, sabin_any, "SABIN")
  classification_all <- .es_relabel(classification_all, npev, "NPEV")
  classification_all <- .es_relabel(classification_all, is_neg, "NEGATIVE")
  classification_all <- .es_relabel(
    classification_all,
    stringr::str_detect(vdpv, "Pending"),
    "PENDING"
  )
  data$classification_all <- classification_all

  # ---- fused "anything detected" flag --------------------------------------
  data$ev_detect <- .es_ev_detect(data, vtype)
  data
}

#' Coalesce a logical condition's NAs to FALSE
#'
#' So a missing input never overwrites a value already assigned (the ES cleaner's
#' equivalent of the `safe()` idiom in clean_afp_classification()).
#' @noRd
.es_safe <- function(cond) dplyr::coalesce(cond, FALSE)

#' Assign `label` where the running classification is still unset
#'
#' Fills a `"none"`/`NA` slot only -- so the higher-priority outcome assigned
#' earlier is never clobbered -- and only where `cond` holds (NA-safe).
#' @noRd
.es_relabel <- function(classification, cond, label) {
  unset <- is.na(classification) | classification == "none"
  dplyr::if_else(.es_safe(unset & cond), label, classification)
}

#' Positive test tolerant of logical / "Yes" / "1" representations
#'
#' POLIS ships some detection fields as logical `TRUE`/`NA`, others as
#' `"Yes"`/`"No"` strings; this reads either as a clean `TRUE`/`FALSE` vector
#' (NA -> FALSE) so the flag logic does not care which form arrived.
#' @noRd
.es_truthy <- function(x) {
  if (is.logical(x)) {
    return(!is.na(x) & x)
  }
  toupper(trimws(as.character(x))) %in% c("YES", "TRUE", "1", "Y")
}

#' Fuse every detection signal into a single 0/1 "anything detected" flag
#'
#' Raised by any poliovirus `vtype`, the derived `npev`/`nvaccine` flags, a Sabin
#' detection, or a poliovirus / non-polio enterovirus named in the combined
#' RT-PCR or cell-culture result. Every test is NA-safe: a missing value or
#' absent column cannot raise the flag.
#' @noRd
.es_ev_detect <- function(data, vtype) {
  detect <- .es_safe(vtype != "none")
  for (col in intersect(
    c("sabin1", "sabin2", "sabin3", "npev", "nvaccine"),
    names(data)
  )) {
    detect <- detect | .es_safe(data[[col]] == 1L)
  }
  if ("final_combinedr_rtpc_rresults" %in% names(data)) {
    detect <- detect |
      .es_safe(stringr::str_detect(
        data$final_combinedr_rtpc_rresults,
        "PV|NPE"
      ))
  }
  if ("final_cell_culture_result" %in% names(data)) {
    detect <- detect |
      .es_safe(stringr::str_detect(
        data$final_cell_culture_result,
        "Poliovirus|NPENT"
      ))
  }
  as.integer(detect)
}
