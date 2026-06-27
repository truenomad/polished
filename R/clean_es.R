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
#'   \item the same geography cleaning as [clean_afp()]: normalised admin names,
#'     canonicalised admin GUIDs (braces stripped, lower-cased, so they are
#'     join-ready with the spatial layer), a title-cased `site` label, and --
#'     when a `shape` is supplied -- admin-GUID reconciliation and
#'     coordinate-based admin recovery (all keyed on `year_collection`);
#'   \item country-keyed enrichment from [polis_country_lookup()] --
#'     `country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2` -- and the
#'     `polio_type` serotype (the ES counterpart of the AFP enrichment; the
#'     case-classification AFP flags do not apply to environmental samples);
#'   \item one row per POLIS `id` (latest by `last_update_date`).
#' }
#' The raw POLIS `virus_types`, `vdpv_classifications`, the per-serotype
#' `vaccine*`/`vdpv*`/`wild*` fields and `sample_condition` are kept as-is
#' alongside the derived columns. The business key `sample_id` + `adm0` (the ES
#' analogue of the AFP `epid` + `adm0` key, `sample_id` being the EPID-equivalent
#' sample identifier) is asserted as a tripwire: violations are flagged to QA,
#' never dropped. (Unlike AFP, a sample can legitimately yield several virus
#' detections, so this key is not collapsed.)
#'
#' @param data A raw POLIS environmental-samples data frame.
#' @param cfg A [polis_config()] object. Defaults to [polis_active_config()] --
#'   the config most recently built by [polis_config()] this session -- so a
#'   no-`cfg` call inherits the active session settings rather than fresh
#'   defaults. Supply `cfg$qa` to route ambiguity flags.
#' @param shape Optional district shape used to reconcile admin names/GUIDs via
#'   [reconcile_admin_guids()] (keyed on `year_collection`), exactly as
#'   [clean_afp()] uses it. Either a long ADM2 attribute table
#'   (`spatial_adm2_long_shape`) or the polygon layer (`spatial_global_adm2`). A
#'   polygon is expanded to its long form here to drive the GUID reconcile and
#'   *also* drives coordinate-based admin recovery: samples still missing
#'   `adm1`/`adm2` (or their GUIDs) but carrying site coordinates have their
#'   admin recovered by a point-in-polygon join via [impute_geo_from_coords()]
#'   (the ES counterpart of AFP EPID-prefix recovery; ES samples carry no
#'   geocoded EPID). Default `NULL` (no shape-based recovery).
#' @param impute_geo If `TRUE` (default) samples still missing `adm2_guid` after
#'   any shape-based recovery have their admin chain borrowed from other samples
#'   at the same site via the self-reference fill (see details); only sites that
#'   map unambiguously to one district are used, so conflicting sites are left
#'   flagged rather than guessed. Needs no shape, so it runs standalone. Adds a
#'   `geo_source` of `"site_match"` to filled rows.
#' @param sites Optional reference list of known environmental site names (a data
#'   frame with a `site_name` column, or a character vector). When supplied,
#'   sites absent from it are flagged via [validate_es_sites()]. Default `NULL`
#'   (no site validation).
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
clean_es <- function(
  data,
  cfg = polis_active_config(),
  shape = NULL,
  impute_geo = TRUE,
  sites = NULL,
  verbose = TRUE
) {
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
  # exactly the clean_afp() recipe, keyed on year_collection: fix admin names,
  # canonicalise the admin GUIDs (strip braces, lower-case; "" already NA from
  # clean_strings) so they are join-ready, add a title-cased site label, then --
  # when a shape is supplied -- reconcile the GUIDs against it and recover admin
  # from the site coordinates. (EPID-prefix recovery is AFP-only: ES samples
  # carry no geocoded EPID, so coordinate recovery is the ES equivalent.)
  step("Standardising admin names", "Standardised admin names")
  data <- fix_geo_names(data) |>
    .es_normalise_guids()
  if ("site_name" %in% names(data)) {
    data$site <- stringr::str_to_title(data$site_name)
  }
  long_shape <- NULL
  poly_shape <- NULL
  if (!is.null(shape)) {
    if (inherits(shape, "sf")) {
      poly_shape <- shape
      step(
        "Building the long district lookup from the shape",
        "Built the long district lookup from the shape"
      )
      long_shape <- create_long_shape(shape, "adm2")
    } else {
      long_shape <- shape
    }
  }
  if (!is.null(long_shape)) {
    step(
      "Reconciling admin GUIDs against the district shape",
      "Reconciled admin GUIDs against the district shape"
    )
    data <- reconcile_admin_guids(
      data,
      long_shape,
      year_var = "year_collection",
      verbose = FALSE
    )
  }
  if (!is.null(poly_shape) && "adm2_guid" %in% names(data)) {
    nc_before <- .geo_miss_admin(data)
    # n_crec is filled below and glued into msg_done when the step ticks.
    n_crec <- "0"
    step(
      "Recovering missing admin from site coordinates",
      "Recovered admin for {n_crec} samples from coordinates"
    )
    data <- .es_impute_geo(data, poly_shape)
    n_crec <- .polis_big_num(max(nc_before - .geo_miss_admin(data), 0L))
  }
  if (isTRUE(impute_geo)) {
    g2_before <- if ("adm2_guid" %in% names(data)) {
      sum(is.na(data$adm2_guid))
    } else {
      0L
    }
    n_srec <- "0"
    step(
      "Recovering missing admin from same-site samples",
      "Recovered admin for {n_srec} samples from same-site records"
    )
    data <- .es_impute_site(data)
    g2_after <- if ("adm2_guid" %in% names(data)) {
      sum(is.na(data$adm2_guid))
    } else {
      0L
    }
    n_srec <- .polis_big_num(max(g2_before - g2_after, 0L))
  }
  # optional: flag sites absent from a reference list (no disk / global state)
  if (!is.null(sites)) {
    step("Validating site names", "Validated site names")
    data <- validate_es_sites(data, sites, verbose = verbose)
  }

  # ---- enrich: country groupings + polio type -------------------------------
  step(
    "Enriching with country groupings",
    "Enriched with country groupings"
  )
  data <- .es_enrich(data)

  # ---- finalise: dedup by id, infer types, assert business key, order -------
  step("Deduplicating by id and finalising", "Deduplicated by id and finalised")
  # the sample identifier is sample_id (from SampleId) or, when that is absent,
  # enviro_sample_id (from EnviroSampleId) -- whichever the pull carries.
  sample_key <- c(
    intersect(c("sample_id", "enviro_sample_id"), names(data)),
    "sample_id"
  )[1]
  out <- data |>
    polis_upsert(id = "id", date = "last_update_date") |>
    .polis_parse_types(cfg) |>
    .polis_drop_empty(cfg) |>
    .geo_guid_display_cols() |>
    flag_ambiguous(key = c(sample_key, "adm0"), sink = cfg$qa) |>
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
#' @details The decoding engine (`.polis_classify_virus()`) is shared with
#'   [clean_human_spec()]: ES samples and human lab specimens have the same
#'   lab-result structure (`virus_types` plus a VDPV classification, which may
#'   arrive as the plural `vdpv_classifications` or the singular
#'   `vdpv_classification`), so both reuse one classifier.
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
  .polis_classify_virus(data)
}

#' Decode the shared POLIS lab-virus classification (ES + human specimens)
#'
#' The generic engine behind [clean_es_classification()] and the classification
#' step of [clean_human_spec()]. Operates on any lab record carrying a
#' `virus_types` string and (optionally) a VDPV classification column under
#' either the plural `vdpv_classifications` or singular `vdpv_classification`
#' name; emits the WPV-vocabulary labels and detection flags.
#' @noRd
.polis_classify_virus <- function(data) {
  if (!"virus_types" %in% names(data)) {
    return(data)
  }
  n <- nrow(data)
  pvt <- data$virus_types
  # ES samples carry `vdpv_classifications` (plural); human lab specimens the
  # singular `vdpv_classification` -- accept either so both reuse this classifier.
  vdpv_col <- intersect(
    c("vdpv_classifications", "vdpv_classification"),
    names(data)
  )
  vdpv <- if (length(vdpv_col) > 0L) {
    data[[vdpv_col[1]]]
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
  # compact wild label, kept so a wild+VDPV co-detection names the real serotype
  wild <- dplyr::case_when(
    has("WILD1") & has("WILD3") ~ "WPV1andWPV3",
    has("WILD3") ~ "WPV3",
    has("WILD2") ~ "WPV2",
    has("WILD1") ~ "WPV1",
    TRUE ~ NA_character_
  )
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
  # a combined wild + VDPV detection -> "<wild>and<vdpv>"
  vtype <- dplyr::if_else(
    has("WILD") & has("VDPV"),
    paste0(wild, "and", vtype),
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
  # move the kind prefix off the wild stem onto the VDPV component of a
  # co-detection (cWPV1andVDPV 2 -> WPV1andcVDPV 2), for any wild serotype
  vtype <- stringr::str_replace(vtype, "^([cai])(WPV.*?and)(VDPV)", "\\2\\1\\3")
  # a VDPV + VDPV co-detection only carries the kind prefix on its first
  # serotype; copy it onto the second so splitting keeps both kinds typed
  # (cVDPV1andVDPV2 -> cVDPV1andcVDPV2), for any kind (c/a/i) and serotype pair.
  vtype <- stringr::str_replace(
    vtype,
    "^([cai])(VDPV[0-9]+and)(VDPV[0-9]+)$",
    "\\1\\2\\1\\3"
  )
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
  # ES: final_cell_culture_result; human lab specimens: ..._result_name
  culture_col <- intersect(
    c("final_cell_culture_result", "final_cell_culture_result_name"),
    names(data)
  )
  if (length(culture_col) > 0L) {
    detect <- detect |
      .es_safe(stringr::str_detect(
        data[[culture_col[1]]],
        "Poliovirus|NPENT|L20B"
      ))
  }
  as.integer(detect)
}

#' Canonicalise the ES admin GUID columns (strip braces, lower-case)
#'
#' Empty strings are already NA from `.polis_clean_strings`; this strips the
#' `{...}` wrapper and lower-cases so the GUIDs match the form the spatial
#' reconcile emits. A no-op for absent columns.
#' @noRd
.es_normalise_guids <- function(data) {
  guid_cols <- intersect(c("adm0_guid", "adm1_guid", "adm2_guid"), names(data))
  if (length(guid_cols) == 0) {
    return(data)
  }
  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(guid_cols), .geo_guid_canon)
  )
}

#' Recover missing admin from the site coordinates (point-in-polygon)
#'
#' The ES counterpart of `.afp_impute_geo`: samples still missing an admin level
#' but carrying site coordinates have it recovered by a point-in-polygon join via
#' [impute_geo_from_coords()], keyed on `year_collection`. The ES coordinate
#' columns (`site_x_coordinate` = longitude, `site_y_coordinate` = latitude) are
#' coerced to numeric first. A no-op when the coordinate columns are absent.
#' @noRd
.es_impute_geo <- function(data, poly_shape) {
  coord_cols <- c("site_x_coordinate", "site_y_coordinate")
  if (!all(coord_cols %in% names(data))) {
    return(data)
  }
  data <- dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(coord_cols),
      \(x) suppressWarnings(as.numeric(x))
    )
  )
  impute_geo_from_coords(
    data,
    poly_shape,
    year_var = "year_collection",
    lon_var = "site_x_coordinate",
    lat_var = "site_y_coordinate",
    verbose = FALSE
  )
}

#' Recover missing admin from other samples at the same site (self-reference)
#'
#' The ES analogue of `.afp_impute_geo`: a site's district rarely changes, so a
#' sample missing its admin chain can borrow it from other samples at the same
#' `site_id` (or `site_code`). Only **unambiguous** sites are used -- a site that
#' maps to more than one `adm2_guid` across the data is left untouched rather
#' than guessed at. For each still-missing sample the whole `adm1`/`adm2` chain
#' (names + GUIDs) is adopted from its site's reference row so the hierarchy
#' stays coherent, and `geo_source` is set to `"site_match"` where it was filled.
#' A no-op when no site key or `adm2_guid` column is present.
#' @noRd
.es_impute_site <- function(data) {
  site_key <- intersect(c("site_id", "site_code"), names(data))[1]
  geo_cols <- intersect(
    c("adm1", "adm2", "adm1_guid", "adm2_guid"),
    names(data)
  )
  if (is.na(site_key) || !"adm2_guid" %in% geo_cols) {
    return(data)
  }
  # build a one-row-per-site reference from samples whose adm2_guid is known,
  # keeping only sites that map to a single adm2_guid (unambiguous).
  ref <- data |>
    dplyr::filter(!is.na(.data[[site_key]]) & !is.na(.data$adm2_guid)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(site_key, geo_cols)))) |>
    dplyr::group_by(.data[[site_key]]) |>
    dplyr::filter(dplyr::n_distinct(.data$adm2_guid) == 1L) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(geo_cols))) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()
  if (nrow(ref) == 0L) {
    return(data)
  }
  ref <- dplyr::rename_with(
    ref,
    \(nm) paste0(nm, "_ref"),
    dplyr::all_of(geo_cols)
  )

  need <- is.na(data$adm2_guid) & data[[site_key]] %in% ref[[site_key]]
  data <- dplyr::left_join(
    data,
    ref,
    by = site_key,
    relationship = "many-to-one"
  )
  for (col in geo_cols) {
    ref_col <- paste0(col, "_ref")
    data[[col]] <- dplyr::if_else(need, data[[ref_col]], data[[col]])
  }
  if ("geo_source" %in% names(data)) {
    data$geo_source <- dplyr::if_else(need, "site_match", data$geo_source)
  } else {
    data$geo_source <- dplyr::if_else(need, "site_match", NA_character_)
  }
  dplyr::select(data, -dplyr::all_of(paste0(geo_cols, "_ref")))
}

#' Flag ES site names absent from a reference site list
#'
#' Diagnostic check: environmental site names present in `data` but missing from
#' the reference `sites` list are flagged -- particularly new sites that also
#' lack coordinates, which usually signal a data-entry issue rather than a
#' genuine new site. Names are compared upper-cased and whitespace-squished
#' (embedded newlines collapsed) on both sides. `data` is returned unchanged,
#' with the unmatched sites attached as the `"polis_new_sites"` attribute, so the
#' check composes into [clean_es()] without writing to disk or touching global
#' state.
#'
#' @param data A cleaned ES data frame (canonical names) carrying `site_name`.
#' @param sites Reference site list: a data frame with a site-name column, or a
#'   character vector of known site names.
#' @param site_col Name of the site-name column in `data` and `sites`
#'   (default `"site_name"`).
#' @param verbose Emit a cli warning when unknown sites are found. Default
#'   `TRUE`.
#'
#' @return `data`, unchanged, with attribute `"polis_new_sites"`: a tibble with
#'   columns `site_name` (character, the raw site label from `data`) and
#'   `no_coords` (logical, `TRUE` when the site lacks a coordinate value,
#'   `NA` when no coordinate column is present in `data`), one row per distinct
#'   unmatched site.
#'
#' @examples
#' es <- data.frame(
#'   site_name = c("SITE A", "SITE B"),
#'   site_y_coordinate = c(6.5, NA)
#' )
#' out <- validate_es_sites(es, sites = "SITE A")
#' attr(out, "polis_new_sites")
#'
#' @export
validate_es_sites <- function(
  data,
  sites,
  site_col = "site_name",
  verbose = TRUE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (!is.character(site_col) || length(site_col) != 1L) {
    cli::cli_abort("{.arg site_col} must be a single character string.")
  }
  if (!is.data.frame(sites) && !is.character(sites)) {
    cli::cli_abort("{.arg sites} must be a data frame or character vector.")
  }
  if (!site_col %in% names(data)) {
    return(data)
  }
  norm <- function(x) stringr::str_squish(toupper(gsub("[\r\n]+", " ", x)))
  known <- norm(if (is.data.frame(sites)) sites[[site_col]] else sites)
  site_norm <- norm(data[[site_col]])
  is_new <- !is.na(site_norm) & !site_norm %in% known

  coord_col <- intersect(c("site_y_coordinate", "latitude"), names(data))[1]
  new_sites <- tibble::tibble(
    site_name = data[[site_col]][is_new],
    no_coords = if (!is.na(coord_col)) {
      is.na(data[[coord_col]][is_new])
    } else {
      NA
    }
  ) |>
    dplyr::distinct()

  if (isTRUE(verbose) && nrow(new_sites) > 0) {
    n_no_coord <- sum(new_sites$no_coords, na.rm = TRUE)
    cli::cli_alert_warning(
      "{nrow(new_sites)} ES site{?s} not in the reference list \\
      ({n_no_coord} without coordinates); flagged, not dropped."
    )
  }
  attr(data, "polis_new_sites") <- new_sites
  data
}

#' Summarise missingness in key ES surveillance variables
#'
#' A tidy, in-memory replacement for the side-file missingness export: returns
#' one row per variable with the count and percentage of missing values, so a
#' caller can inspect or write it however they like.
#'
#' @param data A cleaned ES data frame.
#' @param vars Character vector of columns to summarise. Default `NULL` uses the
#'   key ES surveillance fields that are present in `data`.
#'
#' @return A tibble with columns `variable`, `n`, `n_missing` and `pct_missing`,
#'   ordered most-missing first.
#'
#' @examples
#' es <- data.frame(
#'   collection_date = as.Date(c("2024-01-01", NA)),
#'   adm0 = c("CHAD", "CHAD"),
#'   classification_all = c(NA, "NEGATIVE")
#' )
#' es_missingness(es)
#'
#' @export
es_missingness <- function(data, vars = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0L) {
    cli::cli_abort("{.arg data} is empty.")
  }
  default_vars <- c(
    "collection_date",
    "year_collection",
    "adm0",
    "adm1",
    "adm2",
    "adm2_guid",
    "site_name",
    "virus_types",
    "classification_all"
  )
  vars <- intersect(vars %||% default_vars, names(data))
  n <- nrow(data)
  n_missing <- vapply(
    vars,
    \(v) sum(is.na(data[[v]]), na.rm = TRUE),
    integer(1)
  )
  tibble::tibble(
    variable = vars,
    n = n,
    n_missing = unname(n_missing),
    pct_missing = round(100 * n_missing / n, 2)
  ) |>
    dplyr::arrange(dplyr::desc(.data$pct_missing))
}

#' Enrich cleaned ES data with country groupings and the polio serotype
#'
#' The always-on enrichment layer, the ES counterpart of `.afp_enrich`: joins
#' the country reference (`country_actual`, `risk_group`, `epi_zones` /
#' `epi_zones_v2`) and reads `polio_type` off `classification_all`. ES has no
#' case-classification flags, so the AFP `afp_class` family does not apply. Each
#' piece is a no-op when its source columns are absent.
#' @noRd
.es_enrich <- function(data) {
  data |>
    .polis_join_country() |>
    .polis_polio_type()
}
