# =============================================================================
# Clean POLIS population + reconcile against WorldPop
#
# Turns the raw POLIS Population reference table into analysis-ready adm0/adm1/
# adm2 denominators. POLIS population is noisy -- duplicate (place, year) rows
# (often with conflicting values), zeros/blanks, district GUIDs that have since
# changed boundary, and the occasional value that is wildly off. clean_pop()
# cleans those by itself, and -- when WorldPop is supplied (raster directories or
# pre-extracted adm2 tables) -- reconciles each POLIS value against it, imputing
# missing/implausible cells and rolling the result up to province and country.
#
# It is the package twin of the other cleaners (clean_afp/clean_es/clean_sia):
# one linear, standalone recipe, no hidden globals. run_pipeline() runs it as the
# `population` stream and persists $adm0/$adm1/$adm2 as polished_pop_* files.
# =============================================================================

#' Clean POLIS population and (optionally) reconcile against WorldPop
#'
#' Builds adm0/adm1/adm2 population denominators (under-5, under-15, all-ages)
#' from the raw POLIS Population table. Used standalone it cleans POLIS by itself
#' -- collapses duplicate `(place, year)` rows to their median, drops zeros/blanks
#' to missing, flags values that jump from a district's own history, and fills
#' gaps from a district -> province -> country ladder. Given `worldpop` it adds a
#' cross-source layer: each POLIS value is checked against WorldPop and a
#' missing/implausible one is replaced (WorldPop first, then the same ladder).
#'
#' @param population A raw POLIS Population data frame (columns `PlaceId`,
#'   `PlaceDisplayName`, `Year`, `AgeGroupName`, `Value`), or a path to one.
#' @param cfg A [polis_config()] object; defaults to [polis_active_config()].
#'   Only its presence is required -- clean_pop reads no scoping from it (pop is
#'   foundational/global), but it keeps the cleaner signature uniform.
#' @param shape Optional **already-processed** district shape (an `sf` polygon
#'   layer or its long ADM2 attribute table, or a path to one). Supplies the
#'   adm0/adm1 parents POLIS lacks, the boundary-validity windows used for the
#'   roll-ups, and the universe of districts. With no shape the output is keyed on
#'   `adm2_guid` only and cannot be rolled up. Default `NULL`.
#' @param worldpop Optional named list with elements `all`, `u5`, `u15`. Each is
#'   either a **directory** of annual WorldPop GeoTIFFs (one per year, the year in
#'   the file name; zonal-summed to the `shape` via `terra` + `exactextractr` --
#'   both optional Suggests), **or** a pre-extracted adm2-by-year table (data
#'   frame or path) carrying `adm2_guid`, `year` and a population column. `NULL`
#'   (default) runs the POLIS-only path.
#' @param years Calendar years to keep (POLIS carries 1990-2034 incl. projections).
#'   Default `2010:2027`.
#' @param thresholds Named list of the implausibility tunables: `ratio_lo` /
#'   `ratio_hi` (a POLIS value below/above this fold of WorldPop is implausible),
#'   `mad_k` (scaled-MAD distance from the district's own median), `min_votes`
#'   (how many signals must fire to call a value suspect). Default
#'   `list(ratio_lo = 1/3, ratio_hi = 3, mad_k = 5, min_votes = 1L)`.
#' @param reference_date Date treated as "today" when deciding which boundary
#'   versions are *current* for the orphan-GUID name crosswalk. Default
#'   [Sys.Date()].
#' @param verbose Emit cli progress headers. Default `TRUE`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`adm2`}{district x year, wide: the id columns plus, per age band
#'       (`u5`/`u15`/`all`), `<age>_pop` (chosen), `<age>_pop_polis`,
#'       `<age>_pop_wp`, `<age>_pop_source` (`polis`/`worldpop`/`district_trend`/
#'       `adm1`/`adm0`) and `<age>_pop_imputed`, plus `age_order_bad`. Restricted
#'       to the boundary valid each year (no double-counting versioned shapes).}
#'     \item{`adm1`, `adm0`}{province / country roll-ups (sums) of the nine pop
#'       columns.}
#'     \item{`meta`}{a list (skipped by the file writer): `audit` (one row per
#'       district x year x age, with every signal flag), `dup_conflicts`,
#'       `orphan_xwalk`, `params`.}
#'   }
#'
#' @seealso [run_pipeline()], which runs this as the `population` stream;
#'   [checks_pop()], which turns the result into a data-quality workbook.
#' @examples
#' pop_raw <- data.frame(
#'   PlaceId = "0cda1c45-9529-4188-aaaa-000000000001",
#'   PlaceDisplayName = "SOMEWHERE",
#'   Year = 2020, AgeGroupName = "0 to 15 years", Value = 1000,
#'   check.names = FALSE
#' )
#' res <- clean_pop(pop_raw, years = 2020, verbose = FALSE)
#' names(res)
#'
#' @export
clean_pop <- function(
  population,
  cfg = polis_active_config(),
  shape = NULL,
  worldpop = NULL,
  years = 2010:2027,
  thresholds = list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
  reference_date = Sys.Date()
) {
  th <- utils::modifyList(
    list(ratio_lo = 1 / 3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
    thresholds %||% list()
  )
  # POLIS AgeGroupName -> short band label
  age_map <- c(u5 = "0 to 5 years", u15 = "0 to 15 years", all = "All ages")
  years <- as.integer(years)

  population <- .polis_resolve_ref(population)
  .polis_check_input(population, "population")
  shape <- .polis_resolve_ref(shape)

  # ---- 1. normalise raw POLIS ------------------------------------------------
  cli::cli_h2("Normalising raw POLIS population")
  polis_long <- .pop_normalise_raw(population, age_map, years)
  # conflicting duplicate (place, year, age) rows, for the QA workbook
  dup_conflicts <- .pop_dup_conflicts(polis_long)
  # collapse duplicate (guid, year, age) to one value (median)
  polis_dedup <- polis_long |>
    dplyr::summarise(
      pop_polis = stats::median(pop_polis, na.rm = TRUE),
      .by = c(adm2_guid, year, age)
    )

  # ---- 2. shape-derived parents / orphan crosswalk / district universe -------
  shp_geo <- .pop_shape_geo(shape)
  has_parents <- !is.null(shp_geo) &&
    all(c("adm0_guid", "adm1_guid") %in% names(shp_geo))

  orphan_xwalk <- NULL
  if (!is.null(shp_geo)) {
    orphan_xwalk <- .pop_orphan_xwalk(polis_long, shp_geo, reference_date)
    guid_remap <- orphan_xwalk |>
      dplyr::filter(xwalk_status == "resolved") |>
      dplyr::distinct(polis_guid, current_guid)
    if (nrow(guid_remap) > 0L) {
      polis_dedup <- polis_dedup |>
        dplyr::left_join(guid_remap, by = c("adm2_guid" = "polis_guid")) |>
        dplyr::mutate(adm2_guid = dplyr::coalesce(current_guid, adm2_guid)) |>
        dplyr::select(-current_guid) |>
        # a remap can collide two POLIS guids onto one current district
        dplyr::summarise(
          pop_polis = stats::median(pop_polis, na.rm = TRUE),
          .by = c(adm2_guid, year, age)
        )
    }
  }

  # the district x year universe every output is built on: every district in the
  # shape (so denominators exist even where POLIS is silent), else the districts
  # POLIS itself names.
  universe <- .pop_universe(shp_geo, polis_dedup, years)
  id_cols <- intersect(
    c(
      "who_region",
      "country_iso3code",
      "adm0",
      "adm0_guid",
      "adm1",
      "adm1_guid",
      "adm2",
      "adm2_guid",
      "year"
    ),
    names(universe)
  )

  # ---- 3. impute each age band -----------------------------------------------
  cli::cli_h2("Reconciling POLIS against WorldPop and imputing gaps")
  imp <- lapply(names(age_map), function(a) {
    polis_a <- polis_dedup |>
      dplyr::filter(age == a) |>
      dplyr::select(adm2_guid, year, pop_polis)
    wp_a <- .pop_worldpop_table(
      .pop_wp_element(worldpop, a, age_map),
      a,
      shape,
      reference_date
    )
    base <- universe |>
      dplyr::left_join(polis_a, by = c("adm2_guid", "year"))
    base <- if (!is.null(wp_a)) {
      dplyr::left_join(base, wp_a, by = c("adm2_guid", "year"))
    } else {
      dplyr::mutate(base, pop_wp = NA_real_)
    }
    .pop_impute_age(base, th, a, has_parents)
  })
  names(imp) <- names(age_map)
  audit <- dplyr::bind_rows(imp)

  # ---- 4. widen + age-order reconciliation -----------------------------------
  wide <- .pop_widen_reconcile(imp, id_cols)

  # ---- 5. roll up to adm1 / adm0 via boundary-validity windows ---------------
  cli::cli_h2("Rolling up to province and country")
  adm2 <- .pop_apply_validity(wide, shp_geo, reference_date)
  adm1 <- if (has_parents) .pop_rollup(adm2, .pop_adm1_by) else NULL
  adm0 <- if (has_parents) .pop_rollup(adm2, .pop_adm0_by) else NULL

  cli::cli_alert_success(
    "Cleaned population: {nrow(adm2)} adm2 row{?s} \\
    across {dplyr::n_distinct(adm2$year)} year{?s}."
  )

  list(
    adm0 = adm0,
    adm1 = adm1,
    adm2 = adm2,
    meta = list(
      audit = audit,
      dup_conflicts = dup_conflicts,
      orphan_xwalk = orphan_xwalk,
      params = list(
        years = years,
        thresholds = th,
        age_map = age_map,
        has_worldpop = !is.null(worldpop),
        has_shape = !is.null(shp_geo)
      )
    )
  )
}

# -----------------------------------------------------------------------------
# Raw POLIS normalisation
# -----------------------------------------------------------------------------

# Find the first column matching any candidate name (case-/punctuation-
# insensitive), so a raw OData PascalCase table and a snake_cased one both work.
#' @noRd
.pop_pick <- function(df, candidates, required = TRUE) {
  nm <- names(df)
  hit <- intersect(candidates, nm)
  if (length(hit) > 0L) {
    return(hit[[1L]])
  }
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
  hit <- nm[norm(nm) %in% norm(candidates)]
  if (length(hit) > 0L) {
    return(hit[[1L]])
  }
  if (isTRUE(required)) {
    cli::cli_abort(
      "Population table has no {.val {candidates[[1]]}} column."
    )
  }
  NA_character_
}

# Wrap a GUID in braces + upper-case it, idempotently (POLIS PlaceId is bare;
# shapes/extracts are already braced).
#' @noRd
.pop_brace_guid <- function(x) {
  x <- toupper(trimws(as.character(x)))
  ifelse(
    is.na(x) | x == "",
    NA_character_,
    ifelse(startsWith(x, "{"), x, paste0("{", x, "}"))
  )
}

# Raw POLIS -> long (adm2_guid, place_name, year, age, pop_polis), pre-dedup,
# scoped to the three known age groups and the requested year window.
#' @noRd
.pop_normalise_raw <- function(population, age_map, years) {
  c_id <- .pop_pick(population, c("PlaceId", "place_id"))
  c_nm <- .pop_pick(population, c("PlaceDisplayName", "place_display_name"))
  c_yr <- .pop_pick(population, c("Year", "year"))
  c_ag <- .pop_pick(population, c("AgeGroupName", "age_group_name"))
  c_va <- .pop_pick(population, c("Value", "value"))
  rev_map <- stats::setNames(names(age_map), unname(age_map))
  tibble::tibble(
    adm2_guid = .pop_brace_guid(population[[c_id]]),
    place_name = as.character(population[[c_nm]]),
    year = suppressWarnings(as.integer(population[[c_yr]])),
    age = unname(rev_map[as.character(population[[c_ag]])]),
    pop_polis = suppressWarnings(as.numeric(population[[c_va]]))
  ) |>
    dplyr::filter(
      !is.na(age),
      !is.na(adm2_guid),
      !is.na(year),
      year %in% years,
      !is.na(pop_polis)
    )
}

# Place-years with >1 distinct POLIS value (the median is what dedup keeps).
#' @noRd
.pop_dup_conflicts <- function(polis_long) {
  polis_long |>
    dplyr::summarise(
      n_values = dplyr::n_distinct(pop_polis),
      values = paste(sort(unique(pop_polis)), collapse = "; "),
      chosen_median = stats::median(pop_polis, na.rm = TRUE),
      .by = c(adm2_guid, place_name, year, age)
    ) |>
    dplyr::filter(n_values > 1L) |>
    dplyr::arrange(dplyr::desc(n_values), place_name, year)
}

# -----------------------------------------------------------------------------
# Shape: attributes, orphan crosswalk, district universe
# -----------------------------------------------------------------------------

# Drop geometry (if any) and standardise the ISO column name to the package
# convention (country_iso3code). Returns NULL when no shape was supplied.
#' @noRd
.pop_shape_geo <- function(shape) {
  if (is.null(shape)) {
    return(NULL)
  }
  geo <- if (inherits(shape, "sf")) sf::st_drop_geometry(shape) else shape
  if ("iso_3_code" %in% names(geo) && !"country_iso3code" %in% names(geo)) {
    geo <- dplyr::rename(geo, country_iso3code = "iso_3_code")
  }
  geo
}

# POLIS carries no adm0/adm1 parents, so a POLIS guid absent from the shape can
# only be rescued by an UNAMBIGUOUS current adm2 *name* (after stripping a
# "(YYYY-YYYY)" boundary-version suffix). Returns one row per orphan guid with a
# resolution status; only "resolved" rows are safe to remap.
#' @noRd
.pop_orphan_xwalk <- function(polis_long, shp_geo, reference_date) {
  all_guids <- toupper(shp_geo$adm2_guid)
  cur <- shp_geo
  if ("enddate" %in% names(shp_geo)) {
    cur <- shp_geo[
      .polis_as_date(shp_geo$enddate) > reference_date,
      ,
      drop = FALSE
    ]
  }
  current_names <- cur |>
    dplyr::transmute(
      name = toupper(trimws(adm2)),
      current_guid = toupper(adm2_guid)
    ) |>
    dplyr::add_count(name)
  unambiguous <- current_names |>
    dplyr::filter(n == 1L) |>
    dplyr::distinct(name, current_guid)
  ambiguous_names <- current_names |>
    dplyr::filter(n > 1L) |>
    dplyr::pull(name) |>
    unique()

  polis_long |>
    dplyr::distinct(adm2_guid, place_name) |>
    dplyr::mutate(
      polis_guid = toupper(adm2_guid),
      name = toupper(trimws(stringr::str_remove(
        place_name,
        "\\s*\\([0-9]{4}-[0-9]{4}\\)\\s*$"
      )))
    ) |>
    dplyr::filter(!polis_guid %in% all_guids) |>
    dplyr::left_join(unambiguous, by = "name") |>
    dplyr::summarise(
      polis_name = dplyr::first(place_name),
      n_current = dplyr::n_distinct(current_guid[!is.na(current_guid)]),
      current_guid = current_guid[!is.na(current_guid)][1],
      any_ambiguous = any(name %in% ambiguous_names),
      .by = polis_guid
    ) |>
    dplyr::mutate(
      current_guid = dplyr::if_else(
        n_current == 1L,
        current_guid,
        NA_character_
      ),
      xwalk_status = dplyr::case_when(
        n_current == 1L ~ "resolved",
        n_current > 1L | any_ambiguous ~ "ambiguous",
        .default = "no_match"
      )
    )
}

# Every district x year the outputs are defined on. From the shape when present
# (one row per adm2_guid carrying its parents), else from the POLIS guids.
#' @noRd
.pop_universe <- function(shp_geo, polis_dedup, years) {
  if (!is.null(shp_geo)) {
    id_src <- c(
      "who_region",
      "country_iso3code",
      "adm0",
      "adm0_guid",
      "adm1",
      "adm1_guid",
      "adm2"
    )
    districts <- shp_geo |>
      dplyr::summarise(
        dplyr::across(dplyr::any_of(id_src), dplyr::first),
        .by = adm2_guid
      )
  } else {
    districts <- dplyr::distinct(polis_dedup, adm2_guid)
  }
  tidyr::crossing(districts, year = years)
}

# -----------------------------------------------------------------------------
# WorldPop ingestion (pre-extracted table, or raster directory -> zonal sum)
# -----------------------------------------------------------------------------

# Pull the WorldPop entry for an age band, accepting either the short key (u5 /
# u15 / all) or the full POLIS AgeGroupName as the list name.
#' @noRd
.pop_wp_element <- function(worldpop, age, age_map) {
  if (is.null(worldpop)) {
    return(NULL)
  }
  el <- worldpop[[age]]
  if (is.null(el)) {
    el <- worldpop[[unname(age_map[[age]])]]
  }
  el
}

# Resolve a WorldPop entry to a tidy adm2 x year table (adm2_guid, year, pop_wp).
# A data frame / file is used as-is; a directory is zonal-summed off the shape.
#' @noRd
.pop_worldpop_table <- function(el, age, shape, reference_date) {
  if (is.null(el)) {
    return(NULL)
  }
  if (is.character(el) && length(el) == 1L && dir.exists(el)) {
    if (is.null(shape)) {
      cli::cli_abort(
        "A {.arg shape} is required to extract WorldPop rasters from {.file {el}}."
      )
    }
    return(.polis_extract_worldpop(el, shape))
  }
  tbl <- if (is.data.frame(el)) el else .polis_resolve_ref(el)
  g <- .pop_pick(tbl, c("adm2_guid", "guid", "PlaceId", "place_id"))
  y <- .pop_pick(tbl, c("year", "Year"))
  p <- .pop_pick(
    tbl,
    c(
      "pop_wp",
      paste0(age, "_pop"),
      "u15_pop",
      "u5_pop",
      "total_pop",
      "all_pop",
      "pop",
      "sum",
      "value"
    ),
    required = FALSE
  )
  if (is.na(p)) {
    num <- setdiff(names(tbl)[vapply(tbl, is.numeric, logical(1))], c(g, y))
    if (length(num) != 1L) {
      cli::cli_abort(
        "Cannot identify the WorldPop population column for {.val {age}}."
      )
    }
    p <- num
  }
  tibble::tibble(
    adm2_guid = .pop_brace_guid(tbl[[g]]),
    year = suppressWarnings(as.integer(tbl[[y]])),
    pop_wp = suppressWarnings(as.numeric(tbl[[p]]))
  ) |>
    dplyr::filter(!is.na(adm2_guid), !is.na(year)) |>
    # one value per district x year (a guid may recur across boundary versions)
    dplyr::summarise(
      pop_wp = stats::median(pop_wp, na.rm = TRUE),
      .by = c(adm2_guid, year)
    )
}

#' Zonal-sum a directory of annual WorldPop rasters to a district shape
#'
#' Internal helper behind [clean_pop()]'s `worldpop` directory option. Lists the
#' GeoTIFFs in `dir` (one per year, the year parsed from the file name), sums each
#' over the polygons of `shape`, and returns a long `adm2_guid` x `year` x
#' `pop_wp` table. Requires the optional `terra` + `exactextractr` packages; with
#' either absent it aborts with an install hint (pass a pre-extracted table to
#' `worldpop` instead).
#' Are the optional raster-extraction packages available? (a seam for testing)
#' @noRd
.pop_has_raster_backend <- function() {
  requireNamespace("terra", quietly = TRUE) &&
    requireNamespace("exactextractr", quietly = TRUE)
}

#' @noRd
.polis_extract_worldpop <- function(dir, shape) {
  if (isFALSE(.pop_has_raster_backend())) {
    cli::cli_abort(c(
      "WorldPop raster extraction needs {.pkg terra} + {.pkg exactextractr}.",
      "i" = "Install them, or pass a pre-extracted adm2 table to {.arg worldpop}."
    ))
  }
  sfx <- sf::st_as_sf(shape)
  if (!"adm2_guid" %in% names(sfx)) {
    cli::cli_abort(
      "{.arg shape} must carry an {.field adm2_guid} column for raster extraction."
    )
  }
  files <- list.files(
    dir,
    pattern = "\\.tif{1,2}$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_abort("No GeoTIFFs found in {.file {dir}}.")
  }
  yrs <- as.integer(stringr::str_extract(basename(files), "[0-9]{4}"))
  keep <- !is.na(yrs)
  files <- files[keep]
  yrs <- yrs[keep]
  guid <- .pop_brace_guid(sfx$adm2_guid)
  parts <- lapply(seq_along(files), function(i) {
    r <- terra::rast(files[[i]])
    s <- exactextractr::exact_extract(r, sfx, "sum", progress = FALSE)
    tibble::tibble(adm2_guid = guid, year = yrs[[i]], pop_wp = as.numeric(s))
  })
  dplyr::bind_rows(parts) |>
    dplyr::summarise(
      pop_wp = stats::median(pop_wp, na.rm = TRUE),
      .by = c(adm2_guid, year)
    )
}

# -----------------------------------------------------------------------------
# Imputation: signals + district -> province -> country ladder
# -----------------------------------------------------------------------------

# Impute one age band. `base` is the district x year universe already joined to
# pop_polis and pop_wp. Computes the three implausibility signals, then chooses a
# value + provenance: a trusted POLIS value, else WorldPop, else the district's
# own temporal median, else the province's typical district value that year, else
# the country's. Everything that is not a trusted POLIS value is flagged
# (`imputed = TRUE`).
#' @noRd
.pop_impute_age <- function(base, th, age, has_parents) {
  has_wp <- any(!is.na(base$pop_wp))
  d <- base |>
    dplyr::mutate(
      polis_pos = dplyr::if_else(
        !is.na(pop_polis) & pop_polis > 0,
        pop_polis,
        NA_real_
      )
    ) |>
    # signal 2: the district's own robust centre across the year window
    dplyr::group_by(adm2_guid) |>
    dplyr::mutate(
      dist_med = stats::median(polis_pos, na.rm = TRUE),
      dist_mad = stats::mad(polis_pos, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(wp_ratio = polis_pos / pop_wp)

  if (has_parents) {
    d <- d |>
      dplyr::group_by(adm1_guid, year) |>
      dplyr::mutate(
        adm1_year_med = stats::median(polis_pos, na.rm = TRUE),
        adm1_med_ratio = stats::median(wp_ratio, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(adm0_guid, year) |>
      dplyr::mutate(adm0_year_med = stats::median(polis_pos, na.rm = TRUE)) |>
      dplyr::ungroup()
  } else {
    d$adm1_year_med <- NA_real_
    d$adm1_med_ratio <- NA_real_
    d$adm0_year_med <- NA_real_
  }

  d |>
    dplyr::mutate(
      bad_vs_worldpop = has_wp &
        !is.na(wp_ratio) &
        (wp_ratio < th$ratio_lo | wp_ratio > th$ratio_hi),
      bad_vs_history = !is.na(dist_mad) &
        dist_mad > 0 &
        abs(polis_pos - dist_med) / dist_mad > th$mad_k,
      bad_vs_adm1 = !is.na(adm1_med_ratio) &
        adm1_med_ratio > 0 &
        ((wp_ratio / adm1_med_ratio) < th$ratio_lo |
          (wp_ratio / adm1_med_ratio) > th$ratio_hi),
      n_votes = (bad_vs_worldpop %in% TRUE) +
        (bad_vs_history %in% TRUE) +
        (bad_vs_adm1 %in% TRUE),
      polis_missing = is.na(polis_pos),
      polis_suspect = n_votes >= th$min_votes,
      source = dplyr::case_when(
        !polis_missing & !polis_suspect ~ "polis",
        !is.na(pop_wp) ~ "worldpop",
        !is.na(dist_med) ~ "district_trend",
        !is.na(adm1_year_med) ~ "adm1",
        !is.na(adm0_year_med) ~ "adm0",
        .default = NA_character_
      ),
      pop = dplyr::case_when(
        source == "polis" ~ as.numeric(polis_pos),
        source == "worldpop" ~ as.numeric(pop_wp),
        source == "district_trend" ~ as.numeric(dist_med),
        source == "adm1" ~ as.numeric(adm1_year_med),
        source == "adm0" ~ as.numeric(adm0_year_med),
        .default = NA_real_
      ),
      pop = as.integer(round(pop)),
      imputed = !is.na(source) & source != "polis",
      age_group = age,
      # NA signals read as FALSE in the audit trail
      bad_vs_worldpop = bad_vs_worldpop %in% TRUE,
      bad_vs_history = bad_vs_history %in% TRUE,
      bad_vs_adm1 = bad_vs_adm1 %in% TRUE
    )
}

# -----------------------------------------------------------------------------
# Widen + age-order reconciliation
# -----------------------------------------------------------------------------

# Stitch the three age-band audit frames into one wide district x year row and
# enforce u5 <= u15 <= all. Where the chosen values breach the ordering and a
# WorldPop value exists, the offending band(s) fall back to WorldPop (nested
# bands from one source are internally consistent); the breach is flagged.
#' @noRd
.pop_widen_reconcile <- function(imp, id_cols) {
  to_wide <- function(df, age) {
    out <- df |>
      dplyr::select(
        dplyr::all_of(id_cols),
        pop,
        polis_pos,
        pop_wp,
        source,
        imputed
      )
    keys <- c("pop", "polis_pos", "pop_wp", "source", "imputed")
    names(out)[match(keys, names(out))] <- paste0(
      age,
      c("_pop", "_pop_polis", "_pop_wp", "_pop_source", "_pop_imputed")
    )
    out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with(c("_pop_polis", "_pop_wp")),
          as.integer
        )
      )
  }
  wide <- Reduce(
    function(a, b) dplyr::left_join(a, b, by = id_cols),
    Map(to_wide, imp, names(imp))
  )

  wide <- wide |>
    dplyr::mutate(
      age_order_bad = (!is.na(u5_pop) & !is.na(u15_pop) & u5_pop > u15_pop) |
        (!is.na(u15_pop) & !is.na(all_pop) & u15_pop > all_pop) |
        (!is.na(u5_pop) & !is.na(all_pop) & u5_pop > all_pop)
    )
  for (age in c("u5", "u15", "all")) {
    pop_c <- paste0(age, "_pop")
    wp_c <- paste0(age, "_pop_wp")
    src_c <- paste0(age, "_pop_source")
    imp_c <- paste0(age, "_pop_imputed")
    fix <- wide$age_order_bad & !is.na(wide[[wp_c]])
    wide[[src_c]][fix] <- "worldpop"
    wide[[imp_c]][fix] <- TRUE
    wide[[pop_c]][fix] <- wide[[wp_c]][fix]
  }
  wide
}

# -----------------------------------------------------------------------------
# Roll-ups (boundary-validity aware)
# -----------------------------------------------------------------------------

.pop_pop_cols <- c(
  "u5_pop",
  "u5_pop_polis",
  "u5_pop_wp",
  "u15_pop",
  "u15_pop_polis",
  "u15_pop_wp",
  "all_pop",
  "all_pop_polis",
  "all_pop_wp"
)
.pop_adm1_by <- c(
  "who_region",
  "country_iso3code",
  "adm0",
  "adm0_guid",
  "adm1",
  "adm1_guid",
  "year"
)
.pop_adm0_by <- c("who_region", "country_iso3code", "adm0", "adm0_guid", "year")

# Sum that stays NA only when every input is NA (so an all-missing band is not
# silently reported as zero).
#' @noRd
.pop_sum_or_na <- function(x) {
  if (all(is.na(x))) NA_integer_ else as.integer(sum(x, na.rm = TRUE))
}

# Keep only the boundary valid each year. The shape is time-versioned: a district
# that changed boundary keeps both polygons and they overlap, so summing all
# guids double-counts. For each year keep the guid whose validity window spans
# mid-year; the set valid in any one year tiles the country with no overlap.
#' @noRd
.pop_apply_validity <- function(wide, shp_geo, reference_date) {
  if (
    is.null(shp_geo) ||
      !all(c("startdate", "enddate") %in% names(shp_geo))
  ) {
    if (!is.null(shp_geo)) {
      cli::cli_alert_warning(
        "Shape lacks {.field startdate}/{.field enddate}; rolling up without \\
        validity windows (versioned boundaries may double-count)."
      )
    }
    return(wide)
  }
  validity <- shp_geo |>
    dplyr::summarise(
      startdate = min(.polis_as_date(startdate), na.rm = TRUE),
      enddate = max(.polis_as_date(enddate), na.rm = TRUE),
      .by = adm2_guid
    )
  wide |>
    dplyr::left_join(validity, by = "adm2_guid") |>
    dplyr::mutate(midyear = as.Date(paste0(year, "-07-01"))) |>
    dplyr::filter(
      is.na(startdate) | (startdate <= midyear & enddate >= midyear)
    ) |>
    dplyr::select(-startdate, -enddate, -midyear)
}

# Sum the nine pop columns over an admin level x year.
#' @noRd
.pop_rollup <- function(adm2, by) {
  by <- intersect(by, names(adm2))
  adm2 |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(.pop_pop_cols), .pop_sum_or_na),
      .by = dplyr::all_of(by)
    )
}

# Bare-name bindings used in the dplyr pipelines above.
utils::globalVariables(c(
  "adm2_guid",
  "year",
  "age",
  "pop_polis",
  "place_name",
  "value",
  "n_values",
  "chosen_median",
  "who_region",
  "iso_3_code",
  "country_iso3code",
  "adm0",
  "adm0_guid",
  "adm1",
  "adm1_guid",
  "adm2",
  "startdate",
  "enddate",
  "midyear",
  "name",
  "n",
  "n_current",
  "any_ambiguous",
  "current_guid",
  "polis_guid",
  "xwalk_status",
  "pop_wp",
  "polis_pos",
  "dist_med",
  "dist_mad",
  "wp_ratio",
  "adm1_year_med",
  "adm1_med_ratio",
  "adm0_year_med",
  "bad_vs_worldpop",
  "bad_vs_history",
  "bad_vs_adm1",
  "n_votes",
  "polis_missing",
  "polis_suspect",
  "source",
  "pop",
  "imputed",
  "age_group",
  "u5_pop",
  "u15_pop",
  "all_pop",
  "u5_pop_wp",
  "u15_pop_wp",
  "all_pop_wp",
  "age_order_bad"
))
