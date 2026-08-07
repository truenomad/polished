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
#' cross-source layer: each POLIS value is checked against WorldPop, and one that
#' is missing or implausible is replaced by a substitution ladder that keeps the
#' district on its own population level.
#'
#' @details
#' # What goes wrong in POLIS population, and what catches it
#'
#' Two jobs, deliberately separate. **Detection** decides whether a value is
#' usable; **substitution** decides what replaces it when it is not. Conflating
#' them is how a repaired value becomes a worse problem than the one it fixed
#' (see *Why substitution is level-matched* below).
#'
#' \tabular{lll}{
#'   **Fault** \tab **Origin** \tab **Handled by** \cr
#'   Duplicate `(place, year)` rows, often conflicting \tab POLIS \tab median
#'     collapse; the conflicts are returned in `meta$dup_conflicts` \cr
#'   Zeros / blanks presented as real values \tab POLIS \tab dropped to `NA` \cr
#'   A single year wildly off the district's own series \tab POLIS \tab
#'     `bad_vs_history` (scaled MAD, `mad_k`) \cr
#'   A district carrying its PARENT's population -- typically where an adm2
#'     shares a name with its adm1 \tab POLIS \tab `bad_vs_worldpop` +
#'     `bad_vs_adm1` \cr
#'   District GUIDs that have since changed boundary \tab POLIS \tab orphan
#'     name crosswalk; unresolved ones stay in `meta$orphan_xwalk` \cr
#'   u5 > u15 > all-ages ordering breaches \tab POLIS \tab `age_order_bad` +
#'     whole-set fallback \cr
#'   WorldPop emptying out over water and dense urban cores, so it convicts a
#'     POLIS value for being right \tab WorldPop \tab `wp_implausible`
#'     (density, `dens_lo`) \cr
#'   The three age bands landing on different sources, so they no longer
#'     describe one population \tab clean_pop \tab shared level ratio +
#'     `age_source_split` \cr
#'   A year repeated verbatim from the one before -- a refresh that did not
#'     happen \tab POLIS \tab `<age>_pop_frozen` (flagged, never substituted) \cr
#'   POLIS coverage starting late / ending early, so head and tail years have
#'     no POLIS at all \tab POLIS \tab the substitution ladder \cr
#' }
#'
#' Two further properties are not faults but must be understood before the
#' output is used. The POLIS-to-WorldPop gap is **heterogeneous across
#' districts** -- in Nigeria the ratio spans roughly 0.7 to 2.7 with a long tail
#' -- so no single national rescaling can reconcile them, which is why every
#' correction here is per district. And POLIS tends to run **high in aggregate**
#' (its Nigerian under-15 total implies a country larger than the UN estimate),
#' so rates computed on it are correspondingly lower. Both are visible because
#' `<age>_pop_polis` and `<age>_pop_wp` are always retained.
#'
#' # Why substitution is level-matched
#'
#' The two sources disagree about a district's *level*, not just its value. So
#' replacing a rejected POLIS year with WorldPop's raw number -- the obvious
#' move, and what this function used to do -- turns a rejected **year** into a
#' rejected **level**: the series then steps by the gap between two sources at
#' whichever years happened to be rejected. That is a district-specific,
#' year-specific discontinuity, and it is the single hardest kind for anything
#' downstream to absorb, because a fitted model reads it as a real change in
#' whatever the denominator feeds rather than as an artefact.
#'
#' So a district never leaves its own level. The ladder, in order:
#'
#' 1. **`polis`** -- a POLIS value that survived every signal.
#' 2. **`polis_interp`** -- an interior gap, linearly interpolated from the
#'    district's own trusted years. Never extrapolates.
#' 3. **`worldpop_levelled`** -- head/tail years, where there is no trusted
#'    value on one side and the shape has to come from WorldPop. Rescaled by
#'    `<age>_pop_level_ratio`, the POLIS:WorldPop ratio at the district's
#'    *nearest* trusted year. Nearest rather than an average because the two
#'    sources grow at different rates, and a summary ratio makes a projected
#'    year step down on a rising series.
#' 4. **`worldpop`** -- raw, and only for a district POLIS never usably
#'    described, which therefore has no level of its own to hold.
#' 5. **`district_trend` / `adm1` / `adm0`** -- the admin ladder, when there is
#'    no WorldPop either.
#'
#' A rejected outlier is treated exactly as a missing value, so detection and
#' substitution stay independent.
#'
#' On Nigerian adm2 (774 districts, 2016-2026) the ladder takes district-years
#' moving more than 25% year-on-year from 780 to 21, raw-WorldPop fallbacks from
#' about 2,880 to 484, and districts whose age bands sit on different levels
#' from 23 to 0.
#'
#' # Worked examples
#'
#' *EWEKORO (Ogun, Nigeria)* -- POLIS reports 131,998 under-15s for 2019 against
#' roughly 39,000 either side. `bad_vs_history` rejects it; rung 2 interpolates
#' 40,062 from the district's own 2018 and 2020. Its 2016, 2017 and 2026 have no
#' POLIS at all and take rung 3.
#'
#' *KATSINA (Nigeria)* -- an adm2 sharing its name with its adm1, reporting
#' 2.1-3.9M under-15s where the district holds roughly 265,000.
#' `bad_vs_worldpop` and `bad_vs_adm1` reject every year, so no trusted value
#' exists to define a level and the district correctly falls to rung 4.
#'
#' *BAKASSI (Cross River, Nigeria)* -- the opposite case. WorldPop gives 82
#' under-15s over 26 km2, about 3 per km2, because the district is largely
#' water. Without the density signal WorldPop would convict POLIS's 26,536;
#' `wp_implausible` disqualifies WorldPop as the arbiter instead, POLIS is kept,
#' and the level ratio is around 315.
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
#' @param thresholds Named list of the implausibility tunables:
#'   \describe{
#'     \item{`ratio_lo`, `ratio_hi`}{a POLIS value below/above this fold of
#'       WorldPop is implausible (`bad_vs_worldpop`, and against the province's
#'       typical ratio for `bad_vs_adm1`).}
#'     \item{`mad_k`}{scaled-MAD distance from the district's own median
#'       (`bad_vs_history`) -- the signal that catches a single bad year.}
#'     \item{`min_votes`}{how many signals must fire to call a value suspect.}
#'     \item{`dens_lo`}{people per km2 below which WORLDPOP is treated as the
#'       implausible source rather than the arbiter (`wp_implausible`). Needs a
#'       polygon `shape`; without one the signal never fires. Guards the case
#'       where WorldPop's raster allocation empties a district out and would
#'       otherwise convict a correct POLIS value.}
#'     \item{`share_lo`, `share_hi`}{bounds on the under-15 share of all-ages
#'       used when reporting age-band coherence.}
#'     \item{`min_level_years`}{how many trusted years a district needs before a
#'       POLIS:WorldPop level ratio is established from them. A level cannot be
#'       inferred from one observation, so below this count the district falls
#'       to raw WorldPop instead of being rescaled on a ratio it has no
#'       evidence for.}
#'   }
#'   Default `list(ratio_lo = 1/3, ratio_hi = 3, mad_k = 5, min_votes = 1L,
#'   dens_lo = 5, share_lo = 0.2, share_hi = 0.7, min_level_years = 2L)`.
#'   Supplying a partial list overrides only the keys given.
#' @param reference_date Date treated as "today" when deciding which boundary
#'   versions are *current* for the orphan-GUID name crosswalk. Default
#'   [Sys.Date()].
#' @param pop_source Which population to use as the chosen `<age>_pop` value
#'   (the denominator indicators read). One of:
#'   \describe{
#'     \item{`"reconciled"`}{(default) a trusted POLIS value, then the district's
#'       own interpolated series, then WorldPop rescaled onto the district's
#'       level, then raw WorldPop, then the district -> province -> country
#'       ladder. See the substitution ladder in Details.}
#'     \item{`"polis"`}{the POLIS value, with interior gaps interpolated from the
#'       district's own series and the rest from the admin ladder; WorldPop is
#'       ignored even if supplied -- the full POLIS population.}
#'     \item{`"worldpop"`}{the WorldPop value, else a POLIS value, else the
#'       ladder. No levelling: this mode is asking for WorldPop's own numbers.}
#'   }
#'   The output always keeps `<age>_pop_polis` and `<age>_pop_wp` alongside the
#'   chosen `<age>_pop`, so every source stays inspectable whatever the mode.
#' @param verbose Emit cli progress headers. Default `TRUE`.
#'
#' @return A named list:
#'   \describe{
#'     \item{`adm2`}{district x year, wide: the id columns plus, per age band
#'       (`u5`/`u15`/`all`), `<age>_pop` (chosen), `<age>_pop_polis`,
#'       `<age>_pop_wp`, `<age>_pop_source` (`polis` / `polis_interp` /
#'       `worldpop_levelled` / `worldpop` / `district_trend` / `adm1` / `adm0`),
#'       `<age>_pop_imputed`, `<age>_pop_level_ratio` (the POLIS:WorldPop ratio
#'       used to put a substituted value on the district's level -- `NA` when
#'       none was needed or none could be formed) and `<age>_pop_frozen` (the
#'       POLIS value repeated verbatim from the previous year). Plus
#'       `age_order_bad` and `age_source_split` (the bands do not share a
#'       level -- non-empty only where no band had a ratio to lend). Restricted
#'       to the boundary valid each year (no double-counting versioned shapes).}
#'     \item{`adm1`, `adm0`}{province / country roll-ups (sums) of the nine pop
#'       columns. The per-district ratio and frozen flags are deliberately not
#'       rolled up: neither is meaningful summed across districts.}
#'     \item{`meta`}{a list (skipped by the file writer): `audit` (one row per
#'       district x year x age, with every signal flag -- `bad_vs_worldpop`,
#'       `bad_vs_history`, `bad_vs_adm1`, `wp_implausible`, `frozen`, `n_votes`,
#'       `polis_suspect`), `dup_conflicts`, `orphan_xwalk`, `params`.}
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
  thresholds = list(
    ratio_lo = 1 / 3,
    ratio_hi = 3,
    mad_k = 5,
    min_votes = 1L,
    dens_lo = 5,
    share_lo = 0.2,
    share_hi = 0.7,
    min_level_years = 2L
  ),
  reference_date = Sys.Date(),
  pop_source = c("reconciled", "polis", "worldpop"),
  verbose = TRUE
) {
  pop_source <- match.arg(pop_source)
  if (pop_source == "worldpop" && is.null(worldpop)) {
    cli::cli_warn(
      "{.arg pop_source} is {.val worldpop} but no {.arg worldpop} was \\
      supplied; falling back to POLIS values where present."
    )
  }
  th <- utils::modifyList(
    list(
      ratio_lo = 1 / 3,
      ratio_hi = 3,
      mad_k = 5,
      min_votes = 1L,
      dens_lo = 5,
      share_lo = 0.2,
      share_hi = 0.7,
      min_level_years = 2L
    ),
    thresholds %||% list()
  )
  # POLIS AgeGroupName -> short band label
  age_map <- c(u5 = "0 to 5 years", u15 = "0 to 15 years", all = "All ages")
  years <- as.integer(years)

  population <- .polis_resolve_ref(population)
  .polis_check_input(population, "population")
  shape <- .polis_resolve_ref(shape)

  # ---- 1. normalise raw POLIS ------------------------------------------------
  if (isTRUE(verbose)) {
    cli::cli_h2("Normalising raw POLIS population")
  }
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
  if (isTRUE(verbose)) {
    cli::cli_h2(switch(
      pop_source,
      reconciled = "Reconciling POLIS against WorldPop and imputing gaps",
      polis = "Selecting POLIS population and imputing gaps",
      worldpop = "Selecting WorldPop population and imputing gaps"
    ))
  }
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
    .pop_impute_age(base, th, a, has_parents, pop_source)
  })
  names(imp) <- names(age_map)
  audit <- dplyr::bind_rows(imp)

  # ---- 4. widen + age-order reconciliation -----------------------------------
  wide <- .pop_widen_reconcile(imp, id_cols, pop_source)

  # ---- 5. roll up to adm1 / adm0 via boundary-validity windows ---------------
  if (isTRUE(verbose)) {
    cli::cli_h2("Rolling up to province and country")
  }
  adm2 <- .pop_apply_validity(wide, shp_geo)
  adm1 <- if (has_parents) .pop_rollup(adm2, .pop_adm1_by) else NULL
  adm0 <- if (has_parents) .pop_rollup(adm2, .pop_adm0_by) else NULL

  if (isTRUE(verbose)) {
    n_adm2 <- nrow(adm2)
    n_yr <- dplyr::n_distinct(adm2$year)
    adm2_fmt <- .polis_big_num(n_adm2)
    yr_fmt <- .polis_big_num(n_yr)
    cli::cli_alert_success(
      "Cleaned population: {adm2_fmt} adm2 {cli::qty(n_adm2)}row{?s} \\
      across {yr_fmt} {cli::qty(n_yr)}year{?s}."
    )
  }

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
  # Match the age label tolerantly (case / spacing / punctuation), so e.g.
  # "0 To 5 Years " still maps. Genuinely different wording (e.g. "0-15 years")
  # will not match, which the zero-match guard below surfaces loudly.
  norm <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))
  rev_map <- stats::setNames(names(age_map), norm(unname(age_map)))
  raw_age <- as.character(population[[c_ag]])
  out <- tibble::tibble(
    adm2_guid = .pop_brace_guid(population[[c_id]]),
    place_name = as.character(population[[c_nm]]),
    year = suppressWarnings(as.integer(population[[c_yr]])),
    age = unname(rev_map[norm(raw_age)]),
    pop_polis = suppressWarnings(as.numeric(population[[c_va]]))
  ) |>
    dplyr::filter(
      !is.na(age),
      !is.na(adm2_guid),
      !is.na(year),
      year %in% years,
      !is.na(pop_polis)
    )
  # A non-empty raw table that yields nothing means the age labels (or the year
  # window) did not match. Fail loudly rather than return silent-empty
  # denominators that would knock the base out of every population indicator.
  if (nrow(out) == 0L && nrow(population) > 0L) {
    seen <- unique(stats::na.omit(raw_age))
    cli::cli_warn(c(
      "No population rows matched the expected age groups \\
      ({.val {unname(age_map)}}) within years {.val {range(years)}}.",
      "i" = "Found {.field AgeGroupName} value{?s}: {.val {utils::head(seen, 8L)}}."
    ))
  }
  out
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
  # Area, when the shape is a real polygon layer, is the only check available
  # here that is independent of BOTH population sources -- see the density
  # signal in .pop_impute_age(). Carried on the attribute table so everything
  # downstream can stay geometry-free.
  area <- NULL
  if (
    inherits(shape, "sf") &&
      !"area_km2" %in% names(shape) &&
      requireNamespace("sf", quietly = TRUE)
  ) {
    # s2 rejects a whole layer over one self-intersecting ring, which global
    # boundary sets routinely carry. .spatial_area() measures with s2 off on an
    # equal-area projection (m2), which tolerates them. Warn if that fails too:
    # without an area the density signal silently never fires.
    km2 <- try(.spatial_area(sf::st_geometry(shape)) / 1e6, silent = TRUE)
    if (inherits(km2, "try-error")) {
      cli::cli_warn(c(
        "Could not compute district areas from {.arg shape}; the density signal
         is disabled.",
        i = "Supply an {.field area_km2} column on {.arg shape} to enable it.",
        x = conditionMessage(attr(km2, "condition"))
      ))
    } else {
      area <- data.frame(adm2_guid = shape[["adm2_guid"]], area_km2 = km2)
    }
  }
  geo <- if (inherits(shape, "sf")) sf::st_drop_geometry(shape) else shape
  if ("iso_3_code" %in% names(geo) && !"country_iso3code" %in% names(geo)) {
    geo <- dplyr::rename(geo, country_iso3code = "iso_3_code")
  }
  if (!is.null(area) && !"area_km2" %in% names(geo)) {
    geo <- dplyr::left_join(geo, area, by = "adm2_guid")
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
      "adm2",
      "area_km2"
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
# value + provenance whose ranking depends on `pop_source`:
#   "reconciled" - a trusted POLIS value, else WorldPop, else the admin ladder
#   "polis"      - any positive POLIS value, else the admin ladder (no WorldPop)
#   "worldpop"   - WorldPop, else a POLIS value, else the admin ladder
# The admin ladder is the district's own temporal median, else the province's
# typical district value that year, else the country's. Anything that is not the
# preferred source for the mode is flagged (`imputed = TRUE`).
#' @noRd
.pop_impute_age <- function(
  base,
  th,
  age,
  has_parents,
  pop_source = "reconciled"
) {
  # a non-positive WorldPop value is not a usable denominator: treat it as
  # missing so it is neither compared against (no Inf ratio) nor chosen as the
  # value (no zero denominator reaching the indicators).
  base$pop_wp <- dplyr::if_else(
    !is.na(base$pop_wp) & base$pop_wp > 0,
    base$pop_wp,
    NA_real_
  )
  # absent whenever no polygon shape was supplied; the density signal then
  # simply never fires rather than erroring
  if (!"area_km2" %in% names(base)) {
    base$area_km2 <- NA_real_
  }
  # tolerate a partial `thresholds` list -- this is called directly in tests and
  # by callers written before a key existed, so a missing key must not error
  th$dens_lo <- th$dens_lo %||% 5
  th$min_level_years <- th$min_level_years %||% 2L
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
      # signal 4: WorldPop itself is the implausible one. Every other signal
      # arbitrates POLIS *against* WorldPop, so where WorldPop is the broken
      # source the votes fire backwards and reject a POLIS value for being
      # right. Area is independent of both, so an implied density below
      # `dens_lo` (people per km2) disqualifies WorldPop as an arbiter rather
      # than the value it is judging. WorldPop allocates by raster and empties
      # out over water and dense urban cores: BAKASSI reads 82 under-15s over
      # 26 km2 = 3.2/km2, against POLIS's 26,536 = 1,020/km2.
      wp_implausible = !is.na(pop_wp) &
        !is.na(area_km2) &
        area_km2 > 0 &
        (pop_wp / area_km2) < th$dens_lo,
      bad_vs_worldpop = bad_vs_worldpop %in% TRUE & !wp_implausible,
      bad_vs_adm1 = bad_vs_adm1 %in% TRUE & !wp_implausible,
      n_votes = (bad_vs_worldpop %in% TRUE) +
        (bad_vs_history %in% TRUE) +
        (bad_vs_adm1 %in% TRUE),
      polis_missing = is.na(polis_pos),
      polis_suspect = n_votes >= th$min_votes,
      # the POLIS values that survived every signal -- the only ones allowed to
      # define this district's level
      trusted_polis = dplyr::if_else(
        !polis_missing & !polis_suspect,
        polis_pos,
        NA_real_
      )
    ) |>
    # ---- the substitution ladder ------------------------------------------
    # Detection says WHETHER a value is usable; this says WHAT replaces it when
    # it is not, and the governing rule is that a district must never leave its
    # own level. Substituting WorldPop's raw value (the previous behaviour)
    # turns a rejected YEAR into a rejected LEVEL: the series then steps by the
    # gap between two sources, which is a district-specific, year-specific
    # discontinuity -- the one kind a fitted model cannot absorb, and the kind
    # that reads as a real change in whatever the denominator feeds.
    dplyr::group_by(adm2_guid) |>
    dplyr::mutate(
      # (a) interior gap: interpolate the district's own trusted series.
      # rule = 1 returns NA outside the trusted range, so this fills gaps
      # BETWEEN trusted years only and never extrapolates.
      polis_interp = .pop_interp(year, trusted_polis),
      # (b) head/tail: no trusted value on one side, so the shape has to come
      # from WorldPop -- but rescaled onto this district's own POLIS level by
      # the ratio between them where both were trustworthy.
      #
      # The ratio is taken from the NEAREST trusted year, not as a median over
      # all of them, because the two sources grow at different rates: POLIS
      # typically outpaces WorldPop, so a median ratio sits below the current
      # one and a projected year would step DOWN on a rising series. Carrying
      # the nearest anchor makes the joint continuous, which is the whole point.
      .obs_ratio = dplyr::if_else(
        !is.na(trusted_polis) & !is.na(pop_wp) & pop_wp > 0,
        trusted_polis / pop_wp,
        NA_real_
      ),
      # A level cannot be established from ONE observation. Requiring at least
      # `min_level_years` trusted years is what stops a single surviving bad
      # value defining the scale for a whole series: KATSINA (NGA) reports its
      # PARENT's population in every year but one, and that lone survivor --
      # itself about half the true figure -- would otherwise set a ratio of
      # 0.54 and halve every correctly-sourced WorldPop year around it. With no
      # ratio the district falls to raw WorldPop, which is the right answer for
      # a district POLIS never usably described.
      n_level_years = sum(!is.na(.obs_ratio)),
      level_ratio = dplyr::if_else(
        n_level_years >= th$min_level_years,
        .pop_fill_nearest(year, .obs_ratio),
        NA_real_
      ),
      level_ratio = dplyr::if_else(
        is.finite(level_ratio) & level_ratio > 0,
        level_ratio,
        NA_real_
      ),
      wp_levelled = pop_wp * level_ratio,
      # frozen: POLIS repeated verbatim from the previous year. Flagged, never
      # substituted -- a carried-forward value is stale, not wrong, and the
      # honest response is to say so. NGA 2023 repeats 2022 for 768 of 775
      # districts, which is a POLIS refresh that did not happen.
      frozen = !is.na(polis_pos) &
        !is.na(dplyr::lag(polis_pos, order_by = year)) &
        polis_pos == dplyr::lag(polis_pos, order_by = year)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(".obs_ratio")) |>
    dplyr::mutate(
      source = dplyr::case_when(
        # "polis": trust any positive POLIS value (no WorldPop check)
        pop_source == "polis" & !polis_missing ~ "polis",
        pop_source == "polis" & !is.na(polis_interp) ~ "polis_interp",
        # "worldpop": WorldPop first, then a POLIS value
        pop_source == "worldpop" & !is.na(pop_wp) ~ "worldpop",
        pop_source == "worldpop" & !polis_missing ~ "polis",
        # "reconciled": trusted POLIS, then the district's own series, then
        # WorldPop put on the district's level, then WorldPop raw (only for a
        # district POLIS never described, which therefore has no level to hold)
        pop_source == "reconciled" & !polis_missing & !polis_suspect ~ "polis",
        pop_source == "reconciled" & !is.na(polis_interp) ~ "polis_interp",
        pop_source == "reconciled" & !is.na(wp_levelled) ~ "worldpop_levelled",
        pop_source == "reconciled" & !is.na(pop_wp) ~ "worldpop",
        # shared admin ladder fallback for every mode
        !is.na(dist_med) ~ "district_trend",
        !is.na(adm1_year_med) ~ "adm1",
        !is.na(adm0_year_med) ~ "adm0",
        .default = NA_character_
      ),
      pop = dplyr::case_when(
        source == "polis" ~ as.numeric(polis_pos),
        source == "polis_interp" ~ as.numeric(polis_interp),
        source == "worldpop_levelled" ~ as.numeric(wp_levelled),
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
      bad_vs_adm1 = bad_vs_adm1 %in% TRUE,
      wp_implausible = wp_implausible %in% TRUE,
      frozen = frozen %in% TRUE
    )
}

# Linear interpolation of `y` over `x`, filling INTERIOR gaps only. rule = 1
# leaves anything outside the observed range as NA, which is what keeps the
# head/tail on the levelled-WorldPop rung of the ladder instead of silently
# extrapolating a trend POLIS never supported.
# Carry `y` to every position from its nearest non-missing neighbour in `x`
# (ties resolved backwards, i.e. the earlier year wins). Used for the
# POLIS-to-WorldPop level ratio, where the nearest observation is the right
# anchor and a global summary is not -- see .pop_impute_age().
#' @noRd
.pop_fill_nearest <- function(x, y) {
  ok <- !is.na(y) & !is.na(x)
  if (!any(ok)) {
    return(rep(NA_real_, length(y)))
  }
  xs <- x[ok]
  ys <- y[ok]
  vapply(
    x,
    function(xi) {
      if (is.na(xi)) {
        return(NA_real_)
      }
      ys[which.min(abs(xs - xi))]
    },
    numeric(1)
  )
}

#' @noRd
.pop_interp <- function(x, y) {
  ok <- !is.na(y) & !is.na(x)
  if (sum(ok) < 2L) {
    return(rep(NA_real_, length(y)))
  }
  o <- order(x[ok])
  stats::approx(
    x = x[ok][o],
    y = y[ok][o],
    xout = x,
    rule = 1,
    ties = "ordered"
  )$y
}

# -----------------------------------------------------------------------------
# Widen + age-order reconciliation
# -----------------------------------------------------------------------------

# Stitch the three age-band audit frames into one wide district x year row and
# enforce u5 <= u15 <= all. Where the chosen values breach the ordering and a
# WorldPop value exists, the offending band(s) fall back to WorldPop (nested
# bands from one source are internally consistent); the breach is flagged.
#' @noRd
.pop_widen_reconcile <- function(imp, id_cols, pop_source = "reconciled") {
  to_wide <- function(df, age) {
    out <- df |>
      dplyr::select(
        dplyr::all_of(id_cols),
        pop,
        polis_pos,
        pop_wp,
        source,
        imputed,
        level_ratio,
        frozen
      )
    keys <- c(
      "pop",
      "polis_pos",
      "pop_wp",
      "source",
      "imputed",
      "level_ratio",
      "frozen"
    )
    names(out)[match(keys, names(out))] <- paste0(
      age,
      c(
        "_pop",
        "_pop_polis",
        "_pop_wp",
        "_pop_source",
        "_pop_imputed",
        "_pop_level_ratio",
        "_pop_frozen"
      )
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

  # ---- age-source coherence -------------------------------------------------
  # The three bands are imputed independently, and POLIS covers a different span
  # for each of them (in AFG, u5 only 2020-2023 against u15's 2014-2025). So a
  # district can end up with one band on its own POLIS level and another on raw
  # WorldPop's -- and the bands are then not describing the same population. In
  # NGA 2025 that produced 23 districts with u15 from WorldPop over all-ages from
  # POLIS, reading a 14% under-15 share against the national 47.6%.
  #
  # The POLIS-to-WorldPop level gap is a property of the DISTRICT -- a boundary
  # and allocation difference -- not of the age band, so a band with no ratio of
  # its own can legitimately borrow a sibling's. That is what makes the whole
  # nested set share one level.
  # Only in "reconciled" mode. "worldpop" mode exists to return WorldPop's own
  # numbers, so levelling them onto POLIS is precisely what the caller did not
  # ask for; "polis" mode never lands on a raw-WorldPop value to begin with.
  ratio_cols <- paste0(c("u5", "u15", "all"), "_pop_level_ratio")
  have_ratio <- intersect(ratio_cols, names(wide))
  if (identical(pop_source, "reconciled") && length(have_ratio)) {
    shared_ratio <- do.call(
      dplyr::coalesce,
      lapply(have_ratio, function(cc) wide[[cc]])
    )
    for (age in c("u5", "u15", "all")) {
      src_c <- paste0(age, "_pop_source")
      wp_c <- paste0(age, "_pop_wp")
      pop_c <- paste0(age, "_pop")
      if (!all(c(src_c, wp_c, pop_c) %in% names(wide))) {
        next
      }
      borrow <- wide[[src_c]] %in%
        "worldpop" &
        !is.na(wide[[wp_c]]) &
        !is.na(shared_ratio)
      wide[[pop_c]][borrow] <- as.integer(round(
        wide[[wp_c]][borrow] * shared_ratio[borrow]
      ))
      wide[[src_c]][borrow] <- "worldpop_levelled"
      wide[[paste0(age, "_pop_level_ratio")]][borrow] <- shared_ratio[borrow]
    }
  }

  wide <- wide |>
    dplyr::mutate(
      age_order_bad = (!is.na(u5_pop) & !is.na(u15_pop) & u5_pop > u15_pop) |
        (!is.na(u15_pop) & !is.na(all_pop) & u15_pop > all_pop) |
        (!is.na(u5_pop) & !is.na(all_pop) & u5_pop > all_pop)
    )
  # Only reconcile a breach when all three bands have a WorldPop value: swapping
  # the whole nested set to one source keeps it internally consistent. A partial
  # swap could leave the ordering still broken and would override an otherwise
  # good POLIS value, so those rows are left as-is (still flagged age_order_bad).
  fix <- wide$age_order_bad &
    !is.na(wide$u5_pop_wp) &
    !is.na(wide$u15_pop_wp) &
    !is.na(wide$all_pop_wp)
  for (age in c("u5", "u15", "all")) {
    pop_c <- paste0(age, "_pop")
    wp_c <- paste0(age, "_pop_wp")
    src_c <- paste0(age, "_pop_source")
    imp_c <- paste0(age, "_pop_imputed")
    wide[[src_c]][fix] <- "worldpop"
    wide[[imp_c]][fix] <- TRUE
    wide[[pop_c]][fix] <- wide[[wp_c]][fix]
  }

  # flag any row whose bands STILL disagree about level (no sibling had a ratio)
  lvl <- function(x) {
    dplyr::case_when(
      x %in% c("polis", "polis_interp", "worldpop_levelled") ~ "unit",
      x %in% "worldpop" ~ "worldpop",
      is.na(x) ~ NA_character_,
      .default = "ladder"
    )
  }
  wide$age_source_split <- mapply(
    function(a, b, c) {
      v <- stats::na.omit(c(a, b, c))
      length(unique(v)) > 1L
    },
    lvl(wide$u5_pop_source),
    lvl(wide$u15_pop_source),
    lvl(wide$all_pop_source)
  )
  wide
}

# -----------------------------------------------------------------------------
# Roll-ups (boundary-validity aware)
# -----------------------------------------------------------------------------

# Summed on roll-up. Deliberately the population columns only: `_level_ratio`
# is a per-district scaling factor and `_frozen` a per-district flag, neither of
# which is meaningful added across districts.
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
  # population totals can exceed the 32-bit integer range at large aggregations,
  # so sum as double rather than coercing to integer (which NA-overflows and
  # warns "NAs introduced by coercion to integer range").
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(as.numeric(x), na.rm = TRUE)
}

# Keep only the boundary valid each year. The shape is time-versioned: a district
# that changed boundary keeps both polygons and they overlap, so summing all
# guids double-counts. For each year keep the guid whose validity window spans
# mid-year; the set valid in any one year tiles the country with no overlap.
#' @noRd
.pop_apply_validity <- function(wide, shp_geo) {
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
  "area_km2",
  "wp_implausible",
  "n_votes",
  "polis_missing",
  "polis_suspect",
  "trusted_polis",
  ".obs_ratio",
  "n_level_years",
  "level_ratio",
  "frozen",
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
