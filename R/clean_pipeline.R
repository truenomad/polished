# =============================================================================
# Pipeline configuration
#
# polis_config() is the single, overridable settings object every cleaner and
# the orchestrator share. It holds folder names, run constants, the column
# ordering convention, optional reference handles (synonyms / QA sink), and the
# run manifest (inputs / shape / population / output + cache dirs). There are
# deliberately NO column-name arguments here: column naming lives in the package
# crosswalk (see polis_crosswalk()), so the canonical schema is data, not
# scattered function defaults.
#
# Calling polis_config() also *registers* the result as the session-active
# config, so every later cleaner/orchestrator call defaults to it via
# polis_active_config() -- set it once, and the whole pipeline uses it.
# =============================================================================

# Session store for the active config. Holds a single `active` slot; populated
# by polis_config() and read by polis_active_config().
.polis_config_store <- new.env(parent = emptyenv())

#' The session-active POLIS configuration
#'
#' Returns the configuration most recently built by [polis_config()] in this
#' session -- the object every cleaner and orchestrator defaults to. If none has
#' been built yet, a default `polis_config()` is created, registered, and
#' returned, so the getter always yields a usable config.
#'
#' @return The active `polis_config` object.
#'
#' @seealso [polis_config()], which sets the active config.
#' @examples
#' polis_config(start_year = 2018)
#' polis_active_config()$start_year
#'
#' @export
polis_active_config <- function() {
  if (is.null(.polis_config_store$active)) {
    polis_config()
  }
  .polis_config_store$active
}

#' Build a POLIS pipeline configuration
#'
#' Creates the settings object shared by the cleaners ([clean_afp()],
#' [clean_es()], [clean_virus()], [clean_sia()]) and the orchestrator
#' ([run_pipeline()]). Every field has a sensible default and can be overridden.
#'
#' @param start_year Earliest onset/collection year to retain (default `2020`).
#' @param regions WHO region codes the pipeline is scoped to. Cleaned rows are
#'   filtered to these regions (on the `who_region` column) by [run_pipeline()];
#'   rows with no region value are kept. The default (all six WHO regions) is a
#'   no-op that retains every row.
#' @param column_roles Ordered named list of regex patterns that classify output
#'   columns into ordering groups. Columns are emitted group-by-group in this
#'   order; anything matching no pattern is treated as `other` and placed last.
#'   This is how [order_columns()] enforces id -> location -> time -> other
#'   without hardcoding column names.
#' @param synonyms Optional EPID/geoplace synonym table for [remap_synonyms()].
#'   `NULL` (default) makes synonym remapping a no-op.
#' @param qa Optional handle (path or list) where ambiguous-key flags are routed
#'   by [flag_ambiguous()]. `NULL` (default) collects flags in-memory only.
#' @param population Optional under-15 population denominators used by
#'   [calc_polio_indicators()] in [run_pipeline()]. Either a data frame or a
#'   path to one (read via the file extension); `NULL` (default) skips the
#'   rate indicators that need a denominator -- unless a `population` *input* is
#'   supplied, in which case [run_pipeline()] uses the adm2 table [clean_pop()]
#'   produces as the denominator (so the pipeline makes its own).
#' @param worldpop Optional named list (`all`, `u5`, `u15`) of WorldPop sources
#'   passed to [clean_pop()] when a `population` input is present: each element a
#'   directory of annual GeoTIFFs (zonal-summed to `shape`) or a pre-extracted
#'   adm2-by-year table / path. `NULL` (default) runs the POLIS-only path.
#' @param pop_years Calendar years to retain when cleaning population (POLIS
#'   carries far-future projections). `NULL` (default) uses [clean_pop()]'s
#'   default window.
#' @param shape Optional **already-processed** district shape passed to every
#'   cleaner as `shape =` for admin reconciliation (an `sf` polygon layer or a
#'   long ADM2 attribute table, or a path to one). `NULL` (default) disables
#'   shape-based admin recovery.
#' @param inputs The raw POLIS tables to clean, attached to the config so
#'   [run_pipeline()] can be called as `run_pipeline(cfg = cfg)` with no separate
#'   `inputs` argument. One of: a **directory path** to `raw_*` files; a **named
#'   list of file paths** (recognised names `afp`, `es`, `hum_spec`, `activity`,
#'   `subactivity`, `lqas`, `im`); or a **named list of data frames**. Paths are
#'   read on demand at run time -- prefer them so the config stays a lightweight,
#'   serialisable manifest (`cfg$inputs$afp` is then the file path).
#'   `NULL` (default) means inputs are passed to [run_pipeline()] directly.
#' @param output_dir Optional directory to persist outputs to. When set,
#'   [run_pipeline()] writes the `polished_*` data files to its `data/`
#'   sub-directory and a `checks_*` workbook per dataset to its `checks/`
#'   sub-directory. `NULL` (default) returns the cleaned set without writing.
#' @param cache_dir Optional directory for the opt-in, content-addressed clean
#'   cache. When set, [run_pipeline()] caches each cleaned stream (`afp`, `es`,
#'   `hum_spec`, `sia`) keyed on a fingerprint of its source file (path + size +
#'   mtime), the relevant config, and a per-cleaner logic version. On a later run
#'   with an unchanged source, the cleaned table is read straight from the cache
#'   -- skipping both the raw read and the clean. Delete the directory (or its
#'   `clean_*` files) to force a rebuild. `NULL` (default) disables caching.
#' @param parse_types If `TRUE` (default) each cleaner finishes by inferring
#'   column base types (character -> numeric/integer/date/datetime/logical) via
#'   [auto_parse_types()] (no factor conversion). Set `FALSE` to keep the raw
#'   character columns.
#' @param drop_empty_cols If `TRUE` (default) each cleaner drops columns that are
#'   entirely `NA` after cleaning. Note this makes the output schema depend on
#'   the data; set `FALSE` for a fixed column set across runs.
#'
#' @return An object of class `polis_config` (a named list). Also registered as
#'   the session-active config (see [polis_active_config()]).
#'
#' @examples
#' cfg <- polis_config(start_year = 2018)
#' cfg$column_roles
#'
#' @export
polis_config <- function(
  start_year = 2020,
  regions = c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO"),
  column_roles = NULL,
  synonyms = NULL,
  qa = NULL,
  population = NULL,
  worldpop = NULL,
  pop_years = NULL,
  shape = NULL,
  inputs = NULL,
  output_dir = NULL,
  cache_dir = NULL,
  parse_types = TRUE,
  drop_empty_cols = TRUE
) {
  # ---- validate -------------------------------------------------------------
  if (!is.numeric(start_year) || length(start_year) != 1) {
    cli::cli_abort("{.arg start_year} must be a single number.")
  }
  if (!is.logical(parse_types) || length(parse_types) != 1) {
    cli::cli_abort("{.arg parse_types} must be a single logical.")
  }
  if (!is.logical(drop_empty_cols) || length(drop_empty_cols) != 1) {
    cli::cli_abort("{.arg drop_empty_cols} must be a single logical.")
  }

  # ---- column ordering convention -------------------------------------------
  # Groups are emitted in this order; columns within a group keep their original
  # order; anything unmatched (raw passthrough + `*_source` provenance) goes
  # last. The intent is identity -> location -> onset/age -> core epi dates ->
  # classification -> surveillance indicators -> other dates -> the rest, so the
  # derived analytic columns lead instead of being buried at the end.
  column_roles_default <- list(
    id = "^(id|epid)$",
    iso = "^country_iso3code$",
    country = "^country_actual$",
    geo_group = "^(risk_group|epi_zones|epi_zones_v2)$",
    adm_name = "^adm[0-9]$",
    adm_guid = "^adm[0-9]_guid$",
    coord = "^(latitude|longitude)$",
    # the time slots take both the AFP onset and the ES collection equivalents,
    # so a case orders by onset and an ES sample by collection in the same place.
    onset_date = "^(paralysis_onset_date|collection_date)$",
    onset_month = "^(month_onset|month_collection)$",
    onset_year = "^(year_onset|year_collection)$",
    age = "^age_months$",
    core_dates = paste0(
      "^(notification_date|investigation_date|stool1collection_date|",
      "stool2collection_date|followup_date)$"
    ),
    # the ES virus labels (virus_type(s), npev/nvaccine/ev_detect) sit in the
    # classification slot alongside the AFP ones; sabin[123]/classification_all/
    # vtype are shared by both streams.
    classification = paste0(
      "^(classification|classification_all|vtype|vtype_fixed|",
      "polio_virus_types|vdpv_classifications|sabin[123]|hot_case|",
      "paralysis_hot_case|virus_types|virus_type|npev|nvaccine|ev_detect|",
      "polio_type|afp_class|afp|npafp|pending_results)$"
    ),
    indicators = paste0(
      "^(onset_to_[a-z0-9]+|notify_to_invest|invest_to_stool1|",
      "stool1_to_stool2|onset_date_quality|timeliness|stool[12]_missing|",
      "stool_missing|adequate_stool|adequate_stool_with_condition|",
      "needs_60day_followup|got_60day_followup|followup_on_time)$"
    ),
    dates = "^(date_.*|.*_date)$"
  )
  column_roles <- column_roles %||% column_roles_default

  # ---- crosswalk (data-driven, shipped with the package) --------------------
  crosswalk <- .polis_crosswalk_map()

  cfg <- structure(
    list(
      start_year = as.integer(start_year),
      regions = regions,
      column_roles = column_roles,
      crosswalk = crosswalk,
      synonyms = synonyms,
      qa = qa,
      population = population,
      worldpop = worldpop,
      pop_years = pop_years,
      shape = shape,
      inputs = inputs,
      output_dir = output_dir,
      cache_dir = cache_dir,
      parse_types = parse_types,
      drop_empty_cols = drop_empty_cols
    ),
    class = "polis_config"
  )

  # register as the session-active config so later calls default to it
  .polis_config_store$active <- cfg
  cfg
}

#' Print method for POLIS configuration
#'
#' @param x A `polis_config` object.
#' @param ... Ignored.
#'
#' @return Invisibly returns `x`.
#'
#' @keywords internal
#' @export
print.polis_config <- function(x, ...) {
  cli::cli_h1("POLIS pipeline configuration")
  cli::cli_text("Start year: {.val {x$start_year}}")
  cli::cli_text("Regions: {.val {x$regions}}")
  cli::cli_text("Column-order groups: {.val {names(x$column_roles)}} -> other")
  cli::cli_text("Synonyms: {.val {!is.null(x$synonyms)}}")
  cli::cli_text("Population: {.val {!is.null(x$population)}}")
  cli::cli_text("WorldPop: {.val {!is.null(x$worldpop)}}")
  cli::cli_text("Shape: {.val {!is.null(x$shape)}}")
  inputs_label <- .polis_inputs_label(x$inputs)
  cli::cli_text("Inputs: {.val {inputs_label}}")
  cli::cli_text("Output dir: {.val {x$output_dir %||% \"<none>\"}}")
  cli::cli_text("SIA cache: {.val {x$cache_dir %||% \"<none>\"}}")
  invisible(x)
}

# One-line description of the configured inputs for printing: a directory path,
# the count of supplied tables, or "<none>".
#' @noRd
.polis_inputs_label <- function(inputs) {
  if (is.null(inputs)) {
    return("<none>")
  }
  if (is.character(inputs) && length(inputs) == 1L) {
    return(inputs)
  }
  if (is.list(inputs)) {
    return(paste0(
      length(inputs),
      " table(s): ",
      paste(names(inputs), collapse = ", ")
    ))
  }
  "<set>"
}


# =============================================================================
# Orchestrator
#
# run_pipeline() is the modern replacement for preprocess(): it takes the raw
# POLIS tables already in memory, runs each dataset through its standalone
# cleaner, links virus to the human/ES streams, and returns the cleaned set. It
# is a thin, legible composition over the per-dataset cleaners -- the heavy
# lifting lives in clean_afp()/clean_es()/clean_sia()/clean_virus(). Optional
# historical layers (reconcile) are opt-in, so a first-ever run with no archive
# works unchanged.
# =============================================================================

# TRUE when the configured regions request real scoping -- i.e. they are not the
# permissive default of all six WHO regions. Used to keep a default run a no-op.
#' @noRd
.polis_regions_restrict <- function(regions) {
  all_regions <- c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")
  !setequal(toupper(regions), all_regions)
}

# Restrict a cleaned table to the configured analytic window: rows on/after
# `cfg$start_year` (by `year_col`) and within `cfg$regions` (by who_region).
# Either filter is skipped when its key column is absent; a row whose year or
# region value is missing is kept -- scoping never drops what it cannot classify.
# `warn_missing` controls whether an absent key column is announced (TRUE for the
# surveillance streams, FALSE for the quality roll-ups where region is optional).
#' @noRd
.polis_scope_table <- function(
  data,
  cfg,
  year_col = NULL,
  label = "table",
  warn_missing = TRUE
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    return(data)
  }
  n_in <- nrow(data)

  # ---- year window ----------------------------------------------------------
  if (!is.null(year_col)) {
    if (year_col %in% names(data)) {
      yr <- suppressWarnings(as.integer(data[[year_col]]))
      data <- data[is.na(yr) | yr >= cfg$start_year, , drop = FALSE]
    } else if (isTRUE(warn_missing)) {
      cli::cli_alert_warning(
        "{.val {label}}: no {.field {year_col}} column; year filter skipped."
      )
    }
  }

  # ---- region scope (skipped under the permissive default) ------------------
  if (.polis_regions_restrict(cfg$regions)) {
    if ("who_region" %in% names(data)) {
      reg <- toupper(trimws(as.character(data[["who_region"]])))
      keep <- is.na(data[["who_region"]]) | reg %in% toupper(cfg$regions)
      data <- data[keep, , drop = FALSE]
    } else if (isTRUE(warn_missing)) {
      cli::cli_alert_warning(
        "{.val {label}}: no {.field who_region} column; region filter skipped."
      )
    }
  }

  n_dropped <- n_in - nrow(data)
  if (n_dropped > 0L) {
    dropped_fmt <- .polis_big_num(n_dropped)
    in_fmt <- .polis_big_num(n_in)
    cli::cli_alert_info(
      "{.val {label}}: scoped out {dropped_fmt} of {in_fmt} \\
      {cli::qty(n_in)}row{?s}."
    )
  }
  data
}

# Region-scope each data-frame component of a quality roll-up (lots/district).
# Campaign roll-ups carry no analytic onset year, so only the region filter
# applies, and silently -- a roll-up keyed purely by admin GUID simply passes
# through. A no-op under the default (all-region) configuration.
#' @noRd
.polis_scope_quality <- function(quality, cfg, label) {
  if (!is.list(quality) || !.polis_regions_restrict(cfg$regions)) {
    return(quality)
  }
  for (comp in names(quality)) {
    if (is.data.frame(quality[[comp]])) {
      quality[[comp]] <- .polis_scope_table(
        quality[[comp]],
        cfg,
        year_col = NULL,
        label = paste0(label, "/", comp),
        warn_missing = FALSE
      )
    }
  }
  quality
}

# adm0 (country name, upper-cased) -> WHO region, pooled from the cleaned
# surveillance streams that carry both. Built from the *pre-scope* streams so it
# spans every region; lets the region scope reach the LQAS/IM roll-ups, which
# carry `adm0` but not `who_region`.
#' @noRd
.polis_region_by_adm0 <- function(frames) {
  keys <- character(0)
  regs <- character(0)
  for (t in frames) {
    if (!is.data.frame(t) || !all(c("adm0", "who_region") %in% names(t))) {
      next
    }
    keys <- c(keys, toupper(trimws(as.character(t[["adm0"]]))))
    regs <- c(regs, as.character(t[["who_region"]]))
  }
  .polis_guid_map(keys, regs)
}

# Attach `who_region` to each data-frame component of a quality roll-up by its
# `adm0` country name (case-insensitive), unless it already carries one, so the
# region scope has a column to act on.
#' @noRd
.polis_attach_region <- function(quality, region_map) {
  if (!is.list(quality) || length(region_map) == 0L) {
    return(quality)
  }
  for (comp in names(quality)) {
    df <- quality[[comp]]
    if (
      is.data.frame(df) &&
        !"who_region" %in% names(df) &&
        "adm0" %in% names(df)
    ) {
      df$who_region <- unname(
        region_map[toupper(trimws(as.character(df[["adm0"]])))]
      )
      quality[[comp]] <- df
    }
  }
  quality
}

# ---- cross-stream admin-GUID backfill ---------------------------------------
# A POLIS admin GUID may be present in one stream but missing in another for the
# same district (e.g. SIA rows lack the GUID that AFP/ES carry). Since every
# stream is cleaned to the same standardised admin names and shape ids, the GUID
# is recoverable from within the cleaned set: pool a name -> GUID map (and a
# shape-id -> GUID fallback) from the rows that have it, then fill the blanks.

# The per-level columns used for backfilling: the GUID to fill, the admin-name
# key (joined first), and the shape-id key (the fallback when names don't hit).
.polis_guid_levels <- list(
  adm0 = list(guid = "adm0_guid", name = "adm0", shape = "admin0shape_id"),
  adm1 = list(
    guid = "adm1_guid",
    name = c("adm0", "adm1"),
    shape = "admin1shape_id"
  ),
  adm2 = list(
    guid = "adm2_guid",
    name = c("adm0", "adm1", "adm2"),
    shape = "admin2shape_id"
  )
)

# Composite key from one or more admin-name columns, upper-cased and trimmed so
# trivial case/whitespace differences don't split a district.
#' @noRd
.polis_norm_key <- function(cols) {
  parts <- lapply(cols, function(x) toupper(trimws(as.character(x))))
  do.call(paste, c(parts, sep = "\r"))
}

# key -> most common non-blank GUID. Ties resolved arbitrarily-but-stably; a key
# that never carries a GUID is absent from the map.
#' @noRd
.polis_guid_map <- function(keys, guids) {
  ok <- !is.na(keys) & nzchar(trimws(keys)) & !.polis_blank(guids)
  if (!any(ok)) {
    return(stats::setNames(character(0), character(0)))
  }
  tally <- dplyr::count(
    dplyr::tibble(k = keys[ok], g = as.character(guids[ok])),
    .data$k,
    .data$g,
    name = "n"
  )
  top <- dplyr::slice_max(
    dplyr::group_by(tally, .data$k),
    order_by = .data$n,
    n = 1,
    with_ties = FALSE
  )
  stats::setNames(top$g, top$k)
}

# Pool name -> GUID and shape-id -> GUID maps per admin level across every
# cleaned data frame.
#' @noRd
.polis_guid_pools <- function(frames) {
  lapply(.polis_guid_levels, function(s) {
    name_k <- list()
    name_g <- list()
    shape_k <- list()
    shape_g <- list()
    for (df in frames) {
      if (!s$guid %in% names(df)) {
        next
      }
      g <- as.character(df[[s$guid]])
      if (all(s$name %in% names(df))) {
        name_k[[length(name_k) + 1L]] <- .polis_norm_key(df[s$name])
        name_g[[length(name_g) + 1L]] <- g
      }
      if (s$shape %in% names(df)) {
        shape_k[[length(shape_k) + 1L]] <- as.character(df[[s$shape]])
        shape_g[[length(shape_g) + 1L]] <- g
      }
    }
    list(
      name = .polis_guid_map(unlist(name_k), unlist(name_g)),
      shape = .polis_guid_map(unlist(shape_k), unlist(shape_g))
    )
  })
}

# Fill blank GUIDs in one data frame from the pooled maps: admin-name match
# first, shape-id match for whatever is still blank. Records per-level fill
# counts on the `guid_filled` attribute.
#' @noRd
.polis_fill_guids <- function(df, pools) {
  filled <- c(adm0 = 0L, adm1 = 0L, adm2 = 0L)
  for (lv in names(.polis_guid_levels)) {
    s <- .polis_guid_levels[[lv]]
    p <- pools[[lv]]
    if (!s$guid %in% names(df)) {
      next
    }
    miss <- .polis_blank(df[[s$guid]])
    if (!any(miss)) {
      next
    }
    before <- sum(miss)
    if (length(p$name) && all(s$name %in% names(df))) {
      cand <- unname(p$name[.polis_norm_key(df[s$name])])
      take <- miss & !is.na(cand)
      df[[s$guid]][take] <- cand[take]
      miss <- .polis_blank(df[[s$guid]])
    }
    if (length(p$shape) && s$shape %in% names(df) && any(miss)) {
      cand <- unname(p$shape[as.character(df[[s$shape]])])
      take <- miss & !is.na(cand)
      df[[s$guid]][take] <- cand[take]
    }
    filled[lv] <- before - sum(.polis_blank(df[[s$guid]]))
  }
  attr(df, "guid_filled") <- filled
  df
}

# Every data frame in the cleaned set, flattening list outputs (lqas/im) to
# their data-frame components.
#' @noRd
.polis_cleaned_frames <- function(cleaned) {
  frames <- list()
  for (v in cleaned) {
    if (is.data.frame(v)) {
      frames[[length(frames) + 1L]] <- v
    } else if (is.list(v)) {
      for (comp in v) {
        if (is.data.frame(comp)) {
          frames[[length(frames) + 1L]] <- comp
        }
      }
    }
  }
  frames
}

# Backfill missing admin GUIDs across every cleaned table from the pooled
# within-data consensus. Pure data-in/data-out; reports what it recovered.
#' @noRd
.polis_backfill_guids <- function(cleaned, verbose = TRUE) {
  frames <- .polis_cleaned_frames(cleaned)
  if (length(frames) == 0L) {
    return(cleaned)
  }
  pools <- .polis_guid_pools(frames)
  total <- c(adm0 = 0L, adm1 = 0L, adm2 = 0L)
  fill <- function(df) {
    out <- .polis_fill_guids(df, pools)
    total <<- total + attr(out, "guid_filled")
    attr(out, "guid_filled") <- NULL
    out
  }
  for (key in names(cleaned)) {
    v <- cleaned[[key]]
    if (is.data.frame(v)) {
      cleaned[[key]] <- fill(v)
    } else if (is.list(v)) {
      for (comp in names(v)) {
        if (is.data.frame(v[[comp]])) {
          cleaned[[key]][[comp]] <- fill(v[[comp]])
        }
      }
    }
  }
  nz <- total[total > 0L]
  if (isTRUE(verbose) && length(nz) > 0L) {
    parts <- paste0(vapply(nz, .polis_big_num, character(1)), " ", names(nz))
    cli::cli_alert_info(
      "Backfilled GUIDs from cross-stream consensus: {parts}."
    )
  }
  cleaned
}

#' Run the POLIS cleaning pipeline in memory
#'
#' Cleans the raw POLIS tables supplied in `inputs` and returns the canonical
#' analytic set. Any subset of tables may be supplied; absent tables are skipped.
#' The virus (positives) table is *built* from the cleaned AFP and ES streams via
#' [clean_virus()] whenever either is present -- it is not read from `inputs`.
#'
#' Each cleaner receives `cfg$shape` (an already-processed district shape) for
#' admin reconciliation, and -- when AFP cases are present -- the surveillance
#' indicators are computed via [calc_polio_indicators()] using `cfg$population`
#' as the under-15 denominator.
#'
#' @param inputs The raw POLIS tables to clean; defaults to `cfg$inputs` so a
#'   fully-specified config can be run as `run_pipeline(cfg = cfg)`. Either a
#'   **named list** of raw data frames (recognised names `afp`, `es`, `hum_spec`,
#'   `activity`, `subactivity`, `lqas`, `im`) or a **path** to a directory of
#'   `raw_*` files (read on demand, each output then inheriting its source
#'   format). A raw `virus` table is not used -- positives are derived from the
#'   cleaned `afp`/`es` outputs; `lqas`/`im` become district-year roll-ups via
#'   [process_sia_quality()].
#' @param cfg A [polis_config()] object; defaults to the session-active config
#'   ([polis_active_config()]). Its `shape` and `population` handles drive admin
#'   reconciliation and the indicators step respectively.
#' @param reconcile_with Optional named list of full-pull data frames (same keys
#'   as `inputs`) used to prune deleted/merged `id`s via [reconcile()].
#' @param output_dir Directory to persist outputs to; defaults to
#'   `cfg$output_dir` (set it once on the config). When non-`NULL` the
#'   `polished_*` data files are written to its `data/` sub-directory and a
#'   `checks_<dataset>.xlsx` workbook per dataset to its `checks/` sub-directory.
#'   `NULL` returns the cleaned set without writing anything.
#' @param formats Optional named list mapping output keys (`afp`, `es`,
#'   `hum_spec`, `sia`, `lqas`, `im`) to a file extension, so an output inherits
#'   the format of the raw file it derives from. Unmapped/derived outputs fall
#'   back to `qs2`. Only consulted when `output_dir` is set.
#' @param refresh If `TRUE`, ignore any existing cache and re-run every step from
#'   scratch, overwriting the cache and the output files (the fresh results are
#'   still cached for next time). Default `FALSE` (reuse caches where valid).
#'
#' @details
#' Missing admin GUIDs are always backfilled across the cleaned streams from
#' their pooled consensus: a district whose `adm1_guid` / `adm2_guid` is blank in
#' one stream inherits it from another that carries it, matched on admin name
#' first and `admin{1,2}shape_id` as a fallback. Only blanks are filled; existing
#' GUIDs are never changed.
#'
#' @return A named list holding any of the cleaned tibbles `afp`, `es`,
#'   `hum_spec`, `sia`, `virus`, the SIA-quality roll-ups `lqas` / `im` (each a
#'   list of `lots`/`district`/`meta`), plus `indicators` (the
#'   [calc_polio_indicators()] result list) when AFP cases are present. Returned
#'   invisibly when `output_dir` is set.
#'
#' @examples
#' afp <- data.frame(
#'   Id = 1, Epid = "A-1", `Last Update Date` = "2024-03-01",
#'   `Date Onset` = "2024-01-02", `Admin0 Name` = "NIGERIA",
#'   check.names = FALSE
#' )
#' out <- run_pipeline(list(afp = afp))
#' names(out)
#'
#' @export
run_pipeline <- function(
  inputs = cfg$inputs,
  cfg = polis_active_config(),
  reconcile_with = NULL,
  output_dir = cfg$output_dir,
  formats = list(),
  refresh = FALSE
) {
  # Resolve inputs to handles WITHOUT reading: a directory path becomes a named
  # list of raw_* file paths, a named list passes through. Sourced from
  # cfg$inputs by default, so a fully-specified config runs as
  # run_pipeline(cfg = cfg). Each stream is then read + cleaned through the
  # content-addressed cache, so an unchanged source skips both read and clean.
  inputs <- .polis_input_handles(inputs)
  # Auto-detect each output's format from its source extension, then let any
  # explicit `formats` entry override per stream. A partial override (e.g.
  # formats = list(afp = "csv")) therefore changes only that stream; every other
  # output still inherits its source extension instead of silently falling back
  # to the default format.
  formats <- utils::modifyList(
    .polis_outputs_formats(attr(inputs, "formats") %||% list()),
    formats
  )

  # Lazy reference loaders: the shape (every cleaner) and population (indicators)
  # are read only when a step that needs them actually runs. On a fully-cached
  # run nothing calls them, so these potentially large files are never read.
  shape_fn <- .polis_lazy_ref(cfg$shape)
  population_fn <- .polis_lazy_ref(cfg$population)
  cleaned <- list()
  # the cleaned (pre-scope) streams, kept so human specimens inherit case
  # geography and the LQAS/IM roll-ups inherit WHO region, from the full
  # (all-region) set regardless of the configured region/year scope
  afp_clean <- NULL
  es_clean <- NULL
  sia_clean <- NULL

  # ---- AFP (cases) ----------------------------------------------------------
  # Each cleaned stream is scoped to the configured year window + regions right
  # here, so the virus table (built from the cleaned afp/es) and the indicators
  # (computed off the cleaned set) inherit the scope without filtering twice.
  if (!is.null(inputs$afp)) {
    cli::cli_h1("Cleaning AFP cases")
    afp_clean <- .polis_clean_cached(
      "afp",
      list(afp = inputs$afp),
      cfg,
      .polis_clean_versions[["afp"]],
      function(r) clean_afp(r$afp, cfg, shape = shape_fn()),
      refresh = refresh
    )
    cleaned$afp <- .polis_scope_table(afp_clean, cfg, "year_onset", "afp")
  }

  # ---- ES (environmental) ---------------------------------------------------
  if (!is.null(inputs$es)) {
    cli::cli_h1("Cleaning environmental surveillance")
    es_clean <- .polis_clean_cached(
      "es",
      list(es = inputs$es),
      cfg,
      .polis_clean_versions[["es"]],
      function(r) clean_es(r$es, cfg, shape = shape_fn()),
      refresh = refresh
    )
    cleaned$es <- .polis_scope_table(es_clean, cfg, "year_collection", "es")
  }

  # ---- Human specimens ------------------------------------------------------
  # Cleaned against the full (pre-scope) cases so a specimen inherits its parent
  # case's geography; the AFP handle joins the key so a new AFP pull invalidates.
  if (!is.null(inputs$hum_spec)) {
    cli::cli_h1("Cleaning human specimens")
    hum_spec_clean <- .polis_clean_cached(
      "hum_spec",
      list(hum_spec = inputs$hum_spec, afp = inputs$afp),
      cfg,
      .polis_clean_versions[["hum_spec"]],
      function(r) {
        clean_human_spec(r$hum_spec, cfg, shape = shape_fn(), cases = afp_clean)
      },
      refresh = refresh
    )
    cleaned$hum_spec <- .polis_scope_table(
      hum_spec_clean,
      cfg,
      "year_collection",
      "hum_spec"
    )
  }

  # ---- SIA (campaigns) ------------------------------------------------------
  if (!is.null(inputs$activity)) {
    cli::cli_h1("Cleaning SIA campaigns")
    sia_clean <- .polis_clean_cached(
      "sia",
      list(activity = inputs$activity, subactivity = inputs$subactivity),
      cfg,
      .polis_clean_versions[["sia"]],
      function(r) {
        clean_sia(r$activity, r$subactivity, cfg, shape = shape_fn())
      },
      refresh = refresh
    )
    cleaned$sia <- .polis_scope_table(sia_clean, cfg, "year_start", "sia")
  }

  # ---- SIA quality (LQAS / IM) ----------------------------------------------
  # Campaign-quality monitoring: rolled up to district-year indicators rather
  # than cleaned like the surveillance streams. Each processor's result is a
  # list (lots/district/meta), attached under its own key so the file writer
  # emits one polished_<key>_<component> table per data-frame component.
  if (!is.null(inputs$lqas) || !is.null(inputs$im)) {
    cli::cli_h1("Processing SIA quality (LQAS / IM)")
    # WHO-region lookup from the *pre-scope* surveillance streams (all regions),
    # so the region scope can reach the roll-ups, which carry adm0 but no region.
    region_map <- .polis_region_by_adm0(list(afp_clean, es_clean, sia_clean))
    if (!is.null(inputs$lqas)) {
      lqas_res <- .polis_clean_cached(
        "lqas",
        list(lqas = inputs$lqas),
        cfg,
        .polis_clean_versions[["lqas"]],
        function(r) {
          process_lqas(
            r$lqas,
            cfg,
            shape = shape_fn(),
            verbose = TRUE,
            summary = FALSE
          )
        },
        refresh = refresh
      )
      lqas_res <- .polis_attach_region(lqas_res, region_map)
      cleaned$lqas <- .polis_scope_quality(lqas_res, cfg, "lqas")
    }
    if (!is.null(inputs$im)) {
      im_res <- .polis_clean_cached(
        "im",
        list(im = inputs$im),
        cfg,
        .polis_clean_versions[["im"]],
        function(r) {
          process_im(
            r$im,
            cfg,
            shape = shape_fn(),
            verbose = TRUE,
            summary = FALSE
          )
        },
        refresh = refresh
      )
      im_res <- .polis_attach_region(im_res, region_map)
      cleaned$im <- .polis_scope_quality(im_res, cfg, "im")
    }
  }

  # ---- Population (foundational denominators) --------------------------------
  # POLIS population reconciled against WorldPop and rolled up to adm0/adm1/adm2
  # via clean_pop(). Foundational, so -- unlike the surveillance streams -- it is
  # NOT region-scoped: denominators must exist for every district regardless of
  # the configured region. Cached on the raw population + worldpop handles (the
  # latter passes through .polis_resolve_ref unchanged). Its adm2 table feeds the
  # indicators denominator below when cfg$population is unset.
  if (!is.null(inputs$population)) {
    cli::cli_h1("Cleaning population")
    # clean_pop()'s orphan-GUID crosswalk depends on which boundaries are current
    # as of this date, so it is part of the cache key. Pin cfg$reference_date for
    # reproducible, cache-stable runs; otherwise it follows today's date.
    pop_ref_date <- cfg$reference_date %||% Sys.Date()
    cleaned$pop <- .polis_clean_cached(
      "pop",
      list(population = inputs$population, worldpop = cfg$worldpop),
      cfg,
      .polis_clean_versions[["pop"]],
      function(r) {
        args <- list(
          r$population,
          cfg,
          shape = shape_fn(),
          worldpop = r$worldpop,
          reference_date = pop_ref_date
        )
        if (!is.null(cfg$pop_years)) {
          args$years <- cfg$pop_years
        }
        do.call(clean_pop, args)
      },
      refresh = refresh,
      extra = list(reference_date = pop_ref_date)
    )
  }

  # ---- backfill admin GUIDs from cross-stream consensus ---------------------
  # A district's GUID may be present in one stream and blank in another; pool
  # the known GUIDs across all cleaned streams and fill the blanks (by admin
  # name, then shape id) before virus/indicators derive from them.
  cleaned <- .polis_backfill_guids(cleaned)

  # ---- Virus (positives): built from the cleaned human + ES streams ---------
  # Cached on the afp/es source fingerprints + scope, so an unchanged run rebuilds
  # nothing; built from the post-scope cleaned tables, hence the scope in the key.
  if (!is.null(cleaned$afp) || !is.null(cleaned$es)) {
    cli::cli_h1("Building virus / positives")
    virus_key <- list(
      name = "virus",
      # Fingerprint EVERY source, not just afp/es: the GUID backfill above pools
      # consensus GUIDs across all cleaned streams into afp/es, so a change to
      # any stream (sia/hum_spec/lqas/im) can change the GUIDs the virus table
      # is built on. Keying on afp/es alone would serve a stale virus table when
      # only another stream changed.
      inputs = lapply(inputs, .polis_fingerprint),
      cfg = .polis_clean_cache_fields(cfg),
      scope = .polis_scope_key(cfg),
      version = .polis_clean_versions[["virus"]]
    )
    virus <- .polis_cache_run(
      "virus",
      virus_key,
      cfg$cache_dir,
      function() clean_virus(cases = cleaned$afp, es = cleaned$es, cfg = cfg),
      refresh = refresh
    )
    # only attach when there are positives, so a virus-free run stays trim
    if (nrow(virus) > 0) {
      cleaned$virus <- virus
    }
  }

  # ---- reconcile against a full pull (optional, G5) -------------------------
  if (!is.null(reconcile_with)) {
    for (key in names(cleaned)) {
      full_pull <- reconcile_with[[key]]
      if (!is.null(full_pull)) {
        cleaned[[key]] <- reconcile(cleaned[[key]], full_pull)
      }
    }
  }

  # ---- Indicators -----------------------------------------------------------
  # Computed off the cleaned set; a thin dataset that leaves every indicator
  # un-computable warns and is skipped rather than aborting the whole run.
  if (!is.null(cleaned$afp) && nrow(cleaned$afp) > 0L) {
    cli::cli_h1("Computing surveillance indicators")
    # Cached on every source it derives from (afp/es/sia/hum_spec fingerprints),
    # the population + shape fingerprints, and the scope. On a hit the population
    # and shape files are never read -- population_fn()/shape_fn() run only here.
    ind_key <- list(
      name = "indicators",
      # Fingerprint every source for the same reason as the virus key: the
      # cross-stream GUID backfill means any stream (incl. lqas/im) can alter the
      # afp/es GUIDs the indicators are computed from, so all sources are keyed.
      inputs = lapply(inputs, .polis_fingerprint),
      # the denominator is cfg$population when set, else the pop table the
      # pipeline just produced; fingerprint whichever is used (no file read --
      # cfg$population is path metadata, the produced denominator is in memory).
      population = .polis_fingerprint(cfg$population) %||%
        if (!is.null(cleaned$pop)) {
          .polis_hash(.polis_pop_denominator(cleaned$pop))
        },
      shape = .polis_fingerprint(cfg$shape),
      cfg = .polis_clean_cache_fields(cfg),
      scope = .polis_scope_key(cfg),
      version = .polis_clean_versions[["indicators"]]
    )
    indicators <- tryCatch(
      .polis_cache_run(
        "indicators",
        ind_key,
        cfg$cache_dir,
        function() {
          calc_polio_indicators(
            cases = cleaned$afp,
            es = cleaned$es,
            sia = cleaned$sia,
            virus = cleaned$virus,
            lab = cleaned$hum_spec,
            population = population_fn() %||%
              .polis_pop_denominator(cleaned$pop),
            admin_units = .polio_admin_units_from_shape(shape_fn()),
            summary = FALSE
          )
        },
        refresh = refresh
      ),
      error = function(e) {
        cli::cli_alert_warning(
          "Indicators skipped: {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (!is.null(indicators)) {
      cleaned$indicators <- .polis_attach_indicator_iso3(indicators, cleaned)
    }
  }

  cli::cli_alert_success(
    "Produced {length(cleaned)} output{?s}: {.val {names(cleaned)}}."
  )

  # ---- persist to disk (optional) -------------------------------------------
  # data files -> output_dir/data, check workbooks -> output_dir/checks.
  if (!is.null(output_dir)) {
    .polis_persist_pipeline(cleaned, output_dir, formats, refresh = refresh)
    return(invisible(cleaned))
  }
  cleaned
}

#' Extract the under-15 denominator from a clean_pop() result
#'
#' Pulls `(adm2_guid, year, u15_pop)` from the pop table the pipeline produced so
#' it can stand in as the [calc_polio_indicators()] denominator when
#' `cfg$population` is unset -- letting the pipeline make its own. Returns `NULL`
#' when there is no pop output or it carries no `u15_pop`.
#' @noRd
.polis_pop_denominator <- function(pop) {
  if (is.null(pop) || is.null(pop$adm2)) {
    return(NULL)
  }
  a <- pop$adm2
  if (!all(c("adm2_guid", "year", "u15_pop") %in% names(a))) {
    return(NULL)
  }
  dplyr::distinct(dplyr::select(a, "adm2_guid", "year", "u15_pop"))
}

#' Write a pipeline result to disk: polished data + check workbooks
#'
#' Routes the `polished_*` data files into a `data/` sub-directory of
#' `output_dir` and the per-dataset `checks_*.xlsx` workbooks into a `checks/`
#' sub-directory, creating both. The single place both [run_pipeline()] and
#' [run_pipeline_dir()] persist from, so the on-disk layout is defined once.
#' `refresh = TRUE` rewrites every output file even when its content is unchanged.
#' @noRd
.polis_persist_pipeline <- function(
  cleaned,
  output_dir,
  formats = list(),
  refresh = FALSE
) {
  data_dir <- file.path(output_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  .polis_write_outputs(cleaned, data_dir, formats = formats, refresh = refresh)

  checks_dir <- file.path(output_dir, "checks")
  .polis_write_check_workbooks(cleaned, checks_dir, reference_date = Sys.Date())
  invisible(output_dir)
}

#' Attach `country_iso3code` to each guid-keyed indicator table
#'
#' [calc_polio_indicators()] keys its outputs on admin GUID + name only, so a
#' country filter has nothing to match on and `load_polished(country = ...)`
#' would leak every country's indicators. This maps each adm0/adm1/adm2 GUID
#' seen in the cleaned streams to its ISO3 and joins it onto every data-frame
#' component (`adm0`/`adm1`/`adm2`/`long`), so indicators slice like any other
#' output. The `meta` list and any component lacking a `guid` column pass
#' through untouched.
#' @noRd
.polis_attach_indicator_iso3 <- function(indicators, cleaned) {
  iso_map <- .polis_guid_iso3_map(cleaned)
  if (nrow(iso_map) == 0L) {
    return(indicators)
  }
  for (key in names(indicators)) {
    comp <- indicators[[key]]
    if (!is.data.frame(comp) || !"guid" %in% names(comp)) {
      next
    }
    indicators[[key]] <- comp |>
      dplyr::left_join(iso_map, by = dplyr::join_by(guid)) |>
      dplyr::relocate("country_iso3code", .before = "guid")
  }
  indicators
}

#' adm0/adm1/adm2 GUID -> ISO3 lookup, stacked from every cleaned stream
#'
#' Each cleaned table that carries `country_iso3code` contributes one
#' `(guid, country_iso3code)` row per admin level it holds; the union is taken
#' distinct on `guid` (a GUID belongs to one country). Returns a zero-row tibble
#' when no stream carries ISO3.
#' @noRd
.polis_guid_iso3_map <- function(cleaned) {
  guid_cols <- c("adm0_guid", "adm1_guid", "adm2_guid")
  rows <- lapply(cleaned, function(tbl) {
    if (!is.data.frame(tbl) || !"country_iso3code" %in% names(tbl)) {
      return(NULL)
    }
    present <- intersect(guid_cols, names(tbl))
    if (length(present) == 0L) {
      return(NULL)
    }
    per_level <- lapply(present, function(g) {
      dplyr::transmute(
        tbl,
        guid = as.character(.data[[g]]),
        country_iso3code = as.character(.data[["country_iso3code"]])
      )
    })
    dplyr::bind_rows(per_level)
  })
  dplyr::bind_rows(rows) |>
    dplyr::filter(
      !is.na(.data[["guid"]]),
      !is.na(.data[["country_iso3code"]])
    ) |>
    dplyr::distinct(.data[["guid"]], .keep_all = TRUE)
}

#' Run the cleaning pipeline from a directory of raw files
#'
#' File-based convenience over [run_pipeline()]: reads the `raw_*` POLIS tables
#' (the names [get_polis_data()] writes) from `source_dir`, runs the full
#' pipeline, and (optionally) writes the outputs to `output_dir` as
#' `polished_*` files. Each output's file format follows its source raw file;
#' derived outputs (virus, indicators) default to `qs2`.
#'
#' @param source_dir Directory holding the `raw_*` POLIS tables.
#' @param output_dir Optional directory to write `polished_*` outputs to;
#'   overrides `cfg$output_dir` when supplied. If both are `NULL` the output set
#'   is only returned.
#' @param cfg A [polis_config()] object; defaults to the session-active config
#'   ([polis_active_config()]). Its `shape` and `population` handles drive
#'   reconciliation and indicators.
#' @param refresh If `TRUE`, ignore any existing cache and re-run every step from
#'   scratch, overwriting the cache and output files. Default `FALSE`.
#'
#' @details
#' A thin wrapper over [run_pipeline()] with `inputs = source_dir`: the directory
#' is read into the input list (each output inheriting its source file's format)
#' and writing follows the same rules as [run_pipeline()] -- `polished_*` data in
#' the `data/` sub-directory of `output_dir` and a `checks_<dataset>.xlsx`
#' workbook per dataset in the `checks/` sub-directory. The check workbooks
#' require the optional `openxlsx` package.
#'
#' @return A named list of pipeline outputs (invisibly when writing).
#'
#' @export
run_pipeline_dir <- function(
  source_dir,
  output_dir = NULL,
  cfg = polis_active_config(),
  refresh = FALSE
) {
  cfg$output_dir <- output_dir %||% cfg$output_dir
  run_pipeline(inputs = source_dir, cfg = cfg, refresh = refresh)
}

# Candidate filter columns, first present in a table wins: a year filter matches
# the first year-like column, a country filter the first country/ISO3 column.
.polis_year_cols <- c(
  "year",
  "year_onset",
  "year_start",
  "year_collection",
  "collect_yr"
)
# `adm0` is deliberately excluded: it holds the country *name* (e.g.
# "AFGHANISTAN"), so filtering it by an ISO3 like "AFG" would silently empty a
# name-keyed table rather than match it.
.polis_country_cols <- c("iso3", "country_iso3code", "country")

# Filter a data frame on the first candidate column it actually has. A no-op
# when `values` is NULL or the table carries none of the candidate columns, so a
# filter dimension a dataset lacks is silently skipped rather than an error.
#' @noRd
.polis_filter_dataset <- function(data, values, candidates) {
  if (is.null(values)) {
    return(data)
  }
  col <- intersect(candidates, names(data))
  if (length(col) == 0L) {
    return(data)
  }
  dplyr::filter(data, .data[[col[[1]]]] %in% values)
}

# A distinct ISO3 <-> country-name (`adm0`) lookup pooled from any loaded tables
# that carry both, so a name-keyed table (LQAS/IM roll-ups) can be filtered by an
# ISO3 the user supplied.
#' @noRd
.polis_iso3_adm0_map <- function(tables) {
  rows <- lapply(tables, function(t) {
    if (
      !is.data.frame(t) || !all(c("country_iso3code", "adm0") %in% names(t))
    ) {
      return(NULL)
    }
    dplyr::distinct(dplyr::tibble(
      country_iso3code = as.character(t[["country_iso3code"]]),
      adm0 = as.character(t[["adm0"]])
    ))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(dplyr::tibble(country_iso3code = character(), adm0 = character()))
  }
  map <- dplyr::distinct(dplyr::bind_rows(rows))
  map[!is.na(map$country_iso3code) & !is.na(map$adm0), , drop = FALSE]
}

# Country filter that understands both ISO3 and country name. A table with an
# ISO3 column filters on it directly; an `adm0`-keyed table resolves the
# requested value(s) to country name(s) via `geo` (and also matches a name passed
# directly). A no-op when the table carries no country column.
#' @noRd
.polis_filter_country <- function(data, country, geo) {
  if (is.null(country)) {
    return(data)
  }
  iso_col <- intersect(c("iso3", "country_iso3code"), names(data))
  if (length(iso_col) > 0L) {
    return(dplyr::filter(data, .data[[iso_col[[1]]]] %in% country))
  }
  if ("adm0" %in% names(data)) {
    # Compare case- and whitespace-insensitively: a stream may carry the country
    # name in a different case (e.g. "AFGHANISTAN" vs "Afghanistan") than the one
    # the ISO3 resolved to, which an exact match would silently drop.
    resolved <- geo$adm0[geo$country_iso3code %in% country]
    wanted <- toupper(trimws(unique(c(country, resolved))))
    return(dplyr::filter(data, toupper(trimws(.data[["adm0"]])) %in% wanted))
  }
  if ("country" %in% names(data)) {
    return(dplyr::filter(data, .data[["country"]] %in% country))
  }
  data
}

#' Read a country / period slice of the polished outputs
#'
#' Reads the `polished_*` files [run_pipeline()] wrote to the `data/`
#' sub-directory of `output_dir` and returns them filtered to a country and/or
#' year, as a named list (one element per dataset).
#'
#' @param country Optional country/ISO3 value(s) to keep. Matched against the
#'   first present of `iso3`, `country_iso3code`, `country` in each table, so an
#'   ISO3 like `"AFG"` is expected. The `adm0` *name* column is intentionally
#'   not used for this filter. `NULL` keeps all.
#' @param year Optional year(s) to keep. Matched against the first present of
#'   `year`, `year_onset`, `year_start`, `year_collection`, `collect_yr`. `NULL`
#'   keeps all.
#' @param datasets Optional character vector restricting which datasets to read
#'   (by output key, e.g. `"afp"`, `"es"`, `"virus"`). `NULL` (default) reads
#'   every `polished_*` file present.
#' @param output_dir Directory the outputs were written to; defaults to
#'   `cfg$output_dir`. The files are read from its `data/` sub-directory.
#' @param cfg A [polis_config()] object; defaults to the session-active config
#'   ([polis_active_config()]), so `load_polished(country = "AFG")` works when
#'   the config carries `output_dir`.
#'
#' @return A named list of filtered tibbles, one per `polished_*` dataset found.
#'
#' @seealso [run_pipeline()], which writes the files this reads.
#' @examples
#' \dontrun{
#' afg <- load_polished(country = "AFG")
#' afg$afp
#' load_polished(country = "AFG", year = 2024, datasets = "afp")$afp
#' }
#' @export
load_polished <- function(
  country = NULL,
  year = NULL,
  datasets = NULL,
  output_dir = cfg$output_dir,
  cfg = polis_active_config()
) {
  if (is.null(output_dir)) {
    cli::cli_abort(c(
      "No {.arg output_dir} to read from.",
      "i" = "Pass {.arg output_dir}, or set it on {.fn polis_config}."
    ))
  }
  data_dir <- file.path(output_dir, "data")
  if (!dir.exists(data_dir)) {
    cli::cli_abort("No {.file data} directory under {.file {output_dir}}.")
  }
  all_files <- list.files(
    data_dir,
    pattern = "^polished_.*\\.",
    full.names = TRUE
  )
  all_keys <- sub(
    "^polished_",
    "",
    tools::file_path_sans_ext(basename(all_files))
  )
  # A format change between runs can leave two files for one key (e.g.
  # polished_afp.csv from an earlier run and polished_afp.qs2 from a later one);
  # the writer never prunes the old extension. Keep only the most recently
  # written file per key so a stale leftover never shadows the fresh output.
  if (anyDuplicated(all_keys)) {
    ord <- order(file.mtime(all_files), decreasing = TRUE)
    all_files <- all_files[ord]
    all_keys <- all_keys[ord]
    keep <- !duplicated(all_keys)
    if (any(!keep)) {
      cli::cli_warn(
        "Ignoring {sum(!keep)} stale polished file{?s} shadowed by a newer \\
        same-key output: {.file {basename(all_files[!keep])}}."
      )
    }
    all_files <- all_files[keep]
    all_keys <- all_keys[keep]
  }
  files <- all_files
  keys <- all_keys
  if (!is.null(datasets)) {
    # Match a requested dataset to its output key AND to the component files of a
    # multi-part output: datasets = "lqas" matches "lqas_lots"/"lqas_district".
    sel <- vapply(
      all_keys,
      function(k) any(k == datasets | startsWith(k, paste0(datasets, "_"))),
      logical(1)
    )
    files <- all_files[sel]
    keys <- all_keys[sel]
  }
  if (length(files) == 0L) {
    cli::cli_warn("No matching {.file polished_*} files in {.file {data_dir}}.")
    return(stats::setNames(list(), character(0)))
  }

  tables <- lapply(files, .polis_read)
  # ISO3 <-> country-name map, so an ISO3 country filter also reaches the
  # name-keyed roll-ups (LQAS/IM). Build it from the loaded tables; if none of
  # them carry ISO3 (e.g. only `im_district` was requested), read an ISO3-bearing
  # output (afp/es/...) just for the lookup.
  geo <- .polis_iso3_adm0_map(tables)
  if (!is.null(country) && nrow(geo) == 0L) {
    geo <- .polis_iso3_adm0_map(.polis_country_lookup_tables(
      all_files,
      all_keys
    ))
  }

  out <- lapply(tables, function(data) {
    data <- .polis_filter_dataset(data, year, .polis_year_cols)
    .polis_filter_country(data, country, geo)
  })
  stats::setNames(out, keys)
}

# Read the first ISO3-bearing polished output (afp/es/hum_spec/virus) just to
# build the ISO3 <-> country-name lookup, when the requested datasets don't carry
# ISO3 themselves.
#' @noRd
.polis_country_lookup_tables <- function(all_files, all_keys) {
  prefer <- c("afp", "es", "hum_spec", "virus")
  hit <- all_files[match(intersect(prefer, all_keys), all_keys)]
  hit <- hit[!is.na(hit)]
  if (length(hit) == 0L) {
    return(list())
  }
  list(.polis_read(hit[[1L]]))
}

# Recognised raw-table stems (output key -> file stem) and read extensions,
# shared by the directory-based input resolver.
.polis_input_stems <- c(
  afp = "raw_afp",
  es = "raw_es",
  hum_spec = "raw_hum_spec",
  activity = "raw_activity",
  subactivity = "raw_sub_activity",
  lqas = "raw_lqas",
  im = "raw_im",
  population = "raw_population"
)
.polis_input_exts <- c("qs2", "parquet", "rds", "csv")

# Per-stream cache logic version: bump the entry for a cleaner whenever its
# output for the same input could change, to invalidate stale cache entries.
.polis_clean_versions <- c(
  afp = 1L,
  es = 1L,
  hum_spec = 1L,
  sia = 1L,
  lqas = 1L,
  im = 1L,
  pop = 1L,
  virus = 2L,
  indicators = 2L
)

# Resolve the `inputs` argument to a named list of *handles* WITHOUT reading any
# table. Accepts three shapes, so the config stays a lightweight, serialisable
# manifest of paths and the cache can fingerprint a source from file metadata
# (skipping the read entirely on a cache hit):
#   * a directory path        -> a named list of the raw_* file paths it holds;
#   * a named list of paths    -> passed through (read later, on a cache miss);
#   * a named list of frames   -> passed through unchanged.
# Each result carries a `formats` attribute (per-stream file extension) so
# outputs can follow their source format. NULL / empty is an error.
#' @noRd
.polis_input_handles <- function(inputs) {
  if (is.null(inputs)) {
    cli::cli_abort(c(
      "No inputs supplied.",
      "i" = "Pass {.arg inputs} to {.fn run_pipeline}, or set \\
        {.arg inputs} on {.fn polis_config}."
    ))
  }
  if (is.character(inputs) && length(inputs) == 1L) {
    if (!dir.exists(inputs)) {
      cli::cli_abort("Input directory {.file {inputs}} does not exist.")
    }
    handles <- list()
    formats <- list()
    for (key in names(.polis_input_stems)) {
      cand <- file.path(
        inputs,
        paste0(.polis_input_stems[[key]], ".", .polis_input_exts)
      )
      hit <- cand[file.exists(cand)]
      if (length(hit) > 0L) {
        handles[[key]] <- hit[[1L]]
        formats[[key]] <- tolower(tools::file_ext(hit[[1L]]))
      }
    }
    if (length(handles) == 0L) {
      cli::cli_abort("No recognised raw_* tables found in {.file {inputs}}.")
    }
    attr(handles, "formats") <- formats
    return(handles)
  }
  if (!is.list(inputs) || is.data.frame(inputs)) {
    cli::cli_abort(
      "{.arg inputs} must be a named list (of data frames or file paths) or a \\
      directory path."
    )
  }
  # a named list (paths and/or frames): derive formats from any path elements
  formats <- list()
  for (key in names(inputs)) {
    x <- inputs[[key]]
    if (is.character(x) && length(x) == 1L) {
      formats[[key]] <- tolower(tools::file_ext(x))
    }
  }
  attr(inputs, "formats") <- formats
  inputs
}

# Map source-file formats (keyed by input name) to output-file formats (keyed by
# output name): SIA follows its `activity` source; derived outputs (virus,
# indicators) carry no mapping and fall back to the writer default.
#' @noRd
.polis_outputs_formats <- function(src_formats) {
  list(
    afp = src_formats[["afp"]],
    es = src_formats[["es"]],
    hum_spec = src_formats[["hum_spec"]],
    sia = src_formats[["activity"]],
    lqas = src_formats[["lqas"]],
    im = src_formats[["im"]]
  )
}

# Cheap change-detector for a single input handle. A file path fingerprints by
# path + size + mtime (no read); an in-memory frame falls back to a content
# hash; NULL stays NULL. Two runs whose handles fingerprint identically share a
# cache key.
#' @noRd
.polis_fingerprint <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.character(x) && length(x) == 1L && file.exists(x)) {
    info <- file.info(x)
    return(list(
      path = normalizePath(x, winslash = "/", mustWork = FALSE),
      size = info$size,
      mtime = as.numeric(info$mtime)
    ))
  }
  .polis_hash(x)
}

# The cfg fields that change a *cleaned* (pre-scope) stream: naming, synonyms,
# QA sink, and type/empty-column handling. Region/year scoping is applied AFTER
# the cache, so it is deliberately excluded -- one cache entry serves every
# scope.
#' @noRd
.polis_clean_cache_fields <- function(cfg) {
  cfg[c("crosswalk", "synonyms", "qa", "parse_types", "drop_empty_cols")]
}

# The region/year scope, for caching the *derived* steps (virus, indicators)
# that are built from the post-scope cleaned tables -- so changing `regions` or
# `start_year` invalidates them (unlike the scope-independent clean caches).
#' @noRd
.polis_scope_key <- function(cfg) {
  list(regions = sort(toupper(cfg$regions)), start_year = cfg$start_year)
}

# Atomically write a cleaned object to the cache (temp file + rename), so an
# interrupted write never leaves a half-written cache that reads as a hit.
#' @noRd
.polis_cache_write <- function(obj, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  # Qualify the temp name with the process id so two concurrent runs sharing a
  # cache_dir and computing the same key cannot write the same temp file and
  # rename a torn, half-written file into place (which would then read as a
  # valid cache hit). Mirrors .polis_io_write_atomic().
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  .polis_io_write(obj, tmp, "qs2")
  if (!file.rename(tmp, path)) {
    unlink(tmp)
    cli::cli_warn("Could not write cache to {.file {path}}; skipping cache.")
  }
  invisible(path)
}

# Low-level cache gate. Hashes `key_parts` to a cache file `clean_<name>_<key>`;
# on a hit reads + returns it (skipping `compute`), on a miss runs `compute()`
# (nullary) and writes the result. With `cache_dir` unset, it just computes.
# `refresh = TRUE` ignores any existing entry (always recomputes) and overwrites
# it, so the fresh result is still cached for next time.
# This is what lets an unchanged input skip not just cleaning but also the
# derived virus/indicators steps -- and, with the lazy shape/population loaders,
# avoids reading those reference files at all when nothing needs recomputing.
#' @noRd
.polis_cache_run <- function(
  name,
  key_parts,
  cache_dir,
  compute,
  refresh = FALSE
) {
  if (is.null(cache_dir)) {
    return(compute())
  }
  key <- .polis_hash(key_parts)
  path <- file.path(cache_dir, sprintf("clean_%s_%s.qs2", name, key))
  if (!isTRUE(refresh) && file.exists(path)) {
    cli::cli_alert_success("Loaded cached {.val {name}} (inputs unchanged).")
    return(.polis_io_read(path, "qs2"))
  }
  result <- compute()
  .polis_cache_write(result, path)
  result
}

# Content-addressed clean-and-cache for a stream read from raw input handles.
# `handles` is the named list of raw inputs this stream depends on (fingerprinted
# for the key); `compute(resolved)` reads those handles (paths -> tables) and
# runs the cleaner -- called only on a cache miss, so an unchanged source skips
# both the raw read and the clean. The key excludes region/year scope so one
# entry serves every scope (scoping is applied after the cache). `refresh = TRUE`
# ignores any cached entry and recomputes.
#' @noRd
.polis_clean_cached <- function(
  name,
  handles,
  cfg,
  version,
  compute,
  refresh = FALSE,
  extra = NULL
) {
  key_parts <- list(
    name = name,
    inputs = lapply(handles, .polis_fingerprint),
    shape = .polis_fingerprint(cfg$shape),
    cfg = .polis_clean_cache_fields(cfg),
    version = version,
    # any extra run inputs a stream's output depends on beyond its handles
    # (e.g. the population reference date), so the cache reflects them too
    extra = extra
  )
  .polis_cache_run(
    name,
    key_parts,
    cfg$cache_dir,
    function() compute(lapply(handles, .polis_resolve_ref)),
    refresh = refresh
  )
}

# A memoising lazy loader for a reference handle (shape / population): resolves
# the path on first call and reuses it thereafter, so the (potentially large)
# file is read only when some step that actually needs it runs -- never on a
# fully-cached run.
#' @noRd
.polis_lazy_ref <- function(handle) {
  resolved <- NULL
  loaded <- FALSE
  function() {
    if (!loaded) {
      resolved <<- .polis_resolve_ref(handle)
      loaded <<- TRUE
    }
    resolved
  }
}
