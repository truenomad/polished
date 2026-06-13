# =============================================================================
# Pipeline configuration
#
# polis_config() is the single, overridable settings object every cleaner and
# the orchestrator share. It holds folder names, run constants, the column
# ordering convention, and optional reference handles (synonyms / QA sink).
# There are deliberately NO column-name arguments here: column naming lives in
# the package crosswalk (see polis_crosswalk()), so the canonical schema is
# data, not scattered function defaults.
# =============================================================================

#' Build a POLIS pipeline configuration
#'
#' Creates the settings object shared by the cleaners ([clean_afp()],
#' [clean_es()], [clean_virus()], [clean_sia()]) and the orchestrator
#' ([run_pipeline()]). Every field has a sensible default and can be overridden.
#'
#' @param start_year Earliest onset/collection year to retain (default `2020`).
#' @param regions Valid WHO region codes used for optional region-scoped output
#'   folders (default the six WHO regions).
#' @param folders Named list of pipeline folder names (not full paths). Override
#'   individual entries to relocate outputs; unspecified entries keep defaults.
#' @param column_roles Ordered named list of regex patterns that classify output
#'   columns into ordering groups. Columns are emitted group-by-group in this
#'   order; anything matching no pattern is treated as `other` and placed last.
#'   This is how [order_columns()] enforces id -> location -> time -> other
#'   without hardcoding column names.
#' @param seed Integer seed for any clustering/sampling step, for reproducible
#'   runs (default `1234`).
#' @param synonyms Optional EPID/geoplace synonym table for [remap_synonyms()].
#'   `NULL` (default) makes synonym remapping a no-op.
#' @param qa Optional handle (path or list) where ambiguous-key flags are routed
#'   by [flag_ambiguous()]. `NULL` (default) collects flags in-memory only.
#' @param population Optional under-15 population denominators used by
#'   [calc_polio_indicators()] in [run_pipeline()]. Either a data frame or a
#'   path to one (read via the file extension); `NULL` (default) skips the
#'   rate indicators that need a denominator.
#' @param shape Optional **already-processed** district shape passed to every
#'   cleaner as `shape =` for admin reconciliation (an `sf` polygon layer or a
#'   long ADM2 attribute table, or a path to one). `NULL` (default) disables
#'   shape-based admin recovery.
#' @param parse_types If `TRUE` (default) each cleaner finishes by inferring
#'   column base types (character -> numeric/integer/date/datetime/logical) via
#'   [auto_parse_types()] (no factor conversion). Set `FALSE` to keep the raw
#'   character columns.
#' @param drop_empty_cols If `TRUE` (default) each cleaner drops columns that are
#'   entirely `NA` after cleaning. Note this makes the output schema depend on
#'   the data; set `FALSE` for a fixed column set across runs.
#'
#' @return An object of class `polis_config` (a named list).
#'
#' @examples
#' cfg <- polis_config(start_year = 2018)
#' cfg$column_roles
#'
#' @export
polis_config <- function(
  start_year = 2020,
  regions = c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO"),
  folders = NULL,
  column_roles = NULL,
  seed = 1234L,
  synonyms = NULL,
  qa = NULL,
  population = NULL,
  shape = NULL,
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

  # ---- folder defaults (overridable per entry) ------------------------------
  folders_default <- list(
    data = "data",
    misc = "misc",
    core_ready = "Core_Ready_Files",
    archive = "Archive",
    change_log = "Change Log",
    parts = ".parts"
  )
  folders <- utils::modifyList(folders_default, folders %||% list())

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

  # ---- rules + crosswalk (data-driven, shipped with the package) ------------
  rules <- .polis_load_rules()
  crosswalk <- .polis_crosswalk_map()

  structure(
    list(
      start_year = as.integer(start_year),
      regions = regions,
      folders = folders,
      column_roles = column_roles,
      seed = as.integer(seed),
      rules = rules,
      crosswalk = crosswalk,
      synonyms = synonyms,
      qa = qa,
      population = population,
      shape = shape,
      parse_types = parse_types,
      drop_empty_cols = drop_empty_cols
    ),
    class = "polis_config"
  )
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
  cli::cli_text("Folders: {.val {unlist(x$folders)}}")
  cli::cli_text("Column-order groups: {.val {names(x$column_roles)}} -> other")
  cli::cli_text("Rule tables: {.val {names(x$rules)}}")
  cli::cli_text("Synonyms: {.val {!is.null(x$synonyms)}}")
  cli::cli_text("Population: {.val {!is.null(x$population)}}")
  cli::cli_text("Shape: {.val {!is.null(x$shape)}}")
  invisible(x)
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
#' @param inputs Named list of raw POLIS data frames. Recognised names:
#'   `afp`, `es`, `hum_spec`, `activity`, `subactivity`, `lqas`, `im`. (A raw
#'   `virus` table is not used -- positives are derived from the cleaned
#'   `afp`/`es` outputs. `lqas`/`im` are campaign-quality tables turned into
#'   district-year roll-ups via [process_sia_quality()].)
#' @param cfg A [polis_config()] object (default `polis_config()`). Its
#'   `shape` and `population` handles drive admin reconciliation and the
#'   indicators step respectively.
#' @param reconcile_with Optional named list of full-pull data frames (same keys
#'   as `inputs`) used to prune deleted/merged `id`s via [reconcile()].
#'
#' @return A named list holding any of the cleaned tibbles `afp`, `es`,
#'   `hum_spec`, `sia`, `virus`, the SIA-quality roll-ups `lqas` / `im` (each a
#'   list of `lots`/`district`/`meta`), plus `indicators` (the
#'   [calc_polio_indicators()] result list) when AFP cases are present.
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
  inputs,
  cfg = polis_config(),
  reconcile_with = NULL
) {
  if (!is.list(inputs) || is.data.frame(inputs)) {
    cli::cli_abort("{.arg inputs} must be a named list of raw data frames.")
  }
  # Resolve the reference handles once: a path is read from disk, an in-memory
  # object passes through. The shape feeds every cleaner; population feeds
  # indicators.
  shape <- .polis_resolve_ref(cfg$shape)
  cleaned <- list()

  # ---- AFP (cases) ----------------------------------------------------------
  if (!is.null(inputs$afp)) {
    cli::cli_h1("Cleaning AFP cases")
    cleaned$afp <- clean_afp(inputs$afp, cfg, shape = shape)
  }

  # ---- ES (environmental) ---------------------------------------------------
  if (!is.null(inputs$es)) {
    cli::cli_h1("Cleaning environmental surveillance")
    cleaned$es <- clean_es(inputs$es, cfg, shape = shape)
  }

  # ---- Human specimens ------------------------------------------------------
  # Passed the cleaned cases so a specimen inherits its parent case's geography.
  if (!is.null(inputs$hum_spec)) {
    cli::cli_h1("Cleaning human specimens")
    cleaned$hum_spec <- clean_human_spec(
      inputs$hum_spec,
      cfg,
      shape = shape,
      cases = cleaned$afp
    )
  }

  # ---- SIA (campaigns) ------------------------------------------------------
  if (!is.null(inputs$activity)) {
    cli::cli_h1("Cleaning SIA campaigns")
    cleaned$sia <- clean_sia(
      inputs$activity,
      inputs$subactivity,
      cfg,
      shape = shape
    )
  }

  # ---- SIA quality (LQAS / IM) ----------------------------------------------
  # Campaign-quality monitoring: rolled up to district-year indicators rather
  # than cleaned like the surveillance streams. Each processor's result is a
  # list (lots/district/meta), attached under its own key so the file writer
  # emits one polished_<key>_<component> table per data-frame component.
  if (!is.null(inputs$lqas) || !is.null(inputs$im)) {
    cli::cli_h1("Processing SIA quality (LQAS / IM)")
    sia_quality <- process_sia_quality(
      lqas = inputs$lqas,
      im = inputs$im,
      cfg = cfg,
      shape = shape,
      verbose = TRUE
    )
    if (!is.null(sia_quality$lqas)) {
      cleaned$lqas <- sia_quality$lqas
    }
    if (!is.null(sia_quality$im)) {
      cleaned$im <- sia_quality$im
    }
  }

  # ---- Virus (positives): built from the cleaned human + ES streams ---------
  if (!is.null(cleaned$afp) || !is.null(cleaned$es)) {
    cli::cli_h1("Building virus / positives")
    virus <- clean_virus(cases = cleaned$afp, es = cleaned$es, cfg = cfg)
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
    indicators <- tryCatch(
      calc_polio_indicators(
        cases = cleaned$afp,
        es = cleaned$es,
        sia = cleaned$sia,
        virus = cleaned$virus,
        population = .polis_resolve_ref(cfg$population)
      ),
      error = function(e) {
        cli::cli_alert_warning(
          "Indicators skipped: {conditionMessage(e)}"
        )
        NULL
      }
    )
    if (!is.null(indicators)) {
      cleaned$indicators <- indicators
    }
  }

  cli::cli_alert_success(
    "Produced {length(cleaned)} output{?s}: {.val {names(cleaned)}}."
  )
  cleaned
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
#' @param output_dir Optional directory to write `polished_*` outputs to. If
#'   `NULL` the output set is only returned.
#' @param cfg A [polis_config()] object (default `polis_config()`); its
#'   `shape` and `population` handles drive reconciliation and indicators.
#'
#' @details
#' When `output_dir` is supplied a data-quality workbook (`checks_<dataset>.xlsx`)
#' is also written per cleaned dataset via the `checks_*()` functions -- a
#' `Summary` tab plus one tab of flagged rows per failing check. Requires the
#' optional `openxlsx` package.
#'
#' @return A named list of pipeline outputs (invisibly when writing).
#'
#' @export
run_pipeline_dir <- function(
  source_dir,
  output_dir = NULL,
  cfg = polis_config()
) {
  inputs <- .polis_read_inputs(source_dir)
  if (length(inputs) == 0) {
    cli::cli_abort("No recognised raw_* tables found in {.file {source_dir}}.")
  }
  src_formats <- attr(inputs, "formats") %||% list()
  cleaned <- run_pipeline(inputs, cfg)

  if (is.null(output_dir)) {
    return(cleaned)
  }
  # Map each output to the format of the raw file it derives from; SIA follows
  # its activity source, derived outputs fall back to the writer default.
  out_formats <- list(
    afp = src_formats[["afp"]],
    es = src_formats[["es"]],
    hum_spec = src_formats[["hum_spec"]],
    sia = src_formats[["activity"]],
    lqas = src_formats[["lqas"]],
    im = src_formats[["im"]]
  )
  .polis_write_outputs(cleaned, output_dir, formats = out_formats)

  # ---- data-quality check workbooks ----------------------------------------
  cli::cli_h1("Running data-quality checks")
  .polis_write_check_workbooks(cleaned, output_dir, reference_date = Sys.Date())
  invisible(cleaned)
}
