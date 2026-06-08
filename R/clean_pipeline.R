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
    adm_name = "^adm[0-9]$",
    adm_guid = "^adm[0-9]_guid$",
    coord = "^(latitude|longitude)$",
    onset_date = "^paralysis_onset_date$",
    onset_month = "^month_onset$",
    onset_year = "^year_onset$",
    age = "^age_months$",
    core_dates = paste0(
      "^(notification_date|investigation_date|stool1collection_date|",
      "stool2collection_date|followup_date)$"
    ),
    classification = paste0(
      "^(classification|classification_all|vtype|vtype_fixed|",
      "polio_virus_types|vdpv_classifications|sabin[123]|hot_case|",
      "paralysis_hot_case)$"
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
#' @export
print.polis_config <- function(x, ...) {
  cli::cli_h1("POLIS pipeline configuration")
  cli::cli_text("Start year: {.val {x$start_year}}")
  cli::cli_text("Regions: {.val {x$regions}}")
  cli::cli_text("Folders: {.val {unlist(x$folders)}}")
  cli::cli_text("Column-order groups: {.val {names(x$column_roles)}} -> other")
  cli::cli_text("Rule tables: {.val {names(x$rules)}}")
  cli::cli_text("Synonyms: {.val {!is.null(x$synonyms)}}")
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
#' Virus is linked to cleaned cases/ES when both are present.
#'
#' @param inputs Named list of raw POLIS data frames. Recognised names:
#'   `afp`, `es`, `virus`, `activity`, `subactivity`.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#' @param reconcile_with Optional named list of full-pull data frames (same keys
#'   as `inputs`) used to prune deleted/merged `id`s via [reconcile()].
#'
#' @return A named list of cleaned tibbles: any of `afp`, `es`, `sia`, `virus`.
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
  cleaned <- list()

  # ---- AFP (cases) ----------------------------------------------------------
  if (!is.null(inputs$afp)) {
    cli::cli_h1("Cleaning AFP cases")
    cleaned$afp <- clean_afp(inputs$afp, cfg)
  }

  # ---- ES (environmental) ---------------------------------------------------
  if (!is.null(inputs$es)) {
    cli::cli_h1("Cleaning environmental surveillance")
    cleaned$es <- clean_es(inputs$es, cfg)
  }

  # ---- SIA (campaigns) ------------------------------------------------------
  if (!is.null(inputs$activity)) {
    cli::cli_h1("Cleaning SIA campaigns")
    cleaned$sia <- clean_sia(inputs$activity, inputs$subactivity, cfg)
  }

  # ---- Virus (positives) ----------------------------------------------------
  if (!is.null(inputs$virus)) {
    cli::cli_h1("Cleaning virus / positives")
    cleaned$virus <- clean_virus(
      inputs$virus,
      cases = cleaned$afp,
      es = cleaned$es,
      cfg = cfg
    )
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

  cli::cli_alert_success(
    "Cleaned {length(cleaned)} dataset{?s}: {.val {names(cleaned)}}."
  )
  cleaned
}

#' Run the cleaning pipeline from a directory of raw files
#'
#' File-based convenience over [run_pipeline()]: reads the raw POLIS tables from
#' `source_dir`, cleans them, and (optionally) writes the cleaned outputs to
#' `output_dir`.
#'
#' @param source_dir Directory holding the raw POLIS exports.
#' @param output_dir Optional directory to write cleaned outputs to. If `NULL`
#'   the cleaned set is only returned.
#' @param cfg A [polis_config()] object (default `polis_config()`).
#' @param format Output file extension when writing (default `"rds"`).
#'
#' @return A named list of cleaned tibbles (invisibly when writing).
#'
#' @export
run_pipeline_dir <- function(
  source_dir,
  output_dir = NULL,
  cfg = polis_config(),
  format = "rds"
) {
  inputs <- .polis_read_inputs(source_dir)
  if (length(inputs) == 0) {
    cli::cli_abort("No recognised POLIS tables found in {.file {source_dir}}.")
  }
  cleaned <- run_pipeline(inputs, cfg)

  if (is.null(output_dir)) {
    return(cleaned)
  }
  .polis_write_outputs(cleaned, output_dir, format = format)
  invisible(cleaned)
}
