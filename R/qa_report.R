# =============================================================================
# Unified data-quality (QA) report
#
# preprocess() scatters its data-quality findings across many side files
# (duplicate_AFPcases_*, afp_no_onset, afp_missing_guid_*, afp_empty_coords,
# ctry_sia_mismatch_*, unmatch_positives_*, virus_large_nt_changes,
# Changed_virustype_*, duplicate_ES_*, ...). This module reads them back from
# the output folder, counts the rows flagged by each check, and assembles a
# single tidy QA summary + a cli report grouped by domain and severity -- so a
# run's data-quality posture is visible at a glance instead of buried in CSVs.
# =============================================================================

utils::globalVariables(c(
  "check",
  "domain",
  "severity",
  "n_flagged",
  "n_files",
  "description",
  "file",
  "sev_rank"
))

#' Build a unified QA report from a preprocessing output folder
#'
#' Scans `polis_folder` (recursively) for the data-quality side files written
#' by a POLIS preprocessing run, counts the rows each check flagged, and returns
#' a tidy summary. Unknown files are ignored; missing checks simply don't appear.
#'
#' @param polis_folder Path to the folder the preprocessing run wrote to.
#' @param registry QA check registry (default [polis_qa_checks()]); a tibble
#'   mapping filename patterns to a check name, domain, severity and
#'   description.
#' @param verbose Emit a cli report (default `TRUE`).
#'
#' @return A list with:
#'   \describe{
#'     \item{`summary`}{Tibble: one row per check found, with `check`,
#'       `domain`, `severity`, `n_flagged` (rows), `n_files`, `description`.}
#'     \item{`files`}{Tibble of every matched file and its row count.}
#'     \item{`meta`}{Totals and the folder scanned.}
#'   }
#' @examples
#' \dontrun{
#' qa <- polis_qa_report("data/polis")
#' qa$summary
#' }
#' @export
polis_qa_report <- function(
  polis_folder,
  registry = polis_qa_checks(),
  verbose = TRUE
) {
  if (!dir.exists(polis_folder)) {
    cli::cli_abort("{.arg polis_folder} does not exist: {.path {polis_folder}}")
  }

  all_files <- list.files(
    polis_folder,
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.(csv|parquet|rds|qs2?)$",
    ignore.case = TRUE
  )
  base_names <- basename(all_files)

  file_rows <- list()
  for (i in seq_len(nrow(registry))) {
    rule <- registry[i, ]
    hits <- which(grepl(rule$pattern, base_names, ignore.case = TRUE))
    for (h in hits) {
      file_rows[[length(file_rows) + 1]] <- data.frame(
        check = rule$check,
        domain = rule$domain,
        severity = rule$severity,
        description = rule$description,
        file = all_files[h],
        n_flagged = .polis_count_rows(all_files[h]),
        stringsAsFactors = FALSE
      )
    }
  }

  files <- if (length(file_rows) > 0) {
    dplyr::as_tibble(dplyr::bind_rows(file_rows))
  } else {
    dplyr::tibble(
      check = character(),
      domain = character(),
      severity = character(),
      description = character(),
      file = character(),
      n_flagged = integer()
    )
  }

  summary <- files |>
    dplyr::group_by(check, domain, severity, description) |>
    dplyr::summarise(
      n_flagged = sum(n_flagged, na.rm = TRUE),
      n_files = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(sev_rank = match(severity, c("error", "warning", "info"))) |>
    dplyr::arrange(sev_rank, dplyr::desc(n_flagged)) |>
    dplyr::select(check, domain, severity, n_flagged, n_files, description)

  meta <- list(
    folder = polis_folder,
    n_checks = nrow(summary),
    n_flagged_total = sum(summary$n_flagged, na.rm = TRUE),
    n_errors = sum(summary$severity == "error", na.rm = TRUE),
    n_warnings = sum(summary$severity == "warning", na.rm = TRUE)
  )

  if (verbose) .polis_qa_print(summary, meta)

  list(summary = summary, files = files, meta = meta)
}

#' Registry of known preprocessing QA artifacts
#'
#' The catalogue of side files a preprocessing run can emit, mapping a filename
#' pattern (regex) to a check name, domain, severity and human description.
#' Extend or filter it and pass to [polis_qa_report()] to customise.
#'
#' @return A tibble with columns `check`, `domain`, `severity`, `pattern`,
#'   `description`.
#' @export
polis_qa_checks <- function() {
  dplyr::tribble(
    ~check,
    ~domain,
    ~severity,
    ~pattern,
    ~description,
    "afp_no_onset",
    "AFP",
    "warning",
    "^afp_no_onset",
    "AFP cases with no onset date",
    "afp_no_classification",
    "AFP",
    "warning",
    "afp_epids_none_classification",
    "AFP cases dropped: no usable classification",
    "afp_duplicates",
    "AFP",
    "warning",
    "^duplicate_AFPcases",
    "Duplicate EPID + onset + admin0",
    "afp_missing_guid",
    "AFP",
    "error",
    "^afp_missing_guid",
    "Cases whose admin GUID did not match the shapefile",
    "afp_bad_guid_epids",
    "AFP",
    "error",
    "AFP_epids_bad_guid",
    "EPIDs with an invalid/unmatched admin GUID",
    "afp_empty_coords",
    "AFP",
    "info",
    "^afp_empty_coords",
    "Cases with missing/zero coordinates (imputed in-polygon)",
    "sia_ctry_mismatch",
    "SIA",
    "warning",
    "^ctry_sia_mismatch",
    "SIA rows whose GUID did not match a country shapefile",
    "positives_unmatched",
    "Positives",
    "error",
    "^unmatch_positives",
    "Virus positives whose GUID did not join to geography",
    "virus_large_nt",
    "Virus",
    "warning",
    "virus_large_nt_changes",
    "Vaccine viruses with >= 6 nucleotide changes",
    "virus_type_changed",
    "Virus",
    "info",
    "^Changed_virustype",
    "Viruses whose type changed vs the archive",
    "virus_class_changed",
    "Virus",
    "info",
    "virus_class_changed_date",
    "Viruses reclassified in the last 7 days",
    "virus_duplicates",
    "Virus",
    "warning",
    "^duplicate_viruses",
    "Duplicate virus records",
    "es_duplicate_sampleid",
    "ES",
    "warning",
    "^duplicate_ES_sampleID",
    "Duplicate ES manual-edit sample IDs",
    "es_duplicates",
    "ES",
    "warning",
    "^duplicate_ES_Polis",
    "Duplicate ES samples"
  )
}

# ---- internal helpers -------------------------------------------------------

#' Count data rows in a QA file, format-aware. Returns NA on unreadable files.
#' @keywords internal
#' @noRd
.polis_count_rows <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- tryCatch(
    switch(
      ext,
      csv = nrow(readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        col_types = readr::cols(.default = readr::col_character())
      )),
      rds = {
        obj <- readRDS(path)
        if (is.data.frame(obj)) nrow(obj) else length(obj)
      },
      parquet = if (requireNamespace("arrow", quietly = TRUE)) {
        nrow(arrow::read_parquet(path))
      } else {
        NA_integer_
      },
      qs = if (requireNamespace("qs2", quietly = TRUE)) {
        obj <- qs2::qs_read(path)
        if (is.data.frame(obj)) nrow(obj) else length(obj)
      } else {
        NA_integer_
      },
      qs2 = if (requireNamespace("qs2", quietly = TRUE)) {
        obj <- qs2::qs_read(path)
        if (is.data.frame(obj)) nrow(obj) else length(obj)
      } else {
        NA_integer_
      },
      NA_integer_
    ),
    error = function(e) NA_integer_
  )
  as.integer(out)
}

#' @keywords internal
#' @noRd
.polis_qa_print <- function(summary, meta) {
  cli::cli_rule()
  cli::cli_h2("Data-Quality Report")
  cli::cli_text("Folder: {.path {meta$folder}}")

  if (nrow(summary) == 0) {
    cli::cli_alert_success("No QA artifacts found -- nothing flagged.")
    cli::cli_rule()
    return(invisible(NULL))
  }

  sev_glyph <- c(error = "x", warning = "!", info = "i")
  for (dom in unique(summary$domain)) {
    cli::cli_h3(dom)
    rows <- summary[summary$domain == dom, ]
    bullets <- stats::setNames(
      sprintf(
        "%s: %s rows (%s) -- %s",
        rows$check,
        format(rows$n_flagged, big.mark = ","),
        rows$severity,
        rows$description
      ),
      sev_glyph[rows$severity]
    )
    cli::cli_bullets(bullets)
  }

  cli::cli_rule(left = "Totals")
  if (meta$n_errors > 0) {
    cli::cli_alert_danger("{meta$n_errors} error-level check(s) flagged.")
  }
  if (meta$n_warnings > 0) {
    cli::cli_alert_warning("{meta$n_warnings} warning-level check(s) flagged.")
  }
  cli::cli_alert_info(
    "{meta$n_checks} check(s), {format(meta$n_flagged_total, big.mark = ',')} total rows flagged."
  )
  cli::cli_rule()
  invisible(NULL)
}
