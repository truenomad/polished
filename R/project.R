# =============================================================================
# Project layout: scaffold once, everything downstream derives from it.
# raw -> processed -> validation, with a disposable cache and run logs.
# =============================================================================

.polis_project_zones <- c("raw", "processed", "validation", "cache", "logs")

# year / country filter columns, first present in a table wins
.polis_year_cols <- c(
  "year",
  "year_onset",
  "year_start",
  "year_collection",
  "collect_yr"
)
.polis_country_cols <- c("iso3", "country_iso3code", "adm0", "country")

# -----------------------------------------------------------------------------
# Scaffold + object
# -----------------------------------------------------------------------------

#' Create (or re-open) a data project
#'
#' Sets up the standard project layout under `root` and returns a `polis_project`
#' describing it. The layout has four role-named zones plus logs:
#' \describe{
#'   \item{`raw`}{downloaded source tables and a snapshot `manifest.json`
#'     (precious -- never written by cleaning).}
#'   \item{`processed`}{cleaned analytic tables, one partitioned parquet dataset
#'     per table (derived).}
#'   \item{`validation`}{data-quality reports and checks.}
#'   \item{`cache`}{regenerable process caches (e.g. campaign-round clustering).}
#'   \item{`logs`}{run logs.}
#' }
#' Creation is idempotent and never deletes: calling it again on an existing
#' project just re-opens it. Downstream functions take the returned object via
#' their `project` argument; nothing relies on a hidden global.
#'
#' @param root Path to the project root. Created (recursively) if absent.
#' @param gitignore Write a `.gitignore` ignoring the regenerable/precious-but-
#'   bulky zones (`raw/`, `cache/`, `logs/`) when one is not already present.
#'   Default `TRUE`.
#' @param quiet Suppress the success message. Default `FALSE`.
#'
#' @return A `polis_project`: a list with `root` and one absolute path per zone
#'   (`raw`, `processed`, `validation`, `cache`, `logs`), class `polis_project`.
#'
#' @examples
#' proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
#' proj$processed
#'
#' @export
init_polis_project <- function(root, gitignore = TRUE, quiet = FALSE) {
  if (!is.character(root) || length(root) != 1L || !nzchar(root)) {
    cli::cli_abort("{.arg root} must be a single non-empty path.")
  }
  root <- .polis_project_root(root)
  for (path in c(root, file.path(root, .polis_project_zones))) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  # normalise once the dirs exist, so symlink resolution (e.g. macOS /var ->
  # /private/var) is identical on a first create and a later re-open.
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  zones <- stats::setNames(
    file.path(root, .polis_project_zones),
    .polis_project_zones
  )
  if (isTRUE(gitignore)) {
    .polis_project_gitignore(root)
  }
  project <- .new_polis_project(root, zones)
  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Project ready at {.file {root}}.")
  }
  project
}

#' Construct a `polis_project` from a root and its zone paths.
#' @noRd
.new_polis_project <- function(root, zones) {
  structure(
    c(list(root = root), as.list(zones)),
    class = "polis_project"
  )
}

#' Make a root absolute (expand `~`, prepend cwd if relative) without resolving
#' symlinks -- that happens after the dirs exist.
#' @noRd
.polis_project_root <- function(root) {
  root <- path.expand(root)
  if (!grepl("^(/|[A-Za-z]:)", root)) {
    root <- file.path(getwd(), root)
  }
  root
}

#' Abort unless `x` is a `polis_project`.
#' @noRd
.polis_assert_project <- function(x) {
  if (!inherits(x, "polis_project")) {
    cli::cli_abort(
      "{.arg project} must be a {.cls polis_project} from {.fn init_polis_project}."
    )
  }
  invisible(x)
}

#' Write a default `.gitignore` for the project (only if absent).
#' @noRd
.polis_project_gitignore <- function(root) {
  path <- file.path(root, ".gitignore")
  if (file.exists(path)) {
    return(invisible(path))
  }
  lines <- c(
    "# polis project: ignore bulky / regenerable zones",
    "raw/",
    "cache/",
    "logs/"
  )
  writeLines(lines, path)
  invisible(path)
}

#' Print a polis_project
#' @param x A [polis_project].
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.polis_project <- function(x, ...) {
  cli::cli_h2("polis_project")
  cli::cli_text("{.strong root}: {.file {x$root}}")
  for (zone in .polis_project_zones) {
    n <- length(list.files(x[[zone]], recursive = TRUE))
    cli::cli_text("{.strong {zone}}: {.file {x[[zone]]}} ({n} file{?s})")
  }
  invisible(x)
}

#' Build a path inside a project zone
#'
#' Resolves a path under one of a project's zones. `project_path(proj,
#' "processed", "sia")` returns `<root>/processed/sia`. With no extra parts it
#' returns the zone directory itself.
#'
#' @param project A [polis_project].
#' @param zone One of `"root"`, `"raw"`, `"processed"`, `"validation"`,
#'   `"cache"`, `"logs"`. Default `"root"`.
#' @param ... Further path components appended under the zone.
#'
#' @return A single file path string.
#'
#' @examples
#' proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
#' project_path(proj, "processed", "sia")
#'
#' @export
project_path <- function(project, zone = "root", ...) {
  .polis_assert_project(project)
  zones <- c("root", .polis_project_zones)
  if (!is.character(zone) || length(zone) != 1L || !zone %in% zones) {
    cli::cli_abort("{.arg zone} must be one of {.val {zones}}.")
  }
  parts <- vapply(list(...), as.character, character(1))
  do.call(file.path, c(list(project[[zone]]), as.list(parts)))
}

#' Clear a project's regenerable cache
#'
#' Deletes everything in the project's `cache/` zone and nothing else -- the
#' precious `raw/` and derived `processed/`/`validation/` zones are never
#' touched. Safe to call when the cache is already empty.
#'
#' @param project A [polis_project].
#' @param quiet Suppress the success message. Default `FALSE`.
#'
#' @return The `project`, invisibly.
#'
#' @examples
#' proj <- init_polis_project(file.path(tempdir(), "polis_demo"), quiet = TRUE)
#' clear_cache(proj, quiet = TRUE)
#'
#' @export
clear_cache <- function(project, quiet = FALSE) {
  .polis_assert_project(project)
  entries <- list.files(project$cache, full.names = TRUE, recursive = FALSE)
  unlink(entries, recursive = TRUE)
  if (!isTRUE(quiet)) {
    cli::cli_alert_success("Cleared {length(entries)} cache entr{?y/ies}.")
  }
  invisible(project)
}

# -----------------------------------------------------------------------------
# Partitioned parquet dataset IO
# -----------------------------------------------------------------------------

#' Write a data frame as a partitioned parquet dataset
#'
#' Replaces any existing dataset at `dir` (a derived, regenerable location).
#' Partitions on whichever of `partition_by` are present; an empty intersection
#' writes a single unpartitioned dataset.
#' @noRd
.polis_write_dataset <- function(data, dir, partition_by = "year") {
  arrow <- .polis_require("arrow", "write a parquet dataset")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  part <- intersect(partition_by, names(data))
  arrow$write_dataset(
    data,
    dir,
    partitioning = part,
    format = "parquet",
    existing_data_behavior = "delete_matching"
  )
  invisible(dir)
}

#' Resolve logical partition keys to a table's real columns.
#'
#' Maps the request `"year"` to the table's actual year column (the first of
#' [.polis_year_cols] present); passes any other key through when present.
#' @noRd
.polis_resolve_partition <- function(data, partition_by) {
  out <- character(0)
  for (key in partition_by) {
    if (identical(key, "year")) {
      year_col <- intersect(.polis_year_cols, names(data))
      if (length(year_col) > 0L) {
        out <- c(out, year_col[[1]])
      }
    } else if (key %in% names(data)) {
      out <- c(out, key)
    }
  }
  unique(out)
}

#' Open a parquet dataset directory as a lazy Arrow query.
#' @noRd
.polis_open_dataset <- function(dir) {
  arrow <- .polis_require("arrow", "open a parquet dataset")
  arrow$open_dataset(dir)
}

#' Materialise a (possibly filtered) Arrow query into the requested shape.
#'
#' `arrow` keeps it lazy (out-of-core); `tibble` collects; `dt` collects to a
#' data.table; `dtplyr` collects then wraps a mutable `lazy_dt` so downstream
#' data.table verbs avoid defensive copies.
#' @noRd
.polis_materialize <- function(query, as) {
  if (identical(as, "arrow")) {
    return(query)
  }
  collected <- dplyr::collect(query)
  if (identical(as, "tibble")) {
    return(collected)
  }
  dt <- .polis_require("data.table", "return a data.table")$setDT(collected)
  if (identical(as, "dt")) {
    return(dt)
  }
  .polis_require("dtplyr", "return a dtplyr lazy_dt")$lazy_dt(
    dt,
    immutable = FALSE
  )
}

# -----------------------------------------------------------------------------
# Snapshot manifest (provenance + a cheap cache key)
# -----------------------------------------------------------------------------

#' Write `raw/manifest.json` (content `snapshot_id` + per-table counts); returns
#' the snapshot id. `created` is passed in to stay pure/testable.
#' @noRd
.polis_write_manifest <- function(project, tables, created = NULL) {
  jsonlite <- .polis_require("jsonlite", "write the snapshot manifest")
  snapshot_id <- .polis_hash(tables)
  manifest <- list(
    snapshot_id = snapshot_id,
    created = created %||% "",
    tables = lapply(tables, function(tbl) {
      list(rows = nrow(tbl), cols = ncol(tbl))
    })
  )
  path <- file.path(project$raw, "manifest.json")
  if (!dir.exists(project$raw)) {
    dir.create(project$raw, recursive = TRUE, showWarnings = FALSE)
  }
  jsonlite$write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(snapshot_id)
}

#' Read a project's snapshot manifest, or `NULL` when there isn't one.
#' @noRd
.polis_read_manifest <- function(project) {
  path <- file.path(project$raw, "manifest.json")
  if (!file.exists(path)) {
    return(NULL)
  }
  jsonlite <- .polis_require("jsonlite", "read the snapshot manifest")
  jsonlite$read_json(path, simplifyVector = TRUE)
}

# -----------------------------------------------------------------------------
# Streaming pipeline: clean from raw, write processed, return a manifest
# -----------------------------------------------------------------------------

#' Clean a project's raw tables to disk, one group at a time
#'
#' The disk-streaming counterpart to [run_pipeline()]: reads the raw tables from
#' the project's `raw/` zone, cleans them and writes each as a partitioned
#' parquet dataset under `processed/`, then returns a lightweight result manifest
#' -- the cleaned tables themselves never accumulate in the session.
#'
#' To bound peak memory the human + environmental streams (which together build
#' the virus/positives table) are processed as one group and freed before the
#' independent SIA stream, so at most one group is resident at a time rather than
#' the whole cleaned set.
#'
#' @param project A [polis_project] whose `raw/` zone holds the source tables.
#' @param cfg A [polis_config()] (default `polis_config()`).
#' @param partition_by Columns to partition the parquet output on, when present.
#'   Default `"year"` (resolved per table to its year column).
#' @param cache Use the project `cache/` zone for the SIA round-clustering cache,
#'   keyed on the manifest snapshot id when available. Default `TRUE`.
#' @param quiet Suppress per-table progress. Default `FALSE`.
#'
#' @return Invisibly, a named list with `snapshot_id` and a `tables` data frame
#'   (one row per written dataset: `dataset`, `rows`, `path`).
#'
#' @seealso [load_polis()] to read a slice back, [run_pipeline()] for the
#'   in-memory variant.
#' @examples
#' \dontrun{
#' proj <- init_polis_project(file.path(tempdir(), "demo"), quiet = TRUE)
#' run_pipeline_project(proj)
#' }
#' @export
run_pipeline_project <- function(
  project,
  cfg = polis_config(),
  partition_by = "year",
  cache = TRUE,
  quiet = FALSE
) {
  .polis_assert_project(project)
  inputs <- .polis_read_inputs(project$raw)
  if (length(inputs) == 0L) {
    cli::cli_abort("No recognised source tables in {.file {project$raw}}.")
  }
  # stamp a snapshot on first run so the cache keys on the cheap id
  manifest <- .polis_read_manifest(project)
  snapshot_id <- if (is.null(manifest)) {
    .polis_write_manifest(project, inputs, created = format(Sys.time()))
  } else {
    manifest$snapshot_id
  }
  cache_dir <- if (isTRUE(cache)) project$cache else NULL
  written <- list()
  # returns a one-row record for a written dataset, or NULL when there's nothing
  # to write (assigning NULL drops the slot, so `written` holds real datasets).
  write_one <- function(name, data) {
    if (is.null(data) || nrow(data) == 0L) {
      return(NULL)
    }
    dir <- file.path(project$processed, name)
    .polis_write_dataset(
      data,
      dir,
      .polis_resolve_partition(data, partition_by)
    )
    if (!isTRUE(quiet)) {
      cli::cli_alert_success(
        "Wrote {.val {name}} ({.val {nrow(data)}} rows) to {.file {basename(dir)}}."
      )
    }
    list(dataset = name, rows = nrow(data), path = dir)
  }

  # ---- group 1: human + ES + the virus table they build --------------------
  if (!is.null(inputs$afp) || !is.null(inputs$es)) {
    afp <- if (!is.null(inputs$afp)) clean_afp(inputs$afp, cfg) else NULL
    es <- if (!is.null(inputs$es)) clean_es(inputs$es, cfg) else NULL
    written[["afp"]] <- write_one("afp", afp)
    written[["es"]] <- write_one("es", es)
    if (!is.null(afp) || !is.null(es)) {
      virus <- clean_virus(cases = afp, es = es, cfg = cfg)
      written[["virus"]] <- write_one("virus", virus)
    }
    rm(afp, es)
    invisible(gc(verbose = FALSE))
  }

  # ---- group 2: SIA campaigns (independent) --------------------------------
  if (!is.null(inputs$activity)) {
    sia <- clean_sia(
      inputs$activity,
      inputs$subactivity,
      cfg = cfg,
      cache_dir = cache_dir,
      cache_key = snapshot_id,
      verbose = FALSE
    )
    written[["sia"]] <- write_one("sia", sia)
    rm(sia)
    invisible(gc(verbose = FALSE))
  }

  result <- list(
    snapshot_id = snapshot_id %||% NA_character_,
    tables = .polis_rbind_rows(written)
  )
  if (!isTRUE(quiet)) {
    cli::cli_alert_success(
      "Pipeline wrote {nrow(result$tables)} dataset{?s} to {.file {project$processed}}."
    )
  }
  invisible(result)
}

#' Row-bind a list of one-row record lists into a tibble (empty-safe).
#'
#' The argument is `records`, not `rows`: a `rows` column shadows a `rows`
#' argument in `tibble()`'s sequential evaluation.
#' @noRd
.polis_rbind_rows <- function(records) {
  if (length(records) == 0L) {
    return(tibble::tibble(
      dataset = character(0),
      rows = integer(0),
      path = character(0)
    ))
  }
  tibble::tibble(
    dataset = vapply(records, function(r) r$dataset, character(1)),
    rows = vapply(records, function(r) as.numeric(r$rows), numeric(1)),
    path = vapply(records, function(r) r$path, character(1))
  )
}

# -----------------------------------------------------------------------------
# Loader: one filtered slice across every processed dataset
# -----------------------------------------------------------------------------

#' Load a year/country slice across all processed datasets
#'
#' Opens every dataset written to the project's `processed/` zone, pushes a
#' `year` and/or `country` filter down to the parquet scan, and returns a named
#' list with one filtered table per dataset (`afp`, `es`, `sia`, `virus`, and any
#' others present, such as `indicators`). Because `processed/` is partitioned by
#' year, a year filter prunes whole partitions; the country filter uses parquet
#' predicate pushdown. Only the matching slice is materialised.
#'
#' @param project A [polis_project].
#' @param year Optional year (or years) to keep. Matched against the first of
#'   `year`, `year_onset`, `year_start`, `collect_yr` present in each table.
#' @param country Optional country/ISO3 value(s) to keep. Matched against the
#'   first of `iso3`, `country_iso3code`, `adm0`, `country` present.
#' @param datasets Optional character vector restricting which datasets to load
#'   (by folder name under `processed/`). Default `NULL` (all present).
#' @param as Shape of each returned element: `"tibble"` (collected, default),
#'   `"dtplyr"` (a mutable `lazy_dt` for data.table-backed downstream work),
#'   `"dt"` (a `data.table`), or `"arrow"` (the lazy, uncollected query).
#'
#' @return A named list of tables (one per dataset), each filtered to the slice.
#'
#' @seealso [run_pipeline_project()] which writes the datasets this reads.
#' @examples
#' \dontrun{
#' proj <- init_polis_project(file.path(tempdir(), "demo"), quiet = TRUE)
#' slice_2025 <- load_polis(proj, year = 2025, as = "dtplyr")
#' }
#' @export
load_polis <- function(
  project,
  year = NULL,
  country = NULL,
  datasets = NULL,
  as = c("tibble", "dtplyr", "dt", "arrow")
) {
  .polis_assert_project(project)
  as <- match.arg(as)
  present <- list.dirs(project$processed, recursive = FALSE, full.names = FALSE)
  if (!is.null(datasets)) {
    present <- intersect(present, datasets)
  }
  if (length(present) == 0L) {
    cli::cli_warn("No processed datasets found in {.file {project$processed}}.")
    return(stats::setNames(list(), character(0)))
  }
  out <- lapply(present, function(name) {
    query <- .polis_open_dataset(file.path(project$processed, name))
    query <- .polis_filter_dataset(query, year, .polis_year_cols)
    query <- .polis_filter_dataset(query, country, .polis_country_cols)
    .polis_materialize(query, as)
  })
  stats::setNames(out, present)
}

#' Filter an Arrow query on the first candidate column it actually has.
#'
#' Returns the query unchanged when `values` is `NULL` or the table has none of
#' the candidate columns, so a filter dimension a dataset doesn't carry is a
#' no-op rather than an error.
#' @noRd
.polis_filter_dataset <- function(query, values, candidates) {
  if (is.null(values)) {
    return(query)
  }
  col <- intersect(candidates, names(query))
  if (length(col) == 0L) {
    return(query)
  }
  col <- col[[1]]
  dplyr::filter(query, .data[[col]] %in% values)
}
