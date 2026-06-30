# =============================================================================
# Project layout: scaffold once, everything downstream derives from it.
# raw -> processed -> validation, with a disposable cache and run logs.
# =============================================================================

.polis_project_zones <- c("raw", "processed", "validation", "cache", "logs")

# -----------------------------------------------------------------------------
# Scaffold + object
# -----------------------------------------------------------------------------

#' Create (or re-open) a data project
#'
#' Sets up the standard project layout under `root` and returns a `polis_project`
#' describing it. The layout has four role-named zones plus logs:
#' \describe{
#'   \item{`raw`}{downloaded source tables (precious -- never written by
#'     cleaning).}
#'   \item{`processed`}{cleaned analytic outputs (derived); a natural value for
#'     `polis_config(output_dir = )`.}
#'   \item{`validation`}{data-quality reports and checks.}
#'   \item{`cache`}{regenerable process caches; a natural value for
#'     `polis_config(cache_dir = )`.}
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
#' @param x A `polis_project`.
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
#' @param project A `polis_project`.
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
#' @param project A `polis_project`.
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

# =============================================================================
# Pipeline project scaffold: the opinionated layout this package is built around
# =============================================================================

#' Scaffold a full polished pipeline project
#'
#' Creates the domain-numbered data-pipeline layout this package is built around
#' (`01_data/` domains, `02_scripts/`, `03_outputs/`) and writes a wired
#' `.Rprofile` (the `cfg` manifest), a `.gitignore`, and starter
#' `2a_download_data.R` / `2b_process_data.R` scripts. Everything the generated
#' `cfg` points at exists on disk, so after dropping the WHO polio GDB layers in
#' `01_data/1a_shapefiles/raw/`, sourcing `2a` then `2b` runs the whole
#' download -> clean pipeline (downloads POLIS streams + WorldPop, processes the
#' shapefile, extracts WorldPop, and runs [run_pipeline()] over every stream).
#'
#' Distinct from [init_polis_project()], which scaffolds a lighter generic
#' `raw/processed/validation/cache/logs` layout.
#'
#' @param root Path to the project root (created recursively if absent).
#' @param regions WHO region codes the pipeline is scoped to, written into the
#'   generated `cfg`. Default `"EMRO"`.
#' @param start_year Earliest onset/collection year to retain. Default `2020`.
#' @param pop_years Calendar years for the population / WorldPop step. Default
#'   `2010:2027`.
#' @param pop_source Population denominator preference for [polis_config()], one
#'   of `"reconciled"`, `"polis"`, `"worldpop"`. Default `"reconciled"`.
#' @param domains Which `01_data` domains to create: any of `"shapefiles"`,
#'   `"population"`, `"polis"`, `"vaccination"`. Default all four.
#' @param write_rprofile,write_scripts,gitignore Whether to write the wired
#'   `.Rprofile`, the starter `02_scripts/`, and the `.gitignore`. Default
#'   `TRUE`.
#' @param overwrite Overwrite `.Rprofile` / scripts / `.gitignore` that already
#'   exist. Default `FALSE` -- existing files are kept (and skipped with a note),
#'   so re-running on a live project never clobbers it. Directory creation is
#'   always idempotent.
#' @param renv Set up `renv` in the project for reproducible package versions.
#'   When `TRUE` (and the optional `renv` package is installed) the scaffold runs
#'   [renv::scaffold()] -- writing `renv/activate.R`, a starter `renv.lock`, and
#'   wiring the generated `.Rprofile` to load it -- so a later `renv::snapshot()`
#'   pins your versions and collaborators reproduce them with `renv::restore()`.
#'   Default `FALSE`.
#' @param quiet Suppress the success message. Default `FALSE`.
#'
#' @return Invisibly, a list with `root` and the absolute `dirs` created.
#'
#' @seealso [init_polis_project()], [polis_config()], [run_pipeline()].
#' @examples
#' proj <- init_polis_pipeline(
#'   file.path(tempdir(), "polio_pipeline"), regions = "EMRO", quiet = TRUE
#' )
#' file.exists(file.path(proj$root, ".Rprofile"))
#'
#' @export
init_polis_pipeline <- function(
  root,
  regions = "EMRO",
  start_year = 2020,
  pop_years = 2010:2027,
  pop_source = c("reconciled", "polis", "worldpop"),
  domains = c("shapefiles", "population", "polis", "vaccination"),
  write_rprofile = TRUE,
  write_scripts = TRUE,
  gitignore = TRUE,
  overwrite = FALSE,
  renv = FALSE,
  quiet = FALSE
) {
  if (!is.character(root) || length(root) != 1L || !nzchar(root)) {
    cli::cli_abort("{.arg root} must be a single non-empty path.")
  }
  pop_source <- match.arg(pop_source)
  domains <- match.arg(
    domains,
    c("shapefiles", "population", "polis", "vaccination"),
    several.ok = TRUE
  )

  root <- .polis_project_root(root)
  # whether a (user-authored) .Rprofile already exists, captured BEFORE renv
  # scaffolding can drop a placeholder one -- so re-running never clobbers a
  # real one but the renv placeholder is always replaced by our manifest.
  rprofile_path <- file.path(root, ".Rprofile")
  rprofile_pre <- file.exists(rprofile_path)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  dirs <- .polis_pipeline_dirs(domains)
  for (d in dirs) {
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  }
  # rprojroot sentinel so here::here() in the generated .Rprofile resolves
  # deterministically even before the project is a git repo / RStudio project.
  here_file <- file.path(root, ".here")
  if (!file.exists(here_file)) {
    file.create(here_file)
  }

  # Optional renv setup. renv::scaffold() writes renv/activate.R + a starter
  # renv.lock and prepends an autoloader to .Rprofile; we then write our manifest
  # .Rprofile (overwriting that placeholder) with an unguarded source() line.
  do_renv <- isTRUE(renv)
  if (do_renv && !requireNamespace("renv", quietly = TRUE)) {
    cli::cli_alert_warning(
      "{.pkg renv} is not installed; skipping renv setup. Install it and \\
      re-run with {.code renv = TRUE} for reproducible package versions."
    )
    do_renv <- FALSE
  }
  if (do_renv) {
    renv::scaffold(project = root)
  }

  subs <- list(
    REGIONS = paste(deparse(regions), collapse = " "),
    START_YEAR = format(as.integer(start_year)),
    POP_YEARS = paste(deparse(pop_years), collapse = " "),
    POP_SOURCE = deparse(pop_source),
    RENV_SOURCE = if (do_renv) {
      'source("renv/activate.R")'
    } else {
      'if (file.exists("renv/activate.R")) source("renv/activate.R")'
    }
  )
  if (isTRUE(write_rprofile)) {
    # replace the renv placeholder .Rprofile on a fresh project, but still keep a
    # genuinely pre-existing (user-edited) one unless overwrite = TRUE.
    .polis_write_template(
      "Rprofile",
      rprofile_path,
      subs,
      overwrite || (do_renv && !rprofile_pre)
    )
  }
  if (isTRUE(write_scripts)) {
    sdir <- file.path(root, "02_scripts")
    # template sources have no .R extension (so air / lintr / the format hook
    # leave their {{...}} placeholders alone); the written scripts are .R.
    .polis_write_template(
      "2a_download_data",
      file.path(sdir, "2a_download_data.R"),
      subs,
      overwrite
    )
    .polis_write_template(
      "2b_process_data",
      file.path(sdir, "2b_process_data.R"),
      subs,
      overwrite
    )
  }
  if (isTRUE(gitignore)) {
    .polis_write_template(
      "gitignore",
      file.path(root, ".gitignore"),
      subs,
      overwrite
    )
  }

  if (!isTRUE(quiet)) {
    cli::cli_alert_success(
      "Pipeline project ready at {.file {root}} ({length(dirs)} director{?y/ies})."
    )
    cli::cli_text(
      "Next: drop the WHO polio GDB layers in \\
      {.file 01_data/1a_shapefiles/raw}, then source \\
      {.file 02_scripts/2a_download_data.R} and {.file 2b_process_data.R}."
    )
    if (do_renv) {
      cli::cli_text(
        "renv is set up: install your packages, then {.run renv::snapshot()} to \\
        pin them (collaborators reproduce with {.run renv::restore()})."
      )
    }
  }
  invisible(list(root = root, dirs = file.path(root, dirs)))
}

# The directory set for the chosen 01_data domains, plus the always-present
# 02_scripts / 03_outputs. Paths are relative to the project root.
#' @noRd
.polis_pipeline_dirs <- function(domains) {
  d <- c("02_scripts", "03_outputs/plots", "03_outputs/tables")
  if ("shapefiles" %in% domains) {
    d <- c(d, "01_data/1a_shapefiles/raw", "01_data/1a_shapefiles/processed")
  }
  if ("population" %in% domains) {
    d <- c(
      d,
      "01_data/1b_population/worldpop/raw/all",
      "01_data/1b_population/worldpop/raw/u5",
      "01_data/1b_population/worldpop/raw/u15",
      "01_data/1b_population/worldpop/processed",
      "01_data/1b_population/polis_pop/raw",
      "01_data/1b_population/polis_pop/processed"
    )
  }
  if ("polis" %in% domains) {
    d <- c(
      d,
      "01_data/1c_polis/raw",
      "01_data/1c_polis/processed/data",
      "01_data/1c_polis/processed/checks",
      "01_data/1c_polis/processed/cache"
    )
  }
  if ("vaccination" %in% domains) {
    d <- c(d, "01_data/1d_vaccination/raw", "01_data/1d_vaccination/processed")
  }
  d
}

# Read a packaged template, substitute {{TOKEN}} placeholders, write it out.
# Existing files are left untouched unless `overwrite = TRUE`, so re-scaffolding
# a live project never clobbers an edited .Rprofile / script.
#' @noRd
.polis_write_template <- function(name, dest, subs, overwrite = FALSE) {
  if (file.exists(dest) && !isTRUE(overwrite)) {
    cli::cli_alert_info(
      "{.file {basename(dest)}} exists -- keeping it (use {.code overwrite = TRUE})."
    )
    return(invisible(dest))
  }
  src <- system.file("templates", name, package = "polished")
  if (!nzchar(src) || !file.exists(src)) {
    cli::cli_abort("Template {.val {name}} not found in the installed package.")
  }
  txt <- readLines(src, warn = FALSE)
  for (k in names(subs)) {
    txt <- gsub(paste0("{{", k, "}}"), subs[[k]], txt, fixed = TRUE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, dest)
  invisible(dest)
}
