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
