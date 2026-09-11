# A scope manifest separates completed snapshots from resumable partial pulls.
# It contains no credentials. Unknown legacy scopes must be fetched once again.
.polis_download_scope <- function(
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  ext
) {
  no_date <- is.na(date_field) || !nzchar(date_field)
  list(
    endpoint = endpoint,
    date_field = date_field,
    min_year = if (no_date) NULL else format(as.Date(min_date), "%Y"),
    max_year = if (no_date) NULL else format(as.Date(max_date), "%Y"),
    region = if (endpoint %in% c("LabSpecimen", "Im", "Population")) NULL else
      toupper(region),
    country_code = if (is.null(country_code) || !nzchar(country_code)) NULL else
      toupper(country_code),
    ext = ext
  )
}

.polis_download_manifest <- function(data_dir, stem, ext) {
  file.path(data_dir, ".manifests", paste0(stem, ".", ext, ".rds"))
}

.polis_save_manifest <- function(
  path,
  scope,
  out_file = NULL,
  versions = NULL
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  state <- list(
    version = 1L,
    scope = scope,
    complete = !is.null(out_file),
    file = if (!is.null(out_file)) .polis_part_signature(out_file) else NULL,
    fetched_at = Sys.time(),
    versions = versions
  )
  .polis_io_write_atomic(state, path, "rds")
  invisible(state)
}

.polis_record_versions <- function(df, date_field) {
  if (!is.data.frame(df) || !"Id" %in% names(df)) return(NULL)
  if (!nrow(df)) return(data.frame(Id = numeric(), revision = character()))
  if (!date_field %in% names(df)) return(NULL)
  out <- data.frame(
    Id = as.numeric(df$Id),
    revision = as.character(df[[date_field]])
  )
  if (any(!is.finite(out$Id)) || anyDuplicated(out$Id) || anyNA(out$revision))
    return(NULL)
  out <- out[order(out$Id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.polis_fetch_versions <- function(
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  polis_api_key
) {
  pages <- list()
  last_id <- NULL
  use_select <- TRUE
  repeat {
    page <- .polis_fetch_id_page(
      endpoint,
      date_field,
      min_date,
      max_date,
      region,
      country_code,
      polis_api_key,
      last_id = last_id,
      select = if (use_select) c("Id", date_field) else NULL
    )
    if (!is.data.frame(page)) cli::cli_abort("Invalid version-list response.")
    if (
      is.null(last_id) &&
        use_select &&
        (!nrow(page) || !all(c("Id", date_field) %in% names(page)))
    ) {
      use_select <- FALSE
      next
    }
    if (!nrow(page)) break
    versions <- .polis_record_versions(page, date_field)
    if (
      is.null(versions) || (!is.null(last_id) && any(versions$Id <= last_id))
    ) {
      cli::cli_abort(
        "Invalid or stalled version-list cursor; previous snapshot retained."
      )
    }
    pages[[length(pages) + 1L]] <- versions
    last_id <- max(versions$Id)
  }
  if (!length(pages)) return(data.frame(Id = numeric(), revision = character()))
  dplyr::bind_rows(pages)
}

# Refresh only changed rows, including replacements at an unchanged total count.
# Removals and rows leaving the requested scope are reconciled by the full Id list.
.polis_refresh_snapshot <- function(
  state,
  out_file,
  ext,
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  polis_api_key,
  workers
) {
  current <- .polis_fetch_versions(
    endpoint,
    date_field,
    min_date,
    max_date,
    region,
    country_code,
    polis_api_key
  )
  if (identical(state$versions, current)) return(current)
  downloaded <- .polis_io_read(out_file, ext)
  old <- .polis_record_versions(downloaded, date_field)
  if (is.null(old))
    cli::cli_abort(
      "Cached rows have no usable revision metadata; use force = TRUE."
    )
  pos <- match(current$Id, old$Id)
  changed <- is.na(pos) | current$revision != old$revision[pos]
  ids <- current$Id[changed]
  replacement <- if (length(ids))
    .polis_refetch_missing(endpoint, ids, polis_api_key, workers = workers) else
    downloaded[0, , drop = FALSE]
  if (length(ids)) {
    versions <- .polis_record_versions(replacement, date_field)
    expected <- current[changed, , drop = FALSE]
    rownames(expected) <- NULL
    if (is.null(versions) || !identical(versions, expected)) {
      cli::cli_abort(
        "Refetched rows do not match the requested revisions; previous snapshot retained. Retry the download."
      )
    }
  }
  combined <- dplyr::bind_rows(
    downloaded[downloaded$Id %in% setdiff(current$Id, ids), , drop = FALSE],
    replacement
  )
  combined <- .polis_dedup(combined, "Id", date_field)
  .polis_io_write_atomic(combined, out_file, ext)
  current
}
