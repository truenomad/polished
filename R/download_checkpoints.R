# Each fetched page is immutable. The journal is the commit marker: an orphan
# page left by an interrupted journal write is ignored and fetched again.
.polis_journal_dir <- function(part_file) paste0(part_file, ".pages")

.polis_journal_path <- function(part_file) {
  file.path(.polis_journal_dir(part_file), "state.rds")
}

.polis_read_journal <- function(part_file) {
  path <- .polis_journal_path(part_file)
  if (!file.exists(path)) return(NULL)
  state <- readRDS(path)
  if (
    !is.list(state) ||
      !identical(state$version, 1L) ||
      !is.character(state$pages) ||
      !is.list(state$meta) ||
      !is.numeric(state$meta$n_rows) ||
      length(state$meta$n_rows) != 1L ||
      is.na(state$meta$n_rows) ||
      state$meta$n_rows < 0 ||
      any(basename(state$pages) != state$pages) ||
      !all(file.exists(file.path(dirname(path), state$pages)))
  ) {
    cli::cli_abort("Invalid download checkpoint journal {.file {path}}.")
  }
  state
}

.polis_checkpoint_read <- function(part_file, ext, date_field) {
  state <- .polis_read_journal(part_file)
  frames <- if (file.exists(part_file))
    list(.polis_io_read(part_file, ext)) else list()
  if (!is.null(state)) {
    pages <- file.path(.polis_journal_dir(part_file), state$pages)
    frames <- c(frames, lapply(pages, .polis_io_read, fmt = ext))
  }
  if (!length(frames)) return(data.frame(Id = numeric()))
  .polis_dedup(dplyr::bind_rows(frames), id_col = "Id", date_col = date_field)
}

.polis_checkpoint_files <- function(parts_dir, ext) {
  files <- list.files(parts_dir, full.names = TRUE)
  plain <- files[grepl(paste0("^year_[0-9]+\\.", ext, "$"), basename(files))]
  journals <- files[grepl(
    paste0("^year_[0-9]+\\.", ext, "\\.pages$"),
    basename(files)
  )]
  sort(unique(c(plain, sub("\\.pages$", "", journals))))
}

.polis_fetch_year_worker <- function(spec, on_batch = NULL) {
  part <- spec$part_file
  state <- .polis_read_journal(part)
  if (is.null(state)) {
    base <- if (file.exists(part)) {
      tryCatch(.polis_io_read(part, spec$ext), error = function(e) {
        if (!file.rename(part, paste0(part, ".corrupt.", Sys.getpid()))) {
          cli::cli_abort(
            "Could not quarantine unreadable checkpoint {.file {part}}."
          )
        }
        unlink(.polis_meta_path(part))
        NULL
      })
    } else {
      NULL
    }
    meta <- .polis_compute_part_meta(base, spec$date_field)
    state <- list(
      version = 1L,
      pages = character(),
      meta = meta,
      finished = FALSE
    )
    rm(base)
    dir.create(.polis_journal_dir(part), recursive = TRUE, showWarnings = FALSE)
    .polis_io_write_atomic(state, .polis_journal_path(part), "rds")
  }
  new_rows <- 0L
  last_id <- if (is.finite(state$meta$max_id)) state$meta$max_id else NULL
  while (!isTRUE(state$finished)) {
    page <- .polis_fetch_id_page(
      endpoint = spec$endpoint,
      date_field = spec$date_field,
      min_date = sprintf("%d-01-01", spec$year),
      max_date = sprintf("%d-12-31", spec$year),
      region = spec$region,
      country_code = spec$country_code,
      polis_api_key = spec$polis_api_key,
      last_id = last_id,
      page_size = spec$page_size
    )
    if (!is.data.frame(page))
      cli::cli_abort("Invalid response for year {spec$year}.")
    if (!nrow(page)) {
      state$finished <- TRUE
      .polis_io_write_atomic(state, .polis_journal_path(part), "rds")
      break
    }
    if (!"Id" %in% names(page)) {
      cli::cli_abort(
        "Page for year {spec$year} has no Id column; checkpoint retained."
      )
    }
    ids <- suppressWarnings(as.numeric(page$Id))
    if (
      any(!is.finite(ids)) ||
        anyDuplicated(ids) ||
        (!is.null(last_id) && any(ids <= last_id))
    ) {
      cli::cli_abort(
        "Invalid or stalled Id cursor for year {spec$year}; checkpoint retained."
      )
    }
    name <- sprintf("page_%08d.%s", length(state$pages) + 1L, spec$ext)
    .polis_io_write_atomic(
      page,
      file.path(.polis_journal_dir(part), name),
      spec$ext
    )
    batch <- .polis_compute_part_meta(page, spec$date_field)
    state$pages <- c(state$pages, name)
    state$meta$n_rows <- state$meta$n_rows + nrow(page)
    state$meta$min_id <- min(c(state$meta$min_id, ids), na.rm = TRUE)
    state$meta$max_id <- max(ids)
    dates <- c(state$meta$max_date, batch$max_date)
    state$meta$max_date <- if (all(is.na(dates))) as.Date(NA) else
      max(dates, na.rm = TRUE)
    state$meta$saved_at <- Sys.time()
    .polis_io_write_atomic(state, .polis_journal_path(part), "rds")
    last_id <- state$meta$max_id
    new_rows <- new_rows + nrow(page)
    if (!is.null(on_batch)) {
      on_batch(
        rows_in_batch = nrow(page),
        cumulative_in_year = state$meta$n_rows,
        year = spec$year,
        last_id = last_id
      )
    }
  }
  # The finished journal remains until compaction commits. Replaying it after
  # interruption is safe even if the part rename already succeeded: dedup by Id.
  if (length(state$pages) || !file.exists(part)) {
    combined <- .polis_checkpoint_read(part, spec$ext, spec$date_field)
    .polis_io_write_part(combined, part, spec$ext, spec$date_field)
  }
  rows <- state$meta$n_rows
  unlink(.polis_journal_dir(part), recursive = TRUE)
  list(year = spec$year, rows = rows, new_rows = new_rows, path = part)
}
