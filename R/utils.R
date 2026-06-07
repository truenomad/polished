# Internal helpers for get_polis_data().
#
# None are exported. Grouped by role:
#   - I/O + housekeeping
#   - HTTP primitives
#   - Per-year worker + on-disk caching
#   - Parallel dispatch

# ---------------------------------------------------------------------
# I/O + housekeeping
# ---------------------------------------------------------------------

# get_polis_data only writes rds/rda/csv/parquet/qs2.
.polis_check_format <- function(fmt) {
  allowed <- c("rds", "rda", "csv", "parquet", "qs2")
  if (!fmt %in% allowed) {
    cli::cli_abort(c(
      "x" = "Unsupported {.arg output_format}: {.val {fmt}}.",
      "i" = paste(
        "Supported formats:",
        paste(allowed, collapse = ", ")
      )
    ))
  }
}

.polis_io_read <- function(path, fmt) {
  .polis_check_format(fmt)
  if (
    identical(fmt, "parquet") &&
      !requireNamespace("arrow", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Package {.pkg arrow} must be installed to read parquet files."
    )
  }
  if (
    identical(fmt, "qs2") &&
      !requireNamespace("qs2", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Package {.pkg qs2} must be installed to read qs2 files."
    )
  }
  switch(
    fmt,
    rds = readRDS(path),
    rda = {
      e <- new.env(parent = emptyenv())
      load(path, envir = e)
      nms <- ls(e)
      if ("polis_data" %in% nms) {
        get("polis_data", envir = e)
      } else {
        get(nms[1], envir = e)
      }
    },
    csv = utils::read.csv(path, stringsAsFactors = FALSE),
    parquet = arrow::read_parquet(path),
    qs2 = qs2::qs_read(path)
  )
}

.polis_io_write <- function(x, path, fmt) {
  .polis_check_format(fmt)
  if (
    identical(fmt, "parquet") &&
      !requireNamespace("arrow", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Package {.pkg arrow} must be installed to write parquet files."
    )
  }
  if (
    identical(fmt, "qs2") &&
      !requireNamespace("qs2", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Package {.pkg qs2} must be installed to write qs2 files."
    )
  }
  switch(
    fmt,
    rds = saveRDS(x, path),
    rda = {
      polis_data <- as.data.frame(x, stringsAsFactors = FALSE)
      save(polis_data, file = path)
    },
    csv = utils::write.csv(x, path, row.names = FALSE),
    parquet = arrow::write_parquet(x, path),
    qs2 = qs2::qs_save(x, path)
  )
}

# ---------------------------------------------------------------------
# Part metadata sidecars
# ---------------------------------------------------------------------
#
# Each year part `year_YYYY.<ext>` gets a tiny companion file
# `year_YYYY.meta.rds` that caches:
#   $n_rows      -- for current_rows / verification fast paths
#   $min_id      -- for cross-year overlap detection at merge time
#   $max_id      -- same
#   $max_date    -- date_field's max (useful for "newer than" checks)
#   $saved_at    -- when the sidecar was last refreshed
#
# Reads with lazy backfill: if the sidecar is missing but the part
# exists, we read the part once, write the sidecar, and return it.
# Subsequent reads are cheap.

.polis_meta_path <- function(part_file) {
  paste0(tools::file_path_sans_ext(part_file), ".meta.rds")
}

.polis_compute_part_meta <- function(df, date_field = NULL) {
  has_id <- is.data.frame(df) && nrow(df) > 0L && "Id" %in% names(df)
  has_dt <- is.data.frame(df) &&
    nrow(df) > 0L &&
    !is.null(date_field) &&
    date_field %in% names(df)
  list(
    n_rows = if (is.data.frame(df)) as.integer(nrow(df)) else 0L,
    min_id = if (has_id) {
      suppressWarnings(min(df$Id, na.rm = TRUE))
    } else NA_real_,
    max_id = if (has_id) {
      suppressWarnings(max(df$Id, na.rm = TRUE))
    } else NA_real_,
    max_date = if (has_dt) {
      suppressWarnings(max(as.Date(df[[date_field]]), na.rm = TRUE))
    } else as.Date(NA),
    saved_at = Sys.time()
  )
}

.polis_empty_meta <- function() {
  list(
    n_rows = 0L,
    min_id = NA_real_,
    max_id = NA_real_,
    max_date = as.Date(NA),
    saved_at = as.POSIXct(NA)
  )
}

# Write a part + its meta sidecar in one call. Meta failures are
# non-fatal (we'd rather lose the cache than the data).
.polis_io_write_part <- function(df, part_file, ext, date_field) {
  pid <- Sys.getpid()
  part_tmp <- paste0(part_file, ".tmp.", pid)
  meta_file <- .polis_meta_path(part_file)
  meta_tmp <- paste0(meta_file, ".tmp.", pid)

  # Step (a): write the part to a per-pid tmp file.
  tryCatch(
    .polis_io_write(df, part_tmp, ext),
    error = function(e) {
      try(file.remove(part_tmp), silent = TRUE)
      cli::cli_abort(conditionMessage(e), call = conditionCall(e))
    }
  )

  # Step (b): write the meta to a per-pid tmp file. Meta is best-effort.
  meta_ok <- tryCatch(
    {
      saveRDS(.polis_compute_part_meta(df, date_field), meta_tmp)
      TRUE
    },
    error = function(e) {
      try(file.remove(meta_tmp), silent = TRUE)
      FALSE
    }
  )

  # Step (c): rename tmp files into final position (POSIX-atomic).
  tryCatch(
    file.rename(part_tmp, part_file),
    error = function(e) {
      try(file.remove(part_tmp), silent = TRUE)
      try(file.remove(meta_tmp), silent = TRUE)
      cli::cli_abort(conditionMessage(e), call = conditionCall(e))
    }
  )
  if (isTRUE(meta_ok)) {
    tryCatch(
      file.rename(meta_tmp, meta_file),
      error = function(e) {
        try(file.remove(meta_tmp), silent = TRUE)
      }
    )
  }
  invisible()
}

# Read the meta sidecar. Backfills (reads the part once) when the
# sidecar is missing or unreadable. Returns an empty-meta list when
# both the sidecar and part are absent.
.polis_read_meta <- function(part_file, ext, date_field) {
  meta_file <- .polis_meta_path(part_file)
  if (file.exists(meta_file)) {
    meta <- tryCatch(readRDS(meta_file), error = function(e) NULL)
    if (
      is.list(meta) &&
        all(
          c("n_rows", "min_id", "max_id") %in%
            names(meta)
        )
    ) {
      return(meta)
    }
  }
  if (!file.exists(part_file)) {
    return(.polis_empty_meta())
  }
  # Lazy backfill: read once, write sidecar.
  df <- tryCatch(.polis_io_read(part_file, ext), error = function(e) NULL)
  if (is.null(df)) return(.polis_empty_meta())
  meta <- .polis_compute_part_meta(df, date_field)
  tryCatch(saveRDS(meta, meta_file), error = function(e) invisible())
  meta
}

# True iff any pair of (min_id, max_id) intervals across the metas
# overlap. Used by .polis_merge_parts to decide whether to dedup.
.polis_id_ranges_overlap <- function(metas) {
  ranges <- lapply(metas, function(m) {
    if (is.null(m) || is.na(m$min_id) || is.na(m$max_id)) {
      NULL
    } else {
      c(as.numeric(m$min_id), as.numeric(m$max_id))
    }
  })
  ranges <- ranges[!vapply(ranges, is.null, logical(1))]
  if (length(ranges) < 2L) return(FALSE)
  starts <- vapply(ranges, `[`, numeric(1), 1L)
  ends <- vapply(ranges, `[`, numeric(1), 2L)
  ord <- order(starts)
  starts <- starts[ord]
  ends <- ends[ord]
  for (i in seq.int(2L, length(starts))) {
    if (starts[i] <= ends[i - 1L]) return(TRUE)
  }
  FALSE
}

# Format an integer with thousands separators for display in cli bars.
# e.g. 1991076 -> "1,991,076".
.polis_pretty_num <- function(x) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    return("0")
  }
  formatC(as.numeric(x), format = "d", big.mark = ",")
}

# Clamp a progress-bar "set" value to the bar's total. POLIS's
# @odata.count can under-report the actual fetched row count (new
# rows land between the count query and the fetch), and cli's bar
# crashes with "invalid 'times' argument" when set > total.
.polis_pb_set <- function(value, total) {
  if (is.null(total) || is.na(total)) return(value)
  min(as.integer(value), as.integer(total))
}

# Dedup-keep-latest by Id. Uses data.table when available (a lot
# faster on million-row tables) and falls back to base R's order +
# duplicated when data.table isn't installed. Either way the
# semantics are identical: for each Id, keep the row with the highest
# `date_col` value (NAs sort last).
.polis_dedup <- function(df, id_col = "Id", date_col) {
  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(df)
  }
  if (!id_col %in% names(df)) {
    return(df[!duplicated(df), , drop = FALSE])
  }

  use_dt <- requireNamespace("data.table", quietly = TRUE)

  if (use_dt) {
    dt <- data.table::as.data.table(df)
    if (date_col %in% names(dt)) {
      data.table::setorderv(
        dt,
        c(id_col, date_col),
        c(1L, -1L),
        na.last = TRUE
      )
    } else {
      data.table::setorderv(dt, id_col)
    }
    out <- dt[!duplicated(dt[[id_col]]), ]
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }

  if (!date_col %in% names(df)) {
    return(df[!duplicated(df[[id_col]]), , drop = FALSE])
  }
  ord <- order(df[[date_col]], decreasing = TRUE, na.last = TRUE)
  df <- df[ord, , drop = FALSE]
  df[!duplicated(df[[id_col]]), , drop = FALSE]
}

# Archive the freshly-saved file with a timestamp under data/archive/
# and prune older copies of the same table beyond keep_n.
.polis_archive <- function(
  out_file,
  polis_folder,
  table_name,
  fmt,
  keep_n
) {
  if (keep_n <= 0L) {
    return(invisible())
  }
  arc_dir <- file.path(polis_folder, "data", "archive")
  dir.create(arc_dir, showWarnings = FALSE, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  arc_file <- file.path(
    arc_dir,
    sprintf("%s_%s.%s", table_name, ts, fmt)
  )
  file.copy(out_file, arc_file, overwrite = TRUE)

  pattern <- sprintf(
    "^%s_\\d{8}_\\d{6}\\.%s$",
    table_name,
    fmt
  )
  existing <- list.files(arc_dir, pattern = pattern, full.names = TRUE)
  if (length(existing) > keep_n) {
    sorted <- existing[order(file.info(existing)$mtime, decreasing = TRUE)]
    unlink(sorted[seq.int(keep_n + 1L, length(sorted))])
  }
  invisible()
}

# Append one row to the structured per-batch log (rds).
.polis_log_window <- function(log_file, row) {
  if (is.null(log_file)) {
    return(invisible())
  }
  existing <- if (file.exists(log_file)) {
    tryCatch(readRDS(log_file), error = function(e) data.frame())
  } else {
    data.frame()
  }
  saveRDS(dplyr::bind_rows(existing, row), log_file)
  invisible()
}

# ---------------------------------------------------------------------
# HTTP primitives
# ---------------------------------------------------------------------

# Build a year-aligned OData $filter clause. POLIS only honours
# date filters whose `le` bound falls on YYYY-12-31, so always align
# min_date to Jan 1 and max_date to Dec 31.
.polis_build_id_filter <- function(
  date_field,
  min_date,
  max_date,
  endpoint,
  region,
  country_code,
  last_id = NULL
) {
  year_lo <- as.integer(format(as.Date(min_date), "%Y"))
  year_hi <- as.integer(format(as.Date(max_date), "%Y"))
  parts <- c(
    sprintf("%s ge %d-01-01", date_field, year_lo),
    sprintf("%s le %d-12-31", date_field, year_hi)
  )
  if (
    isTRUE(length(region) == 1L) &&
      isTRUE(nzchar(region)) &&
      tolower(region) != "global" &&
      !(endpoint %in% c("LabSpecimen", "Im"))
  ) {
    region_field <- if (endpoint == "Virus") "RegionName" else "WHORegion"
    parts <- c(parts, sprintf("%s eq '%s'", region_field, region))
  }
  if (isTRUE(length(country_code) == 1L) && isTRUE(nzchar(country_code))) {
    parts <- c(parts, sprintf("CountryISO3Code eq '%s'", country_code))
  }
  if (!is.null(last_id)) {
    parts <- c(
      parts,
      sprintf("Id gt %s", format(last_id, scientific = FALSE))
    )
  }
  paste(parts, collapse = " and ")
}

# Single GET with retry + gzip. Returns the parsed JSON body.
.polis_get_body <- function(
  url,
  polis_api_key,
  max_attempts = 5L,
  timeout_seconds = 120L
) {
  resp <- httr2::request(url) |>
    httr2::req_headers(`authorization-token` = polis_api_key) |>
    httr2::req_options(accept_encoding = "gzip") |>
    httr2::req_retry(max_tries = max_attempts) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_perform()
  httr2::resp_body_json(resp)
}

# Ask POLIS how many rows match a year-aligned filter. Returns
# numeric or NA.
.polis_get_count <- function(
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  polis_api_key
) {
  flt <- .polis_build_id_filter(
    date_field,
    min_date,
    max_date,
    endpoint,
    region,
    country_code
  )
  url <- paste0(
    "https://extranet.who.int/polis/api/v2/",
    endpoint,
    "?$filter=",
    utils::URLencode(flt),
    "&$count=true&$top=0"
  )
  body <- .polis_get_body(url, polis_api_key)
  raw <- body[["@odata.count"]]
  if (is.null(raw) || length(raw) == 0L) return(NA_real_)
  suppressWarnings(as.numeric(raw))
}

# Fetch one page of records with `Id gt last_id`. Returns a data.frame
# (0 rows at end-of-data).
.polis_fetch_id_page <- function(
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  polis_api_key,
  last_id,
  page_size = 2000L,
  select = NULL
) {
  flt <- .polis_build_id_filter(
    date_field,
    min_date,
    max_date,
    endpoint,
    region,
    country_code,
    last_id = last_id
  )
  select_q <- if (!is.null(select) && length(select) > 0L) {
    paste0("&$select=", paste(select, collapse = ","))
  } else {
    ""
  }
  url <- paste0(
    "https://extranet.who.int/polis/api/v2/",
    endpoint,
    "?$filter=",
    utils::URLencode(flt),
    "&$orderby=Id&$top=",
    as.integer(page_size),
    select_q
  )
  body <- .polis_get_body(url, polis_api_key)
  if (length(body$value) == 0L) {
    return(data.frame())
  }
  dplyr::bind_rows(body$value)
}

# Page through all matching rows using Id-range pagination. Returns
# the integer Id vector. Used by the verification step.
.polis_fetch_id_list <- function(
  endpoint,
  date_field,
  min_date,
  max_date,
  region,
  country_code,
  polis_api_key,
  page_size = 2000L
) {
  ids <- list()
  last_id <- 0L
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
      page_size = page_size,
      select = if (use_select) "Id" else NULL
    )
    if (!is.data.frame(page) || nrow(page) == 0L) {
      if (use_select && length(ids) == 0L) {
        # Endpoint may be silently rejecting $select (POLIS does
        # this on Virus and Case). Retry once without it.
        use_select <- FALSE
        next
      }
      break
    }
    if (!"Id" %in% names(page)) break
    ids[[length(ids) + 1L]] <- page$Id
    last_id <- max(page$Id, na.rm = TRUE)
    # Do NOT break on `nrow(page) < page_size`. POLIS sometimes
    # truncates mid-query (gateway load, query timeout) and returns
    # a partial page even when more rows exist for `Id gt last_id`.
    # The empty-page check above is the correct termination signal.
  }
  if (length(ids) == 0L) integer(0) else unlist(ids, use.names = FALSE)
}

# Refetch one chunk of Ids via OData `Id in (...)`. Pulled out so it
# can run inside a PSOCK worker.
.polis_refetch_chunk <- function(
  chunk,
  endpoint,
  polis_api_key,
  max_attempts = 3L
) {
  id_list <- paste(format(chunk, scientific = FALSE), collapse = ",")
  flt <- sprintf("Id in (%s)", id_list)
  url <- paste0(
    "https://extranet.who.int/polis/api/v2/",
    endpoint,
    "?$filter=",
    utils::URLencode(flt),
    "&$top=",
    length(chunk)
  )
  body <- .polis_get_body(
    url,
    polis_api_key,
    max_attempts = max_attempts
  )
  dplyr::bind_rows(body$value)
}

# Refetch a specific set of Id values in small OData `in (...)` chunks.
# Sequential when `workers <= 1`; otherwise fans chunks across a
# small PSOCK cluster (one fresh subprocess per worker -- same
# isolation pattern as the main downloader).
.polis_refetch_missing <- function(
  endpoint,
  ids,
  polis_api_key,
  chunk_size = 25L,
  max_attempts = 3L,
  workers = 1L
) {
  if (length(ids) == 0L) {
    return(data.frame())
  }
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))

  workers <- as.integer(workers)
  use_parallel <- isTRUE(workers > 1L) && length(chunks) > 1L

  if (!use_parallel) {
    out <- lapply(chunks, function(chunk) {
      .polis_refetch_chunk(
        chunk,
        endpoint,
        polis_api_key,
        max_attempts
      )
    })
    return(dplyr::bind_rows(out))
  }

  cl <- parallel::makePSOCKcluster(min(workers, length(chunks)))
  worker_pids <- .polis_cluster_pids(cl)
  cluster_stopped <- FALSE
  on.exit(
    if (!cluster_stopped) .polis_stop_cluster(cl, worker_pids),
    add = TRUE
  )
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(httr2)
      library(dplyr)
      library(polished)
    })
    NULL
  })

  worker_fn <- function(chunk, endpoint, polis_api_key, max_attempts) {
    fn <- utils::getFromNamespace(
      ".polis_refetch_chunk",
      "polished"
    )
    fn(chunk, endpoint, polis_api_key, max_attempts)
  }

  out <- parallel::parLapplyLB(
    cl,
    chunks,
    worker_fn,
    endpoint = endpoint,
    polis_api_key = polis_api_key,
    max_attempts = max_attempts
  )
  .polis_stop_cluster(cl, worker_pids)
  cluster_stopped <- TRUE
  dplyr::bind_rows(out)
}

# ---------------------------------------------------------------------
# Per-year worker + on-disk caching
# ---------------------------------------------------------------------

# Fetch all rows for one calendar year via Id-range pagination and
# write them to a part file. The part is flushed after every batch so
# a crash loses at most the in-flight 2K-row request.
.polis_fetch_year_worker <- function(spec, on_batch = NULL) {
  year <- spec$year
  min_date <- sprintf("%d-01-01", year)
  max_date <- sprintf("%d-12-31", year)

  existing <- NULL
  last_id <- NULL
  if (file.exists(spec$part_file)) {
    existing <- tryCatch(
      .polis_io_read(spec$part_file, spec$ext),
      error = function(e) {
        qpath <- paste0(spec$part_file, ".corrupt.", Sys.getpid())
        try(file.rename(spec$part_file, qpath), silent = TRUE)
        mpath <- .polis_meta_path(spec$part_file)
        if (file.exists(mpath)) {
          try(
            file.rename(mpath, paste0(mpath, ".corrupt.", Sys.getpid())),
            silent = TRUE
          )
        }
        NULL
      }
    )
    if (
      is.data.frame(existing) &&
        nrow(existing) > 0L &&
        "Id" %in% names(existing)
    ) {
      max_existing <- suppressWarnings(max(existing$Id, na.rm = TRUE))
      if (is.finite(max_existing)) last_id <- max_existing
    }
  }

  cum_new <- 0L
  page_size <- spec$page_size

  repeat {
    new_data <- .polis_fetch_id_page(
      endpoint = spec$endpoint,
      date_field = spec$date_field,
      min_date = min_date,
      max_date = max_date,
      region = spec$region,
      country_code = spec$country_code,
      polis_api_key = spec$polis_api_key,
      last_id = last_id,
      page_size = page_size
    )

    if (!is.data.frame(new_data) || nrow(new_data) == 0L) break

    # POLIS occasionally returns a page with no Id column for very old
    # records (or after a server-side error). Without Id we can't
    # advance the cursor, so treat it as end-of-data.
    if (!"Id" %in% names(new_data)) break
    new_last_id <- suppressWarnings(
      max(as.numeric(new_data$Id), na.rm = TRUE)
    )
    if (
      !is.finite(new_last_id) ||
        (!is.null(last_id) && new_last_id <= last_id)
    ) {
      cli::cli_alert_warning(paste0(
        "year ",
        year,
        ": cursor stalled at Id ",
        last_id,
        " (page returned no Id > last_id); ending year early"
      ))
      break
    }

    existing <- if (is.data.frame(existing)) {
      dplyr::bind_rows(existing, new_data)
    } else {
      new_data
    }

    last_id <- new_last_id
    cum_new <- cum_new + nrow(new_data)

    .polis_io_write_part(
      existing,
      spec$part_file,
      spec$ext,
      spec$date_field
    )

    if (!is.null(on_batch)) {
      on_batch(
        rows_in_batch = nrow(new_data),
        cumulative_in_year = nrow(existing),
        year = year,
        last_id = last_id
      )
    }

    # Do NOT break on `nrow(new_data) < page_size`. POLIS sometimes
    # truncates a query under load and returns a partial page even
    # though more rows exist for `Id gt last_id`. The empty-page
    # check above is the correct termination signal.
  }

  list(
    year = year,
    rows = if (is.data.frame(existing)) nrow(existing) else 0L,
    new_rows = cum_new,
    path = spec$part_file
  )
}

# One-time migration: when an older run left a single canonical file
# on disk with no per-year parts, split it into parts so the workers
# can resume per-year. Tolerant of a corrupt/truncated input.
.polis_migrate_to_parts <- function(out_file, parts_dir, ext, date_field) {
  if (dir.exists(parts_dir)) return(invisible())
  if (!file.exists(out_file)) return(invisible())

  df <- tryCatch(
    .polis_io_read(out_file, ext),
    error = function(e) {
      cli::cli_alert_warning(c(
        paste0(
          "Existing ",
          out_file,
          " is unreadable (",
          conditionMessage(e),
          ")."
        ),
        i = paste0(
          "Removing the corrupt file and starting a fresh pull. ",
          "No resume marker available."
        )
      ))
      try(file.remove(out_file), silent = TRUE)
      NULL
    }
  )

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
    return(invisible())
  }
  if (!date_field %in% names(df)) return(invisible())

  yrs <- as.integer(format(as.Date(df[[date_field]]), "%Y"))
  keep <- !is.na(yrs)
  if (!any(keep)) return(invisible())
  df <- df[keep, , drop = FALSE]
  yrs <- yrs[keep]

  dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)
  for (yr in unique(yrs)) {
    part_df <- df[yrs == yr, , drop = FALSE]
    part_file <- file.path(parts_dir, sprintf("year_%d.%s", yr, ext))
    .polis_io_write_part(part_df, part_file, ext, date_field)
  }
  invisible()
}

# Bind all part files into the canonical file, then dedup by Id keeping the
# latest update date. This is unconditional (G3): a single keep-latest rule
# governs recency, so a duplicate can never slip through -- even when a record's
# update date crosses a year boundary between runs and the per-year Id ranges
# overlap. (.polis_id_ranges_overlap / .polis_read_meta remain available for
# diagnostics and are exercised by the test suite.)
.polis_merge_parts <- function(parts_dir, out_file, ext, date_field) {
  if (!dir.exists(parts_dir)) {
    return(invisible(0L))
  }
  part_files <- list.files(
    parts_dir,
    pattern = paste0("^year_\\d+\\.", ext, "$"),
    full.names = TRUE
  )
  if (length(part_files) == 0L) {
    return(invisible(0L))
  }

  dfs <- lapply(part_files, function(f) {
    tryCatch(
      .polis_io_read(f, ext),
      error = function(e) {
        cli::cli_alert_warning(paste0(
          "Skipping unreadable part ",
          f,
          " (",
          conditionMessage(e),
          ")"
        ))
        data.frame()
      }
    )
  })
  combined <- .polis_dedup(
    dplyr::bind_rows(dfs),
    id_col = "Id",
    date_col = date_field
  )

  .polis_io_write(combined, out_file, ext)
  invisible(nrow(combined))
}

# ---------------------------------------------------------------------
# Parallel dispatch
# ---------------------------------------------------------------------

# Capture worker R-session PIDs so we can force-kill survivors if the
# clean stopCluster() handshake fails (e.g. a worker still mid-request).
# Round-trip cost is one quick Sys.getpid() per worker -- negligible.
.polis_cluster_pids <- function(cl) {
  tryCatch(
    as.integer(unlist(parallel::clusterCall(cl, Sys.getpid))),
    error = function(e) integer(0)
  )
}

# Belt-and-suspenders cleanup for a PSOCK cluster.
#
# Calls stopCluster() (closes worker sockets -- normally enough for the
# worker R process to exit). On POSIX, then checks each captured PID
# and SIGKILLs any survivor. Warns audibly when force-killing is needed
# or when stopCluster() errors -- the happy path is silent.
#
# On Windows we can't cheaply check liveness (`tools::pskill` with
# signal=0 isn't supported there), so we trust stopCluster().
.polis_stop_cluster <- function(cl, pids = integer(0)) {
  stop_err <- tryCatch(
    {
      parallel::stopCluster(cl)
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  if (
    !is.null(stop_err) &&
      !grepl("invalid connection", stop_err, ignore.case = TRUE)
  ) {
    cli::cli_alert_warning(paste0(
      "parallel::stopCluster() errored: ",
      stop_err,
      if (length(pids) > 0L) "; will try to force-kill workers." else "."
    ))
  }
  if (length(pids) == 0L || .Platform$OS.type == "windows") {
    return(invisible(NULL))
  }
  Sys.sleep(0.2)
  alive <- pids[vapply(
    pids,
    function(p) {
      isTRUE(tryCatch(
        tools::pskill(p, signal = 0L),
        error = function(e) FALSE
      ))
    },
    logical(1)
  )]
  if (length(alive) == 0L) return(invisible(NULL))
  cli::cli_alert_warning(paste0(
    length(alive),
    " worker process(es) still alive after stopCluster; ",
    "sending SIGKILL to PID(s) ",
    paste(alive, collapse = ", "),
    "."
  ))
  for (p in alive) {
    try(tools::pskill(p, signal = tools::SIGKILL), silent = TRUE)
  }
  invisible(NULL)
}

# Dispatch year workers in parallel and drive a single live cli bar.
#
# Uses parallel::makePSOCKcluster, which is the only cluster type
# available on all platforms (Windows, macOS, Linux). FORK clusters
# would be lighter on Unix but break on Windows, so PSOCK is the
# baseline.
#
# Async via parallel's internal sendCall + socketSelect + recvOneResult
# so we can poll the part files between socket reads. Pulled via
# getFromNamespace() to keep R CMD check quiet about :::.
.polis_dispatch_parallel <- function(
  specs,
  workers_actual,
  declared_total,
  current_rows,
  nm,
  ext,
  log_file,
  quiet
) {
  # PSOCK workers run a brand-new R session and need polished
  # installed in their library path. devtools::load_all() doesn't
  # install, so guard against that with a friendly error.
  if (length(find.package("polished", quiet = TRUE)) == 0L) {
    cli::cli_abort(c(
      "x" = "Parallel mode requires {.pkg polished} to be installed.",
      "i" = paste(
        "PSOCK workers spawn fresh R sessions and load polished via",
        "{.code library()}, so a {.code devtools::load_all()} session",
        "won't work."
      ),
      "*" = paste(
        "Install once via {.run devtools::install()} or use",
        "{.code workers = 1L}."
      )
    ))
  }

  parallel_sendCall <- utils::getFromNamespace("sendCall", "parallel")
  parallel_recvOneResult <- utils::getFromNamespace(
    "recvOneResult",
    "parallel"
  )

  n_specs <- length(specs)
  pf_paths <- vapply(specs, function(s) s$part_file, character(1))

  file_rows <- stats::setNames(integer(n_specs), pf_paths)
  file_mtimes <- stats::setNames(numeric(n_specs), pf_paths)
  for (i in seq_len(n_specs)) {
    pf <- pf_paths[i]
    if (file.exists(pf)) {
      file_rows[i] <- tryCatch(
        nrow(.polis_io_read(pf, ext)),
        error = function(e) 0L
      )
      file_mtimes[i] <- as.numeric(file.info(pf)$mtime)
    }
  }
  prev_total <- current_rows

  n_new <- "0"
  n_cum <- .polis_pretty_num(current_rows)
  pb_tot <- if (is.na(declared_total)) "?" else {
    .polis_pretty_num(declared_total)
  }
  pb_cur <- n_cum

  pb_id <- if (!isTRUE(quiet)) {
    cli::cli_progress_bar(
      name = "Downloaded",
      total = if (is.na(declared_total)) {
        NA
      } else {
        as.integer(declared_total)
      },
      format = paste(
        "{cli::pb_spin} {cli::pb_name}",
        "{pb_cur}/{pb_tot}",
        "{cli::pb_bar} {cli::pb_percent}",
        "| ETA {cli::pb_eta}",
        "| +{.strong {n_new}} (cum {.strong {n_cum}})"
      ),
      clear = FALSE
    )
  } else {
    NULL
  }
  if (!is.null(pb_id)) {
    cli::cli_progress_update(
      id = pb_id,
      set = .polis_pb_set(current_rows, declared_total)
    )
  }

  update_progress_from_disk <- function() {
    changed <- FALSE
    for (i in seq_len(n_specs)) {
      pf <- pf_paths[i]
      if (!file.exists(pf)) next
      cur_mtime <- as.numeric(file.info(pf)$mtime)
      if (cur_mtime != file_mtimes[i]) {
        file_mtimes[i] <<- cur_mtime
        file_rows[i] <<- tryCatch(
          nrow(.polis_io_read(pf, ext)),
          error = function(e) file_rows[i]
        )
        changed <- TRUE
      }
    }
    current_total <- sum(file_rows)
    if (current_total > prev_total) {
      delta <- current_total - prev_total
      n_new <<- .polis_pretty_num(delta)
      n_cum <<- .polis_pretty_num(current_total)
      pb_cur <<- n_cum
      if (!is.null(pb_id)) {
        cli::cli_progress_update(
          id = pb_id,
          set = .polis_pb_set(current_total, declared_total)
        )
      }
      .polis_log_window(
        log_file,
        data.frame(
          table = nm,
          year = NA_integer_,
          last_id_after = NA_real_,
          rows_in_window = delta,
          cumulative_rows = current_total,
          elapsed_seconds = NA_real_,
          timestamp = Sys.time(),
          status = "ok",
          stringsAsFactors = FALSE
        )
      )
      prev_total <<- current_total
    }
    invisible(changed)
  }

  cl <- parallel::makePSOCKcluster(workers_actual)
  worker_pids <- .polis_cluster_pids(cl)
  cluster_stopped <- FALSE
  on.exit(
    {
      if (!cluster_stopped) {
        .polis_stop_cluster(cl, worker_pids)
      }
      if (!is.null(pb_id)) {
        try(cli::cli_progress_done(id = pb_id), silent = TRUE)
      }
    },
    add = TRUE
  )
  tryCatch(
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(httr2)
        library(dplyr)
        library(polished)
      })
      NULL
    }),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Could not load {.pkg polished} on parallel workers.",
        "i" = conditionMessage(e),
        "*" = "Try {.code workers = 1L} or reinstall the package."
      ))
    }
  )

  worker_wrapper <- function(spec) {
    tryCatch(
      list(
        ok = TRUE,
        value = utils::getFromNamespace(
          ".polis_fetch_year_worker",
          "polished"
        )(spec)
      ),
      error = function(e) {
        # Preserve where in the worker stack the error fired -- the
        # bare conditionMessage often loses the call site.
        call_str <- tryCatch(
          deparse(conditionCall(e))[1L],
          error = function(...) NA_character_
        )
        list(
          ok = FALSE,
          message = paste0(
            conditionMessage(e),
            if (!is.na(call_str)) paste0(" [in: ", call_str, "]") else ""
          ),
          year = spec$year
        )
      }
    )
  }

  n_workers <- length(cl)
  node_spec <- integer(n_workers)
  remaining <- seq_along(specs)
  all_results <- vector("list", n_specs)
  completed <- 0L

  while (completed < n_specs) {
    for (n in which(node_spec == 0L)) {
      if (length(remaining) == 0L) break
      idx <- remaining[1L]
      remaining <- remaining[-1L]
      parallel_sendCall(
        cl[[n]],
        worker_wrapper,
        list(specs[[idx]]),
        tag = idx
      )
      node_spec[n] <- idx
    }

    busy <- which(node_spec != 0L)
    if (length(busy) == 0L) break
    cons <- lapply(busy, function(n) cl[[n]]$con)
    ready <- tryCatch(
      socketSelect(cons, timeout = 0.5),
      error = function(e) rep(FALSE, length(cons))
    )

    if (any(ready)) {
      result <- parallel_recvOneResult(cl)
      idx <- result$tag
      all_results[[idx]] <- result$value
      completed <- completed + 1L
      node_spec[node_spec == idx] <- 0L
    }

    update_progress_from_disk()
  }

  .polis_stop_cluster(cl, worker_pids)
  cluster_stopped <- TRUE
  update_progress_from_disk()
  if (!is.null(pb_id)) {
    cli::cli_progress_done(id = pb_id)
  }

  result_values <- vector("list", n_specs)
  for (k in seq_along(all_results)) {
    r <- all_results[[k]]
    if (is.list(r) && isTRUE(r$ok)) {
      result_values[[k]] <- r$value
    } else {
      msg <- if (is.list(r) && !is.null(r$message)) {
        r$message
      } else {
        "unknown worker error"
      }
      yr <- if (is.list(r) && !is.null(r$year)) {
        r$year
      } else {
        specs[[k]]$year
      }
      cli::cli_abort(c(
        "x" = paste0(nm, " ", yr, ": worker failed - ", msg),
        "i" = paste0(
          "Part file at ",
          specs[[k]]$part_file,
          " holds progress so far."
        ),
        "*" = paste0(
          "Resume by re-running get_polis_data(",
          "tables = \"",
          nm,
          "\", ...). DO NOT delete the ",
          ".parts/ directory."
        )
      ))
    }
  }
  result_values
}
