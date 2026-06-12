# Package utilities: shared primitives (some exported) and internal helpers.
#
# Grouped by role:
#   - General cross-cutting helpers (used across modules)
#   - Column naming & ordering (exported)
#   - Deduplication & record reconciliation (exported primitives)
#   - get_polis_data(): I/O + housekeeping
#   - get_polis_data(): HTTP primitives
#   - get_polis_data(): per-year worker + on-disk caching
#   - get_polis_data(): parallel dispatch
#
# Domain-specific helpers stay co-located with the code they serve (e.g.
# .afp_*, .spatial_*, .epid_*); only genuinely shared primitives live here.

# ---------------------------------------------------------------------
# General cross-cutting helpers
# ---------------------------------------------------------------------

# Null-coalescing helper (internal; mirrors rlang::`%||%` without the dep).
`%||%` <- function(x, y) if (is.null(x)) y else x

# Content hash for cache keys via xxhash64; digest is an optional dep.
.polis_hash <- function(x) {
  .polis_require("digest", "hash objects for caching")$digest(
    x,
    algo = "xxhash64"
  )
}

# Resolve a path to a packaged extdata file, dev-mode aware.
.polis_extdata_path <- function(file) {
  path <- system.file("extdata", file, package = "polished")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  # development fallback (package not installed)
  dev <- file.path("inst", "extdata", file)
  if (file.exists(dev)) {
    return(dev)
  }
  cli::cli_abort("Could not locate extdata file {.file {file}}.")
}

# Trim whitespace and convert empty strings to NA across character columns.
# POLIS exports often arrive padded or with "" for a true missing value;
# normalising both up front lets every downstream check treat "absent" alike.
# Shared by all cleaners.
.polis_clean_strings <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::where(is.character),
      \(x) dplyr::na_if(trimws(x), "")
    )
  )
}

# Humanise a count for messages: 1991076 -> "1.99M", 142641 -> "142.6K".
.polis_big_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    return("0")
  }
  if (abs(x) >= 1e6) {
    sprintf("%.2fM", x / 1e6)
  } else if (abs(x) >= 1e3) {
    sprintf("%.1fK", x / 1e3)
  } else {
    formatC(x, format = "d", big.mark = ",")
  }
}

# Normalise a GUID to an upper-case, brace-free comparison key ({ABC} -> ABC).
.geo_guid_key <- function(x) toupper(gsub("[{}]", "", x))

# Normalise a GUID to the canonical lower-case, brace-free form used internally
# while matching against the shape.
.geo_guid_canon <- function(x) tolower(gsub("[{}]", "", x))

# Format a GUID for OUTPUT in the raw POLIS / shapefile form: brace-wrapped and
# upper-case (abc -> {ABC}). Matching is unaffected -- .geo_guid_key() and
# .geo_guid_canon() strip braces and re-case on the fly, so a column stored in
# this form still joins correctly. Blank/NA stays NA.
.geo_guid_display <- function(x) {
  key <- .geo_guid_key(x)
  dplyr::if_else(
    is.na(key) | !nzchar(key),
    NA_character_,
    paste0("{", key, "}")
  )
}

# Re-format the admin GUID columns present in `data` to the output form.
.geo_guid_display_cols <- function(
  data,
  cols = c("adm0_guid", "adm1_guid", "adm2_guid")
) {
  present <- intersect(cols, names(data))
  if (length(present) == 0L) {
    return(data)
  }
  dplyr::mutate(data, dplyr::across(dplyr::all_of(present), .geo_guid_display))
}

# Count rows missing any admin level (adm1/adm2). Shared by clean_afp() and
# clean_es() to size the "recovered N from coordinates" progress message; 0 when
# neither admin column is present.
.geo_miss_admin <- function(data) {
  cols <- intersect(c("adm1", "adm2"), names(data))
  if (length(cols) == 0L) {
    return(0L)
  }
  sum(Reduce(`|`, lapply(cols, function(col) is.na(data[[col]]))), na.rm = TRUE)
}

# ---------------------------------------------------------------------
# get_polis_data(): I/O + housekeeping
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

# Archive the freshly-saved file with a timestamp under <polis_folder>/archive/
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
  arc_dir <- file.path(polis_folder, "archive")
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
  # Reference tables (e.g. Population) carry no usable update date and are
  # pulled whole: a NA `date_field` means "no date filter, fetch everything".
  parts <- character(0)
  if (!is.na(date_field) && nzchar(date_field)) {
    year_lo <- as.integer(format(as.Date(min_date), "%Y"))
    year_hi <- as.integer(format(as.Date(max_date), "%Y"))
    parts <- c(
      sprintf("%s ge %d-01-01", date_field, year_lo),
      sprintf("%s le %d-12-31", date_field, year_hi)
    )
  }
  if (
    isTRUE(length(region) == 1L) &&
      isTRUE(nzchar(region)) &&
      tolower(region) != "global" &&
      !(endpoint %in% c("LabSpecimen", "Im", "Population"))
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

# OData query prefix for an optional filter clause: "?$filter=...&" when a
# clause is present, or a bare "?" when there is none (reference-table pulls).
.polis_filter_query <- function(flt) {
  if (nzchar(flt)) {
    paste0("?$filter=", utils::URLencode(flt), "&")
  } else {
    "?"
  }
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
    .polis_filter_query(flt),
    "$count=true&$top=0"
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
    .polis_filter_query(flt),
    "$orderby=Id&$top=",
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

# ---------------------------------------------------------------------
# Pipeline file I/O (read raw tables / write cleaned outputs)
# ---------------------------------------------------------------------

# Read a data file, dispatching on extension (.rds/.csv/.parquet/.qs2).
.polis_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    rds = readRDS(path),
    csv = readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    parquet = .polis_require("arrow", "read .parquet")$read_parquet(path),
    qs2 = .polis_require("qs2", "read .qs2")$qs_read(path),
    cli::cli_abort("Unsupported file type {.val {ext}} for {.file {path}}.")
  )
}

# Write a data frame, dispatching on extension; returns `path` invisibly.
.polis_write <- function(obj, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    rds = saveRDS(obj, path),
    csv = readr::write_csv(obj, path),
    parquet = .polis_require("arrow", "write .parquet")$write_parquet(
      obj,
      path
    ),
    qs2 = .polis_require("qs2", "write .qs2")$qs_save(obj, path),
    cli::cli_abort("Unsupported file type {.val {ext}} for {.file {path}}.")
  )
  invisible(path)
}

# Require an optional package, returning its namespace, with a clear message.
.polis_require <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("Package {.pkg {pkg}} is needed to {what}.")
  }
  asNamespace(pkg)
}

# Read the raw POLIS tables present in a directory (most recent match wins).
.polis_read_inputs <- function(
  dir,
  patterns = c(
    afp = "Human",
    es = "EnvSample",
    virus = "Virus",
    subactivity = "SubActivity",
    activity = "Activity"
  )
) {
  if (!dir.exists(dir)) {
    cli::cli_abort("Input directory {.file {dir}} does not exist.")
  }
  files <- list.files(dir, full.names = TRUE)
  inputs <- list()
  # match subactivity before activity, claiming each file once, so "Activity"
  # can't grab the SubActivity file.
  for (key in names(patterns)) {
    hit <- sort(
      files[grepl(patterns[[key]], basename(files))],
      decreasing = TRUE
    )
    if (length(hit) > 0) {
      inputs[[key]] <- .polis_read(hit[[1]])
      files <- setdiff(files, hit[[1]])
      cli::cli_alert_info(
        "Read {.val {key}} from {.file {basename(hit[[1]])}}."
      )
    }
  }
  inputs
}

# Write a named list of cleaned tibbles to a directory; returns `dir` invisibly.
.polis_write_outputs <- function(cleaned, dir, format = "rds") {
  for (key in names(cleaned)) {
    .polis_write(cleaned[[key]], file.path(dir, paste0(key, ".", format)))
  }
  cli::cli_alert_success(
    "Wrote {length(cleaned)} cleaned table{?s} to {.file {dir}}."
  )
  invisible(dir)
}

# ---------------------------------------------------------------------
# Naming + input validation
# ---------------------------------------------------------------------

# Build the API-name -> Snake_Name rename vector from the packaged crosswalk.
.polis_crosswalk_map <- function() {
  cw <- polis_crosswalk()
  cw <- cw[!is.na(cw$API_Name) & !is.na(cw$Snake_Name), ]
  cw <- cw[!duplicated(cw$API_Name), ]
  stats::setNames(cw$API_Name, cw$Snake_Name)
}

# Minimal up-front guard shared by all cleaners: non-empty data frame.
.polis_check_input <- function(data, dataset) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} for {.val {dataset}} must be a data.frame.")
  }
  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} for {.val {dataset}} is empty.")
  }
  invisible(data)
}

# ---------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------

# Read every CSV under inst/extdata/rules into a named list keyed by file stem.
.polis_load_rules <- function() {
  rules_dir <- system.file("extdata", "rules", package = "polished")
  if (!nzchar(rules_dir) || !dir.exists(rules_dir)) {
    return(list())
  }
  files <- list.files(rules_dir, pattern = "\\.csv$", full.names = TRUE)
  rules <- lapply(files, readr::read_csv, show_col_types = FALSE)
  names(rules) <- tools::file_path_sans_ext(basename(files))
  rules
}

# =============================================================================
# Country enrichment reference (shared by clean_afp() and clean_es())
#
# A small country-keyed lookup (ISO3 -> standardised name, polio risk tier,
# epidemiological zone) and the two generic transforms the cleaners apply from
# it. Kept here because both the human and environmental cleaners enrich the
# same way; each cleaner owns only its thin orchestrator (.afp_enrich /
# .es_enrich) that composes these.
# =============================================================================

#' Country reference lookup shipped with the package
#'
#' Returns the packaged country reference that maps an ISO3 code to the
#' standardised display name, polio risk tier and epidemiological zone groupings
#' the cleaners attach. Used by [clean_afp()] and [clean_es()] via
#' `.polis_join_country()`.
#'
#' @return A tibble with columns `iso3`, `country_actual`, `risk_group`,
#'   `epi_zones`, `epi_zones_v2`.
#'
#' @examples
#' head(polis_country_lookup())
#'
#' @export
polis_country_lookup <- function() {
  readr::read_csv(
    .polis_extdata_path("country_lookup.csv"),
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' Join the country lookup by ISO3 and fill the country grouping fields
#' @noRd
.polis_join_country <- function(
  data,
  iso3_var = "country_iso3code",
  adm0_var = "adm0",
  lookup = polis_country_lookup()
) {
  if (!iso3_var %in% names(data)) {
    return(data)
  }
  lookup <- dplyr::distinct(lookup, iso3, .keep_all = TRUE)
  pos <- match(toupper(trimws(data[[iso3_var]])), lookup$iso3)
  data$country_actual <- lookup$country_actual[pos]
  data$risk_group <- lookup$risk_group[pos]
  data$epi_zones <- dplyr::coalesce(lookup$epi_zones[pos], "Other")
  data$epi_zones_v2 <- dplyr::coalesce(lookup$epi_zones_v2[pos], "Other")
  if (adm0_var %in% names(data)) {
    data$country_actual <- dplyr::coalesce(
      data$country_actual,
      stringr::str_to_title(data[[adm0_var]])
    )
  }
  data
}

#' Read the poliovirus serotype (Type 1/2/3) off a classification column
#' @noRd
.polis_polio_type <- function(
  data,
  type_var = "classification_all",
  fallback_var = "polio_virus_types"
) {
  src_var <- if (type_var %in% names(data)) {
    type_var
  } else if (fallback_var %in% names(data)) {
    fallback_var
  } else {
    return(data)
  }
  src <- data[[src_var]]
  data$polio_type <- dplyr::case_when(
    stringr::str_detect(src, "1") ~ "Type 1",
    stringr::str_detect(src, "2") ~ "Type 2",
    stringr::str_detect(src, "3") ~ "Type 3",
    TRUE ~ NA_character_
  )
  data
}

# =============================================================================
# Column-name standardisation
#
# Canonical names come from the package crosswalk (inst/extdata/crosswalk.csv).
# The crosswalk is a corrections layer over janitor::clean_names(): for the
# tokens janitor mis-splits (PoNS_OnSetDate -> pons_on_set_date, not
# po_ns_...; CountryISO2Code -> country_iso2code) it pins the agreed Snake_Name.
# Every other column is handled by janitor. There is deliberately no per-dataset
# dictionary -- the crosswalk is the single source of truth for naming.
# =============================================================================

#' POLIS column crosswalk
#'
#' Returns the packaged crosswalk that maps raw POLIS API column names to the
#' canonical `Snake_Name` used across cleaned datasets.
#'
#' @return A tibble with columns `Table`, `API_Name`, `Snake_Name`, `Web_Name`,
#'   `Label`.
#'
#' @examples
#' head(polis_crosswalk())
#'
#' @export
polis_crosswalk <- function() {
  path <- .polis_extdata_path("crosswalk.csv")
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' Standardise POLIS column names
#'
#' Renames raw POLIS columns to their canonical `Snake_Name` via the crosswalk,
#' then applies [janitor::clean_names()] to everything the crosswalk does not
#' cover. The result is fully snake_case with the agreed names for special
#' tokens.
#'
#' @param data A raw POLIS data frame.
#' @param crosswalk Rename vector from `.polis_crosswalk_map()` (default).
#'
#' @return `data` with canonical column names.
#'
#' @examples
#' raw <- data.frame(PoNS_OnSetDate = 1, Admin0Name = "X", DateOnset = 2,
#'   check.names = FALSE)
#' names(standardise_names(raw))
#'
#' @export
standardise_names <- function(data, crosswalk = .polis_crosswalk_map()) {
  data <- dplyr::rename(data, dplyr::any_of(crosswalk))
  janitor::clean_names(data)
}

#' Order columns: identifiers, then location, then time, then everything else
#'
#' Classifies each column by the first matching `column_roles` pattern and emits
#' the groups in role order, then all unmatched columns. Order within a group is
#' preserved.
#'
#' @param data A data frame.
#' @param roles Ordered named list of regex patterns (see [polis_config()]'s
#'   `column_roles`).
#'
#' @return The data frame with reordered columns.
#'
#' @export
order_columns <- function(data, roles) {
  remaining <- names(data)
  ordered <- character(0)
  for (pattern in roles) {
    hit <- remaining[grepl(pattern, remaining)]
    ordered <- c(ordered, hit)
    remaining <- setdiff(remaining, hit)
  }
  dplyr::relocate(data, dplyr::all_of(c(ordered, remaining)))
}

# =============================================================================
# Deduplication: upsert-by-Id keep-latest, plus an ambiguity tripwire
#
# POLIS is an Id-keyed, keep-latest store: a record is uniquely the row with
# the newest update timestamp for its Id. polis_upsert() is the single primitive
# that enforces that, used by both the download layer and the cleaners. It never
# decides correctness from business columns -- that is what flag_ambiguous() is
# for: it asserts the business key (e.g. epid + adm0) and routes violations to
# QA instead of silently dropping a reclassified case.
# =============================================================================

#' Upsert by Id, keeping the latest record
#'
#' Combines an existing store with an optional new pull, optionally collapses
#' exact duplicate rows at a finer grain, then keeps exactly one row per `id`:
#' the one with the maximum `date`. This is unconditional (no Id-range
#' shortcut), so a single primitive governs recency everywhere.
#'
#' @param store A data frame (the accumulated store, or simply the data to
#'   dedup).
#' @param pull Optional new data frame to upsert into `store`.
#' @param id Name of the canonical identifier column (default `"id"`).
#' @param date Name of the update-timestamp column used for recency
#'   (default `"last_update_date"`).
#' @param grain Optional character vector of columns defining a finer row grain.
#'   When supplied, exact duplicates at this grain are collapsed (keep-latest)
#'   before the per-`id` step.
#'
#' @return A data frame with one row per `id`.
#'
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2),
#'   last_update_date = as.Date(c("2024-01-01", "2024-03-01", "2024-02-01")),
#'   value = c("old", "new", "x")
#' )
#' polis_upsert(df)
#'
#' @export
polis_upsert <- function(
  store,
  pull = NULL,
  id = "id",
  date = "last_update_date",
  grain = NULL
) {
  combined <- if (is.null(pull)) store else dplyr::bind_rows(store, pull)
  if (!is.data.frame(combined) || nrow(combined) == 0) {
    return(combined)
  }

  if (!id %in% names(combined)) {
    cli::cli_warn(
      "No {.field {id}} column found; falling back to exact-row dedup."
    )
    return(dplyr::distinct(combined))
  }

  if (!is.null(grain)) {
    combined <- .polis_keep_latest(combined, keys = grain, date_col = date)
  }
  .polis_keep_latest(combined, keys = id, date_col = date)
}

#' Keep one row per key combination, latest by date
#'
#' Sorts by `date` descending and keeps the first row per `keys` combination.
#' Uses data.table when available for speed, otherwise base R. Mirrors the
#' download layer's keep-latest semantics.
#'
#' @param df A data frame.
#' @param keys Character vector of key columns.
#' @param date_col Name of the recency column (may be absent).
#'
#' @return A data frame with one row per `keys` combination.
#'
#' @keywords internal
#' @noRd
.polis_keep_latest <- function(df, keys, date_col) {
  has_date <- date_col %in% names(df)

  # Fast path: data.table sorts in place (the expensive step). We deliberately
  # subset with base `duplicated()` on the key columns rather than
  # `duplicated(dt, by = ...)`, because the data.table method only dispatches
  # when the package is attached -- and we merely import it.
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(df)
    if (has_date) {
      data.table::setorderv(
        dt,
        c(keys, date_col),
        c(rep(1L, length(keys)), -1L),
        na.last = TRUE
      )
    } else {
      data.table::setorderv(dt, keys)
    }
    df <- as.data.frame(dt, stringsAsFactors = FALSE)
  } else if (has_date) {
    df <- df[
      order(df[[date_col]], decreasing = TRUE, na.last = TRUE),
      ,
      drop = FALSE
    ]
  }

  keep <- !duplicated(df[, keys, drop = FALSE])
  tibble::as_tibble(df[keep, , drop = FALSE])
}

#' Flag (do not drop) rows whose business key spans multiple Ids
#'
#' A tripwire on the assumed business uniqueness key. After [polis_upsert()] has
#' reduced the data to one row per `id`, a well-formed dataset should also be
#' unique on its business key. Rows that violate this are surfaced to QA -- and
#' left in the data -- so a genuine reclassification is never silently dropped.
#'
#' @param data A data frame (already deduped by `id`).
#' @param key Character vector naming the business key columns.
#' @param id Name of the identifier column (default `"id"`).
#' @param sink Optional destination for the flagged rows: a file path (CSV is
#'   written) or `NULL` (flags are attached as the `polis_ambiguous` attribute).
#'
#' @return `data`, unchanged, possibly carrying a `polis_ambiguous` attribute.
#'
#' @export
flag_ambiguous <- function(data, key, id = "id", sink = NULL) {
  if (!all(c(key, id) %in% names(data))) {
    return(data)
  }

  flags <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::filter(dplyr::n_distinct(.data[[id]]) > 1) |>
    dplyr::ungroup()

  if (nrow(flags) == 0) {
    return(data)
  }

  n_flagged <- nrow(flags)
  n_fmt <- .polis_big_num(n_flagged)
  cli::cli_alert_warning(
    "{n_fmt} {cli::qty(n_flagged)}row{?s} share a business key \\
    ({.field {key}}) across multiple {.field {id}}; flagged for QA, \\
    not dropped."
  )
  if (is.character(sink) && nzchar(sink)) {
    readr::write_csv(flags, sink)
  }
  attr(data, "polis_ambiguous") <- flags
  data
}

# =============================================================================
# Synonym remapping (G4) and full-pull reconcile (G5)
#
# POLIS merges duplicate cases by rewriting an EPID to its canonical value and
# exposes the history via HistorizedSynonyms. A consumer that stored both the
# winner and the loser keeps the loser forever -- no Id keep-latest rule removes
# it, because the two rows have different Ids. remap_synonyms() rewrites merged
# EPIDs to canonical *before* dedup so the rows then collapse. reconcile() is the
# heavier safety net: a full pull anti-joined against the store prunes Ids that
# POLIS has since deleted or merged away.
# =============================================================================

#' Remap merged EPIDs to their canonical value
#'
#' Rewrites the `epid` column using a synonym table (old EPID -> canonical EPID)
#' so that records POLIS has merged collapse together in the subsequent
#' [polis_upsert()] step. A no-op when `synonyms` is `NULL`, so cleaners can call
#' it unconditionally.
#'
#' @param data A data frame with an `epid` column.
#' @param synonyms A data frame with columns `epid` (old) and `canonical_epid`,
#'   or `NULL` (default no-op).
#'
#' @return `data` with `epid` remapped where a synonym exists.
#'
#' @export
remap_synonyms <- function(data, synonyms = NULL) {
  if (is.null(synonyms) || !"epid" %in% names(data)) {
    return(data)
  }
  required <- c("epid", "canonical_epid")
  if (!all(required %in% names(synonyms))) {
    cli::cli_warn(
      "Synonym table needs columns {.field {required}}; skipping remap."
    )
    return(data)
  }

  lookup <- stats::setNames(synonyms$canonical_epid, synonyms$epid)
  n_hit <- sum(data$epid %in% names(lookup))
  data <- dplyr::mutate(
    data,
    epid = dplyr::if_else(epid %in% names(lookup), unname(lookup[epid]), epid)
  )
  if (n_hit > 0) {
    cli::cli_alert_info("Remapped {n_hit} EPID{?s} via synonyms.")
  }
  data
}

#' Prune records absent from a full pull (reconcile)
#'
#' POLIS's current-view API hides deletes and merges, so an incrementally-built
#' store accumulates rows POLIS has since removed. Given a fresh full pull,
#' reconcile keeps only the `id`s still present, pruning the rest.
#'
#' @param store The accumulated data frame.
#' @param full_pull A complete fresh pull of the same table.
#' @param id Name of the identifier column (default `"id"`).
#'
#' @return `store` filtered to `id`s present in `full_pull`.
#'
#' @export
reconcile <- function(store, full_pull, id = "id") {
  if (!id %in% names(store) || !id %in% names(full_pull)) {
    cli::cli_warn("No {.field {id}} column on both sides; skipping reconcile.")
    return(store)
  }
  live <- unique(full_pull[[id]])
  pruned <- sum(!store[[id]] %in% live)
  if (pruned > 0) {
    cli::cli_alert_info(
      "Reconcile pruned {pruned} stale {.field {id}} row{?s}."
    )
  }
  dplyr::filter(store, .data[[id]] %in% live)
}

# ---------------------------------------------------------------------
# Column type inference (auto_parse_types / detect_factors)
# ---------------------------------------------------------------------

#' Infer column types after cleaning, then optionally layer factor detection
#'
#' POLIS returns every field as character. This parses character columns to
#' their natural base type (numeric, integer, date, datetime, logical) via
#' [readr::type_convert()], protecting identifier-like names and leading-zero
#' codes from coercion, and optionally proposes low-cardinality character
#' columns as factors. The cleaners ([clean_afp()] etc.) call it with
#' `apply = FALSE` (base types only) when `polis_config(parse_types = TRUE)`.
#'
#' @param data A data frame or tibble.
#' @param max_levels Maximum distinct values for a factor candidate. Default 50.
#' @param max_unique_ratio Maximum unique/non-NA ratio for a factor. Default
#'   `0.2`.
#' @param protect_patterns Regexes for names kept as character. Default
#'   `c("id$", "uid$", "code$", "ref$", "key$")`.
#' @param keep_leading_zero_chars Keep a character column when any value is a
#'   leading-zero digit string (e.g. `"00123"`). Default `TRUE`.
#' @param apply If `TRUE` (default) apply factor conversions on top of the
#'   parsed base types; if `FALSE` parse base types only.
#' @param return One of `"data"`, `"both"`, `"plan"`. Default `"data"`.
#'
#' @return Depending on `return`: the parsed tibble (`"data"`), the type plan
#'   (`"plan"`), or `list(plan, data)` (`"both"`).
#'
#' @examples
#' df <- tibble::tibble(id = c("001", "002"), age = c("1", "2"))
#' auto_parse_types(df, apply = FALSE)
#'
#' @export
auto_parse_types <- function(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE,
  apply = TRUE,
  return = c("data", "both", "plan")
) {
  return <- match.arg(return)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }
  if (length(max_levels) != 1 || max_levels < 2) {
    cli::cli_abort("{.arg max_levels} must be a single integer >= 2.")
  }
  if (
    length(max_unique_ratio) != 1 ||
      max_unique_ratio <= 0 ||
      max_unique_ratio > 1
  ) {
    cli::cli_abort("{.arg max_unique_ratio} must be in (0, 1].")
  }

  cols <- names(data)
  is_char <- vapply(data, is.character, logical(1))
  protected <- vapply(cols, .is_protected, logical(1), protect_patterns)
  lead0 <- keep_leading_zero_chars &
    is_char &
    vapply(
      data,
      function(x) is.character(x) && .has_leading_zeros(x),
      logical(1)
    )

  # parse base types via readr, holding protected / leading-zero columns as text
  data_parsed <- if (any(is_char)) {
    col_map <- stats::setNames(
      rep(list(readr::col_guess()), length(cols)),
      cols
    )
    for (nm in cols[protected | lead0]) {
      col_map[[nm]] <- readr::col_character()
    }
    suppressWarnings(suppressMessages(readr::type_convert(
      tibble::as_tibble(data),
      col_types = do.call(readr::cols, col_map),
      guess_integer = TRUE
    )))
  } else {
    tibble::as_tibble(data)
  }

  # fast path: base types only, no factor plan needed
  if (!isTRUE(apply) && return == "data") {
    return(data_parsed)
  }

  fplan <- detect_factors(
    data,
    max_levels = max_levels,
    max_unique_ratio = max_unique_ratio,
    protect_patterns = protect_patterns,
    keep_leading_zero_chars = keep_leading_zero_chars
  )
  f_names <- if (nrow(fplan) == 0L) character(0) else fplan$name

  data_final <- data_parsed
  if (isTRUE(apply) && length(f_names) > 0L) {
    for (nm in f_names) {
      x <- as.character(data[[nm]])
      # first-seen order, ignoring NA
      data_final[[nm]] <- factor(x, levels = unique(x[!is.na(x)]))
    }
  }
  if (return == "data") {
    return(data_final)
  }

  n_non_na <- vapply(data, function(x) sum(!is.na(x)), integer(1))
  n_unique <- vapply(
    data,
    function(x) dplyr::n_distinct(x, na.rm = TRUE),
    integer(1)
  )
  proposed <- vapply(
    cols,
    function(nm) class(data_parsed[[nm]])[1],
    character(1)
  )
  proposed[protected | lead0] <- "character"
  proposed[cols %in% f_names] <- "factor"
  plan <- tibble::tibble(
    name = cols,
    current_type = vapply(data, function(x) class(x)[1], character(1)),
    proposed_type = proposed,
    protected = unname(protected | lead0),
    n = nrow(data),
    n_non_na = unname(n_non_na),
    n_unique = unname(n_unique),
    unique_ratio = unname(n_unique / pmax(n_non_na, 1L))
  )
  if (return == "plan") {
    return(plan)
  }
  list(plan = plan, data = data_final)
}

#' Detect factor-like character columns (low-cardinality only)
#'
#' Identifies character columns that look categorical, protecting id-like names
#' and leading-zero codes.
#'
#' @inheritParams auto_parse_types
#'
#' @return A tibble of factor candidates: `name`, `n`, `n_non_na`, `n_unique`,
#'   `unique_ratio`, `reason`.
#'
#' @examples
#' detect_factors(tibble::tibble(adm = c("A", "B", "A")))
#'
#' @export
detect_factors <- function(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame or tibble.")
  }
  cols <- names(data)
  n_non_na <- vapply(data, function(x) sum(!is.na(x)), integer(1))
  n_unique <- vapply(
    data,
    function(x) dplyr::n_distinct(x, na.rm = TRUE),
    integer(1)
  )
  is_char <- vapply(data, is.character, logical(1))
  protected <- vapply(cols, .is_protected, logical(1), protect_patterns)
  lead0 <- keep_leading_zero_chars &
    is_char &
    vapply(
      data,
      function(x) is.character(x) && .has_leading_zeros(x),
      logical(1)
    )

  tibble::tibble(
    name = cols,
    n = nrow(data),
    n_non_na = unname(n_non_na),
    n_unique = unname(n_unique),
    unique_ratio = unname(n_unique / pmax(n_non_na, 1L)),
    is_char = unname(is_char),
    protected = unname(protected),
    lead0 = unname(lead0)
  ) |>
    dplyr::mutate(
      keep = dplyr::case_when(
        !is_char ~ FALSE,
        protected | lead0 ~ FALSE,
        n_unique == 0L ~ FALSE,
        n_unique > max_levels ~ FALSE,
        n_non_na > 0L & n_unique == 1L ~ TRUE,
        unique_ratio > max_unique_ratio ~ FALSE,
        TRUE ~ TRUE
      ),
      reason = dplyr::case_when(
        !is_char ~ "not character",
        protected | lead0 ~ "protected",
        n_unique == 0L ~ "all NA",
        n_unique > max_levels ~ "too many levels",
        n_non_na > 0L & n_unique == 1L ~ "constant (1 level)",
        unique_ratio > max_unique_ratio ~ "too unique for factor",
        TRUE ~ "low cardinality character"
      )
    ) |>
    dplyr::filter(keep) |>
    dplyr::select(name, n, n_non_na, n_unique, unique_ratio, reason)
}

# Is a column name protected from coercion (case-insensitive)?
.is_protected <- function(nm, patterns) {
  if (length(patterns) == 0L) {
    return(FALSE)
  }
  any(vapply(
    patterns,
    function(p) grepl(p, nm, ignore.case = TRUE),
    logical(1)
  ))
}

#' Return TRUE if `x` contains any leading-zero all-digit value (e.g. "007").
#' @noRd
.has_leading_zeros <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) return(FALSE)
  # a leading-zero value is all digits, length > 1, starting with "0" (e.g.
  # "007"). Short-circuit on startsWith() and test only those candidates with a
  # non-backtracking char class -- the equivalent "^0+\\d+$" regex backtracks
  # catastrophically on long all-zero/digit strings across millions of rows.
  starts_zero <- startsWith(x, "0")
  if (!any(starts_zero)) return(FALSE)
  # cap the all-digit test at the first few thousand zero-starting values: enough
  # to spot a leading-zero code, bounded so a column of many such values cannot
  # dominate the scan over millions of rows.
  zero_start_candidates <- utils::head(x[starts_zero], 5000L)
  any(
    nchar(zero_start_candidates) > 1L &
      !grepl("[^0-9]", zero_start_candidates, perl = TRUE)
  )
}

# Cleaner finishing step: infer base column types when cfg$parse_types is on.
.polis_parse_types <- function(data, cfg) {
  if (isTRUE(cfg$parse_types)) {
    auto_parse_types(data, apply = FALSE, return = "data")
  } else {
    data
  }
}

# Cleaner finishing step: drop all-NA columns when cfg$drop_empty_cols is on.
.polis_drop_empty <- function(data, cfg) {
  if (!isTRUE(cfg$drop_empty_cols)) {
    return(data)
  }
  keep <- vapply(data, function(x) !all(is.na(x)), logical(1))
  data[, keep, drop = FALSE]
}
