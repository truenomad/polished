#' Download POLIS tables
#'
#' @description
#' The canonical entry point for fetching data from the POLIS OData service.
#' Works around three POLIS quirks that make naive OData clients silently
#' lose data:
#'
#' * **Date filters are year-aligned only.** POLIS only honours `field le
#'   YYYY-12-31` bounds; any sub-year `le` returns 0 rows. The function
#'   aligns `min_date` / `max_date` to year boundaries before building any
#'   filter.
#' * **No `@odata.nextLink` and `$skip` is rejected.** POLIS caps `$top` at
#'   2000 and refuses to paginate via the OData standard mechanism. The
#'   only way to walk past 2000 rows is Id-range pagination:
#'   `$orderby=Id&$top=2000&$filter=... and Id gt <last>`.
#' * **The clinical date columns (`CaseDate`, `VirusDate`, ...) have NULL
#'   coverage** for historical records. The function filters on the
#'   table's "update" column (`LastUpdateDate` / `UpdatedDate` / `Start` /
#'   `PublishDate`) which probes have confirmed is 100%-populated.
#'
#' @details
#' **Cache layout and recovery.** Pages are committed under
#' `<polis_folder>/.parts/<stem>/year_YYYY.<ext>.pages/`, then compacted once
#' per year. A journal records the last committed page. Interrupted pulls
#' resume at that page's maximum Id; an uncommitted page is fetched again.
#' After all years finish, the verified table replaces
#' `<polis_folder>/<stem>.<ext>` atomically. The stem is the table's `raw_*`
#' name (e.g. `case` is written as `raw_afp`; see [polis_tables_mapping]).
#' Merging requires memory for the complete table.
#'
#' Scope manifests under `.manifests/` record the effective country, region,
#' year range and format. Changing those filters starts a fresh pull while
#' preserving the previous canonical file until the replacement succeeds.
#' Legacy files are renamed to their `raw_*` names, but caches without a scope
#' manifest must be downloaded once again because their filters are unknown.
#' `prune_parts = TRUE` removes completed parts; unchanged snapshots can be
#' reused without splitting or reading the full canonical file.
#'
#' **Parallelism.** Each calendar year between `min_date` and `max_date`
#' is an independent Id-range walk. With `workers > 1`, years are
#' dispatched across a `parallel::makePSOCKcluster()` cluster (the same
#' transport on Windows, macOS, and Linux, so any user can opt in); a
#' live cli progress bar polls the part files between socket reads so
#' you see rows accumulate across workers in real time. With
#' `workers = 1`, a single sequential loop drives the bar per 2K-row
#' batch. PSOCK workers need `polished` installed in their library
#' path -- `devtools::load_all()` is not enough.
#'
#' **Freshness.** For tables with `LastUpdateDate` or `UpdatedDate`, completed
#' snapshots are checked against the full scoped list of Ids and revision
#' timestamps. Changed and new rows are fetched selectively; deleted rows are
#' removed. Equal row counts never establish freshness. This relies on the
#' service advancing its revision timestamp when a record changes.
#' Tables filtered by event dates (`Start` or `PublishDate`) are fetched again
#' because those dates cannot establish whether a row was edited.
#' Reference tables with no update date use `reference_refresh_days` instead.
#' `force = TRUE` always starts a full pull. `auto_refetch = FALSE` explicitly
#' trusts completed snapshots until forced or their scope changes.
#'
#' Fresh pulls also check for missing Ids over `verify_years` and refetch them.
#' Revision-based reconciliation covers the full scope, including after an
#' interrupted pull resumes. Verification failures retain checkpoints and
#' leave the previous canonical file in place for a later retry.
#'
#' **Resilience.** Read timeouts are retried inside each request, and a year
#' whose parallel worker still fails is requeued up to three times.
#' `POLIS_TIMEOUT_SECONDS` overrides the 120-second default.
#'
#' @param tables Optional character vector of table names (see
#'   [polis_tables_mapping] for the supported set, e.g. `"case"`, `"virus"`,
#'   `"population"`). `NULL` (default) downloads every table in the catalogue.
#'   Unknown names abort with a list of valid names. The `population` reference
#'   table has no update date, so `min_date`/`max_date`/`region` do not apply to
#'   it -- it is pulled whole.
#' @param min_date Earliest date to fetch. Defaults to `"2000-01-01"`.
#'   POLIS rejects sub-year date ranges, so this is aligned to January 1
#'   of its year.
#' @param max_date Latest date to fetch. Defaults to `Sys.Date()`. Aligned
#'   to December 31 of its year.
#' @param region WHO region filter (`"Global"` (default), `"AFRO"`,
#'   `"AMRO"`, `"EMRO"`, `"EURO"`, `"SEARO"`, `"WPRO"`).
#' @param country_code Optional ISO3 country code (e.g. `"NGA"`). Adds an
#'   `and CountryISO3Code eq '<code>'` clause. Default `NULL` (no country
#'   filter).
#' @param polis_folder Root folder for cached data. Files land under
#'   `<polis_folder>/`. Default
#'   `tools::R_user_dir("polished", which = "cache")` -- the standard
#'   per-user cache location, persistent across sessions so incremental
#'   updates "just work". Pass an explicit path to keep data alongside a
#'   project.
#' @param output_format Output format. One of `"rds"` (default), `"rda"`,
#'   `"csv"`, `"parquet"`, `"qs2"`. `"parquet"` requires the `arrow`
#'   package; `"qs2"` requires the `qs2` package.
#' @param workers Number of parallel workers. `1` (default) runs a
#'   sequential loop with a live per-batch progress bar. `> 1` opts into
#'   a PSOCK cluster that dispatches one year per worker; pass e.g.
#'   `parallel::detectCores() - 1L` to use most cores.
#' @param auto_refetch If `TRUE` (default), verify fresh downloads and refresh
#'   completed snapshots as described above. `FALSE` trusts a completed
#'   snapshot with matching scope and skips post-download verification.
#' @param verify_years Number of most recent calendar years covered by the
#'   missing-Id check on a fresh pull, counted back from `max_date` and clamped
#'   to `min_date`. Default `3L`; `NULL` covers the full requested range.
#'   Revision-based reconciliation always covers the full scope. Ignored when
#'   `auto_refetch = FALSE` and for reference tables with no update date.
#' @param log_file Optional path to a per-batch log file (`.rds`). Default
#'   `NULL`.
#' @param keep_archives When `> 0`, on each save also writes a timestamped
#'   copy under `archive/` and prunes older copies. Default `0` (no
#'   archive).
#' @param force If `TRUE`, discard resume parts and fetch a fresh snapshot.
#'   The previous canonical file remains until replacement succeeds.
#' @param prune_parts If `TRUE` (default), delete the per-year resume cache
#'   after publishing the complete snapshot. `FALSE` retains completed parts
#'   for inspection; they are discarded when the snapshot's revisions change.
#' @param reference_refresh_days Maximum age in days of a completed reference
#'   table without an update timestamp (currently `population`). Default `1`;
#'   `0` refreshes on every call. Ignored when `auto_refetch = FALSE`.
#' @param polis_api_key API key. Defaults to `Sys.getenv("POLIS_API_KEY")`.
#' @param quiet Suppress headers, progress bars, and the info alert.
#'   Default `FALSE`.
#'
#' @return `NULL`, invisibly. Each selected table is written to
#'   `<polis_folder>/<raw_stem>.<ext>`. Pages and year partitions are loaded
#'   during downloading; the final merge loads the complete table into memory.
#'   Read a saved table with `readRDS(file.path(polis_folder, "raw_im.rds"))`.
#'
#' @examples
#' \dontrun{
#' # Pull one table into the default per-user cache
#' get_polis_data(tables = "im")
#'
#' # Read it back from disk when you need it
#' cache <- tools::R_user_dir("polished", which = "cache")
#' im <- readRDS(file.path(cache, "raw_im.rds"))
#'
#' # The whole catalogue in parallel into a project folder
#' get_polis_data(
#'   polis_folder = "data/polis",
#'   workers = parallel::detectCores() - 1L
#' )
#' }
#' @seealso [polis_tables_mapping] for the table catalogue.
#' @export
get_polis_data <- function(
  tables = NULL,
  min_date = "2000-01-01",
  max_date = Sys.Date(),
  region = "Global",
  country_code = NULL,
  polis_folder = tools::R_user_dir("polished", which = "cache"),
  output_format = c("rds", "rda", "csv", "parquet", "qs2"),
  workers = 1L,
  auto_refetch = TRUE,
  verify_years = 3L,
  log_file = NULL,
  keep_archives = 0L,
  force = FALSE,
  prune_parts = TRUE,
  polis_api_key = Sys.getenv("POLIS_API_KEY"),
  quiet = FALSE,
  reference_refresh_days = 1
) {
  ext <- match.arg(output_format)
  pending_files <- character()
  on.exit(unlink(pending_files), add = TRUE)

  if (
    !is.numeric(reference_refresh_days) ||
      length(reference_refresh_days) != 1L ||
      is.na(reference_refresh_days) ||
      !is.finite(reference_refresh_days) ||
      reference_refresh_days < 0
  ) {
    cli::cli_abort("reference_refresh_days must be a finite number >= 0.")
  }

  if (!isTRUE(nzchar(polis_api_key))) {
    cli::cli_abort(c(
      "x" = "POLIS API key is empty.",
      "i" = "Set {.envvar POLIS_API_KEY} or pass {.arg polis_api_key}."
    ))
  }

  if (!.polis_valid_verify_years(verify_years)) {
    cli::cli_abort(c(
      "x" = "{.arg verify_years} must be a single whole number >= 1, or NULL.",
      "i" = "Got {.val {verify_years}}."
    ))
  }

  selected <- polis_tables_mapping
  if (!is.null(tables)) {
    bad <- setdiff(tables, polis_tables_mapping$table_name)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "x" = "Unknown table name{?s}: {.val {bad}}.",
        "i" = "Valid names: {.val {polis_tables_mapping$table_name}}."
      ))
    }
    selected <- polis_tables_mapping[
      polis_tables_mapping$table_name %in% tables,
      ,
      drop = FALSE
    ]
  }

  # Downloads land directly in `polis_folder` (no extra `data/` subfolder); the
  # per-year parts and timestamped archives live in `.parts/` and `archive/`
  # alongside the canonical files.
  data_dir <- polis_folder
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  min_date <- as.Date(min_date)
  max_date <- as.Date(max_date)
  if (
    length(min_date) != 1L ||
      length(max_date) != 1L ||
      is.na(min_date) ||
      is.na(max_date) ||
      min_date > max_date
  ) {
    cli::cli_abort("min_date and max_date must define a valid date range.")
  }
  region <- toupper(region)
  if (!is.null(country_code)) country_code <- toupper(country_code)

  if (!isTRUE(quiet)) {
    cli::cli_h1("Downloading POLIS data")
  }

  for (i in seq_len(nrow(selected))) {
    row <- selected[i, ]
    nm <- row$table_name
    date_field <- row$date_field
    endpoint <- row$endpoint
    # `nm` stays the human-facing identity (logging, resume hints); `stem` is the
    # on-disk `raw_*` name used for the canonical file and the per-year parts.
    stem <- row$file_stem

    out_file <- file.path(data_dir, paste0(stem, ".", ext))
    parts_dir <- file.path(data_dir, ".parts", stem)

    if (!isTRUE(quiet)) {
      cli::cli_h2(paste0(
        cli::col_cyan(cli::style_bold(nm)),
        " [",
        i,
        "/",
        nrow(selected),
        "]"
      ))
    }

    # these endpoints have no region field, so .polis_build_filter drops it
    region_ignored <- endpoint %in% c("LabSpecimen", "Im", "Population")
    if (
      region_ignored &&
        !identical(tolower(region), "global") &&
        !isTRUE(quiet)
    ) {
      cli::cli_alert_warning(
        "{.val {nm}}: POLIS has no region field for this table; \\
        {.arg region} = {.val {region}} is ignored and all regions \\
        are returned."
      )
    }

    # Rename any files left under the old bare `<table_name>` convention to the
    # `raw_*` stem so an existing download is reused, not re-fetched.
    .polis_migrate_legacy_names(data_dir, nm, stem, ext)

    no_date <- is.na(date_field) || !nzchar(date_field)
    scope <- .polis_download_scope(
      endpoint,
      date_field,
      min_date,
      max_date,
      region,
      country_code,
      ext
    )
    manifest_path <- .polis_download_manifest(data_dir, stem, ext)
    state <- if (file.exists(manifest_path))
      tryCatch(readRDS(manifest_path), error = function(e) NULL) else NULL
    same_scope <- is.list(state) &&
      identical(state$version, 1L) &&
      identical(state$scope, scope)
    completed <- same_scope &&
      isTRUE(state$complete) &&
      file.exists(out_file) &&
      identical(state$file, .polis_part_signature(out_file))
    if (completed && !isTRUE(force)) {
      age <- as.numeric(difftime(Sys.time(), state$fetched_at, units = "days"))
      reusable <- !isTRUE(auto_refetch) ||
        (no_date && is.finite(age) && age >= 0 && age < reference_refresh_days)
      if (
        !reusable &&
          date_field %in% c("LastUpdateDate", "UpdatedDate") &&
          !is.null(state$versions)
      ) {
        versions <- .polis_refresh_snapshot(
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
        )
        .polis_save_manifest(manifest_path, scope, out_file, versions)
        .polis_archive(out_file, polis_folder, stem, ext, keep_archives)
        reusable <- TRUE
        # Retained year parts may contain prior revisions after selective refresh.
        if (!identical(state$versions, versions)) .polis_prune_parts(parts_dir)
      }
      if (reusable) {
        if (isTRUE(prune_parts)) .polis_prune_parts(parts_dir)
        if (!isTRUE(quiet))
          cli::cli_alert_info("Up to date. Using the completed snapshot.")
        next
      }
    }
    # Resume only an incomplete pull whose effective filters are known to match.
    # Completed, expired, unknown, or changed scopes start with fresh parts.
    if (isTRUE(force) || !same_scope || isTRUE(state$complete)) {
      .polis_prune_parts(parts_dir)
    }
    .polis_save_manifest(manifest_path, scope)
    dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)

    # Ask POLIS for the total row count for the year-aligned range.
    # The count is for progress only; it cannot certify freshness.
    declared_total <- tryCatch(
      .polis_get_count(
        endpoint,
        date_field,
        min_date,
        max_date,
        region,
        country_code,
        polis_api_key
      ),
      error = function(e) {
        if (!isTRUE(quiet)) {
          cli::cli_alert_warning(paste0(
            nm,
            ": count query failed (",
            conditionMessage(e),
            "); progress bar will be unbounded."
          ))
        }
        NA_real_
      }
    )

    # Reference tables (NA date_field, e.g. population) carry no usable update
    # date: pull the whole table in a single Id-paginated pass rather than one
    # request per calendar year. The lone part is named `year_0` so the
    # existing merge/dedup-by-Id machinery picks it up unchanged.
    no_date <- is.na(date_field) || !nzchar(date_field)
    if (no_date) {
      years <- 0L
    } else {
      year_lo <- as.integer(format(as.Date(min_date), "%Y"))
      year_hi <- as.integer(format(as.Date(max_date), "%Y"))
      years <- seq.int(year_lo, year_hi)
    }
    specs <- lapply(years, function(yr) {
      list(
        year = yr,
        endpoint = endpoint,
        date_field = date_field,
        region = region,
        country_code = country_code,
        polis_api_key = polis_api_key,
        part_file = file.path(parts_dir, sprintf("year_%d.%s", yr, ext)),
        ext = ext,
        page_size = 2000L
      )
    })

    # Current row count = sum across existing part files.
    current_rows <- sum(vapply(
      specs,
      function(s) {
        .polis_read_meta(s$part_file, ext, date_field)$n_rows
      },
      integer(1)
    ))

    use_parallel <- isTRUE(workers > 1L) && length(specs) > 1L
    workers_actual <- if (use_parallel) {
      min(as.integer(workers), length(specs))
    } else {
      1L
    }

    # Summary line right under the rule. The expected total is already
    # the denominator in the progress bar below, so we don't repeat it
    # here -- just disk resume state + worker mode.
    if (!isTRUE(quiet)) {
      disk_label <- if (current_rows > 0L) {
        paste0(.polis_pretty_num(current_rows), " on disk")
      } else {
        "fresh pull"
      }
      mode_label <- if (workers_actual > 1L) {
        paste0(workers_actual, "x parallel")
      } else {
        "sequential"
      }
      cli::cli_alert_info(paste(
        disk_label,
        "\u00b7",
        mode_label
      ))
    }

    if (use_parallel) {
      .polis_dispatch_parallel(
        specs = specs,
        workers_actual = workers_actual,
        declared_total = declared_total,
        current_rows = current_rows,
        nm = nm,
        ext = ext,
        log_file = log_file,
        quiet = quiet
      )
    } else {
      # Sequential: drive a single live progress bar across years.
      n_new <- "0"
      n_cum <- .polis_pretty_num(current_rows)
      pb_tot <- if (is.na(declared_total)) {
        "?"
      } else {
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

      running_total <- current_rows
      pb_env <- environment()
      lapply(specs, function(spec) {
        on_batch_cb <- function(
          rows_in_batch,
          cumulative_in_year,
          year,
          last_id
        ) {
          pb_env$running_total <- pb_env$running_total + rows_in_batch
          pb_env$n_new <- .polis_pretty_num(rows_in_batch)
          pb_env$n_cum <- .polis_pretty_num(pb_env$running_total)
          pb_env$pb_cur <- pb_env$n_cum
          if (!is.null(pb_id)) {
            cli::cli_progress_update(
              id = pb_id,
              set = .polis_pb_set(running_total, declared_total)
            )
          }
          .polis_log_window(
            log_file,
            data.frame(
              table = nm,
              year = year,
              last_id_after = last_id,
              rows_in_window = rows_in_batch,
              cumulative_rows = running_total,
              elapsed_seconds = NA_real_,
              timestamp = Sys.time(),
              status = "ok",
              stringsAsFactors = FALSE
            )
          )
        }
        tryCatch(
          .polis_fetch_year_worker(spec, on_batch = on_batch_cb),
          error = function(e) {
            if (!is.null(pb_id)) {
              cli::cli_progress_done(id = pb_id)
            }
            on_disk <- sum(vapply(
              specs,
              function(s) .polis_read_meta(s$part_file, ext, date_field)$n_rows,
              integer(1)
            ))
            cli::cli_abort(c(
              "x" = paste0(
                nm,
                " ",
                spec$year,
                ": fetch failed - ",
                conditionMessage(e)
              ),
              "i" = paste0(
                .polis_pretty_num(on_disk),
                " rows checkpointed to ",
                parts_dir
              ),
              "*" = paste0(
                "Resume by re-running get_polis_data(",
                "tables = \"",
                nm,
                "\", ...). DO NOT file.remove() ",
                "the saved file or .parts/ directory."
              )
            ))
          }
        )
      })

      if (!is.null(pb_id)) {
        cli::cli_progress_done(id = pb_id)
      }
    }

    # Merge per-year parts into the single canonical file. This is
    # single-threaded and CPU-heavy on large tables (RDS compression +
    # optional dedup sort) -- surface it so a CPU spike here doesn't
    # look like "workers refusing to die".
    if (!isTRUE(quiet)) {
      cli::cli_alert_info("Merging year parts -> canonical file...")
    }
    canonical_file <- out_file
    out_file <- paste0(canonical_file, ".pending.", Sys.getpid())
    pending_files <- c(pending_files, out_file)
    .polis_merge_parts(parts_dir, out_file, ext, date_field)

    # Completeness check. The Id walk is the slow half of a run (one request
    # per 2000 rows, and full rows on the endpoints that ignore `$select`), so
    # by default it covers only the most recent `verify_years` calendar years.
    # A separate revision check reconciles update-dated tables over the full scope.
    if (isTRUE(auto_refetch) && file.exists(out_file)) {
      verify_min <- .polis_verify_min_date(
        min_date,
        max_date,
        verify_years,
        no_date
      )
      if (!isTRUE(quiet)) {
        range_label <- if (no_date) {
          ""
        } else {
          paste0(
            " for ",
            format(verify_min, "%Y"),
            "-",
            format(max_date, "%Y")
          )
        }
        cli::cli_alert_info(paste0(
          "Verifying completeness against POLIS",
          range_label,
          " (may take a moment)..."
        ))
      }
      downloaded <- .polis_io_read(out_file, ext)
      if (!is.data.frame(downloaded) || !"Id" %in% names(downloaded)) {
        if (!isTRUE(quiet)) {
          cli::cli_alert_warning(
            "{nm}: no Id column on disk; skipping verification."
          )
        }
      } else {
        canonical <- tryCatch(
          .polis_fetch_id_list(
            endpoint = row$endpoint,
            date_field = date_field,
            min_date = verify_min,
            max_date = max_date,
            region = region,
            country_code = country_code,
            polis_api_key = polis_api_key
          ),
          error = function(e) {
            cli::cli_abort(paste0(
              nm,
              ": Id list fetch failed: ",
              conditionMessage(e),
              ". Checkpoints retained; retry the download."
            ))
          }
        )

        if (!is.null(canonical)) {
          missing_ids <- setdiff(canonical, downloaded$Id)
          if (length(missing_ids) > 0L) {
            if (!isTRUE(quiet)) {
              cli::cli_alert_warning(paste0(
                nm,
                ": ",
                length(missing_ids),
                " missing ID(s) detected; refetching."
              ))
            }
            refetched <- tryCatch(
              .polis_refetch_missing(
                endpoint = row$endpoint,
                ids = missing_ids,
                polis_api_key = polis_api_key,
                workers = workers
              ),
              error = function(e) {
                cli::cli_abort(paste0(
                  nm,
                  ": refetch failed: ",
                  conditionMessage(e),
                  ". Checkpoints retained; retry the download."
                ))
              }
            )
            if (
              !is.data.frame(refetched) ||
                !"Id" %in% names(refetched) ||
                length(setdiff(missing_ids, refetched$Id))
            ) {
              cli::cli_abort(
                "Refetch did not return all missing IDs; checkpoints retained."
              )
            }
            if (nrow(refetched) > 0L) {
              combined <- .polis_dedup(
                dplyr::bind_rows(downloaded, refetched),
                id_col = "Id",
                date_col = date_field
              )
              .polis_io_write_atomic(combined, out_file, ext)
              if (!isTRUE(quiet))
                cli::cli_alert_success(paste0(
                  nm,
                  ": verification + refetch complete."
                ))
            }
          } else if (!isTRUE(quiet)) {
            cli::cli_alert_success(paste0(
              nm,
              ": verification passed (",
              .polis_pretty_num(length(canonical)),
              " IDs)."
            ))
          }
        }
      }
    }

    # Publish only after the complete candidate passes verification.
    downloaded <- .polis_io_read(out_file, ext)
    versions <- if (!no_date)
      .polis_record_versions(downloaded, date_field) else NULL
    if (
      isTRUE(auto_refetch) && date_field %in% c("LastUpdateDate", "UpdatedDate")
    ) {
      versions <- .polis_refresh_snapshot(
        list(versions = versions),
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
      )
    }
    if (!file.rename(out_file, canonical_file))
      cli::cli_abort(
        "Failed to commit the verified snapshot; previous file retained."
      )
    out_file <- canonical_file
    .polis_save_manifest(manifest_path, scope, out_file, versions)
    .polis_archive(out_file, polis_folder, stem, ext, keep_archives)

    if (isTRUE(prune_parts) && file.exists(out_file)) {
      .polis_prune_parts(parts_dir)
    }
  }

  # Pure side effect: every selected table is written under
  # <polis_folder>/. Nothing is returned -- read a table back from
  # disk yourself (e.g. readRDS()) when you need it.
  invisible(NULL)
}

#' POLIS table catalogue
#'
#' Static mapping of the tables `get_polis_data()` supports.
#'
#' @details
#' Each `date_field` is the "update" column the package uses when
#' filtering. Probes against POLIS confirmed each value is 100%-populated
#' AND clustered post-2010 (records were imported into POLIS then), so a
#' filter on this field catches every row in the table including pre-2000
#' legacy records. The clinical/event columns (`CaseDate`, `VirusDate`,
#' `CollectionDate`) are skipped because they contain pre-2000 legacy
#' dates that fall outside typical user-supplied ranges.
#'
#' A `date_field` of `NA` marks a **reference table** (e.g. `population`) that
#' carries no usable update date: it is pulled whole in a single Id-paginated
#' pass, ignoring `min_date`/`max_date`/`region`.
#'
#' @format A data.frame with one row per supported table and columns:
#' \describe{
#'   \item{table_name}{Short identifier used by `tables = "..."`.}
#'   \item{endpoint}{OData endpoint suffix appended to
#'         `https://extranet.who.int/polis/api/v2/`.}
#'   \item{date_field}{Column used for both the OData filter and the
#'         dedup tiebreaker.}
#'   \item{file_stem}{Canonical on-disk filename stem (the `raw_*` name the
#'         downloaded table is written under, e.g. `raw_afp` for `case`). The
#'         cleaning pipeline reads these stems and writes `polished_*` outputs.}
#' }
#' @export
polis_tables_mapping <- data.frame(
  table_name = c(
    "virus",
    "case",
    "human_specimen",
    "environmental_sample",
    "activity",
    "sub_activity",
    "lqas",
    "im",
    "historized_synonyms",
    "historized_geoplace_names",
    "population"
  ),
  endpoint = c(
    "Virus",
    "Case",
    "LabSpecimen",
    "EnvSample",
    "Activity",
    "SubActivity",
    "Lqas",
    "Im",
    "HistorizedSynonyms",
    "HistorizedGeoplaceNames",
    "Population"
  ),
  # `NA` = reference table with no usable update date: pulled whole in one
  # pass (no year partition, no date filter), paginated by Id only.
  date_field = c(
    "UpdatedDate",
    "LastUpdateDate",
    "LastUpdateDate",
    "LastUpdateDate",
    "LastUpdateDate",
    "UpdatedDate",
    "Start",
    "PublishDate",
    "LastUpdateDate",
    "LastUpdateDate",
    NA_character_
  ),
  # On-disk stem: the cleaning pipeline speaks the afp/es/sia language, so each
  # table is written as `raw_<key>` to keep one naming convention end to end.
  file_stem = c(
    "raw_virus",
    "raw_afp",
    "raw_hum_spec",
    "raw_es",
    "raw_activity",
    "raw_sub_activity",
    "raw_lqas",
    "raw_im",
    "raw_historized_synonyms",
    "raw_historized_geoplace_names",
    "raw_population"
  ),
  stringsAsFactors = FALSE
)
