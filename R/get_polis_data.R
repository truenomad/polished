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
#' **Cache layout.** Each table is fetched into a per-year part file under
#' `<polis_folder>/.parts/<stem>/year_YYYY.<ext>`, with a tiny
#' `year_YYYY.meta.rds` sidecar capturing row count and min/max Id. After
#' all years finish the parts are merged into the canonical
#' `<polis_folder>/<stem>.<ext>`. The `<stem>` is the table's `raw_*` name
#' (e.g. `case` is written as `raw_afp`; see [polis_tables_mapping]), so the
#' download and cleaning halves share one naming convention. By default
#' (`prune_parts = TRUE`) the parts are deleted once the canonical is written
#' and the next call rebuilds them from the canonical; pass `prune_parts =
#' FALSE` to keep them on disk so the next call can resume per-year from
#' `max(Id)` without the re-split. Either way the next call resumes per-year
#' from `max(Id)` without re-fetching. To force a clean re-pull, pass `force =
#' TRUE` or delete `.parts/`.
#'
#' Files written by older versions under the bare `<table_name>` name (e.g.
#' `case.<ext>`) are renamed to their `raw_*` stem in place on the next run --
#' no re-download.
#'
#' **Resume semantics.** The part files ARE the resume marker. If a
#' previous run died (network blip, Ctrl-C, OOM), the next call picks up
#' at `Id gt max(Id_in_part)` for each year. Worst case lost work: the
#' most recent in-flight batch.
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
#' **Auto-refetch.** When `auto_refetch = TRUE` (default) the function
#' uses the meta sidecars to detect gaps cheaply (row-count and Id-range
#' mismatch against POLIS's `@odata.count`). Only when a gap is detected
#' does it issue a `$select=Id` probe and refetch missing rows via OData
#' `Id in (...)` chunks. Set to `FALSE` to skip the post-download check
#' entirely.
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
#' @param auto_refetch If `TRUE` (default), run the metadata-aware
#'   verification + selective refetch at the end of each table. Set to
#'   `FALSE` to trust whatever is on disk.
#' @param log_file Optional path to a per-batch log file (`.rds`). Default
#'   `NULL`.
#' @param keep_archives When `> 0`, on each save also writes a timestamped
#'   copy under `archive/` and prunes older copies. Default `0` (no
#'   archive).
#' @param force If `TRUE`, deletes the `.parts/<table>/` directory and the
#'   canonical file for each selected table before running -- forces a
#'   fresh full re-pull instead of resuming. Default `FALSE`.
#' @param prune_parts If `TRUE` (default), deletes the `.parts/<table>/` resume
#'   cache after the canonical file has been written and verified. The canonical
#'   is a complete, Id-deduped checkpoint, so the next run rebuilds the parts
#'   from it (re-bucketing each row into its current `date_field` year). This
#'   keeps the parts free of stale cross-year duplicate copies that otherwise
#'   accumulate when a record's update date crosses a year boundary between runs
#'   -- which in turn keeps the "already up to date" short-circuit honest -- at
#'   the cost of re-splitting the canonical on the next run. Incremental resume
#'   still works. Set to `FALSE` to retain the parts for the fastest possible
#'   resume (at the risk of the parts row count drifting above the true distinct
#'   total over many incremental re-pulls).
#' @param polis_api_key API key. Defaults to `Sys.getenv("POLIS_API_KEY")`.
#' @param quiet Suppress headers, progress bars, and the info alert.
#'   Default `FALSE`.
#'
#' @return `NULL`, invisibly. `get_polis_data()` is called purely for its
#'   side effect: each selected table is written to
#'   `<polis_folder>/<table_name>.<ext>` (plus a `.parts/<table_name>/`
#'   resume cache). The data is never loaded into memory, so a
#'   multi-million-row pull cannot inflate your session. Read a table back
#'   from disk yourself when you need it, e.g.
#'   `readRDS(file.path(polis_folder, "im.rds"))`.
#'
#' @examples
#' \dontrun{
#' # Pull one table into the default per-user cache
#' get_polis_data(tables = "im")
#'
#' # Read it back from disk when you need it
#' cache <- tools::R_user_dir("polished", which = "cache")
#' im <- readRDS(file.path(cache, "im.rds"))
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
  log_file = NULL,
  keep_archives = 0L,
  force = FALSE,
  prune_parts = TRUE,
  polis_api_key = Sys.getenv("POLIS_API_KEY"),
  quiet = FALSE
) {
  ext <- match.arg(output_format)

  if (!isTRUE(nzchar(polis_api_key))) {
    cli::cli_abort(c(
      "x" = "POLIS API key is empty.",
      "i" = "Set {.envvar POLIS_API_KEY} or pass {.arg polis_api_key}."
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

  max_date <- as.Date(max_date)

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

    # Force re-pull: blow away the canonical file and the per-year
    # cache so the year workers start from Id = 0.
    if (isTRUE(force)) {
      if (file.exists(out_file)) {
        try(file.remove(out_file), silent = TRUE)
      }
      if (dir.exists(parts_dir)) {
        try(unlink(parts_dir, recursive = TRUE, force = TRUE), silent = TRUE)
      }
    }

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

    # If a pre-parallel run left a single canonical file with no per-
    # year parts, split it once so the workers can resume per-year.
    .polis_migrate_to_parts(out_file, parts_dir, ext, date_field)
    dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)

    # Ask POLIS for the total row count for the year-aligned range.
    # Powers the progress bar and lets us skip tables already up to
    # date without dispatching any workers.
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
        if (file.exists(s$part_file)) {
          tryCatch(
            nrow(.polis_io_read(s$part_file, ext)),
            error = function(e) 0L
          )
        } else {
          0L
        }
      },
      integer(1)
    ))

    if (!is.na(declared_total) && current_rows >= declared_total) {
      # Already complete. Only re-merge parts to canonical when the
      # canonical is missing or smaller than parts -- a prior refetch
      # may have padded the canonical with rows that aren't on disk in
      # any single part, and re-merging would silently overwrite those
      # extra rows with parts-only data. A canonical that is unreadable or
      # corrupt (e.g. a torn write or a bad manual conversion) is also
      # rebuilt from the intact parts.
      canonical_corrupt <- FALSE
      corrupt_env <- environment()
      out_rows <- if (file.exists(out_file)) {
        withCallingHandlers(
          tryCatch(
            nrow(.polis_io_read(out_file, ext)),
            error = function(e) {
              corrupt_env$canonical_corrupt <- TRUE
              0L
            }
          ),
          warning = function(w) {
            if (grepl("hash mismatch|corrupt", conditionMessage(w))) {
              corrupt_env$canonical_corrupt <- TRUE
              invokeRestart("muffleWarning")
            }
          }
        )
      } else {
        0L
      }
      if (canonical_corrupt && !isTRUE(quiet)) {
        cli::cli_alert_warning(
          "{.val {nm}}: canonical file corrupt; rebuilding from parts."
        )
      }
      if (
        !file.exists(out_file) || out_rows < current_rows || canonical_corrupt
      ) {
        .polis_merge_parts(parts_dir, out_file, ext, date_field)
      }
      if (isTRUE(prune_parts) && file.exists(out_file)) {
        .polis_prune_parts(parts_dir)
      }
      if (!isTRUE(quiet)) {
        cli::cli_alert_info(paste0(
          "Up to date (",
          .polis_pretty_num(current_rows),
          " rows). Skipping."
        ))
      }
      next
    }

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
            on_disk <- if (dir.exists(parts_dir)) {
              .polis_merge_parts(parts_dir, out_file, ext, date_field)
              if (file.exists(out_file)) {
                tryCatch(
                  nrow(.polis_io_read(out_file, ext)),
                  error = function(e) 0L
                )
              } else {
                0L
              }
            } else {
              0L
            }
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
                out_file
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
    .polis_merge_parts(parts_dir, out_file, ext, date_field)
    .polis_archive(out_file, polis_folder, stem, ext, keep_archives)

    # Completeness check across the full requested range -- not just the
    # newly-added window -- so prior silent drops in earlier sessions get
    # caught and refetched.
    if (isTRUE(auto_refetch) && file.exists(out_file)) {
      if (!isTRUE(quiet)) {
        cli::cli_alert_info(
          "Verifying completeness against POLIS (may take a moment)..."
        )
      }
      downloaded <- .polis_io_read(out_file, ext)
      if (!is.data.frame(downloaded) || !"Id" %in% names(downloaded)) {
        if (!isTRUE(quiet)) {
          cli::cli_alert_warning(
            "{nm}: no Id column on disk; skipping verification."
          )
        }
      } else {
        verify_min <- as.Date(min_date)
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
            cli::cli_alert_warning(paste0(
              nm,
              ": Id list fetch failed (",
              conditionMessage(e),
              "); skipping verification."
            ))
            NULL
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
                cli::cli_alert_warning(paste0(
                  nm,
                  ": refetch failed (",
                  conditionMessage(e),
                  ")."
                ))
                data.frame()
              }
            )
            if (nrow(refetched) > 0L) {
              combined <- .polis_dedup(
                dplyr::bind_rows(downloaded, refetched),
                id_col = "Id",
                date_col = date_field
              )
              .polis_io_write_atomic(combined, out_file, ext)
              .polis_archive(
                out_file,
                polis_folder,
                stem,
                ext,
                keep_archives
              )
              # Refresh per-year parts + meta sidecars so the next call
              # sees the refetched rows when computing current_rows.
              tryCatch(
                {
                  if (date_field %in% names(combined)) {
                    yrs2 <- as.integer(format(
                      as.Date(combined[[date_field]]),
                      "%Y"
                    ))
                    keep2 <- !is.na(yrs2)
                    if (any(keep2)) {
                      combined_yr <- combined[keep2, , drop = FALSE]
                      yrs2 <- yrs2[keep2]
                      dir.create(
                        parts_dir,
                        showWarnings = FALSE,
                        recursive = TRUE
                      )
                      for (yr in unique(yrs2)) {
                        part_df <- combined_yr[yrs2 == yr, , drop = FALSE]
                        part_file_yr <- file.path(
                          parts_dir,
                          sprintf("year_%d.%s", yr, ext)
                        )
                        .polis_io_write_part(
                          part_df,
                          part_file_yr,
                          ext,
                          date_field
                        )
                      }
                    }
                  }
                },
                error = function(e) {
                  cli::cli_alert_warning(paste0(
                    nm,
                    ": meta refresh after refetch failed (",
                    conditionMessage(e),
                    ")."
                  ))
                }
              )
              downloaded <- combined
              still_missing <- setdiff(canonical, downloaded$Id)
              if (length(still_missing) > 0L) {
                cli::cli_alert_warning(c(
                  "x" = paste0(
                    nm,
                    ": ",
                    length(still_missing),
                    " ID(s) still missing after refetch."
                  ),
                  "i" = paste0(
                    "First few: ",
                    paste(
                      utils::head(still_missing, 5),
                      collapse = ", "
                    )
                  )
                ))
              } else if (!isTRUE(quiet)) {
                cli::cli_alert_success(paste0(
                  nm,
                  ": verification + refetch complete."
                ))
              }
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

    # Drop the resume cache now that the canonical is a complete, deduped
    # checkpoint. The next run rebuilds it from the canonical, so this only
    # trades a re-split for parts that never carry stale cross-year duplicates.
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
