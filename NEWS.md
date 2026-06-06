# polished 0.1.0

Initial release. Single-function (`get_polis_data()`) interface to the
WHO POLIS OData API. Designed as a clean-break replacement for the
older `polisapi` package, dropping every helper whose POLIS-side
behaviour proved unreliable in practice.

## What's in

* `get_polis_data()` — pulls one or many POLIS tables to disk and/or
  to memory. Handles:
  * Year-aligned date filters (POLIS rejects sub-year ranges).
  * Id-range pagination (POLIS rejects `$skip`, never returns
    `@odata.nextLink`).
  * Per-batch checkpointing into a per-year `.parts/` cache so a
    crashed run resumes at `max(Id)` on next call.
  * Optional parallel year-fetching via a PSOCK cluster with a live
    cli progress bar.
  * Post-download completeness verification + refetch of any missing
    rows by Id.
  * `tidypolis_style = TRUE` for the filename layout
    `tidypolis::preprocess_cdc()` expects.
* `polis_tables_mapping` — the eight supported tables, their
  endpoints, and the columns used for filtering.

## What's out (compared to polisapi)

* `get_polis_api_data()`, `get_polis_api_data_parallel()`,
  `update_polis_api_data()` — all replaced by `get_polis_data()`.
* `iterative_api_call()`, `construct_api_url()`,
  `process_api_response()`, `get_api_date_suffix()`,
  `save_polis_data()`, `write_log_file_api()`, `check_status_api()`,
  `check_tables_availability()`, `validate_polis_api_key()` — all
  internalised (the first three didn't work against POLIS once it
  stopped emitting `@odata.nextLink`).
