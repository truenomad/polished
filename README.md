# polished <img src="man/figures/logo.png" align="right" height="138" alt="" />

End-to-end toolkit for working with WHO POLIS data in R: pull the raw
tables, then turn them into analytic-ready files for AFP surveillance,
SIA campaigns, environmental sampling, and virus positives.

Two exported entry points:

- **`get_polis_data()`** — downloads the eight POLIS OData tables and
  caches them on disk. Handles POLIS's actual server behaviour
  (year-aligned date filters, Id-range pagination, `$top = 2000` cap,
  no `@odata.nextLink`) instead of fighting it.
- **`preprocess()`** — five-step cleaning pipeline that takes the raw
  tables and produces the analytic linelists / aggregates the rest of
  the polio surveillance workflow expects.

The preprocessing logic is adapted from the older `tidypolis` pipeline
(which I helped restructure). polished's version is more modular,
doesn't depend on the EDAV file share, and can run on any machine
with the package's static input files in place — no shared-drive
mount required.

## Install

```r
# install.packages("remotes")
remotes::install_github("truenomad/polished")
Sys.setenv(POLIS_API_KEY = "<your key>")
```

`POLIS_API_KEY` must be set in your `.Renviron` or via `Sys.setenv()`
before any download call.

## Quickstart

```r
library(polished)

# 1. Download the eight raw tables (parallel year-fetch)
get_polis_data(
  polis_folder = "data/polis",
  workers = parallel::detectCores() - 1L
)

# 2. Turn them into analytic-ready outputs
preprocess(polis_folder = "data/polis")
```

That's the full loop. Both steps write to disk by default and return
invisibly — pass `return = "df"` to `get_polis_data()` if you want a
data.frame back.

## How long does this take?

`case` is the heaviest table — **~1.99 million rows as of May 2026**
— and on a good connection it pulls end-to-end in roughly **35
minutes** with `workers = 1`. The other seven tables are much smaller
(`virus` ~347K, `lqas` ~85K, `im` ~62K, etc.), single-digit minutes
each.

Going parallel is where the wall-clock win really shows up. Each
calendar year of `case` is an independent Id-range walk, so a
`workers = parallel::detectCores() - 1L` run dispatches one year per
worker, fetches them simultaneously, and stitches the per-year part
files into the canonical `case.rds` at the end. On an 11-core machine
the same `case` pull drops from ~35 min to **under 5 min**.

```r
# One table, full history, parallel
get_polis_data(
  tables  = "case",
  workers = parallel::detectCores() - 1L
)

# All eight tables, parallel, just write to disk
get_polis_data(
  workers = parallel::detectCores() - 1L,
  return  = "paths"
)
```

Yearly batching is what makes resume cheap too — if the network drops
at year 2019, the next call resumes from year 2019's `max(Id)`
without re-fetching 2000–2018.

## Use cases

### Just write to disk

```r
get_polis_data(tables = "im")  # default return = "invisible"
```

The file lands under `<polis_folder>/data/im.rds`.

### One table to data.frame

```r
df <- get_polis_data(tables = "im", return = "df")
```

### Multiple tables in parallel, results in a named list

```r
out <- get_polis_data(
  tables  = c("im", "lqas", "virus"),
  workers = parallel::detectCores() - 1L,
  return  = "list"
)
out$virus  # ~347K rows
out$lqas   # ~85K rows
out$im     # ~62K rows
```

### All eight tables, into a project folder

```r
get_polis_data(
  polis_folder = "data/polis",
  workers      = parallel::detectCores() - 1L,
  return       = "paths"
)
# Returns a named vector of file paths, no data loaded into RAM.
```

### Filter by region or country

```r
get_polis_data(tables = "case", region = "AFRO")
get_polis_data(tables = "case", country_code = "NGA")
```

`region` takes WHO region codes (`Global`, `AFRO`, `AMRO`, `EMRO`,
`EURO`, `SEARO`, `WPRO`). `country_code` takes ISO3.

### Force a clean re-pull

By default, every call resumes from whatever's already on disk. Pass
`force = TRUE` to discard the cache for the selected tables and start
fresh:

```r
get_polis_data(tables = "case", force = TRUE)
```

### Other formats

```r
get_polis_data(tables = "im", output_format = "parquet")  # also rda, csv
```

## Full argument reference

| Argument | What it does | Default |
|---|---|---|
| `tables` | Character vector of table names, `NULL` for all 8 | `NULL` |
| `min_date` / `max_date` | Date range (year-aligned, see [POLIS quirks](#how-polished-handles-polis-quirks)) | `2000-01-01` / `Sys.Date()` |
| `region` | WHO region filter | `"Global"` |
| `country_code` | ISO3 filter | `NULL` |
| `polis_folder` | Where part files + canonical files live | per-user cache dir |
| `return` | `"invisible"` / `"auto"` / `"df"` / `"list"` / `"paths"` | `"invisible"` |
| `output_format` | `rds` / `rda` / `csv` / `parquet` | `rds` |
| `workers` | `> 1` opts into a PSOCK cluster | `1` |
| `auto_refetch` | Metadata-aware diff + selective refetch | `TRUE` |
| `log_file` | Per-batch log `.rds` | `NULL` |
| `keep_archives` | Timestamped backups under `data/archive/` | `0` |
| `force` | Delete `.parts/` + canonical file, start fresh | `FALSE` |
| `polis_api_key` | API key | `Sys.getenv("POLIS_API_KEY")` |
| `quiet` | Suppress headers / progress bar / alerts | `FALSE` |

Run `?get_polis_data` for the full prose. The eight supported tables
are listed in `polis_tables_mapping`:

```r
polished::polis_tables_mapping
#             table_name    endpoint     date_field
# 1                virus       Virus    UpdatedDate
# 2                 case        Case LastUpdateDate
# 3       human_specimen LabSpecimen LastUpdateDate
# 4 environmental_sample   EnvSample LastUpdateDate
# 5             activity    Activity LastUpdateDate
# 6         sub_activity SubActivity    UpdatedDate
# 7                 lqas        Lqas          Start
# 8                   im          Im    PublishDate
```

## How resume works

The function writes per-year part files under

```
<polis_folder>/data/.parts/<table_name>/year_YYYY.rds
```

Each part has a tiny `year_YYYY.meta.rds` sidecar caching row count
and min/max Id, so subsequent runs can compute totals and detect gaps
without re-reading every part. After all years finish, the parts are
merged into the canonical `<polis_folder>/data/<table_name>.<ext>`.

The part files persist on purpose. If a run dies mid-fetch (network
blip, Ctrl-C, OOM), the next call reads `max(Id)` from each part and
continues from there with `Id gt <max>`. Worst-case work lost: the
in-flight 2K-row batch (the worker flushes after every batch).

**Do not delete the `.parts/` directory between runs** — it's the
resume marker. Use `force = TRUE` if you really want to start over.

```r
# This is the bug, not a feature:
file.remove(".../data/case.rds")   # throws away resume progress

# This is correct:
get_polis_data(tables = "case", force = TRUE)
```

## Parallelism

`workers > 1` dispatches one year per worker via
`parallel::makePSOCKcluster()` — the same transport on Windows,
macOS, and Linux, so anyone can opt in. Each worker runs the Id-range
walk for its year independently and writes to its own part file. The
master polls the part files between worker results so the cli
progress bar updates in real time.

```r
# Single-process, live per-batch progress
get_polis_data(tables = "case", workers = 1)

# 11-way parallel year-fetch
get_polis_data(tables = "case", workers = 11)
```

PSOCK workers spawn fresh R sessions and load polished via
`library()`, so the package must be **installed**. A
`devtools::load_all()` session won't work in parallel mode — pre-flight
will abort with a clear message pointing you at `workers = 1L`.

## Auto-refetch

By default (`auto_refetch = TRUE`), after the workers finish each
table the function uses the meta sidecars to compare the on-disk row
count against POLIS's `@odata.count`. If they match, the table is
considered complete and no further network calls are made.

If they don't match — e.g. a transient HTTP error swallowed a page —
the function issues a `$select=Id` Id-paginated probe for the same
year-aligned filter, diffs against what's on disk, and refetches any
missing IDs in chunks via `Id in (...)`. Set `auto_refetch = FALSE`
if you'd rather trust whatever is on disk.

## How polished handles POLIS quirks

POLIS's OData service has several non-standard behaviours that
silently lose data if you treat it like a regular OData endpoint:

* **`$skip` is rejected (HTTP 400) at every value.** polished
  paginates with `$orderby=Id&$filter=... and Id gt <last>` instead.
* **`@odata.nextLink` is never returned**, so a naive walker stops
  at the first `$top` (= 2000) rows. polished drives pagination
  itself via the Id-range trick.
* **`$top` caps at 2000.** Any larger value returns HTTP 400. Every
  page in polished is exactly 2K rows.
* **Date filters with `le YYYY-MM-DD` only honour `MM-DD = 12-31`.**
  Sub-year ranges silently return 0 rows. polished year-aligns both
  bounds before sending the request.
* **`CaseDate`, `VirusDate`, and `CollectionDate` are NULL for many
  legacy records.** polished filters on each table's always-populated
  "update" column (`LastUpdateDate` / `UpdatedDate` / `Start` /
  `PublishDate` — see `polis_tables_mapping$date_field`).

## Cache management

The default `polis_folder` is `tools::R_user_dir("polished", "cache")` —
the platform-standard per-user cache location. Persistent across
sessions, so an incremental update next week resumes correctly.

```r
# Where's my cache?
tools::R_user_dir("polished", "cache")

# Clear the whole cache (forces fresh full re-pulls of everything)
unlink(tools::R_user_dir("polished", "cache"), recursive = TRUE)

# Or clear just one table
unlink(file.path(
  tools::R_user_dir("polished", "cache"), "data", ".parts", "case"
), recursive = TRUE)
```

For project-attached data, pass an explicit `polis_folder`:

```r
get_polis_data(polis_folder = "data/polis", ...)
```

## Troubleshooting

**`POLIS API key is empty`.** Set `POLIS_API_KEY` in your `.Renviron`
or pass `polis_api_key = "..."` directly.

**`get_polis_data(tables = "...")` returns far fewer rows than
expected.** Make sure `auto_refetch = TRUE` (the default) — the
post-download diff catches silent drops. If the refetch couldn't
recover missing IDs, your API key may be rate-limited or POLIS may
be returning HTML interstitials. Re-run; the part files preserve
progress.

**`fetch failed at Id gt N`.** The error message lists how many rows
were checkpointed and tells you to re-run, not delete the file. Just
re-run `get_polis_data(tables = "...")` — it will resume.

**`Parallel mode requires polished to be installed`.** You're in a
`devtools::load_all()` session and asked for `workers > 1`. Either
run `devtools::install()` once, or drop to `workers = 1L`.

## Preprocessing

After `get_polis_data()` has written the eight POLIS tables to
`<polis_folder>/data/`, `preprocess()` turns them into the analytic-ready
files downstream code (e.g. `sirfunctions::get_all_polio_data()`) expects.
It runs a five-step pipeline — basic cleaning + crosswalk, AFP analytic
dataset, SIA analytic dataset, environmental-samples dataset, and the
combined virus positives dataset — and writes the outputs to
`<polis_folder>/data/Core_Ready_Files/`.

```r
polished::preprocess(
  polis_folder = "data/polis",
  output_format = "rds",
  archive = TRUE
)
```

`preprocess()` expects a small set of reference files alongside the
downloaded tables:

```
<polis_folder>/
  misc/
    crosswalk.rds         # column rename table (API_Name -> Web_Name)
    env_sites.rds         # environmental surveillance sites
    global.dist.rds       # district shapefile
    global.ctry.rds       # country shapefile
    nopv_emg.table.rds    # nOPV2 emergence groups
  data/
    core_files_to_combine/
      afp_linelist_2001-01-01_2012-12-31.rds
      afp_linelist_2013-01-01_2016-12-31.rds
      afp_linelist_2017-01-01_2019-12-31.rds
      other_surveillance_type_linelist_2016_2016.rds
      other_surveillance_type_linelist_2017_2019.rds
      sia_2000_2019.rds
```

Run `?preprocess` for the full argument reference.

> **Acknowledgement.** The preprocessing pipeline implemented in
> `preprocess()` is adapted from the
> [`tidypolis`](https://github.com/CDCgov/tidypolis) package
> (MIT-licensed), which I helped develop as one of its contributors. The
> original column-crosswalk, GUID-fixing, SIA-clustering, and
> metadata-comparison logic are all from that work; `polished`
> reorganises it into a local-only entry point, drops the Azure / EDAV
> code paths, and makes the `mutate_at()` selectors resilient to POLIS
> schema drift.

## License

MIT.
