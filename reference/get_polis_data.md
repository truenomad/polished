# Download POLIS tables

The canonical entry point for fetching data from the POLIS OData
service. Works around three POLIS quirks that make naive OData clients
silently lose data:

- **Date filters are year-aligned only.** POLIS only honours
  `field le YYYY-12-31` bounds; any sub-year `le` returns 0 rows. The
  function aligns `min_date` / `max_date` to year boundaries before
  building any filter.

- **No `@odata.nextLink` and `$skip` is rejected.** POLIS caps `$top` at
  2000 and refuses to paginate via the OData standard mechanism. The
  only way to walk past 2000 rows is Id-range pagination:
  `$orderby=Id&$top=2000&$filter=... and Id gt <last>`.

- **The clinical date columns (`CaseDate`, `VirusDate`, ...) have NULL
  coverage** for historical records. The function filters on the table's
  "update" column (`LastUpdateDate` / `UpdatedDate` / `Start` /
  `PublishDate`) which probes have confirmed is 100%-populated.

## Usage

``` r
get_polis_data(
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
)
```

## Arguments

- tables:

  Optional character vector of table names (see
  [polis_tables_mapping](https://truenomad.github.io/polished/reference/polis_tables_mapping.md)
  for the supported set, e.g. `"case"`, `"virus"`, `"population"`).
  `NULL` (default) downloads every table in the catalogue. Unknown names
  abort with a list of valid names. The `population` reference table has
  no update date, so `min_date`/`max_date`/`region` do not apply to it –
  it is pulled whole.

- min_date:

  Earliest date to fetch. Defaults to `"2000-01-01"`. POLIS rejects
  sub-year date ranges, so this is aligned to January 1 of its year.

- max_date:

  Latest date to fetch. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). Aligned to
  December 31 of its year.

- region:

  WHO region filter (`"Global"` (default), `"AFRO"`, `"AMRO"`, `"EMRO"`,
  `"EURO"`, `"SEARO"`, `"WPRO"`).

- country_code:

  Optional ISO3 country code (e.g. `"NGA"`). Adds an
  `and CountryISO3Code eq '<code>'` clause. Default `NULL` (no country
  filter).

- polis_folder:

  Root folder for cached data. Files land under `<polis_folder>/`.
  Default `tools::R_user_dir("polished", which = "cache")` – the
  standard per-user cache location, persistent across sessions so
  incremental updates "just work". Pass an explicit path to keep data
  alongside a project.

- output_format:

  Output format. One of `"rds"` (default), `"rda"`, `"csv"`,
  `"parquet"`, `"qs2"`. `"parquet"` requires the `arrow` package;
  `"qs2"` requires the `qs2` package.

- workers:

  Number of parallel workers. `1` (default) runs a sequential loop with
  a live per-batch progress bar. `> 1` opts into a PSOCK cluster that
  dispatches one year per worker; pass e.g.
  `parallel::detectCores() - 1L` to use most cores.

- auto_refetch:

  If `TRUE` (default), run the metadata-aware verification + selective
  refetch at the end of each table. Set to `FALSE` to trust whatever is
  on disk.

- log_file:

  Optional path to a per-batch log file (`.rds`). Default `NULL`.

- keep_archives:

  When `> 0`, on each save also writes a timestamped copy under
  `archive/` and prunes older copies. Default `0` (no archive).

- force:

  If `TRUE`, deletes the `.parts/<table>/` directory and the canonical
  file for each selected table before running – forces a fresh full
  re-pull instead of resuming. Default `FALSE`.

- prune_parts:

  If `TRUE` (default), deletes the `.parts/<table>/` resume cache after
  the canonical file has been written and verified. The canonical is a
  complete, Id-deduped checkpoint, so the next run rebuilds the parts
  from it (re-bucketing each row into its current `date_field` year).
  This keeps the parts free of stale cross-year duplicate copies that
  otherwise accumulate when a record's update date crosses a year
  boundary between runs – which in turn keeps the "already up to date"
  short-circuit honest – at the cost of re-splitting the canonical on
  the next run. Incremental resume still works. Set to `FALSE` to retain
  the parts for the fastest possible resume (at the risk of the parts
  row count drifting above the true distinct total over many incremental
  re-pulls).

- polis_api_key:

  API key. Defaults to `Sys.getenv("POLIS_API_KEY")`.

- quiet:

  Suppress headers, progress bars, and the info alert. Default `FALSE`.

## Value

`NULL`, invisibly. `get_polis_data()` is called purely for its side
effect: each selected table is written to
`<polis_folder>/<table_name>.<ext>` (plus a `.parts/<table_name>/`
resume cache). The data is never loaded into memory, so a
multi-million-row pull cannot inflate your session. Read a table back
from disk yourself when you need it, e.g.
`readRDS(file.path(polis_folder, "im.rds"))`.

## Details

**Cache layout.** Each table is fetched into a per-year part file under
`<polis_folder>/.parts/<stem>/year_YYYY.<ext>`, with a tiny
`year_YYYY.meta.rds` sidecar capturing row count and min/max Id. After
all years finish the parts are merged into the canonical
`<polis_folder>/<stem>.<ext>`. The `<stem>` is the table's `raw_*` name
(e.g. `case` is written as `raw_afp`; see
[polis_tables_mapping](https://truenomad.github.io/polished/reference/polis_tables_mapping.md)),
so the download and cleaning halves share one naming convention. By
default (`prune_parts = TRUE`) the parts are deleted once the canonical
is written and the next call rebuilds them from the canonical; pass
`prune_parts = FALSE` to keep them on disk so the next call can resume
per-year from `max(Id)` without the re-split. Either way the next call
resumes per-year from `max(Id)` without re-fetching. To force a clean
re-pull, pass `force = TRUE` or delete `.parts/`.

Files written by older versions under the bare `<table_name>` name (e.g.
`case.<ext>`) are renamed to their `raw_*` stem in place on the next run
– no re-download.

**Resume semantics.** The part files ARE the resume marker. If a
previous run died (network blip, Ctrl-C, OOM), the next call picks up at
`Id gt max(Id_in_part)` for each year. Worst case lost work: the most
recent in-flight batch.

**Parallelism.** Each calendar year between `min_date` and `max_date` is
an independent Id-range walk. With `workers > 1`, years are dispatched
across a
[`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html)
cluster (the same transport on Windows, macOS, and Linux, so any user
can opt in); a live cli progress bar polls the part files between socket
reads so you see rows accumulate across workers in real time. With
`workers = 1`, a single sequential loop drives the bar per 2K-row batch.
PSOCK workers need `polished` installed in their library path –
`devtools::load_all()` is not enough.

**Auto-refetch.** When `auto_refetch = TRUE` (default) the function uses
the meta sidecars to detect gaps cheaply (row-count and Id-range
mismatch against POLIS's `@odata.count`). Only when a gap is detected
does it issue a `$select=Id` probe and refetch missing rows via OData
`Id in (...)` chunks. Set to `FALSE` to skip the post-download check
entirely.

## See also

[polis_tables_mapping](https://truenomad.github.io/polished/reference/polis_tables_mapping.md)
for the table catalogue.

## Examples

``` r
if (FALSE) { # \dontrun{
# Pull one table into the default per-user cache
get_polis_data(tables = "im")

# Read it back from disk when you need it
cache <- tools::R_user_dir("polished", which = "cache")
im <- readRDS(file.path(cache, "im.rds"))

# The whole catalogue in parallel into a project folder
get_polis_data(
  polis_folder = "data/polis",
  workers = parallel::detectCores() - 1L
)
} # }
```
