# Run the POLIS cleaning pipeline in memory

Cleans the raw POLIS tables supplied in `inputs` and returns the
canonical analytic set. Any subset of tables may be supplied; absent
tables are skipped. The virus (positives) table is *built* from the
cleaned AFP and ES streams via
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
whenever either is present – it is not read from `inputs`.

## Usage

``` r
run_pipeline(
  inputs = cfg$inputs,
  cfg = polis_active_config(),
  reconcile_with = NULL,
  output_dir = cfg$output_dir,
  formats = list(),
  refresh = FALSE
)
```

## Arguments

- inputs:

  The raw POLIS tables to clean; defaults to `cfg$inputs` so a
  fully-specified config can be run as `run_pipeline(cfg = cfg)`. Either
  a **named list** of raw data frames (recognised names `afp`, `es`,
  `hum_spec`, `activity`, `subactivity`, `lqas`, `im`) or a **path** to
  a directory of `raw_*` files (read on demand, each output then
  inheriting its source format). A raw `virus` table is not used –
  positives are derived from the cleaned `afp`/`es` outputs; `lqas`/`im`
  become district-year roll-ups via
  [`process_sia_quality()`](https://truenomad.github.io/polished/reference/process_sia_quality.md).

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object; defaults to the session-active config
  ([`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)).
  Its `shape` and `population` handles drive admin reconciliation and
  the indicators step respectively.

- reconcile_with:

  Optional named list of full-pull data frames (same keys as `inputs`)
  used to prune deleted/merged `id`s via
  [`reconcile()`](https://truenomad.github.io/polished/reference/reconcile.md).

- output_dir:

  Directory to persist outputs to; defaults to `cfg$output_dir` (set it
  once on the config). When non-`NULL` the `polished_*` data files are
  written to its `data/` sub-directory and a `checks_<dataset>.xlsx`
  workbook per dataset to its `checks/` sub-directory. `NULL` returns
  the cleaned set without writing anything.

- formats:

  Optional named list mapping output keys (`afp`, `es`, `hum_spec`,
  `sia`, `lqas`, `im`) to a file extension, so an output inherits the
  format of the raw file it derives from. Unmapped/derived outputs fall
  back to `qs2`. Only consulted when `output_dir` is set.

- refresh:

  If `TRUE`, ignore any existing cache and re-run every step from
  scratch, overwriting the cache and the output files (the fresh results
  are still cached for next time). Default `FALSE` (reuse caches where
  valid).

## Value

A named list holding any of the cleaned tibbles `afp`, `es`, `hum_spec`,
`sia`, `virus`, the SIA-quality roll-ups `lqas` / `im` (each a list of
`lots`/`district`/`meta`), plus `indicators` (the
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
result list) when AFP cases are present. Returned invisibly when
`output_dir` is set.

## Details

Each cleaner receives `cfg$shape` (an already-processed district shape)
for admin reconciliation, and – when AFP cases are present – the
surveillance indicators are computed via
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
using `cfg$population` as the under-15 denominator.

Missing admin GUIDs are always backfilled across the cleaned streams
from their pooled consensus: a district whose `adm1_guid` / `adm2_guid`
is blank in one stream inherits it from another that carries it, matched
on admin name first and `admin{1,2}shape_id` as a fallback. Only blanks
are filled; existing GUIDs are never changed.

## Examples

``` r
afp <- data.frame(
  Id = 1, Epid = "A-1", `Last Update Date` = "2024-03-01",
  `Date Onset` = "2024-01-02", `Admin0 Name` = "NIGERIA",
  check.names = FALSE
)
out <- run_pipeline(list(afp = afp))
#> 
#> ── Cleaning AFP cases ──────────────────────────────────────────────────────────
#> ℹ Standardising names on 1 rows
#> ✔ Standardised names on 1 rows [25ms]
#> 
#> ℹ Parsing dates and deriving onset/age/intervals/timeliness
#> ✔ Parsed dates and derived onset/age/intervals/timeliness [19ms]
#> 
#> ℹ Classifying virus type and case classification
#> ✔ Classified virus type and case classification [13ms]
#> 
#> ℹ Standardising admin names
#> ✔ Standardised admin names [13ms]
#> 
#> ℹ Recovering missing admin from the EPID
#> ✔ Recovered admin for 0 cases from the EPID [13ms]
#> 
#> ℹ Enriching with country groupings and AFP flags
#> ✔ Enriched with country groupings and AFP flags [14ms]
#> 
#> ℹ Deduplicating by id and finalising
#> ✔ Deduplicated by id and finalised [16ms]
#> 
#> ✔ Cleaned 1 AFP cases.
#> ! "afp": no year_onset column; year filter skipped.
#> 
#> ── Building virus / positives ──────────────────────────────────────────────────
#> ℹ Extracting poliovirus positives from cases and ES
#> ℹ No poliovirus positives found.
#> ℹ Extracting poliovirus positives from cases and ES
#> ✔ Extracted poliovirus positives from cases and ES [8ms]
#> 
#> 
#> ── Computing surveillance indicators ───────────────────────────────────────────
#> ! Indicators skipped: Assigned data `.as_int(df[[m$year]])` must be compatible with existing data. ✖ Existing data has 1 row. ✖ Assigned data has 0 rows. ℹ Row updates require a list value. Do you need `list()` or `as.list()`? Caused by error in `vectbl_recycle_rhs_rows()`: ! Can't recycle input of size 0 to size 1.
#> ✔ Produced 1 output: "afp".
names(out)
#> [1] "afp"
```
