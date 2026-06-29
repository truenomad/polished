# Build a POLIS pipeline configuration

Creates the settings object shared by the cleaners
([`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md),
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md),
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md),
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md))
and the orchestrator
([`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)).
Every field has a sensible default and can be overridden.

## Usage

``` r
polis_config(
  start_year = 2020,
  regions = c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO"),
  column_roles = NULL,
  synonyms = NULL,
  qa = NULL,
  population = NULL,
  worldpop = NULL,
  pop_years = NULL,
  shape = NULL,
  inputs = NULL,
  output_dir = NULL,
  cache_dir = NULL,
  parse_types = TRUE,
  drop_empty_cols = TRUE
)
```

## Arguments

- start_year:

  Earliest onset/collection year to retain (default `2020`).

- regions:

  WHO region codes the pipeline is scoped to. Cleaned rows are filtered
  to these regions (on the `who_region` column) by
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md);
  rows with no region value are kept. The default (all six WHO regions)
  is a no-op that retains every row.

- column_roles:

  Ordered named list of regex patterns that classify output columns into
  ordering groups. Columns are emitted group-by-group in this order;
  anything matching no pattern is treated as `other` and placed last.
  This is how
  [`order_columns()`](https://truenomad.github.io/polished/reference/order_columns.md)
  enforces id -\> location -\> time -\> other without hardcoding column
  names.

- synonyms:

  Optional EPID/geoplace synonym table for
  [`remap_synonyms()`](https://truenomad.github.io/polished/reference/remap_synonyms.md).
  `NULL` (default) makes synonym remapping a no-op.

- qa:

  Optional handle (path or list) where ambiguous-key flags are routed by
  [`flag_ambiguous()`](https://truenomad.github.io/polished/reference/flag_ambiguous.md).
  `NULL` (default) collects flags in-memory only.

- population:

  Optional under-15 population denominators used by
  [`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
  in
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md).
  Either a data frame or a path to one (read via the file extension);
  `NULL` (default) skips the rate indicators that need a denominator –
  unless a `population` *input* is supplied, in which case
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  uses the adm2 table
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
  produces as the denominator (so the pipeline makes its own).

- worldpop:

  Optional named list (`all`, `u5`, `u15`) of WorldPop sources passed to
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
  when a `population` input is present: each element a directory of
  annual GeoTIFFs (zonal-summed to `shape`) or a pre-extracted
  adm2-by-year table / path. `NULL` (default) runs the POLIS-only path.

- pop_years:

  Calendar years to retain when cleaning population (POLIS carries
  far-future projections). `NULL` (default) uses
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)'s
  default window.

- shape:

  Optional **already-processed** district shape passed to every cleaner
  as `shape =` for admin reconciliation (an `sf` polygon layer or a long
  ADM2 attribute table, or a path to one). `NULL` (default) disables
  shape-based admin recovery.

- inputs:

  The raw POLIS tables to clean, attached to the config so
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  can be called as `run_pipeline(cfg = cfg)` with no separate `inputs`
  argument. One of: a **directory path** to `raw_*` files; a **named
  list of file paths** (recognised names `afp`, `es`, `hum_spec`,
  `activity`, `subactivity`, `lqas`, `im`); or a **named list of data
  frames**. Paths are read on demand at run time – prefer them so the
  config stays a lightweight, serialisable manifest (`cfg$inputs$afp` is
  then the file path). `NULL` (default) means inputs are passed to
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  directly.

- output_dir:

  Optional directory to persist outputs to. When set,
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  writes the `polished_*` data files to its `data/` sub-directory and a
  `checks_*` workbook per dataset to its `checks/` sub-directory. `NULL`
  (default) returns the cleaned set without writing.

- cache_dir:

  Optional directory for the opt-in, content-addressed clean cache. When
  set,
  [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  caches each cleaned stream (`afp`, `es`, `hum_spec`, `sia`) keyed on a
  fingerprint of its source file (path + size + mtime), the relevant
  config, and a per-cleaner logic version. On a later run with an
  unchanged source, the cleaned table is read straight from the cache –
  skipping both the raw read and the clean. Delete the directory (or its
  `clean_*` files) to force a rebuild. `NULL` (default) disables
  caching.

- parse_types:

  If `TRUE` (default) each cleaner finishes by inferring column base
  types (character -\> numeric/integer/date/datetime/logical) via
  [`auto_parse_types()`](https://truenomad.github.io/polished/reference/auto_parse_types.md)
  (no factor conversion). Set `FALSE` to keep the raw character columns.

- drop_empty_cols:

  If `TRUE` (default) each cleaner drops columns that are entirely `NA`
  after cleaning. Note this makes the output schema depend on the data;
  set `FALSE` for a fixed column set across runs.

## Value

An object of class `polis_config` (a named list). Also registered as the
session-active config (see
[`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)).

## Examples

``` r
cfg <- polis_config(start_year = 2018)
cfg$column_roles
#> $id
#> [1] "^(id|epid)$"
#> 
#> $iso
#> [1] "^country_iso3code$"
#> 
#> $country
#> [1] "^country_actual$"
#> 
#> $geo_group
#> [1] "^(risk_group|epi_zones|epi_zones_v2)$"
#> 
#> $adm_name
#> [1] "^adm[0-9]$"
#> 
#> $adm_guid
#> [1] "^adm[0-9]_guid$"
#> 
#> $coord
#> [1] "^(latitude|longitude)$"
#> 
#> $onset_date
#> [1] "^(paralysis_onset_date|collection_date)$"
#> 
#> $onset_month
#> [1] "^(month_onset|month_collection)$"
#> 
#> $onset_year
#> [1] "^(year_onset|year_collection)$"
#> 
#> $age
#> [1] "^age_months$"
#> 
#> $core_dates
#> [1] "^(notification_date|investigation_date|stool1collection_date|stool2collection_date|followup_date)$"
#> 
#> $classification
#> [1] "^(classification|classification_all|vtype|vtype_fixed|polio_virus_types|vdpv_classifications|sabin[123]|hot_case|paralysis_hot_case|virus_types|virus_type|npev|nvaccine|ev_detect|polio_type|afp_class|afp|npafp|pending_results)$"
#> 
#> $indicators
#> [1] "^(onset_to_[a-z0-9]+|notify_to_invest|invest_to_stool1|stool1_to_stool2|onset_date_quality|timeliness|stool[12]_missing|stool_missing|adequate_stool|adequate_stool_with_condition|needs_60day_followup|got_60day_followup|followup_on_time)$"
#> 
#> $dates
#> [1] "^(date_.*|.*_date)$"
#> 
```
