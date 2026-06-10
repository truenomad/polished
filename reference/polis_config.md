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
  folders = NULL,
  column_roles = NULL,
  seed = 1234L,
  synonyms = NULL,
  qa = NULL,
  parse_types = TRUE,
  drop_empty_cols = TRUE
)
```

## Arguments

- start_year:

  Earliest onset/collection year to retain (default `2020`).

- regions:

  Valid WHO region codes used for optional region-scoped output folders
  (default the six WHO regions).

- folders:

  Named list of pipeline folder names (not full paths). Override
  individual entries to relocate outputs; unspecified entries keep
  defaults.

- column_roles:

  Ordered named list of regex patterns that classify output columns into
  ordering groups. Columns are emitted group-by-group in this order;
  anything matching no pattern is treated as `other` and placed last.
  This is how
  [`order_columns()`](https://truenomad.github.io/polished/reference/order_columns.md)
  enforces id -\> location -\> time -\> other without hardcoding column
  names.

- seed:

  Integer seed for any clustering/sampling step, for reproducible runs
  (default `1234`).

- synonyms:

  Optional EPID/geoplace synonym table for
  [`remap_synonyms()`](https://truenomad.github.io/polished/reference/remap_synonyms.md).
  `NULL` (default) makes synonym remapping a no-op.

- qa:

  Optional handle (path or list) where ambiguous-key flags are routed by
  [`flag_ambiguous()`](https://truenomad.github.io/polished/reference/flag_ambiguous.md).
  `NULL` (default) collects flags in-memory only.

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

An object of class `polis_config` (a named list).

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
