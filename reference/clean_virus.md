# Build the POLIS virus (positives) dataset from cleaned cases and ES

Constructs the combined positives/virus analytic table from the outputs
of
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
and
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
– it does not read a raw POLIS viruses table. Every poliovirus-positive
record (classification matching `WPV`/`VDPV` – i.e. `WPV`, `cVDPV`,
`aVDPV`, `iVDPV` or untyped `VDPV`) becomes one row, harmonised to a
shared schema and tagged by source:

- `surveillance_type` (`"human"` / `"environmental"`) and the finer
  `source` (the case `surveillance_type_name` – AFP / Community /
  Contact – or `"Environmental"`);

- `measurement` and `classification_all`: the analytic virus label in
  the shared `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary both cleaners
  emit;

- the case/sample geography (`country_iso3code`, `adm0`/`adm1`/`adm2` +
  GUIDs), `latitude`/`longitude`, the event `virus_date` (paralysis
  onset for cases, collection for ES) with `year_onset`/`month_onset`,
  and `notification_date`;

- `report_date`: the VDPV classification-change date for VDPV records,
  the notification date for WPV records;

- `emergence_group`, `nt_changes`, `virus_cluster`, `virus_is_orphan`
  (the orphan-isolate flag, `NA` for ES rows since POLIS carries it only
  on cases), and – when a `nopv_emergence` reference is supplied – the
  novel-OPV2 flag `nopv2`.

## Usage

``` r
clean_virus(
  cases = NULL,
  es = NULL,
  cfg = polis_active_config(),
  nopv_emergence = NULL,
  separate_rows = FALSE,
  verbose = TRUE
)
```

## Arguments

- cases:

  Optional cleaned AFP table (from
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)).
  Its poliovirus-positive rows become the human positives.

- es:

  Optional cleaned ES table (from
  [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)).
  Its poliovirus-positive rows become the environmental positives.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object. Defaults to
  [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)
  – the config most recently built by
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  this session – so a no-`cfg` call inherits the active session settings
  rather than fresh defaults.

- nopv_emergence:

  Optional reference of novel-OPV2 (nOPV2) emergence-group names: a
  character vector, or a data frame with an `emergence_group` column.
  When supplied, records whose `emergence_group` matches are flagged
  `nopv2`. Default `NULL` (no nOPV2 flag).

- separate_rows:

  If `TRUE`, co-detection records (a fused label such as
  `WPV1andcVDPV 2` or `VDPV12and3`) are split into one row per detected
  serotype, with `measurement`/`classification_all` set to the component
  label. Default `FALSE` (one row per positive record, co-detections
  kept as the fused label).

- verbose:

  Emit cli progress messages. Default `TRUE`.

## Value

A tibble of poliovirus positives, one row per positive case/sample,
columns ordered id -\> location -\> time -\> classification -\> dates
-\> other. When neither input has any positives, returns a 0-row,
0-column tibble.

## Examples

``` r
cases <- clean_afp(data.frame(
  Id = 1, Epid = "A-1", `Last Update Date` = "2024-03-01",
  `Paralysis Onset Date` = "2024-01-02", `Notification Date` = "2024-01-09",
  `Polio Virus Types` = "WILD1", Classification = "Confirmed (wild)",
  `Admin0 Name` = "NIGERIA", check.names = FALSE
), verbose = FALSE)
clean_virus(cases = cases, verbose = FALSE)
#> # A tibble: 1 × 11
#>   epid  month_onset year_onset notification_date vtype classification_all
#>   <chr>       <dbl>      <dbl> <date>            <chr> <chr>             
#> 1 A-1             1       2024 2024-01-09        WPV 1 WPV 1             
#> # ℹ 5 more variables: virus_date <date>, report_date <date>,
#> #   surveillance_type <chr>, measurement <chr>, source <chr>
```
