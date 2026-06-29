# Clean POLIS AFP case data

Standardises one raw POLIS case table and derives the case-level
analytic variables AFP surveillance relies on:

- canonical snake_case names (via the crosswalk + janitor) and
  merged-EPID remapping;

- parsed epidemiological/laboratory `*_date` columns;

- `year_onset` / `month_onset` from `paralysis_onset_date` (falling back
  to the stool-1 then notification year when onset is missing), and a
  numeric `age_months`;

- the standard onset-relative date intervals (`onset_to_notify`,
  `onset_to_invest`, `onset_to_stool1`, `onset_to_stool2`,
  `invest_to_stool1`, `stool1_to_stool2`, `notify_to_invest`,
  `onset_to_followup`), in days;

- the `stool1_missing` / `stool2_missing` / `stool_missing` flags;

- stool `timeliness` and the 60-day follow-up flags
  (`needs_60day_followup`, `got_60day_followup`, `followup_on_time`);

- the fused analytic classification `classification_all` (with its
  building blocks `vtype` / `vtype_fixed`) plus the Sabin flags `sabin1`
  / `sabin2` / `sabin3` and a recomputed `hot_case`;

- country-keyed enrichment from
  [`polis_country_lookup()`](https://truenomad.github.io/polished/reference/polis_country_lookup.md)
  – `country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2` – the
  `polio_type` serotype, and the surveillance AFP flags `afp_class`,
  `afp`, `npafp` and `pending_results`;

- normalised admin names and one row per POLIS `id` (latest by
  `last_update_date`).

The raw POLIS `classification`, `polio_virus_types`,
`vdpv_classifications`, `adequate_stool` and `paralysis_hot_case` fields
are kept as-is alongside the derived columns. Records sharing the
business key `epid` + `paralysis_onset_date` + `adm0` (the same case
re-entered under a new POLIS Id) are collapsed to the latest by
`last_update_date`; cases that share an EPID and country but differ in
onset date are treated as distinct and kept. A tripwire then flags any
`epid` + `adm0` still spanning multiple Ids to QA, never dropping it –
so a genuine reclassification or a same-EPID onset conflict surfaces for
review rather than vanishing.

## Usage

``` r
clean_afp(
  data,
  cfg = polis_active_config(),
  shape = NULL,
  impute_geo = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  A raw POLIS case data frame.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object. Defaults to
  [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)
  – the config most recently built by
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  this session – so a no-`cfg` call inherits the active session settings
  rather than fresh defaults. Supply `cfg$synonyms` to remap merged
  EPIDs and `cfg$qa` to route ambiguity flags.

- shape:

  Optional district shape that drives admin recovery. Either form works
  and a single input does everything:

  - a polygon layer (`spatial_global_adm2`, an `sf` object) – its long
    form is derived here (as
    [`process_spatial()`](https://truenomad.github.io/polished/reference/process_spatial.md)
    does) for the GUID/name reconcile via
    [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md),
    and its geometry drives coordinate recovery via
    [`impute_geo_from_coords()`](https://truenomad.github.io/polished/reference/impute_geo_from_coords.md)
    for cases still missing a district but carrying coordinates;

  - a long ADM2 attribute table (`spatial_adm2_long_shape`) – reconcile
    only, since it has no geometry for the coordinate step.

  Reconciliation adds a `geo_source` column. Default `NULL` (no
  shape-based recovery).

- impute_geo:

  If `TRUE` (default) cases still missing `adm1`/`adm2` (and their
  GUIDs) after reconciliation have them recovered from the EPID prefix
  via
  [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
  (self-reference + prefix matching, every row kept). Adds `*_source`
  provenance columns.

- verbose:

  Emit cli progress messages for each phase. Default `TRUE`.

## Value

A tibble of cleaned AFP records, one row per POLIS `id` (and at most one
per `epid` + `paralysis_onset_date` + `adm0` business key after the
duplicate collapse), with columns ordered id -\> location -\> time -\>
other. The canonical and derived columns (`year_onset`, `month_onset`,
`age_months`, the `*_to_*` intervals, `onset_date_quality`, `timeliness`
and the 60-day follow-up flags) are added only when their prerequisite
source columns are present in `data`, so a trimmed input yields a
correspondingly trimmed output rather than an error.

## Examples

``` r
raw <- data.frame(
  Id = c(1, 1, 2),
  Epid = c("A-1", "A-1", "B-2"),
  LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
  ParalysisOnsetDate = c("2024-01-02", "2024-01-02", "2024-02-03"),
  NotificationDate = c("2024-01-05", "2024-01-05", "2024-02-06"),
  Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
  check.names = FALSE
)
clean_afp(raw)
#> ℹ Standardising names on 3 rows
#> ✔ Standardised names on 3 rows [196ms]
#> 
#> ℹ Parsing dates and deriving onset/age/intervals/timeliness
#> ✔ Parsed dates and derived onset/age/intervals/timeliness [28ms]
#> 
#> ℹ Classifying virus type and case classification
#> ✔ Classified virus type and case classification [26ms]
#> 
#> ℹ Standardising admin names
#> ✔ Standardised admin names [15ms]
#> 
#> ℹ Recovering missing admin from the EPID
#> ✔ Recovered admin for 0 cases from the EPID [13ms]
#> 
#> ℹ Enriching with country groupings and AFP flags
#> ✔ Enriched with country groupings and AFP flags [15ms]
#> 
#> ℹ Deduplicating by id and finalising
#> ✔ Deduplicated by id and finalised [72ms]
#> 
#> ✔ Cleaned 2 AFP cases.
#> # A tibble: 2 × 10
#>      id epid  adm0    paralysis_onset_date month_onset year_onset
#>   <dbl> <chr> <chr>   <date>                     <dbl>      <dbl>
#> 1     1 A-1   NIGERIA 2024-01-02                     1       2024
#> 2     2 B-2   CHAD    2024-02-03                     2       2024
#> # ℹ 4 more variables: notification_date <date>, onset_date_quality <chr>,
#> #   onset_to_notify <dbl>, last_update_date <date>
```
