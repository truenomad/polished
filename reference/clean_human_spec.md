# Clean POLIS human specimen (laboratory) data

Standardises one raw POLIS lab-specimen table and derives the
specimen-level analytic variables surveillance relies on:

- canonical snake_case names (via the crosswalk + janitor);

- every collection/laboratory date parsed to `Date` and sanitised with
  the same "sensible date" rule
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
  uses – a value before the dawn of surveillance (`min_year`) or in the
  future is a data-entry error and is set to `NA`, never dropped (audit
  timestamps such as `last_update_date` stay ISO strings for the
  keep-latest dedup);

- `year_collection` / `month_collection` from the sanitised
  `date_stool_collected`, plus the lab-turnaround intervals (in days)
  `collect_to_lab`, `lab_to_culture`, `culture_to_itd`,
  `sent_to_seq_result` and `collect_to_seq` – the specimen timeliness
  measure (specimens carry no onset date, so this replaces clean_afp()'s
  onset-relative intervals);

- the AFP-style virus classification (via the shared lab-virus
  classifier, the same engine
  [`clean_es_classification()`](https://truenomad.github.io/polished/reference/clean_es_classification.md)
  uses): a normalised `virus_type` list, `vtype` and the fused
  `classification_all` label in the **same**
  `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary the AFP and ES cleaners emit,
  the per-serotype Sabin flags `sabin1`/`sabin2`/`sabin3`, the `npev` /
  `nvaccine` flags and the fused `ev_detect` flag;

- the specimen `adequate` flag (`1`/`0` from `adequate_specimen`),
  alongside the raw `specimen_stool_condition_name` and
  `adequate_specimen_with_condition`;

- country-keyed enrichment from
  [`polis_country_lookup()`](https://truenomad.github.io/polished/reference/polis_country_lookup.md)
  (`country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2`) and the
  `polio_type` serotype;

- the same applicable geography cleaning as
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md):
  normalised admin names, GUID reconciliation against `shape` and
  EPID-prefix admin recovery (both keyed on `year_collection`);
  coordinate recovery does not apply as specimens carry no coordinates;

- normalised admin names and one row per POLIS `id` (latest by
  `last_update_date`).

The raw POLIS `virus_types`, `vdpv_classification`, the per-serotype
result fields and the lab-result columns are kept as-is alongside the
derived columns. The business key `specimen_id` + `adm0` is asserted as
a tripwire: violations are flagged to QA, never dropped.

## Usage

``` r
clean_human_spec(
  data,
  cfg = polis_active_config(),
  shape = NULL,
  impute_geo = TRUE,
  cases = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A raw POLIS lab-specimen data frame.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object. Defaults to
  [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)
  – the config most recently built by
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  this session – so a no-`cfg` call inherits the active session settings
  rather than fresh defaults. Supply `cfg$qa` to route ambiguity flags.

- shape:

  Optional district shape used to reconcile admin names/GUIDs via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  (keyed on `year_collection`), exactly as
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
  uses it – a long ADM2 attribute table or the polygon layer (the
  polygon is expanded to its long form here). Default `NULL`.

- impute_geo:

  If `TRUE` (default) specimens still missing `adm1`/`adm2` (and their
  GUIDs) after reconciliation have them recovered from the EPID prefix
  via
  [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md),
  keyed on `year_collection`.

- cases:

  Optional cleaned AFP table (from
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)).
  A specimen reuses its parent case's EPID, so the case's
  fully-recovered geography is the authoritative source for a specimen
  that carries no district of its own: when supplied, the case
  `adm1`/`adm2` (and their GUIDs) fill blank specimen cells by exact
  EPID match (a fast direct join, before the specimen-internal prefix
  match). Default `NULL`.

- verbose:

  Emit cli progress messages for each phase. Default `TRUE`.

## Value

A tibble of cleaned specimen records, one row per POLIS `id`, with
columns ordered identically to
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
(id -\> location -\> time -\> classification -\> dates -\> other). The
derived columns (`year_collection`, `month_collection`,
`collect_to_lab`, `lab_to_culture`, `culture_to_itd`,
`sent_to_seq_result`, `collect_to_seq`, `virus_type`, `vtype`,
`classification_all`, `sabin1`/`sabin2`/`sabin3`, `npev`, `nvaccine`,
`ev_detect`, `adequate`) are added only when their prerequisite source
columns are present in `data`, so a trimmed input yields a
correspondingly trimmed output rather than an error.

## Examples

``` r
raw <- data.frame(
  Id = c(1, 1, 2),
  SpecimenId = c("S1", "S1", "S2"),
  Epid = c("A-1", "A-1", "B-2"),
  LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
  DateStoolCollected = c("2024-01-05", "2024-01-05", "2024-02-09"),
  VirusTypes = c("cVDPV2", "cVDPV2", NA),
  VdpvClassification = c("Circulating", "Circulating", NA),
  SpecimenStoolConditionName = c("Good", "Good", "Poor"),
  AdequateSpecimen = c("Yes", "Yes", "No"),
  Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
  check.names = FALSE
)
clean_human_spec(raw)
#> ℹ Standardising names on 3 rows
#> ✔ Standardised names on 3 rows [17ms]
#> 
#> ℹ Parsing dates and deriving collection vars + lab intervals
#> ✔ Parsed dates and derived collection vars + lab intervals [18ms]
#> 
#> ℹ Deriving virus classification and adequacy
#> ✔ Derived virus classification and adequacy [18ms]
#> 
#> ℹ Standardising admin names
#> ✔ Standardised admin names [12ms]
#> 
#> ℹ Recovering missing admin from the EPID
#> ✔ Recovered admin for 0 specimens from the EPID [11ms]
#> 
#> ℹ Enriching with country groupings
#> ✔ Enriched with country groupings [11ms]
#> 
#> ℹ Deduplicating by id and finalising
#> ✔ Deduplicated by id and finalised [23ms]
#> 
#> ✔ Cleaned 2 specimens.
#> # A tibble: 2 × 23
#>      id epid  adm0    month_collection year_collection virus_types virus_type
#>   <dbl> <chr> <chr>              <dbl>           <dbl> <chr>       <chr>     
#> 1     1 A-1   NIGERIA                1            2024 cVDPV2      cVDPV 2   
#> 2     2 B-2   CHAD                   2            2024 NA          NA        
#> # ℹ 16 more variables: vtype <chr>, sabin1 <int>, sabin2 <int>, sabin3 <int>,
#> #   npev <int>, nvaccine <int>, classification_all <chr>, ev_detect <int>,
#> #   polio_type <chr>, last_update_date <date>, date_stool_collected <date>,
#> #   specimen_id <chr>, vdpv_classification <chr>,
#> #   specimen_stool_condition_name <chr>, adequate_specimen <chr>,
#> #   adequate <int>
```
