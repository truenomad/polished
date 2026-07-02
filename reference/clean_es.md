# Clean POLIS environmental surveillance data

Standardises one raw POLIS environmental-samples table and derives the
sample-level analytic variables ES surveillance relies on:

- canonical snake_case names (via the crosswalk + janitor);

- every collection/laboratory date parsed to `Date` and sanitised with
  the same "sensible date" rule clean_afp() uses – a value before the
  dawn of surveillance (`min_year`) or in the future is a data-entry
  error and is set to `NA`, never dropped (audit timestamps such as
  `last_update_date` stay ISO strings for the keep-latest dedup);

- `year_collection` / `month_collection` from the sanitised
  `collection_date`;

- the AFP-style virus classification (via
  [`clean_es_classification()`](https://truenomad.github.io/polished/reference/clean_es_classification.md)):
  a normalised `virus_type` list, `vtype` and the fused
  `classification_all` label in the **same**
  `WPV`/`cVDPV`/`aVDPV`/`iVDPV` vocabulary the AFP cleaner emits (so one
  `grepl("WPV|cVDPV", classification_all)` works across both streams),
  the per-serotype Sabin flags `sabin1`/`sabin2`/`sabin3`, the `npev`
  and `nvaccine` (nOPV2) flags and the fused `ev_detect` "anything
  detected" flag;

- the same geography cleaning as
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md):
  normalised admin names, canonicalised admin GUIDs (braces stripped,
  lower-cased, so they are join-ready with the spatial layer), a
  title-cased `site` label, and – when a `shape` is supplied –
  admin-GUID reconciliation and coordinate-based admin recovery (all
  keyed on `year_collection`);

- country-keyed enrichment from
  [`polis_country_lookup()`](https://truenomad.github.io/polished/reference/polis_country_lookup.md)
  – `country_actual`, `risk_group`, `epi_zones` / `epi_zones_v2` – and
  the `polio_type` serotype (the ES counterpart of the AFP enrichment;
  the case-classification AFP flags do not apply to environmental
  samples);

- one row per POLIS `id` (latest by `last_update_date`).

The raw POLIS `virus_types`, `vdpv_classifications`, the per-serotype
`vaccine*`/`vdpv*`/`wild*` fields and `sample_condition` are kept as-is
alongside the derived columns. The business key `sample_id` + `adm0`
(the ES analogue of the AFP `epid` + `adm0` key, `sample_id` being the
EPID-equivalent sample identifier) is asserted as a tripwire: violations
are flagged to QA, never dropped. (Unlike AFP, a sample can legitimately
yield several virus detections, so this key is not collapsed.)

## Usage

``` r
clean_es(
  data,
  cfg = polis_active_config(),
  shape = NULL,
  impute_geo = TRUE,
  sites = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A raw POLIS environmental-samples data frame.

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
  uses it. Either a long ADM2 attribute table
  (`spatial_adm2_long_shape`) or the polygon layer
  (`spatial_global_adm2`). A polygon is expanded to its long form here
  to drive the GUID reconcile and *also* drives coordinate-based admin
  recovery: samples still missing `adm1`/`adm2` (or their GUIDs) but
  carrying site coordinates have their admin recovered by a
  point-in-polygon join via
  [`impute_geo_from_coords()`](https://truenomad.github.io/polished/reference/impute_geo_from_coords.md)
  (the ES counterpart of AFP EPID-prefix recovery; ES samples carry no
  geocoded EPID). Default `NULL` (no shape-based recovery).

- impute_geo:

  If `TRUE` (default) samples still missing `adm2_guid` after any
  shape-based recovery have their admin chain borrowed from other
  samples at the same site via the self-reference fill (see details);
  only sites that map unambiguously to one district are used, so
  conflicting sites are left flagged rather than guessed. Needs no
  shape, so it runs standalone. Adds a `geo_source` of `"site_match"` to
  filled rows.

- sites:

  Optional reference list of known environmental site names (a data
  frame with a `site_name` column, or a character vector). When
  supplied, sites absent from it are flagged via
  [`validate_es_sites()`](https://truenomad.github.io/polished/reference/validate_es_sites.md).
  Default `NULL` (no site validation).

- verbose:

  Emit cli progress messages for each phase. Default `TRUE`.

## Value

A tibble of cleaned ES records, one row per POLIS `id`, with columns
ordered identically to
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
(id -\> location -\> time -\> classification -\> dates -\> other). The
derived columns (`year_collection`, `month_collection`, `vtype`,
`classification_all`, `sabin1`/`sabin2`/ `sabin3`, `npev`, `nvaccine`,
`ev_detect`) are added only when their prerequisite source columns are
present in `data`, so a trimmed input yields a correspondingly trimmed
output rather than an error.

## Examples

``` r
raw <- data.frame(
  Id = c(1, 1, 2),
  EnviroSampleId = c("E1", "E1", "E2"),
  LastUpdateDate = c("2024-01-01", "2024-03-01", "2024-02-01"),
  CollectionDate = c("2024-01-05", "2024-01-05", "2024-02-09"),
  VirusTypes = c("cVDPV2", "cVDPV2", NA),
  VACCINE1 = c("No", "No", "Yes"),
  IsNPEV = c("No", "No", "Yes"),
  Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
  check.names = FALSE
)
clean_es(raw)
#> ℹ Standardising names on 3 rows
#> ✔ Standardised names on 3 rows [21ms]
#> 
#> ℹ Parsing dates and deriving year/month of collection
#> ✔ Parsed dates and derived year/month of collection [21ms]
#> 
#> ℹ Deriving virus-detection flags
#> ✔ Derived virus-detection flags [17ms]
#> 
#> ℹ Standardising admin names
#> ✔ Standardised admin names [20ms]
#> 
#> ℹ Recovering missing admin from same-site samples
#> ✔ Recovered admin for 0 samples from same-site records [14ms]
#> 
#> ℹ Enriching with country groupings
#> ✔ Enriched with country groupings [15ms]
#> 
#> ℹ Deduplicating by id and finalising
#> ✔ Deduplicated by id and finalised [24ms]
#> 
#> ✔ Cleaned 2 ES samples.
#> # A tibble: 2 × 20
#>      id adm0    collection_date month_collection year_collection virus_types
#>   <dbl> <chr>   <date>                     <dbl>           <dbl> <chr>      
#> 1     1 NIGERIA 2024-01-05                     1            2024 cVDPV2     
#> 2     2 CHAD    2024-02-09                     2            2024 NA         
#> # ℹ 14 more variables: virus_type <chr>, vtype <chr>, sabin1 <int>,
#> #   sabin2 <int>, sabin3 <int>, npev <int>, nvaccine <int>,
#> #   classification_all <chr>, ev_detect <int>, polio_type <chr>,
#> #   last_update_date <date>, enviro_sample_id <chr>, vaccine1 <chr>,
#> #   is_npev <chr>
```
