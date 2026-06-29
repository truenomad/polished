# Cleaning POLIS data

The cleaning layer turns the raw POLIS tables into canonical,
analysis-ready tables — one cleaner per surveillance stream, all
following the same recipe: standardise names, sanitise dates, derive the
analytic variables, clean the geography, enrich, and deduplicate to one
row per POLIS `id`.

| Function | Stream | Built from |
|----|----|----|
| [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md) | AFP cases | the Case table |
| [`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md) | human lab specimens | the LabSpecimen table |
| [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md) | environmental samples | the EnvSamples table |
| [`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md) | immunisation campaigns | the Activity + SubActivity tables |
| [`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md) | poliovirus positives | the **cleaned** AFP + ES outputs |

Two functions tie it together:
[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
holds the settings every cleaner shares, and
[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
runs the whole set in one call.

The examples below use small synthetic frames with raw POLIS-style
column names (no spaces) so they render without a live POLIS connection;
on real data you pass the downloaded tables straight in.

## Shared settings: `polis_config()`

Every cleaner takes a `cfg` argument.
[`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
builds it — a single, overridable object holding the column crosswalk,
the output column ordering, and run constants. There are deliberately no
per-column arguments: the canonical schema lives in the packaged
crosswalk, not in scattered defaults.

``` r

cfg <- polis_config(start_year = 2018)
cfg

# the column-ordering convention every cleaner emits to
names(cfg$column_roles)
#>  [1] "id"             "iso"            "country"        "geo_group"     
#>  [5] "adm_name"       "adm_guid"       "coord"          "onset_date"    
#>  [9] "onset_month"    "onset_year"     "age"            "core_dates"    
#> [13] "classification" "indicators"     "dates"
```

The crosswalk that maps raw POLIS API names to canonical `snake_case`
names is shipped with the package and inspectable directly:

``` r

head(polis_crosswalk()[, c("Table", "API_Name", "Snake_Name")])
#> # A tibble: 6 × 3
#>   Table    API_Name                                      Snake_Name             
#>   <chr>    <chr>                                         <chr>                  
#> 1 Activity ActivityAdminCoveragePercentage               activity_admin_coverag…
#> 2 Activity ActivityParent_IM_HH_MissedChildrenPercentage activity_parent_im_hh_…
#> 3 Activity ActivityParent_IM_OH_MissedChildrenPercentage activity_parent_im_oh_…
#> 4 Activity ActivityParent_LqasFailPercentage             activity_parent_lqas_f…
#> 5 Activity ActivityParent_LqasPassPercentage             activity_parent_lqas_p…
#> 6 Activity Admin0ShapeId                                 admin0shape_id
```

## AFP cases: `clean_afp()`

[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
standardises the Case table and derives the case-level analytic
variables AFP surveillance relies on: onset year/age, the onset-relative
date intervals, stool timeliness and 60-day follow-up flags, the fused
virus classification (`classification_all`), and the country enrichment.

``` r

afp_raw <- data.frame(
  Id = c(1, 1, 2),
  Epid = c("NIE-BOR-MMC-24-001", "NIE-BOR-MMC-24-001", "NIE-BOR-JER-24-014"),
  LastUpdateDate = c("2024-02-01", "2024-04-01", "2024-03-01"),
  ParalysisOnsetDate = c("2024-01-02", "2024-01-02", "2024-02-03"),
  NotificationDate = c("2024-01-05", "2024-01-05", "2024-02-06"),
  Stool1CollectionDate = c("2024-01-08", "2024-01-08", "2024-02-10"),
  Stool2CollectionDate = c("2024-01-10", "2024-01-10", "2024-02-13"),
  Classification = c("Confirmed (wild)", "Confirmed (wild)", "Discarded"),
  PolioVirusTypes = c("WILD1", "WILD1", NA),
  CountryISO3Code = c("NGA", "NGA", "NGA"),
  Admin0Name = c("NIGERIA", "NIGERIA", "NIGERIA"),
  check.names = FALSE
)
```

The two rows that share `Id = 1` are the same record updated twice; the
cleaner keeps the latest by `LastUpdateDate`.

``` r

cases <- clean_afp(afp_raw, cfg = cfg, verbose = FALSE)

cases[, c(
  "epid", "year_onset", "onset_to_notify", "onset_to_stool1",
  "timeliness", "classification_all", "risk_group"
)]
#> # A tibble: 2 × 7
#>   epid  year_onset onset_to_notify onset_to_stool1 timeliness classification_all
#>   <chr>      <dbl>           <dbl>           <dbl> <chr>      <chr>             
#> 1 NIE-…       2024               3               6 Timely     WPV 1             
#> 2 NIE-…       2024               3               7 Timely     NPAFP             
#> # ℹ 1 more variable: risk_group <chr>
```

The raw classification is decoded into the standard **WPV** / **cVDPV**
/ **aVDPV** / **iVDPV** vocabulary, so a single filter works across
every stream:

``` r

cases[grepl("WPV|VDPV", cases$classification_all), c("epid", "classification_all")]
#> # A tibble: 1 × 2
#>   epid               classification_all
#>   <chr>              <chr>             
#> 1 NIE-BOR-MMC-24-001 WPV 1
```

Supplying a cleaned district `shape` adds GUID reconciliation and
coordinate / EPID-prefix admin recovery (see the *EPID geography* and
*spatial data* vignettes); without one,
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
still runs standalone as above.

## Human lab specimens: `clean_human_spec()`

[`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md)
is the specimen-level companion to
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md):
where the Case table summarises stool 1/2, the LabSpecimen table carries
every specimen with its laboratory results. It derives the same virus
classification, a specimen `adequate` flag, and — since specimens have
no onset date — the **lab-turnaround** intervals (collection → lab →
culture → ITD → sequencing) in place of the onset-based ones.

``` r

spec_raw <- data.frame(
  Id = 1:2,
  SpecimenId = c("S-1", "S-2"),
  Epid = c("NIE-BOR-MMC-24-001", "NIE-BOR-JER-24-014"),
  LastUpdateDate = c("2024-03-01", "2024-03-01"),
  DateStoolCollected = c("2024-01-05", "2024-02-09"),
  DateStoolReceivedInLab = c("2024-01-10", "2024-02-14"),
  DateFinalCellCultureResults = c("2024-01-20", NA),
  VirusTypes = c("cVDPV2", NA),
  VdpvClassification = c("Circulating", NA),
  AdequateSpecimen = c("Yes", "No"),
  Admin0Name = c("NIGERIA", "NIGERIA"),
  check.names = FALSE
)

spec <- clean_human_spec(spec_raw, verbose = FALSE)
spec[, c(
  "specimen_id", "classification_all", "adequate",
  "collect_to_lab", "lab_to_culture"
)]
#> # A tibble: 2 × 5
#>   specimen_id classification_all adequate collect_to_lab lab_to_culture
#>   <chr>       <chr>                 <int>          <dbl>          <dbl>
#> 1 S-1         cVDPV 2                   1              5             10
#> 2 S-2         <NA>                      0              5             NA
```

## Environmental samples: `clean_es()`

[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
cleans the EnvSamples table and derives the same virus classification
plus the ES detection flags (`sabin1/2/3`, `npev`, `nvaccine`, and the
fused `ev_detect` “anything detected” flag).

``` r

es_raw <- data.frame(
  Id = c(1, 1, 2),
  SampleId = c("E-1", "E-1", "E-2"),
  LastUpdateDate = c("2024-02-01", "2024-04-01", "2024-03-01"),
  CollectionDate = c("2024-01-05", "2024-01-05", "2024-02-09"),
  VirusTypes = c("cVDPV2", "cVDPV2", "NPEV"),
  VdpvClassifications = c("Circulating", "Circulating", NA),
  IsNPEV = c(NA, NA, TRUE),
  Admin0Name = c("NIGERIA", "NIGERIA", "CHAD"),
  CountryISO3Code = c("NGA", "NGA", "TCD"),
  check.names = FALSE
)

es <- clean_es(es_raw, verbose = FALSE)
es[, c(
  "sample_id", "year_collection", "classification_all",
  "ev_detect", "npev", "risk_group"
)]
#> # A tibble: 2 × 6
#>   sample_id year_collection classification_all ev_detect  npev risk_group    
#>   <chr>               <dbl> <chr>                  <int> <int> <chr>         
#> 1 E-1                  2024 cVDPV 2                    1     0 Very High Risk
#> 2 E-2                  2024 NPEV                       1     1 Very High Risk
```

## Immunisation campaigns: `clean_sia()`

[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
combines the Activity and SubActivity tables into one analytic SIA table
on the sub-activity grain (one row per round × district), joining the
parent campaign on by the sub-activity code and deriving the campaign
start year/month.

``` r

activity <- data.frame(
  Id = 1,
  SIASubActivityCode = "S1",
  LastUpdateDate = "2024-03-01",
  VaccineType = "bOPV",
  check.names = FALSE
)
subactivity <- data.frame(
  Id = 10,
  SIASubActivityCode = "S1",
  LastModificationDate = "2024-03-01",
  DateFrom = "2024-03-10",
  Admin0Name = "NIGERIA",
  check.names = FALSE
)

sia <- clean_sia(activity, subactivity, verbose = FALSE)
sia[, c("id", "adm0", "year_start", "month_start", "vaccine_type")]
#> # A tibble: 1 × 5
#>      id adm0    year_start month_start vaccine_type
#>   <dbl> <chr>        <dbl>       <dbl> <chr>       
#> 1    10 NIGERIA       2024           3 bOPV
```

## Poliovirus positives: `clean_virus()`

[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
does **not** read a raw viruses table — it *builds* the positives
dataset from the already-cleaned AFP and ES outputs. Every
poliovirus-positive case or sample becomes one harmonised row, tagged by
`surveillance_type`, with a `report_date` (VDPV classification-change
date, or notification date for WPV).

``` r

positives <- clean_virus(cases = cases, es = es, verbose = FALSE)
positives[, c(
  "epid", "surveillance_type", "measurement", "classification_all", "report_date"
)]
#> # A tibble: 2 × 5
#>   epid              surveillance_type measurement classification_all report_date
#>   <chr>             <chr>             <chr>       <chr>              <date>     
#> 1 NIE-BOR-MMC-24-0… human             WPV 1       WPV 1              2024-01-05 
#> 2 E-1               environmental     cVDPV 2     cVDPV 2            NA
```

Pass `separate_rows = TRUE` to split a co-detection
(e.g. `WPV1andcVDPV 2`) into one row per serotype.

## One call: `run_pipeline()`

[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
runs the per-stream cleaners over a named list of raw tables (`afp`,
`es`, `hum_spec`, `activity`, `subactivity`) and returns the cleaned
set, building the virus positives from the cleaned AFP/ES streams
automatically.

``` r

cleaned <- run_pipeline(
  inputs = list(afp = afp_raw, es = es_raw),
  cfg = cfg
)
names(cleaned)
#> [1] "afp"        "es"         "virus"      "detections"
```

Each cleaned element is exactly what the matching `clean_*()` call
produces above. Two settings on `cfg` extend the run end to end:

- a `shape` (an already-processed district layer) is passed to every
  cleaner for admin reconciliation and geography recovery;
- a `population` table (under-15 denominators) triggers the surveillance
  indicator catalogue via
  [`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md),
  attached as `cleaned$indicators`.

``` r

cfg <- polis_config(
  shape = "data/gpei_adm2_shape.rds",
  population = "data/under15_pop.csv"
)
run_pipeline(list(afp = afp_raw, es = es_raw), cfg = cfg)
```

To run the whole thing from a folder of downloaded `raw_*` files
straight to `polished_*` outputs (plus data-quality workbooks), use
[`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
— see the *End-to-end pipeline* article.
