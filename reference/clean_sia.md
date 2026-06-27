# Clean POLIS SIA (campaign) data

Combines the raw POLIS activity and sub-activity tables into one
analytic SIA dataset and standardises it the same way
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
/
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
do:

- canonical snake_case names (via the crosswalk + janitor);

- the sub-activity grain enriched with its parent campaign: the activity
  table is restricted to the sub-activity codes actually present, then
  joined onto each sub-activity by `sia_sub_activity_code` (parent
  columns that clash take an `_activity` suffix; the redundant
  geographic parent copies – region, ISO, admin name/GUID, shape id, IST
  – are dropped);

- every campaign/planning date parsed to `Date` and sanitised with the
  same "sensible date" rule (a value before `min_year` or in the future
  is a data-entry error and set to `NA`); audit timestamps stay ISO
  strings for the keep-latest dedup;

- `year_start` / `month_start` from the sanitised `date_from` (the
  sub-activity start);

- normalised admin names and – when a `shape` is supplied – admin-GUID
  reconciliation against it (keyed on `year_start`), exactly as
  [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
  uses it;

- GUIDs emitted in the braced upper-case POLIS form and one row per
  POLIS `id` (latest by `last_update_date`);

- campaign rounds: within each district (`adm2_guid`) x `vaccine_type`,
  sub-activities are ordered by `date_from` and split into rounds
  wherever the gap to the previous campaign exceeds `round_gap_days`,
  giving a sequential `round_num`; `max_round_date` / `last_campaign`
  flag each district's most recent campaign.

## Usage

``` r
clean_sia(
  activity,
  subactivity = NULL,
  cfg = polis_active_config(),
  shape = NULL,
  round_gap_days = 21L,
  reference_date = Sys.Date(),
  cache_dir = NULL,
  cache_key = NULL,
  verbose = TRUE
)
```

## Arguments

- activity:

  A raw POLIS activity data frame.

- subactivity:

  Optional raw POLIS sub-activity data frame. When supplied it is the
  grain of the output and `activity` is joined onto it; when `NULL` the
  activity table is cleaned on its own.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object. Defaults to
  [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)
  – the config most recently built by
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  this session – so a no-`cfg` call inherits the active session settings
  rather than fresh defaults.

- shape:

  Optional district shape used to reconcile admin names/GUIDs via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  (keyed on `year_start`), exactly as
  [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
  uses it. Either a long ADM2 attribute table or the polygon layer
  (expanded to its long form here). Default `NULL` (no shape-based
  recovery).

- round_gap_days:

  Maximum number of days between consecutive campaigns in the same
  district and `vaccine_type` for them to count as one round; a larger
  gap starts a new round. Default `21`.

- reference_date:

  Date treated as "today" when sanitising campaign dates: any parsed
  date after it is nulled as a data-entry error (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)). It is part of
  the cache key, so a run on a later day does not return a stale cached
  table in which then-future dates are still `NA`. Pin it for
  reproducible output.

- cache_dir:

  Optional directory for an opt-in, content-addressed cache. When set,
  the cleaned table is written to (and on a later identical call read
  back from) a `qs2` file whose name hashes every input that affects the
  output (`activity`, `subactivity`, `cfg`, `shape`, `round_gap_days`,
  `reference_date`); any change to an input recomputes and writes a new
  entry. Default `NULL` (no caching).

- cache_key:

  Optional cheap stand-in for the raw tables in the cache key (e.g. a
  download snapshot id). When supplied, the key is built from it instead
  of hashing `activity`/`subactivity`, avoiding a full content hash of
  large inputs; `cfg`, `shape` and `round_gap_days` still contribute.
  Ignored unless `cache_dir` is set. Default `NULL` (hash the tables).

- verbose:

  Emit cli progress messages for each phase. Default `TRUE`.

## Value

A tibble of cleaned SIA records, one row per POLIS `id`, with columns
ordered id -\> location -\> time -\> other. Derived columns
(`year_start`, `month_start`, `round_num`, `max_round_date`,
`last_campaign`) are added only when their source columns are present.

## Examples

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
clean_sia(activity, subactivity, verbose = FALSE)
#> # A tibble: 1 × 10
#>      id adm0    last_modification_date date_from  last_update_date
#>   <dbl> <chr>   <date>                 <date>     <date>          
#> 1    10 NIGERIA 2024-03-01             2024-03-10 2024-03-01      
#> # ℹ 5 more variables: sia_sub_activity_code <chr>, id_activity <dbl>,
#> #   vaccine_type <chr>, year_start <dbl>, month_start <dbl>
```
