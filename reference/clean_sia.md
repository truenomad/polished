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
  columns that clash with a sub-activity column take an `_activity`
  suffix);

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
  POLIS `id` (latest by `last_update_date`).

## Usage

``` r
clean_sia(
  activity,
  subactivity = NULL,
  cfg = polis_config(),
  shape = NULL,
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
  object (default
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)).

- shape:

  Optional district shape used to reconcile admin names/GUIDs via
  [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  (keyed on `year_start`), exactly as
  [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
  uses it. Either a long ADM2 attribute table or the polygon layer
  (expanded to its long form here). Default `NULL` (no shape-based
  recovery).

- verbose:

  Emit cli progress messages for each phase. Default `TRUE`.

## Value

A tibble of cleaned SIA records, one row per POLIS `id`, with columns
ordered id -\> location -\> time -\> other. Derived columns
(`year_start`, `month_start`) are added only when their source columns
are present.

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
