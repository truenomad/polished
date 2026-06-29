# Clean POLIS population and (optionally) reconcile against WorldPop

Builds adm0/adm1/adm2 population denominators (under-5, under-15,
all-ages) from the raw POLIS Population table. Used standalone it cleans
POLIS by itself – collapses duplicate `(place, year)` rows to their
median, drops zeros/blanks to missing, flags values that jump from a
district's own history, and fills gaps from a district -\> province -\>
country ladder. Given `worldpop` it adds a cross-source layer: each
POLIS value is checked against WorldPop and a missing/implausible one is
replaced (WorldPop first, then the same ladder).

## Usage

``` r
clean_pop(
  population,
  cfg = polis_active_config(),
  shape = NULL,
  worldpop = NULL,
  years = 2010:2027,
  thresholds = list(ratio_lo = 1/3, ratio_hi = 3, mad_k = 5, min_votes = 1L),
  reference_date = Sys.Date(),
  pop_source = c("reconciled", "polis", "worldpop"),
  verbose = TRUE
)
```

## Arguments

- population:

  A raw POLIS Population data frame (columns `PlaceId`,
  `PlaceDisplayName`, `Year`, `AgeGroupName`, `Value`), or a path to
  one.

- cfg:

  A
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  object; defaults to
  [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md).
  Only its presence is required – clean_pop reads no scoping from it
  (pop is foundational/global), but it keeps the cleaner signature
  uniform.

- shape:

  Optional **already-processed** district shape (an `sf` polygon layer
  or its long ADM2 attribute table, or a path to one). Supplies the
  adm0/adm1 parents POLIS lacks, the boundary-validity windows used for
  the roll-ups, and the universe of districts. With no shape the output
  is keyed on `adm2_guid` only and cannot be rolled up. Default `NULL`.

- worldpop:

  Optional named list with elements `all`, `u5`, `u15`. Each is either a
  **directory** of annual WorldPop GeoTIFFs (one per year, the year in
  the file name; zonal-summed to the `shape` via `terra` +
  `exactextractr` – both optional Suggests), **or** a pre-extracted
  adm2-by-year table (data frame or path) carrying `adm2_guid`, `year`
  and a population column. `NULL` (default) runs the POLIS-only path.

- years:

  Calendar years to keep (POLIS carries 1990-2034 incl. projections).
  Default `2010:2027`.

- thresholds:

  Named list of the implausibility tunables: `ratio_lo` / `ratio_hi` (a
  POLIS value below/above this fold of WorldPop is implausible), `mad_k`
  (scaled-MAD distance from the district's own median), `min_votes` (how
  many signals must fire to call a value suspect). Default
  `list(ratio_lo = 1/3, ratio_hi = 3, mad_k = 5, min_votes = 1L)`.

- reference_date:

  Date treated as "today" when deciding which boundary versions are
  *current* for the orphan-GUID name crosswalk. Default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- pop_source:

  Which population to use as the chosen `<age>_pop` value (the
  denominator indicators read). One of:

  `"reconciled"`

  :   (default) a trusted POLIS value, else WorldPop, else the district
      -\> province -\> country ladder.

  `"polis"`

  :   the POLIS value (gaps filled from the ladder; WorldPop is ignored
      even if supplied) – the full POLIS population.

  `"worldpop"`

  :   the WorldPop value, else a POLIS value, else the ladder.

  The output always keeps `<age>_pop_polis` and `<age>_pop_wp` alongside
  the chosen `<age>_pop`, so every source stays inspectable whatever the
  mode.

- verbose:

  Emit cli progress headers. Default `TRUE`.

## Value

A named list:

- `adm2`:

  district x year, wide: the id columns plus, per age band
  (`u5`/`u15`/`all`), `<age>_pop` (chosen), `<age>_pop_polis`,
  `<age>_pop_wp`, `<age>_pop_source`
  (`polis`/`worldpop`/`district_trend`/ `adm1`/`adm0`) and
  `<age>_pop_imputed`, plus `age_order_bad`. Restricted to the boundary
  valid each year (no double-counting versioned shapes).

- `adm1`, `adm0`:

  province / country roll-ups (sums) of the nine pop columns.

- `meta`:

  a list (skipped by the file writer): `audit` (one row per district x
  year x age, with every signal flag), `dup_conflicts`, `orphan_xwalk`,
  `params`.

## See also

[`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md),
which runs this as the `population` stream;
[`checks_pop()`](https://truenomad.github.io/polished/reference/checks_pop.md),
which turns the result into a data-quality workbook.

## Examples

``` r
pop_raw <- data.frame(
  PlaceId = "0cda1c45-9529-4188-aaaa-000000000001",
  PlaceDisplayName = "SOMEWHERE",
  Year = 2020, AgeGroupName = "0 to 15 years", Value = 1000,
  check.names = FALSE
)
res <- clean_pop(pop_raw, years = 2020, verbose = FALSE)
names(res)
#> [1] "adm0" "adm1" "adm2" "meta"
```
