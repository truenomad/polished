# Normalise admin names on a cleaned data frame

Convenience wrapper used by every cleaner: applies the country-level
(`adm0_name`) fixes to the `adm0` column and the province-level
(`adm1_name`) fixes to the `adm1` column, when present. Columns are
expected to already carry canonical names (post
[`standardise_names()`](https://truenomad.github.io/polished/reference/standardise_names.md)).

## Usage

``` r
fix_geo_names(data, fixes = polis_geo_name_fixes())
```

## Arguments

- data:

  A data frame with canonical `adm0`/`adm1` columns.

- fixes:

  The lookup table (default
  [`polis_geo_name_fixes()`](https://truenomad.github.io/polished/reference/polis_geo_name_fixes.md)).

## Value

`data` with admin names normalised.
