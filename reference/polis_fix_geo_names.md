# Apply geographic name fixes to a character vector

Applies every rule in
[`polis_geo_name_fixes()`](https://truenomad.github.io/polished/reference/polis_geo_name_fixes.md)
whose `field` matches the requested `field`, in table order, to a vector
of names. Matching is literal (not regex). `NA`s are preserved.

## Usage

``` r
polis_fix_geo_names(x, field, fixes = polis_geo_name_fixes())
```

## Arguments

- x:

  A character vector of geographic names.

- field:

  Which rule set to apply: `"adm0_name"`, `"adm1_name"` or `"location"`.

- fixes:

  The lookup table (default
  [`polis_geo_name_fixes()`](https://truenomad.github.io/polished/reference/polis_geo_name_fixes.md)).
  Pass a filtered/extended table to customise.

## Value

`x` with the matching fixes applied.

## Examples

``` r
polis_fix_geo_names(c("REPUBLIQUE DE COTE D'IVOIRE", "NIGERIA"), "adm0_name")
#> [1] "COTE D IVOIRE" "NIGERIA"      
polis_fix_geo_names(c("KHYBER PAKHTOON", "FATA", "SINDH"), "adm1_name")
#> [1] "NWFP"  "NWFP"  "SINDH"
```
