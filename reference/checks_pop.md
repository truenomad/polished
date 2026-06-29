# Run POLIS population data-quality checks

Documents every POLIS-vs-WorldPop reconciliation issue
[`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
found and how each was resolved, as a `checks_*` result ready for
[`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md).
Unlike the other `checks_*()` functions it takes the **whole
[`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
list** (it reads `$adm2` and the `$meta` audit), not a single table.

## Usage

``` r
checks_pop(pop, reference_date = Sys.Date())
```

## Arguments

- pop:

  The list returned by
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
  (`$adm0`/`$adm1`/`$adm2`/`$meta`).

- reference_date:

  Unused; accepted for a uniform `checks_*()` signature.

## Value

A named list: `summary` (one row per issue with `check`, `domain`,
`severity`, `n_flagged`, `description`) followed by the detail tables
(`conflicting_dups`, `age_violations`, `orphan_guids`, `ratio_outliers`,
`coverage_by_country`, `overrides`, `source_mix`). Pass it to
[`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md).

## See also

[`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md).

## Examples

``` r
pop_raw <- data.frame(
  PlaceId = "g-1", PlaceDisplayName = "X", Year = 2020,
  AgeGroupName = "0 to 15 years", Value = 1000, check.names = FALSE
)
checks_pop(clean_pop(pop_raw, years = 2020, verbose = FALSE))$summary
#> # A tibble: 5 × 5
#>   check            domain     severity n_flagged description                    
#>   <chr>            <chr>      <chr>        <int> <chr>                          
#> 1 conflicting_dups population warning          0 POLIS place-years with >1 dist…
#> 2 age_violations   population warning          0 District-years where u5 <= u15…
#> 3 orphan_guids     population warning          0 POLIS guids absent from the sh…
#> 4 ratio_outliers   population info             0 POLIS values implausible vs Wo…
#> 5 overrides        population info             0 Cells where a present POLIS va…
```
