# Dictionary of available polio surveillance indicators

Returns a tidy description of every indicator
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
can compute – its code, label, formula, numerator/denominator
definition, the admin levels it is reported at, whether it needs a
population denominator, its WHO target/warning thresholds and the data
source. Call it to discover what is available without reading the
source.

## Usage

``` r
available_indicators(as_tibble = TRUE)
```

## Arguments

- as_tibble:

  If `TRUE` (default) return a tibble; if `FALSE` return the underlying
  named list of specs.

## Value

A tibble (one row per indicator) with columns: `code`, `label`, `kind`,
`formula`, `numerator`, `denominator`, `levels`, `requires_pop`,
`target`, `warn`, `unit`, `source`, `notes`.

## Examples

``` r
available_indicators()
#> # A tibble: 4 × 13
#>   code      label kind  formula numerator denominator levels requires_pop target
#>   <chr>     <chr> <chr> <chr>   <chr>     <chr>       <chr>  <lgl>         <dbl>
#> 1 npafp_ra… NPAF… rate  annual… Non-poli… Under-15 p… adm0,… TRUE              3
#> 2 stool_ad… Stoo… perc… 100 * … AFP case… AFP cases … adm0,… FALSE            80
#> 3 inv_time… Inve… perc… 100 * … AFP case… AFP cases … adm0,… FALSE            80
#> 4 pct_dist… Dist… perc… 100 * … Child di… Child dist… adm0,… TRUE             NA
#> # ℹ 4 more variables: warn <dbl>, unit <chr>, source <chr>, notes <chr>
available_indicators()[, c("code", "label", "formula")]
#> # A tibble: 4 × 3
#>   code                    label                          formula                
#>   <chr>                   <chr>                          <chr>                  
#> 1 npafp_rate              NPAFP rate (per 100k under-15) annualise(npafp_cases …
#> 2 stool_adequacy_pct      Stool adequacy (%)             100 * adequate_afp_cas…
#> 3 inv_timeliness_pct      Investigation timeliness (%)   100 * cases(notif->inv…
#> 4 pct_districts_npafp_ge2 Districts NPAFP >= 2 (%)       100 * districts(npafp_…
```
