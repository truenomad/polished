# Dictionary of available polio surveillance indicators

Returns a tidy description of every indicator
[`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
knows about – code, label, family, formula, numerator/denominator,
source, period basis, admin levels, whether it needs a population
denominator, WHO target/warn thresholds, unit, the POLIS source function
and notes. Derived from the same registry the engine runs, so the
catalogue and the engine never drift.

## Usage

``` r
available_indicators(as_tibble = TRUE, family = NULL, core_only = FALSE)
```

## Arguments

- as_tibble:

  If `TRUE` (default) return a tibble; if `FALSE` return the underlying
  named list of registry specs.

- family:

  Optional family filter (e.g. `"AFP"`, `"ES"`, `"Virus"`, `"SIA"`,
  `"Composite"`, `"Timeliness"`, `"Stool"`, `"Dose"`, `"Lab"`).

- core_only:

  If `TRUE`, return only the core KPI indicators (the set
  `calc_polio_indicators(indicators = "core")` computes). Default
  `FALSE`.

## Value

A tibble (one row per indicator) with columns `code`, `label`, `family`,
`kind`, `core`, `formula`, `numerator`, `denominator`, `source`,
`period_basis`, `levels`, `requires_pop`, `target`, `warn`, `unit`,
`polis_fn`, `notes`.

## Examples

``` r
available_indicators()
#> # A tibble: 62 × 17
#>    code            label family kind  core  formula numerator denominator source
#>    <chr>           <chr> <chr>  <chr> <lgl> <chr>   <chr>     <chr>       <chr> 
#>  1 afp_count       AFP … AFP    count FALSE COUNT(… AFP cases (count)     cases 
#>  2 npafp_count     NPAF… AFP    count FALSE COUNT(… NPAFP ca… (count)     cases 
#>  3 npafp_rate      NPAF… AFP    rate  TRUE  annual… Non-poli… Under-15 p… cases 
#>  4 npafp_rate_nop… NPAF… AFP    rate  FALSE annual… Non-poli… Under-15 p… cases 
#>  5 stool_adequacy… Stoo… Stool  perc… FALSE 100 * … AFP case… AFP cases   cases 
#>  6 stool_adequacy… Stoo… Stool  perc… TRUE  100 * … AFP case… Cases code… cases 
#>  7 stool_adequacy… Stoo… Stool  perc… FALSE 100 * … AFP case… Assessable… cases 
#>  8 afp_dose_0      AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#>  9 afp_dose_1_2    AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#> 10 afp_dose_3plus  AFP … Dose   perc… FALSE 100 * … AFP case… AFP/NPAFP … cases 
#> # ℹ 52 more rows
#> # ℹ 8 more variables: period_basis <chr>, levels <chr>, requires_pop <lgl>,
#> #   target <dbl>, warn <dbl>, unit <chr>, polis_fn <chr>, notes <chr>
available_indicators(family = "ES")[, c("code", "label", "formula")]
#> # A tibble: 8 × 3
#>   code                  label                        formula                    
#>   <chr>                 <chr>                        <chr>                      
#> 1 env_count             ES sample count              COUNT(ES samples)          
#> 2 env_wpv_count         ES WPV count                 COUNT(ES WPV+)             
#> 3 env_cvdpv_count       ES cVDPV count               COUNT(ES cVDPV+)           
#> 4 env_wpv_count_rep     ES WPV count (reporting)     COUNT(ES WPV+) on reportin…
#> 5 env_cvdpv_count_rep   ES cVDPV count (reporting)   COUNT(ES cVDPV+) on report…
#> 6 ev_rate               EV detection rate (%)        100 * ev_positive / samples
#> 7 sites_with_entero_pct Sites with EV > 49% (%)      100 * sites(EV rate > 49%)…
#> 8 case_es_35days_pct    ES result within 35 days (%) 100 * results within 35d o…
available_indicators(core_only = TRUE)$code
#> [1] "npafp_rate"              "stool_adequacy_cond_pct"
#> [3] "inv_timeliness_pct"      "ev_rate"                
```
