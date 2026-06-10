# Country reference lookup shipped with the package

Returns the packaged country reference that maps an ISO3 code to the
standardised display name, polio risk tier and epidemiological zone
groupings the cleaners attach. Used by
[`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
and
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
via `.polis_join_country()`.

## Usage

``` r
polis_country_lookup()
```

## Value

A tibble with columns `iso3`, `country_actual`, `risk_group`,
`epi_zones`, `epi_zones_v2`.

## Examples

``` r
head(polis_country_lookup())
#> # A tibble: 6 × 5
#>   iso3  country_actual risk_group epi_zones                 epi_zones_v2        
#>   <chr> <chr>          <chr>      <chr>                     <chr>               
#> 1 ABW   NA             NA         Other                     Other               
#> 2 AFG   Afghanistan    Endemic    Other                     Other               
#> 3 AGO   Angola         High Risk  Central/Equatorial Africa Central/Equatorial …
#> 4 AIA   NA             NA         Other                     Other               
#> 5 ALA   NA             NA         Other                     Other               
#> 6 ALB   Albania        NA         Other                     Other               
```
