# Run SIA data-quality checks

Reads columns
[`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
already produced to flag rows missing an admin2 GUID or a start year.
See
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
for the return shape.

## Usage

``` r
checks_sia(sia, reference_date = Sys.Date())
```

## Arguments

- sia:

  A cleaned SIA tibble (from
  [`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)).

- reference_date:

  Date treated as "today" for future-date checks (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

A named list (`summary` + one tibble per flagged check); see
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md).

## Examples

``` r
sia <- data.frame(id = 1, adm0 = "CHAD", year_start = NA_integer_)
checks_sia(sia)$summary
#> # A tibble: 1 × 5
#>   check             domain severity n_flagged description                
#>   <chr>             <chr>  <chr>        <int> <chr>                      
#> 1 sia_no_start_year SIA    info             1 SIA rows with no start year
```
