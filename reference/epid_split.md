# Split an EPID into its component segments

Splits each EPID on `sep` into one column per element of `parts`,
preserving `NA` and never erroring on malformed input.

## Usage

``` r
epid_split(
  epid,
  sep = "-",
  parts = c("country", "province", "district", "year", "serial"),
  extra = "drop",
  fill = "right"
)
```

## Arguments

- epid:

  Character vector of EPID strings.

- sep:

  Single-character delimiter between segments. Default `"-"`.

- parts:

  Character vector naming the output columns, in order. Default
  `c("country", "province", "district", "year", "serial")`.

- extra:

  How to treat segments beyond `length(parts)`: `"drop"` (default)
  discards them; `"merge"` collapses the remainder into the last column.

- fill:

  How to pad EPIDs with fewer segments than `parts`. Only `"right"` is
  supported: missing trailing segments become `NA`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
character column per `parts` element. Blank/`NA` EPIDs yield an all-`NA`
row.

## Examples

``` r
epid_split(c("NIE-BOS-XYZ-24-001", "AGO-LUA", NA))
#> # A tibble: 3 × 5
#>   country province district year  serial
#>   <chr>   <chr>    <chr>    <chr> <chr> 
#> 1 NIE     BOS      XYZ      24    001   
#> 2 AGO     LUA      NA       NA    NA    
#> 3 NA      NA       NA       NA    NA    
epid_split("NIE-BOS-XYZ-24-001-EXTRA", extra = "merge")
#> # A tibble: 1 × 5
#>   country province district year  serial   
#>   <chr>   <chr>    <chr>    <chr> <chr>    
#> 1 NIE     BOS      XYZ      24    001-EXTRA
```
