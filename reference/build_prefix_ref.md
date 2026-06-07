# Build a (prefix, year) -\> unique admin-value reference

For each geographic prefix and year, returns the admin value only when
it is unambiguous (exactly one distinct non-blank value); otherwise the
value is `NA` and `n_candidates` records how many distinct values
competed.

## Usage

``` r
build_prefix_ref(
  data,
  admin_col,
  epid_var = "epid",
  year_var = "year",
  prefix_length = 11
)
```

## Arguments

- data:

  Data frame containing `epid_var`, `year_var`, and `admin_col`.

- admin_col:

  Name of the admin column to summarise.

- epid_var:

  EPID column name. Default `"epid"`.

- year_var:

  Year column name. Default `"year"`.

- prefix_length:

  Prefix length passed to
  [`epid_prefix()`](https://truenomad.github.io/polished/reference/epid_prefix.md).
  Default `11`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns `prefix`, `year`, `n_candidates`, and `admin_col`.

## Examples

``` r
cases <- tibble::tibble(
  epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2", "NIE-BOS-BBB-1"),
  year = c(2024, 2024, 2024),
  district = c("BOSSO", "BOSSO", "BIRNI")
)
build_prefix_ref(cases, "district", prefix_length = 7)
#> # A tibble: 1 × 4
#>   prefix   year n_candidates district
#>   <chr>   <dbl>        <int> <chr>   
#> 1 NIE-BOS  2024            2 NA      
```
