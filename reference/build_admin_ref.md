# Build an EPID -\> admin-value reference (most-recent-per-EPID)

For each EPID, takes the most recent non-blank value of `admin_col`
(ties broken deterministically). Used to fill missing admin values from
other records that share the exact same EPID.

## Usage

``` r
build_admin_ref(data, admin_col, epid_var = "epid", year_var = "year")
```

## Arguments

- data:

  Data frame containing `epid_var`, `year_var`, and `admin_col`.

- admin_col:

  Name of the admin column to summarise.

- epid_var:

  EPID column name. Default `"epid"`.

- year_var:

  Recency column name. Default `"year"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns `epid_var` and `admin_col`, one row per EPID.

## Examples

``` r
cases <- tibble::tibble(
  epid = c("A-1", "A-1", "B-2"),
  year = c(2023, 2024, 2024),
  district = c(NA, "BOSSO", "LUANDA")
)
build_admin_ref(cases, "district")
#> # A tibble: 2 × 2
#>   epid  district
#>   <chr> <chr>   
#> 1 A-1   BOSSO   
#> 2 B-2   LUANDA  
```
