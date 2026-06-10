# Expand admin shapes to one row per active year

Turns a shape table with `year_start` / `year_end` validity windows into
a long table with one row per administrative unit per active year,
spanning each unit's earliest start to the current year (a unit stays
matchable in every year from its start; `year_end` does not close the
span here), plus a `9999` sentinel year used to match records with an
unknown year. When `checks_dir` is supplied, any administrative unit
with more than one shape active in the same year is written to a CSV for
manual review.

## Usage

``` r
create_long_shape(data, level, checks_dir = NULL)
```

## Arguments

- data:

  An `sf` object (or data frame) carrying the admin name/GUID columns
  plus `year_start` and `year_end`.

- level:

  Administrative level, `"adm1"` or `"adm2"`. Determines the grouping
  columns.

- checks_dir:

  Optional directory for the multiple-shape check CSV. When `NULL`
  (default) no check file is written.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with the
grouping columns plus `active_year`, `year_start` and `year_end`, one
row per unit-year.

## Examples

``` r
shapes <- tibble::tibble(
  adm0_guid = "g0", adm0 = "NIGERIA",
  adm1_guid = "g1", adm1 = "BORNO",
  year_start = 2018, year_end = 2020
)
create_long_shape(shapes, "adm1")
#> # A tibble: 10 × 7
#>    adm0_guid adm0    adm1  adm1_guid active_year year_start year_end
#>    <chr>     <chr>   <chr> <chr>           <int>      <dbl>    <dbl>
#>  1 g0        NIGERIA BORNO g1               2018       2018     2020
#>  2 g0        NIGERIA BORNO g1               2019       2018     2020
#>  3 g0        NIGERIA BORNO g1               2020       2018     2020
#>  4 g0        NIGERIA BORNO g1               2021       2018     2020
#>  5 g0        NIGERIA BORNO g1               2022       2018     2020
#>  6 g0        NIGERIA BORNO g1               2023       2018     2020
#>  7 g0        NIGERIA BORNO g1               2024       2018     2020
#>  8 g0        NIGERIA BORNO g1               2025       2018     2020
#>  9 g0        NIGERIA BORNO g1               2026       2018     2020
#> 10 g0        NIGERIA BORNO g1               9999       2018     2020
```
