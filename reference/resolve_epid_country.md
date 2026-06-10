# Resolve an EPID country code to a country name

Maps the 3-character country code (see
[`epid_country_code()`](https://truenomad.github.io/polished/reference/epid_country_code.md))
to a name and ISO3 via a caller-supplied crosswalk, matching the code
against either the crosswalk's code or ISO3 column. Resolution never
guesses: a code that maps to more than one distinct name is flagged
ambiguous and left `NA`.

## Usage

``` r
resolve_epid_country(
  epid,
  ref = NULL,
  region = NULL,
  code_var = "code",
  name_var = "name",
  iso3_var = "iso3",
  region_var = NULL
)
```

## Arguments

- epid:

  Character vector of EPID strings.

- ref:

  Optional crosswalk data frame. When `NULL`, the raw code is returned
  with `resolved = FALSE` (no fabrication).

- region:

  Optional region value to filter `ref` by (needs `region_var`).

- code_var:

  Crosswalk column holding the country code. Default `"code"`.

- name_var:

  Crosswalk column holding the country name. Default `"name"`.

- iso3_var:

  Crosswalk column holding the ISO3 code. Default `"iso3"`.

- region_var:

  Optional crosswalk column holding the region. Default `NULL`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
row-aligned to `epid` with columns `code`, `name`, `iso3`, `n_matches`,
`ambiguous`, `resolved`.

## Details

Temporal validity is the caller's responsibility – pre-filter `ref` to
the period of interest (or pass a `region` with `region_var`) before
calling.

## Examples

``` r
crosswalk <- tibble::tibble(
  code = c("NIE", "AGO"),
  name = c("NIGERIA", "ANGOLA"),
  iso3 = c("NGA", "AGO")
)
resolve_epid_country(c("NIE-BOS-1", "AGO-LUA-1"), ref = crosswalk)
#> # A tibble: 2 × 6
#>   code  n_matches name    iso3  ambiguous resolved
#>   <chr>     <int> <chr>   <chr> <lgl>     <lgl>   
#> 1 NIE           1 NIGERIA NGA   FALSE     TRUE    
#> 2 AGO           1 ANGOLA  AGO   FALSE     TRUE    
```
