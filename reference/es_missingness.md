# Summarise missingness in key ES surveillance variables

A tidy, in-memory replacement for the side-file missingness export:
returns one row per variable with the count and percentage of missing
values, so a caller can inspect or write it however they like.

## Usage

``` r
es_missingness(data, vars = NULL)
```

## Arguments

- data:

  A cleaned ES data frame.

- vars:

  Character vector of columns to summarise. Default `NULL` uses the key
  ES surveillance fields that are present in `data`.

## Value

A tibble with columns `variable`, `n`, `n_missing` and `pct_missing`,
ordered most-missing first.

## Examples

``` r
es <- data.frame(
  collection_date = as.Date(c("2024-01-01", NA)),
  adm0 = c("CHAD", "CHAD"),
  classification_all = c(NA, "NEGATIVE")
)
es_missingness(es)
#> # A tibble: 3 × 4
#>   variable               n n_missing pct_missing
#>   <chr>              <int>     <int>       <dbl>
#> 1 collection_date        2         1          50
#> 2 classification_all     2         1          50
#> 3 adm0                   2         0           0
```
