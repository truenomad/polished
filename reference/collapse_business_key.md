# Collapse business-key duplicates, keeping the latest record

After
[`polis_upsert()`](https://truenomad.github.io/polished/reference/polis_upsert.md)
has reduced the data to one row per `id`, distinct ids can still share a
business key – the same case re-entered under a new POLIS Id. This keeps
one row per `key` combination, the latest by `date`, so those duplicates
collapse. Rows with a missing or blank key value are passed through
untouched, never merged together.

## Usage

``` r
collapse_business_key(data, key, date = "last_update_date", verbose = TRUE)
```

## Arguments

- data:

  A data frame (already deduped by `id`).

- key:

  Character vector naming the business key columns.

- date:

  Name of the recency column (default `"last_update_date"`).

- verbose:

  Emit a cli summary of how many rows collapsed. Default `TRUE`.

## Value

`data` with at most one row per non-blank `key` combination; rows whose
key is missing or blank are passed through unchanged. Row order is not
guaranteed.

## Examples

``` r
df <- data.frame(
  id = c(1, 2, 3),
  epid = c("A-1", "A-1", "B-2"),
  adm0 = c("X", "X", "X"),
  last_update_date = as.Date(c("2024-01-01", "2024-03-01", "2024-02-01"))
)
collapse_business_key(df, key = c("epid", "adm0"))
#> ℹ Collapsed 1 duplicate record sharing a business key (epid and adm0); kept the latest by last_update_date.
#> # A tibble: 2 × 4
#>      id epid  adm0  last_update_date
#>   <dbl> <chr> <chr> <date>          
#> 1     2 A-1   X     2024-03-01      
#> 2     3 B-2   X     2024-02-01      
```
