# Upsert by Id, keeping the latest record

Combines an existing store with an optional new pull, optionally
collapses exact duplicate rows at a finer grain, then keeps exactly one
row per `id`: the one with the maximum `date`. This is unconditional (no
Id-range shortcut), so a single primitive governs recency everywhere.

## Usage

``` r
polis_upsert(
  store,
  pull = NULL,
  id = "id",
  date = "last_update_date",
  grain = NULL
)
```

## Arguments

- store:

  A data frame (the accumulated store, or simply the data to dedup).

- pull:

  Optional new data frame to upsert into `store`.

- id:

  Name of the canonical identifier column (default `"id"`).

- date:

  Name of the update-timestamp column used for recency (default
  `"last_update_date"`).

- grain:

  Optional character vector of columns defining a finer row grain. When
  supplied, exact duplicates at this grain are collapsed (keep-latest)
  before the per-`id` step.

## Value

A data frame with one row per `id`.

## Examples

``` r
df <- data.frame(
  id = c(1, 1, 2),
  last_update_date = as.Date(c("2024-01-01", "2024-03-01", "2024-02-01")),
  value = c("old", "new", "x")
)
polis_upsert(df)
#> # A tibble: 2 × 3
#>      id last_update_date value
#>   <dbl> <date>           <chr>
#> 1     1 2024-03-01       new  
#> 2     2 2024-02-01       x    
```
