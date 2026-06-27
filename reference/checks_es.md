# Run environmental-surveillance data-quality checks

Reads columns
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
already produced to flag duplicate sample IDs, blank/future collection
dates, unreconciled GUIDs and missing/zero coordinates. See
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
for the return shape.

## Usage

``` r
checks_es(es, reference_date = Sys.Date())
```

## Arguments

- es:

  A cleaned ES tibble (from
  [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)).

- reference_date:

  Date treated as "today" for future-date checks (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

A named list (`summary` + one tibble per flagged check); see
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md).

## Examples

``` r
es <- data.frame(
  id = 1, sample_id = "E1", adm0 = "CHAD",
  collection_date = "2024-02-01"
)
checks_es(es)$summary
#> # A tibble: 3 × 5
#>   check                 domain severity n_flagged description                   
#>   <chr>                 <chr>  <chr>        <int> <chr>                         
#> 1 es_duplicates         ES     warning          0 Duplicate sample ID + admin0  
#> 2 es_no_collection_date ES     warning          0 ES samples with no collection…
#> 3 es_future_collection  ES     warning          0 Collection date later than th…
```
