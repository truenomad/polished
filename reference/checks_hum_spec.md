# Run human-specimen data-quality checks

Reads columns
[`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md)
already produced to flag duplicate specimens, blank collection dates,
unreconciled GUIDs and inadequate specimens. See
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
for the return shape.

## Usage

``` r
checks_hum_spec(hum_spec, reference_date = Sys.Date())
```

## Arguments

- hum_spec:

  A cleaned human-specimen tibble (from
  [`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md)).

- reference_date:

  Date treated as "today" for future-date checks (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

A named list (`summary` + one tibble per flagged check); see
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md).

## Examples

``` r
hs <- data.frame(id = 1, specimen_id = c("S1"), collection_date = NA)
checks_hum_spec(hs)$summary
#> # A tibble: 2 × 5
#>   check                       domain  severity n_flagged description            
#>   <chr>                       <chr>   <chr>        <int> <chr>                  
#> 1 hum_spec_no_collection_date HumSpec warning          1 Specimens with no coll…
#> 2 hum_spec_duplicates         HumSpec warning          0 Duplicate specimen rec…
```
