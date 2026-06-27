# Run virus/positives data-quality checks

Reads columns
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
already produced to flag duplicate records, vaccine viruses with large
nucleotide changes, and VDPV positives with no emergence group. See
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
for the return shape.

## Usage

``` r
checks_virus(virus, reference_date = Sys.Date())
```

## Arguments

- virus:

  A cleaned virus tibble (from
  [`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)).

- reference_date:

  Date treated as "today" for future-date checks (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

A named list (`summary` + one tibble per flagged check); see
[`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md).

## Examples

``` r
virus <- data.frame(id = c(1, 1), nt_changes = c(7, 7))
checks_virus(virus)$summary
#> # A tibble: 1 × 5
#>   check          domain severity n_flagged description                          
#>   <chr>          <chr>  <chr>        <int> <chr>                                
#> 1 virus_large_nt Virus  warning          2 Vaccine viruses with >= 6 nucleotide…
```
