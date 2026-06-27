# Run AFP data-quality checks

Surfaces data-quality problems in a cleaned AFP table by reading columns
the cleaner already produced (duplicates, blank keys, unreconciled
GUIDs, missing/zero coordinates, future onset dates, out-of-range age,
negative timeliness intervals, inadequate stool). Checks whose required
columns are absent are skipped, so a trimmed input is handled
gracefully.

## Usage

``` r
checks_afp(afp, reference_date = Sys.Date())
```

## Arguments

- afp:

  A cleaned AFP tibble (from
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)).

- reference_date:

  Date treated as "today" for future-date checks (default
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

## Value

A named list: `summary` (a tibble with one row per applicable check:
`check`, `domain`, `severity`, `n_flagged`, `description`) followed by
one tibble of flagged rows (key columns) per check that found problems.
Pass it to
[`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md)
to export a workbook.

## Examples

``` r
afp <- data.frame(
  id = c(1, 1), epid = c("A-1", "A-1"),
  paralysis_onset_date = c("2024-01-02", "2024-01-02"),
  adm0 = "NIGERIA"
)
checks_afp(afp)$summary
#> # A tibble: 3 × 5
#>   check            domain severity n_flagged description                        
#>   <chr>            <chr>  <chr>        <int> <chr>                              
#> 1 afp_duplicates   AFP    warning          2 Duplicate EPID + onset date + admi…
#> 2 afp_no_onset     AFP    warning          0 AFP cases with no paralysis onset …
#> 3 afp_future_onset AFP    warning          0 Onset date later than the run date 
```
