# Detect factor-like character columns (low-cardinality only)

Identifies character columns that look categorical, protecting id-like
names and leading-zero codes.

## Usage

``` r
detect_factors(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE
)
```

## Arguments

- data:

  A data frame or tibble.

- max_levels:

  Maximum distinct values for a factor candidate. Default 50.

- max_unique_ratio:

  Maximum unique/non-NA ratio for a factor. Default `0.2`.

- protect_patterns:

  Regexes for names kept as character. Default
  `c("id$", "uid$", "code$", "ref$", "key$")`.

- keep_leading_zero_chars:

  Keep a character column when any value is a leading-zero digit string
  (e.g. `"00123"`). Default `TRUE`.

## Value

A tibble of factor candidates: `name`, `n`, `n_non_na`, `n_unique`,
`unique_ratio`, `reason`.

## Examples

``` r
detect_factors(tibble::tibble(adm = c("A", "B", "A")))
#> # A tibble: 0 × 6
#> # ℹ 6 variables: name <chr>, n <int>, n_non_na <int>, n_unique <int>,
#> #   unique_ratio <dbl>, reason <chr>
```
