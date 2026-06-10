# Infer column types after cleaning, then optionally layer factor detection

POLIS returns every field as character. This parses character columns to
their natural base type (numeric, integer, date, datetime, logical) via
[`readr::type_convert()`](https://readr.tidyverse.org/reference/type_convert.html),
protecting identifier-like names and leading-zero codes from coercion,
and optionally proposes low-cardinality character columns as factors.
The cleaners
([`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
etc.) call it with `apply = FALSE` (base types only) when
`polis_config(parse_types = TRUE)`.

## Usage

``` r
auto_parse_types(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE,
  apply = TRUE,
  return = c("data", "both", "plan")
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

- apply:

  If `TRUE` (default) apply factor conversions on top of the parsed base
  types; if `FALSE` parse base types only.

- return:

  One of `"data"`, `"both"`, `"plan"`. Default `"data"`.

## Value

Depending on `return`: the parsed tibble (`"data"`), the type plan
(`"plan"`), or `list(plan, data)` (`"both"`).

## Examples

``` r
df <- tibble::tibble(id = c("001", "002"), age = c("1", "2"))
auto_parse_types(df, apply = FALSE)
#> # A tibble: 2 × 2
#>   id      age
#>   <chr> <int>
#> 1 001       1
#> 2 002       2
```
