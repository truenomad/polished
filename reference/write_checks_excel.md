# Write a checks result to an Excel workbook

Turns the list returned by a `checks_*()` function into a single styled
`.xlsx` workbook: a `Summary` tab listing every applicable check and its
flagged-row count, followed by one tab of flagged rows per check that
found problems. Sheets get a navy header, sized columns and inferred
number formats. No versioning – the file is written straight to `path`.

## Usage

``` r
write_checks_excel(checks, path)
```

## Arguments

- checks:

  A list returned by
  [`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
  (or any `checks_*()`), carrying a `summary` element and zero or more
  flagged-row tibbles.

- path:

  Output `.xlsx` path.

## Value

`path`, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
write_checks_excel(checks_afp(afp), "checks_afp.xlsx")
} # }
```
