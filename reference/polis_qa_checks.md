# Registry of known preprocessing QA artifacts

The catalogue of side files a preprocessing run can emit, mapping a
filename pattern (regex) to a check name, domain, severity and human
description. Extend or filter it and pass to
[`polis_qa_report()`](https://truenomad.github.io/polished/reference/polis_qa_report.md)
to customise.

## Usage

``` r
polis_qa_checks()
```

## Value

A tibble with columns `check`, `domain`, `severity`, `pattern`,
`description`.
