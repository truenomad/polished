# Order columns: identifiers, then location, then time, then everything else

Classifies each column by the first matching `column_roles` pattern and
emits the groups in role order, then all unmatched columns. Order within
a group is preserved.

## Usage

``` r
order_columns(data, roles)
```

## Arguments

- data:

  A data frame.

- roles:

  Ordered named list of regex patterns (see
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)'s
  `column_roles`).

## Value

The data frame with reordered columns.
