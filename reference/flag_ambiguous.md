# Flag (do not drop) rows whose business key spans multiple Ids

A tripwire on the assumed business uniqueness key. After
[`polis_upsert()`](https://truenomad.github.io/polished/reference/polis_upsert.md)
has reduced the data to one row per `id`, a well-formed dataset should
also be unique on its business key. Rows that violate this are surfaced
to QA – and left in the data – so a genuine reclassification is never
silently dropped.

## Usage

``` r
flag_ambiguous(data, key, id = "id", sink = NULL)
```

## Arguments

- data:

  A data frame (already deduped by `id`).

- key:

  Character vector naming the business key columns.

- id:

  Name of the identifier column (default `"id"`).

- sink:

  Optional destination for the flagged rows: a file path (CSV is
  written) or `NULL` (flags are attached as the `polis_ambiguous`
  attribute).

## Value

`data`, unchanged, possibly carrying a `polis_ambiguous` attribute.
