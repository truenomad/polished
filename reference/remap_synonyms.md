# Remap merged EPIDs to their canonical value

Rewrites the `epid` column using a synonym table (old EPID -\> canonical
EPID) so that records POLIS has merged collapse together in the
subsequent
[`polis_upsert()`](https://truenomad.github.io/polished/reference/polis_upsert.md)
step. A no-op when `synonyms` is `NULL`, so cleaners can call it
unconditionally.

## Usage

``` r
remap_synonyms(data, synonyms = NULL)
```

## Arguments

- data:

  A data frame with an `epid` column.

- synonyms:

  A data frame with columns `epid` (old) and `canonical_epid`, or `NULL`
  (default no-op).

## Value

`data` with `epid` remapped where a synonym exists.
