# Prune records absent from a full pull (reconcile)

POLIS's current-view API hides deletes and merges, so an
incrementally-built store accumulates rows POLIS has since removed.
Given a fresh full pull, reconcile keeps only the `id`s still present,
pruning the rest.

## Usage

``` r
reconcile(store, full_pull, id = "id")
```

## Arguments

- store:

  The accumulated data frame.

- full_pull:

  A complete fresh pull of the same table.

- id:

  Name of the identifier column (default `"id"`).

## Value

`store` filtered to `id`s present in `full_pull`.
