# Standardise POLIS column names

Renames raw POLIS columns to their canonical `Snake_Name` via the
crosswalk, then applies
[`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html)
to everything the crosswalk does not cover. The result is fully
snake_case with the agreed names for special tokens.

## Usage

``` r
standardise_names(data, crosswalk = .polis_crosswalk_map())
```

## Arguments

- data:

  A raw POLIS data frame.

- crosswalk:

  Rename vector from `.polis_crosswalk_map()` (default).

## Value

`data` with canonical column names.

## Examples

``` r
raw <- data.frame(PoNS_OnSetDate = 1, Admin0Name = "X", DateOnset = 2,
  check.names = FALSE)
names(standardise_names(raw))
#> [1] "pons_on_set_date" "adm0"             "date_onset"      
```
