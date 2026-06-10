# Process raw Independent Monitoring (IM) data into missed-children rates

Reproduces the POLIS IM missed-children computation: per
district/parent, `missed = 1 - sum(marked)/sum(checked)`, falling back
to `mean(result)` when no children were checked; with a Valid/Invalid
status flag (Invalid when the result is missing or negative).

## Usage

``` r
process_im(
  im,
  adm2_guid_var = "Admin2Guid",
  adm2_name_var = "Admin2Name",
  adm1_name_var = "Admin1Name",
  adm0_name_var = "Admin0Name",
  checked_var = "ChildrenChecked",
  marked_var = "ChildrenMarked",
  result_var = "Result",
  date_var = "ActivityStart",
  verbose = TRUE
)
```

## Arguments

- im:

  Raw IM table (data.frame/tibble).

- adm2_guid_var, adm2_name_var:

  District GUID / name columns.

- adm1_name_var, adm0_name_var:

  Optional province / country name columns.

- checked_var:

  Column with children checked.

- marked_var:

  Column with children marked (finger-marked / vaccinated).

- result_var:

  Optional pre-computed per-row missed result (used as the fallback when
  no children were checked).

- date_var:

  Optional date column (for a `year` grouping).

- verbose:

  Emit a cli summary (default `TRUE`).

## Value

A list with `district` (per-district missed-children rate + status) and
`meta`.
