# Reconcile case admin names and GUIDs against the long district shape

Validates and, where possible, corrects the admin GUIDs and names on a
case table against the authoritative long ADM2 attribute table produced
by
[`process_spatial()`](https://truenomad.github.io/polished/reference/process_spatial.md)
/
[`create_long_shape()`](https://truenomad.github.io/polished/reference/create_long_shape.md)
(one row per district per active year, with the parent ADM0/ADM1 names
and GUIDs). For each case:

1.  If the ADM2 GUID matches a district valid in the case's onset year,
    the shape is authoritative: missing/mismatched admin names and
    parent GUIDs are filled from it.

2.  Otherwise, if the ADM0+ADM1+ADM2 names unambiguously identify one
    district that year, its GUIDs are adopted
    (`guid_corrected_from_name`).

3.  Otherwise the row is left unchanged and flagged `unresolved`.

GUIDs are compared case- and brace-insensitively (`{ABC}` == `abc`) and
emitted lower-case without braces. A missing onset year matches the
`9999` catch-all rows in the shape.

## Usage

``` r
reconcile_admin_guids(
  data,
  shape,
  year_var = "year_onset",
  guid_vars = c(adm0 = "adm0_guid", adm1 = "adm1_guid", adm2 = "adm2_guid"),
  name_vars = c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2"),
  sink = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A case data frame (e.g. the output of
  [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)).

- shape:

  The long ADM2 attribute table: a data frame with `adm0`/`adm1`/
  `adm2`, `adm0_guid`/`adm1_guid`/`adm2_guid` and `active_year` (an `sf`
  object is accepted and its geometry dropped).

- year_var:

  Case onset-year column. Default `"year_onset"`.

- guid_vars:

  Named (`adm0`/`adm1`/`adm2`) case GUID columns. Default
  `c(adm0 = "adm0_guid", adm1 = "adm1_guid", adm2 = "adm2_guid")`.

- name_vars:

  Named (`adm0`/`adm1`/`adm2`) case admin-name columns. Default
  `c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2")`.

- sink:

  Optional file path; when set, the per-row reconciliation flags for
  changed/unresolved rows are written there as CSV.

- verbose:

  Emit a cli summary. Default `TRUE`.

## Value

`data` with reconciled admin name/GUID columns and an added `geo_source`
factor (`guid_match` / `guid_corrected_from_name` / `unresolved`). A
`reconcile_qa` attribute carries per-country issue counts.

## Examples

``` r
shape <- data.frame(
  adm0 = "NIGERIA", adm1 = "BORNO", adm2 = "BOSSO",
  adm0_guid = "{A0}", adm1_guid = "{A1}", adm2_guid = "{A2}",
  active_year = 2024
)
cases <- data.frame(
  adm0 = "NIGERIA", adm1 = NA, adm2 = NA,
  adm0_guid = "a0", adm1_guid = NA, adm2_guid = "a2",
  year_onset = 2024
)
reconcile_admin_guids(cases, shape, verbose = FALSE)[, c("adm1", "adm2")]
#>    adm1  adm2
#> 1 BORNO BOSSO
```
