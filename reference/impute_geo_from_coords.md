# Recover missing admin from coordinates, in place

For cases still missing `adm2`/`adm2_guid` that carry valid coordinates,
a point-in-polygon join against the district polygons (`shape_adm2`, the
`spatial_global_adm2` layer from
[`process_spatial()`](https://truenomad.github.io/polished/reference/process_spatial.md))
recovers `adm1`/`adm2` and their GUIDs, matched to the case's onset
year. Unlike
[`get_admin_info_from_coords()`](https://truenomad.github.io/polished/reference/get_admin_info_from_coords.md)
every row is kept – only the missing cells of unambiguously matched
cases are filled.

## Usage

``` r
impute_geo_from_coords(
  data,
  shape_adm2,
  year_var = "year_onset",
  lon_var = "longitude",
  lat_var = "latitude",
  target = c("adm1", "adm2", "adm1_guid", "adm2_guid"),
  verbose = TRUE
)
```

## Arguments

- data:

  A case data frame with coordinate and admin columns.

- shape_adm2:

  An `sf` object of district polygons carrying the canonical admin
  name + GUID columns and `year_start`/`year_end`.

- year_var:

  Onset-year column for temporal filtering. Default `"year_onset"`.

- lon_var, lat_var:

  Coordinate columns. Default `"longitude"` / `"latitude"`.

- target:

  Admin columns whose `NA` marks a case as needing recovery: a case is
  recovered when *any* of them is missing. Default
  `c("adm1", "adm2", "adm1_guid", "adm2_guid")`, so a present-but-stale
  GUID no longer blocks recovery of a missing name.

- verbose:

  Emit a cli summary. Default `TRUE`.

## Value

`data` with `adm1`/`adm2`/`adm1_guid`/`adm2_guid` filled where the
coordinates resolved to a single district; all rows retained.

## Examples

``` r
if (FALSE) { # \dontrun{
shp <- qs2::qs_read("spatial_global_adm2.qs2")
impute_geo_from_coords(cases, shp)
} # }
```
