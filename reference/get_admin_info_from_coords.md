# Recover administrative info for point data via a spatial join

Builds point geometries from a data frame's longitude/latitude columns,
spatially joins them to a district (`adm2`) shape table, optionally
filtering the join to shapes whose validity window
(`year_start`..`year_end`) contains the point's year, and fills any
missing admin names/GUIDs from the matched shape. Points that match more
than one shape (e.g. on a boundary) are dropped rather than guessed.

## Usage

``` r
get_admin_info_from_coords(
  data,
  shp_adm2,
  year_col = NULL,
  lon_var = "longitude",
  lat_var = "latitude",
  crs = 4326
)
```

## Arguments

- data:

  A data frame with `lon_var` and `lat_var` columns, and optionally
  `adm0` and a `year_col`.

- shp_adm2:

  An `sf` object of district boundaries carrying the canonical admin
  name columns (`adm0` / `adm1` / `adm2`) and GUID columns, and, for
  temporal filtering, `year_start` / `year_end`.

- year_col:

  Optional name of the year column in `data`. When supplied and present,
  the join is filtered to temporally valid shapes. Default `NULL`.

- lon_var:

  Longitude column name. Default `"longitude"`.

- lat_var:

  Latitude column name. Default `"latitude"`.

- crs:

  CRS of the input coordinates as an EPSG code. Default `4326`.

## Value

A data frame (geometry dropped) with imputed `adm1` / `adm1_guid` /
`adm2` / `adm2_guid` where they were missing, restricted to rows with an
unambiguous district match.

## Examples

``` r
if (FALSE) { # \dontrun{
get_admin_info_from_coords(case_points, shp_adm2, year_col = "year_onset")
} # }
```
