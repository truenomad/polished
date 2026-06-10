# Place a random point inside the district polygon for cases missing coordinates

For each case whose coordinates are missing or `(0, 0)`, draws a random
point uniformly within its district (`adm2`) polygon, so downstream maps
and point-in-polygon work have a usable location. Cases with valid
coordinates are untouched. A district whose polygon cannot be sampled
falls back to its centroid buffered by `fallback_buffer` metres.

## Usage

``` r
impute_missing_coords(
  data,
  shape_adm2,
  guid_var = "adm2_guid",
  lon_var = "longitude",
  lat_var = "latitude",
  shape_guid_var = "adm2_guid",
  seed = 1234,
  fallback_buffer = 3000,
  verbose = TRUE
)
```

## Arguments

- data:

  A case data frame carrying an ADM2 GUID and coordinate columns.

- shape_adm2:

  An `sf` object of district polygons with an `adm2_guid` column (the
  `spatial_global_adm2` layer from
  [`process_spatial()`](https://truenomad.github.io/polished/reference/process_spatial.md)).

- guid_var:

  Case ADM2 GUID column. Default `"adm2_guid"`.

- lon_var, lat_var:

  Case coordinate columns. Default `"longitude"` / `"latitude"`.

- shape_guid_var:

  ADM2 GUID column in `shape_adm2`. Default `"adm2_guid"`.

- seed:

  Integer seed for reproducible sampling. Default `1234`.

- fallback_buffer:

  Buffer radius in metres for the centroid fallback when a polygon
  cannot be sampled. Default `3000`.

- verbose:

  Emit a cli summary. Default `TRUE`.

## Value

`data` with `lon_var`/`lat_var` filled for sampled rows and a logical
`coord_imputed` column marking them.

## Examples

``` r
if (FALSE) { # \dontrun{
shp <- qs2::qs_read("spatial_global_adm2.qs2")
impute_missing_coords(cases, shp)
} # }
```
