# Clean WHO administrative spatial data

Reads the country (ADM0), province (ADM1) and district (ADM2) boundary
layers from any source
[sf](https://r-spatial.github.io/sf/reference/st_read.html) can open and
turns each into a cleaned, validity-checked, reprojected `sf` object
written to `output_dir`. For ADM1 and ADM2 a year-expanded "long" shape
(one row per active year) is also written, ready for temporal
point-in-polygon joins.

## Usage

``` r
process_spatial(
  input_path,
  output_dir,
  layers = c(adm0 = "GLOBAL_ADM0", adm1 = "GLOBAL_ADM1", adm2 = "GLOBAL_ADM2"),
  transform = TRUE,
  crs = 4326,
  fix_issues = TRUE,
  sliver_area = 10000,
  output_format = "rds",
  verbose = TRUE
)
```

## Arguments

- input_path:

  Path to the spatial source: a `.gdb` / `.gpkg` dataset, or a folder of
  standalone `.shp` / `.gpkg` / `.geojson` files.

- output_dir:

  Directory for cleaned outputs and the `checks/` subfolder.

- layers:

  Named character vector mapping admin levels to a layer name (for a
  `.gdb` / `.gpkg`) or a filename stem (for a folder of files). Names
  must be a subset of `adm0` / `adm1` / `adm2`; only the levels present
  are processed, so pass a single entry to clean one level. Matching is
  case-insensitive, preferring an exact match and otherwise the
  substring match. Defaults to all three WHO `GLOBAL_ADM*` layers.

- transform:

  Whether to reproject each layer to `crs`. The reprojection is skipped
  automatically when a layer's CRS is already equivalent to `crs`
  (ignoring cosmetic CRS-label differences), so no coordinates are swept
  needlessly. Default `TRUE`.

- crs:

  Target CRS as an EPSG code. Default `4326` (WGS84).

- fix_issues:

  Whether to repair geometries (in addition to flagging them): drop Z/M
  dimensions, make invalid geometries valid
  ([`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)),
  and strip sliver holes and sliver polygon parts smaller than
  `sliver_area`. Real enclaves, lakes and islands above the threshold
  are kept, and a feature never loses its largest part. Set `FALSE` for
  the fastest run (flag-only). Default `TRUE`.

- sliver_area:

  Area threshold in square metres below which interior holes and
  detached polygon parts are treated as digitising artifacts and removed
  when `fix_issues = TRUE`. Default `1e4` (1 hectare).

- output_format:

  Serialization format for the cleaned shapes, `"rds"` or `"qs2"`.
  `"qs2"` writes and reads large geometries far faster (needs the qs2
  package). Default `"rds"`.

- verbose:

  Whether to print a cli progress summary. Default `TRUE`.

## Value

`output_dir`, invisibly. The function's outputs are the files it writes
(`{ext}` is `output_format`):

- `spatial_global_{level}.{ext}`: cleaned per-level shapes;

- `spatial_{level}_long_shape.{ext}`: year-expanded shapes (ADM1, ADM2);

- `checks/spatial_*_{level}_*.csv`: any validity/duplicate issues.

## Details

`input_path` may be any of:

- an Esri geodatabase (`.gdb`) or GeoPackage (`.gpkg`) holding the three
  admin layers, matched by name via `layers`;

- a folder of standalone files – shapefiles (`.shp`), GeoPackages
  (`.gpkg`) or GeoJSON (`.geojson` / `.json`) – one per level, matched
  by filename via `layers`.

Per layer the cleaner:

- reads the layer and snake_cases its columns (via janitor);

- parses `startdate` / `enddate` to lubridate::Date and derives
  `year_start` / `year_end`;

- normalises the Cote d'Ivoire country name;

- renames the admin name columns to the package's canonical `adm0` /
  `adm1` / `adm2`, the level GUID column to `adm0_guid` / `adm1_guid` /
  `adm2_guid`, and the geometry column to `shape`;

- runs validity, empty-geometry and duplicate (GUID and name) checks,
  writing a CSV per issue found into a `checks/` subfolder;

- when `fix_issues = TRUE`, repairs geometries: drops Z/M, makes invalid
  geometries valid, and removes sliver holes and sliver polygon parts
  below `sliver_area`;

- reprojects to `crs` when `transform` is `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
# an Esri geodatabase with the GLOBAL_ADM* layers
process_spatial("path/to/who.gdb", output_dir = "outputs/spatial")

# a folder of shapefiles named adm0.shp / adm1.shp / adm2.shp
process_spatial(
  "path/to/shapefiles",
  output_dir = "outputs/spatial",
  layers = c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2")
)
} # }
```
