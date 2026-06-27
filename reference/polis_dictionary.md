# POLIS data dictionary (raw or cleaned schema)

Returns the packaged data dictionary as either the raw POLIS download
schema or the cleaned-output schema. Both are views of the same
[`polis_crosswalk()`](https://truenomad.github.io/polished/reference/polis_crosswalk.md).

## Usage

``` r
polis_dictionary(type = c("clean", "raw"), table = NULL)
```

## Arguments

- type:

  Which dictionary to return:

  - `"clean"` (default) – one row per column the cleaners emit (the
    canonical `Snake_Name`s, including the derived and indicator
    columns). Excludes raw fields the cleaners drop and the raw
    poliovirus columns outside the curated
    [`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
    subset.

  - `"raw"` – one row per raw POLIS API column
    [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
    downloads, with the `Snake_Name` each is renamed to.

- table:

  Optional POLIS source table(s) to keep (e.g. `"Case"`, `"EnvSample"`).
  `NULL` (default) returns every table.

## Value

A tibble with three columns: `data_type` (the POLIS source table/stream
the column belongs to, e.g. `"Case"`, `"EnvSample"`, `"Indicators"`),
`column_name` (the raw `API_Name` when `type = "raw"`, the cleaned
`Snake_Name` when `type = "clean"`) and `label` (the description).

## See also

[`polis_crosswalk()`](https://truenomad.github.io/polished/reference/polis_crosswalk.md),
the full raw-to-clean mapping this reads.

## Examples

``` r
head(polis_dictionary("clean"))
#> # A tibble: 6 × 3
#>   data_type column_name                                      label              
#>   <chr>     <chr>                                            <chr>              
#> 1 Activity  activity_admin_coverage_percentage               Activity admin cov…
#> 2 Activity  activity_parent_im_hh_missed_children_percentage Activity parent in…
#> 3 Activity  activity_parent_im_oh_missed_children_percentage Activity parent in…
#> 4 Activity  activity_parent_lqas_fail_percentage             Activity parent lo…
#> 5 Activity  activity_parent_lqas_pass_percentage             Activity parent lo…
#> 6 Activity  admin0shape_id                                   Country (admin lev…
head(polis_dictionary("raw", table = "Case"))
#> # A tibble: 6 × 3
#>   data_type column_name      label                                     
#>   <chr>     <chr>            <chr>                                     
#> 1 Case      Admin0ShapeId    Country (admin level 0) — shape ID.       
#> 2 Case      Admin1ShapeId    Province/state (admin level 1) — shape ID.
#> 3 Case      Admin2ShapeId    District (admin level 2) — shape ID.      
#> 4 Case      CaseManualEditId Internal identifier for case manual edit. 
#> 5 Case      CountryISO2Code  Country ISO country code 2 code.          
#> 6 Case      DatasetCodes     Dataset codes.                            
```
