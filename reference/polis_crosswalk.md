# POLIS column crosswalk

Returns the packaged crosswalk that maps raw POLIS API column names to
the canonical `Snake_Name` used across cleaned datasets.

## Usage

``` r
polis_crosswalk()
```

## Value

A tibble with columns `Table`, `API_Name`, `Snake_Name`, `Web_Name`,
`Label`, `note`, `clean`. Rows with a blank `API_Name` document columns
the cleaners *derive* (no raw POLIS source); `note` records how each is
derived. `clean` is `TRUE` when the column is emitted in the cleaned
output (`FALSE` for raw fields dropped or not carried through by the
cleaners).

## Examples

``` r
head(polis_crosswalk())
#> # A tibble: 6 × 7
#>   Table    API_Name                        Snake_Name Web_Name Label note  clean
#>   <chr>    <chr>                           <chr>      <chr>    <chr> <chr> <lgl>
#> 1 Activity ActivityAdminCoveragePercentage activity_… Activit… Acti… NA    TRUE 
#> 2 Activity ActivityParent_IM_HH_MissedChi… activity_… Activit… Acti… NA    TRUE 
#> 3 Activity ActivityParent_IM_OH_MissedChi… activity_… Activit… Acti… NA    TRUE 
#> 4 Activity ActivityParent_LqasFailPercent… activity_… Activit… Acti… NA    TRUE 
#> 5 Activity ActivityParent_LqasPassPercent… activity_… Activit… Acti… NA    TRUE 
#> 6 Activity Admin0ShapeId                   admin0sha… Admin0S… Coun… NA    TRUE 
```
