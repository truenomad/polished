# POLIS column crosswalk

Returns the packaged crosswalk that maps raw POLIS API column names to
the canonical `Snake_Name` used across cleaned datasets.

## Usage

``` r
polis_crosswalk()
```

## Value

A tibble with columns `Table`, `API_Name`, `Snake_Name`, `Web_Name`,
`Label`.

## Examples

``` r
head(polis_crosswalk())
#> # A tibble: 6 × 6
#>   Table    API_Name                              Snake_Name Web_Name Label note 
#>   <chr>    <chr>                                 <chr>      <chr>    <chr> <chr>
#> 1 Activity ActivityAdminCoveragePercentage       activity_… Activit… Acti… NA   
#> 2 Activity ActivityParent_IM_HH_MissedChildrenP… activity_… Activit… Acti… NA   
#> 3 Activity ActivityParent_IM_OH_MissedChildrenP… activity_… Activit… Acti… NA   
#> 4 Activity ActivityParent_LqasFailPercentage     activity_… Activit… Acti… NA   
#> 5 Activity ActivityParent_LqasPassPercentage     activity_… Activit… Acti… NA   
#> 6 Activity Admin0ShapeId                         admin0sha… Admin0S… Coun… NA   
```
