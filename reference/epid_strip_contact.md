# Separate a contact EPID from its base case EPID

Contact records reuse a case EPID with a trailing contact marker (`C`,
`CC`, `HC`, or `C` followed by digits). This splits the marker off so
contacts collapse onto their parent case for matching.

## Usage

``` r
epid_strip_contact(epid)
```

## Arguments

- epid:

  Character vector of EPID strings.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
`epid_base` (marker removed) and `contact_code` (the extracted marker,
`NA` when absent).

## Examples

``` r
epid_strip_contact(c("NIE-BOS-XYZ-24-001", "NIE-BOS-XYZ-24-001CC"))
#> # A tibble: 2 × 2
#>   epid_base          contact_code
#>   <chr>              <chr>       
#> 1 NIE-BOS-XYZ-24-001 NA          
#> 2 NIE-BOS-XYZ-24-001 CC          
```
