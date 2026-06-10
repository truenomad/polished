# Flag ES site names absent from a reference site list

Diagnostic check: environmental site names present in `data` but missing
from the reference `sites` list are flagged – particularly new sites
that also lack coordinates, which usually signal a data-entry issue
rather than a genuine new site. Names are compared upper-cased and
whitespace-squished (embedded newlines collapsed) on both sides. `data`
is returned unchanged, with the unmatched sites attached as the
`"polis_new_sites"` attribute, so the check composes into
[`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
without writing to disk or touching global state.

## Usage

``` r
validate_es_sites(data, sites, site_col = "site_name", verbose = TRUE)
```

## Arguments

- data:

  A cleaned ES data frame (canonical names) carrying `site_name`.

- sites:

  Reference site list: a data frame with a site-name column, or a
  character vector of known site names.

- site_col:

  Name of the site-name column in `data` and `sites` (default
  `"site_name"`).

- verbose:

  Emit a cli warning when unknown sites are found. Default `TRUE`.

## Value

`data`, unchanged, with attribute `"polis_new_sites"`: a tibble with
columns `site_name` (character, the raw site label from `data`) and
`no_coords` (logical, `TRUE` when the site lacks a coordinate value,
`NA` when no coordinate column is present in `data`), one row per
distinct unmatched site.

## Examples

``` r
es <- data.frame(
  site_name = c("SITE A", "SITE B"),
  site_y_coordinate = c(6.5, NA)
)
out <- validate_es_sites(es, sites = "SITE A")
#> ! 1 ES site not in the reference list (1 without coordinates); flagged, not dropped.
attr(out, "polis_new_sites")
#> # A tibble: 1 × 2
#>   site_name no_coords
#>   <chr>     <lgl>    
#> 1 SITE B    TRUE     
```
