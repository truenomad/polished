# Recover administrative geography from the EPID

Fills missing administrative names (and optionally GUIDs) using the EPID
as a recovery key, through an ordered, provenance-stamped cascade. Only
blank cells are filled; present values are never overwritten and nothing
is fabricated on ambiguity.

## Usage

``` r
impute_geo_from_epid(
  data,
  epid_var = "epid",
  year_var = "year_onset",
  admin0_var = "adm0",
  admin1_var = "adm1",
  admin2_var = "adm2",
  guid_vars = c(adm2 = "adm2_guid"),
  reference = NULL,
  country_ref = NULL,
  strategies = c("self_ref", "prefix_match", "reference", "country_prefix"),
  prefix_length = 11,
  year_window = 0,
  sep = "-",
  canonicalise = TRUE,
  audit = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Non-empty data frame carrying an EPID column.

- epid_var:

  EPID column name. Default `"epid"`.

- year_var:

  Year/onset column name. Default `"year_onset"`.

- admin0_var, admin1_var, admin2_var:

  Admin name columns; `NULL` skips a level. Defaults
  `"adm0"`/`"adm1"`/`"adm2"`.

- guid_vars:

  Named character vector mapping levels (`adm0`/`adm1`/ `adm2`) to GUID
  columns to fill. Default `c(adm2 = "adm2_guid")`; set `NULL` to fill
  no GUIDs.

- reference:

  Optional external table (keyed on `epid` or `prefix`) of admin
  names/GUIDs, for data with no names of its own. Default `NULL`.

- country_ref:

  Optional country code -\> name/ISO3 crosswalk. Default `NULL`.

- strategies:

  Ordered subset of
  `c("self_ref", "prefix_match", "reference", "country_prefix")`.
  Default uses all four.

- prefix_length:

  Prefix length for prefix-matching. Default `11`.

- year_window:

  Allowed +/- year distance when prefix-matching. Default `0`.

- sep:

  EPID segment delimiter. Default `"-"`.

- canonicalise:

  Whether to canonicalise filled name cells when a canonicaliser is
  available. Default `TRUE`.

- audit:

  Whether to build the per-level self-reference tables returned in
  `$ref` (for inspection). Default `TRUE`; set `FALSE` to skip the extra
  reference passes when the audit handle is not needed.

- verbose:

  Whether to print a cli summary. Default `TRUE`.

## Value

A named list:

- data:

  `data` with filled admin/GUID columns plus a `<col>_source` provenance
  factor per filled column.

- ref:

  The self-reference lookups built (for audit).

- qa:

  A tibble of per-level fill counts and `pct_resolved`.

- meta:

  The settings used.

## Details

The cascade, per admin level (Admin0, then Admin1, then Admin2):

- original:

  Value already present – kept.

- self_ref:

  Most-recent non-blank value for the exact same EPID elsewhere in
  `data`.

- prefix_match:

  Unique value among records sharing the geographic prefix within
  `year_window` years; a parent-level tie-break is applied before
  declaring ambiguity.

- reference:

  An external `reference` table keyed on EPID or prefix.

- country_prefix:

  Admin0 only – the country code resolved via `country_ref`.

Any cell still blank after the cascade is labelled `unresolved`.

## Examples

``` r
cases <- tibble::tibble(
  epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002", "AGO-LUA-BBB-24-001"),
  year_onset = c(2024, 2024, 2024),
  adm0 = c("NIGERIA", NA, "ANGOLA"),
  adm1 = c("BORNO", NA, "LUANDA"),
  adm2 = c("BOSSO", NA, "LUANDA"),
  adm2_guid = c("g-bosso", NA, "g-luanda")
)
result <- impute_geo_from_epid(cases, verbose = FALSE)
result$qa
#> # A tibble: 4 × 10
#>   level  column    n_missing_before n_filled_self_ref n_filled_prefix_match
#>   <chr>  <chr>                <int>             <int>                 <int>
#> 1 admin0 adm0                     1                 0                     1
#> 2 admin1 adm1                     1                 0                     1
#> 3 admin2 adm2                     1                 0                     1
#> 4 adm2   adm2_guid                1                 0                     1
#> # ℹ 5 more variables: n_filled_reference <int>, n_filled_country_prefix <int>,
#> #   n_ambiguous <int>, n_unresolved <int>, pct_resolved <dbl>
```
