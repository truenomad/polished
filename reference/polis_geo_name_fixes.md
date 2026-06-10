# Geographic name-fix lookup table

Returns the curated table of geographic name normalisations applied
during preprocessing, read from `inst/extdata/geo_name_fixes.csv`. Each
row is one rule:

- `field`:

  Logical field the rule targets: `"adm0_name"`, `"adm1_name"` or
  `"location"`.

- `match_type`:

  How to match: `"contains"` (replace the whole value if the pattern
  occurs anywhere), `"exact"` (replace only on an exact match), or
  `"substr"` (replace the matched substring in place).

- `pattern`:

  Literal string to match (not a regex).

- `replacement`:

  Replacement value.

- `note`:

  Why the rule exists.

## Usage

``` r
polis_geo_name_fixes()
```

## Value

A tibble of name-fix rules.

## Examples

``` r
polis_geo_name_fixes()
#> # A tibble: 5 × 5
#>   field     match_type pattern                                 replacement note 
#>   <chr>     <chr>      <chr>                                   <chr>       <chr>
#> 1 adm0_name contains   IVOIRE                                  COTE D IVO… Cote…
#> 2 adm1_name exact      KHYBER PAKHTOON                         NWFP        Paki…
#> 3 adm1_name exact      FATA                                    NWFP        Paki…
#> 4 location  substr     ISLAMIC REPUBLIC OF IRAN                IRAN (ISLA… Iran…
#> 5 location  substr     UNITED KINGDOM OF GREAT BRITAIN AND NO… THE UNITED… UK l…
```
