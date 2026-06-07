# Extract the country code from an EPID

Returns the leading country code: the first run of `n` word-characters,
matching how upstream systems parse the code.

## Usage

``` r
epid_country_code(epid, n = 3, upper = TRUE)
```

## Arguments

- epid:

  Character vector of EPID strings.

- n:

  Number of leading word-characters that form the code. Default `3`.

- upper:

  Whether to upper-case the result. Default `TRUE`.

## Value

Character vector of country codes (`NA` where none is found).

## Examples

``` r
epid_country_code(c("NIE-BOS-XYZ-24-001", "ago-lua-01", NA))
#> [1] "NIE" "AGO" NA   
```
