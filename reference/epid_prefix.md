# Geographic prefix used for prefix-matching

Returns the leading `length` characters of the normalised EPID (trimmed,
whitespace-collapsed, upper-cased) – the country+province+ district stem
used to recover geography from sibling records.

## Usage

``` r
epid_prefix(epid, length = 11)
```

## Arguments

- epid:

  Character vector of EPID strings.

- length:

  Number of leading characters in the prefix. Default `11`.

## Value

Character vector of prefixes (`NA` where the EPID is blank).

## Examples

``` r
epid_prefix(c("NIE-BOS-XYZ-24-001", NA))
#> [1] "NIE-BOS-XYZ" NA           
```
