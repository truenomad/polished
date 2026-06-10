# Derive the fused AFP virus type and analytic classification

Decodes the specific poliovirus and fuses it with the case
classification into one analytic label, using standard **WPV** (wild
poliovirus) nomenclature throughout – *not* the legacy `WILD n` strings,
which do not match how downstream surveillance code filters
(`grepl("WPV|cVDPV", ...)`).

## Usage

``` r
clean_afp_classification(data)
```

## Arguments

- data:

  A cleaned AFP data frame carrying at least `classification` (and
  ideally `polio_virus_types`, `vdpv_classifications`).

## Value

`data` with `vtype`, `vtype_fixed`, `classification_all`,
`sabin1`/`sabin2`/`sabin3` and (when derivable) `hot_case` added; the
raw `classification`, `polio_virus_types` and `vdpv_classifications`
columns are left untouched.

## Details

Two layers:

1.  `vtype` / `vtype_fixed` decode the virus from `polio_virus_types` +
    `vdpv_classifications`. A VDPV always carries an explicit kind
    prefix – `cVDPV` (circulating), `aVDPV` (ambiguous), `iVDPV`
    (immune-deficient) – so the three are never merged or silently
    dropped; an untyped `VDPV n` only remains when the kind is genuinely
    unknown. A few historical country corrections patch early records
    (Congo 2010, Nigeria 2011, pre-2010 wild) where the virus field was
    not yet populated.

2.  `classification_all` is the single analysis label: the `vtype_fixed`
    virus string for virus-positive cases, otherwise the raw POLIS
    `classification` recoded – Discarded -\> NPAFP, Compatible -\>
    COMPATIBLE, Not an AFP -\> NOT-AFP, Pending -\> PENDING (LAB PENDING
    when the specimen never reached the lab), VAPP -\> VAPP, Not
    Applicable/Others/VDPV -\> UNKNOWN. Cases matching none stay
    `none`/`NA` for manual review.

## Classification vocabulary (match on these prefixes, not free text)

- Wild:

  `WPV 1`, `WPV 2`, `WPV 3`, `WPV1andWPV3` – prefix `WPV`.

- Circulating VDPV:

  `cVDPV 1/2/3` and combinations – prefix `cVDPV`.

- Ambiguous VDPV:

  `aVDPV 1/2/3` – prefix `aVDPV`. **Include/exclude is a deliberate
  analyst choice**; these are labelled, never folded into `cVDPV`.

- Immune-deficient VDPV:

  `iVDPV 1/2/3` – prefix `iVDPV`. Same explicit choice as `aVDPV`.

- Untyped VDPV:

  `VDPV 1/2/3` – a VDPV whose kind is unknown.

- Wild + VDPV co-detection:

  `WPV1and...` (e.g. `WPV1andcVDPV 2`).

- Non-virus:

  `NPAFP`, `COMPATIBLE`, `NOT-AFP`, `PENDING`, `LAB PENDING`, `VAPP`,
  `UNKNOWN`.

So "any WPV1" is `grepl("^WPV 1|^WPV1and", classification_all)`, and
"any circulating VDPV2" is `grepl("cVDPV 2", classification_all)`.

Also derives the Sabin-detection flags (`sabin1`/`sabin2`/`sabin3`) and,
where the paralysis fields are present, a recomputed `hot_case` (POLIS
also ships `paralysis_hot_case`; this applies the standard asymmetric +
onset-fever + rapid-progression definition, which can differ). Every
condition is NA-safe: a missing classification/admin/year leaves the
prior value intact rather than nulling it.

## Examples

``` r
clean_afp_classification(data.frame(
  classification = c("Discarded", "Confirmed (wild)"),
  polio_virus_types = c(NA, "WILD1"),
  vdpv_classifications = c(NA, NA)
))
#>     classification polio_virus_types vdpv_classifications vtype vtype_fixed
#> 1        Discarded              <NA>                   NA  <NA>        <NA>
#> 2 Confirmed (wild)             WILD1                   NA WPV 1       WPV 1
#>   classification_all sabin1 sabin2 sabin3
#> 1              NPAFP     NA     NA     NA
#> 2              WPV 1      0      0      0
```
