# Derive the AFP-style virus classification and detection flags for ES

The environmental analogue of
[`clean_afp_classification()`](https://truenomad.github.io/polished/reference/clean_afp_classification.md):
it decodes the same poliovirus vocabulary so a single downstream filter
(`grepl("WPV|cVDPV", classification_all)`) works identically across the
human (AFP) and environmental streams. Detection is read from the
combined `virus_types` string and the `vdpv_classifications` field – the
ES equivalents of POLIS `polio_virus_types` / `vdpv_classifications` –
using standard **WPV** (wild poliovirus) nomenclature, *not* the legacy
`WILD n` strings.

## Usage

``` r
clean_es_classification(data)
```

## Arguments

- data:

  A cleaned ES data frame carrying at least `virus_types` (and ideally
  `vdpv_classifications`). Any source column may be absent – each
  derived column is added only when its inputs are present.

## Value

`data` with `virus_type` (the normalised full virus-type list), `vtype`,
`classification_all`, `sabin1`/`sabin2`/`sabin3`, `npev`, `nvaccine` and
`ev_detect` added where derivable; the raw POLIS columns are left
untouched.

## Details

Two layers, mirroring the AFP cleaner:

1.  `vtype` decodes the specific poliovirus. A VDPV always carries an
    explicit kind prefix from `vdpv_classifications` – `cVDPV`
    (circulating), `aVDPV` (ambiguous), `iVDPV` (immune-deficient) – so
    the three are never merged; an untyped `VDPV n` only remains when
    the kind is unknown. Samples with no poliovirus are `none` (and `NA`
    when the sample was never typed).

2.  `classification_all` is the single analysis label: the `vtype` virus
    string for poliovirus-positive samples, otherwise the sample outcome
    – `SABIN` (Sabin vaccine virus only), `NPEV` (non-polio enterovirus
    only), `NEGATIVE` (tested negative) or `PENDING` (classification
    pending); samples matching none stay `none`/`NA`.

The decoding engine (`.polis_classify_virus()`) is shared with
[`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md):
ES samples and human lab specimens have the same lab-result structure
(`virus_types` plus a VDPV classification, which may arrive as the
plural `vdpv_classifications` or the singular `vdpv_classification`), so
both reuse one classifier.

## Classification vocabulary (match on these prefixes, not free text)

- Wild:

  `WPV 1`, `WPV 2`, `WPV 3`, `WPV1andWPV3` – prefix `WPV`.

- Circulating VDPV:

  `cVDPV 1/2/3` and combinations – prefix `cVDPV`.

- Ambiguous VDPV:

  `aVDPV 1/2/3` – prefix `aVDPV`. Labelled, never folded into `cVDPV`.

- Immune-deficient VDPV:

  `iVDPV 1/2/3` – prefix `iVDPV`.

- Untyped VDPV:

  `VDPV 1/2/3` – kind unknown.

- Wild + VDPV co-detection:

  `WPV1and...` (e.g. `WPV1andcVDPV 2`).

- Sample outcome:

  `SABIN`, `NPEV`, `NEGATIVE`, `PENDING`, `none`.

Alongside the labels it derives the Sabin-detection flags `sabin1` /
`sabin2` / `sabin3` (per serotype, exactly as the AFP cleaner), the
non-polio-enterovirus flag `npev`, the novel-OPV2 flag `nvaccine` and
the fused `ev_detect` ("any poliovirus or enterovirus detected"). Every
condition is NA-safe: a missing source value leaves the prior value
intact rather than nulling it. The raw `virus_types` and
`vdpv_classifications` columns are kept.

## Examples

``` r
clean_es_classification(data.frame(
  virus_types = c("cVDPV2", "WILD1", "NPEV, VACCINE3", NA),
  vdpv_classifications = c("Circulating", NA, NA, NA),
  is_npev = c(NA, NA, TRUE, NA)
))
#>      virus_types vdpv_classifications is_npev      virus_type   vtype sabin1
#> 1         cVDPV2          Circulating      NA         cVDPV 2 cVDPV 2      0
#> 2          WILD1                 <NA>      NA          WILD 1   WPV 1      0
#> 3 NPEV, VACCINE3                 <NA>    TRUE NPEV, VACCINE 3    none      0
#> 4           <NA>                 <NA>      NA            <NA>    <NA>     NA
#>   sabin2 sabin3 npev nvaccine classification_all ev_detect
#> 1      0      0    0        0            cVDPV 2         1
#> 2      0      0    0        0              WPV 1         1
#> 3      0      1    1        0              SABIN         1
#> 4     NA     NA    0        0               <NA>         0
```
