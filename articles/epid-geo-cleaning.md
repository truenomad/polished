# Recovering geography from EPIDs

> **Note**
>
> Every table in this vignette is **synthetic** — fabricated EPIDs and
> place names, never real case data.

## The problem this solves

Surveillance extracts are rarely complete. The same pull will often
contain:

- rows that are **fully geocoded** — country, province, district, and
  the matching GUIDs all present; and
- rows where some or all of that admin geography is **missing** — but
  which still carry an **EPID**.

An EPID is a structured case identifier, conventionally
`COUNTRY-PROVINCE-DISTRICT-YEAR-SERIAL` (e.g. `NIE-BOR-MMC-24-001`). The
leading three characters are a country code; the next segments are
province and district abbreviations. So even when the *names* are blank,
the EPID still encodes *where* the case is.

That is the whole idea: **when one row tells us that the code
`NIE-BOR-MMC` means Maiduguri, Borno, Nigeria, we can carry that
geography to every other row whose EPID shares the same code** — without
ever inventing a place that the data doesn’t already vouch for.

[`polished::impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
does exactly this triangulation. Here is the kind of messy input it is
built for:

| epid                 | adm0    | adm2      | adm2_guid |
|:---------------------|:--------|:----------|:----------|
| NIE-BOR-MMC-24-001   | NIGERIA | MAIDUGURI | guid-mmc  |
| NIE-BOR-MMC-24-014   | NIGERIA | NA        | NA        |
| NIE-BOR-MMC-24-001CC | NIGERIA | NA        | NA        |
| NIE-BOR-JER-23-007   | NIGERIA | JERE      | guid-jere |
| NIE-BOR-JER-24-021   | NIGERIA | NA        | NA        |
| AGO-LUA-CAC-24-002   | NA      | CACUACO   | guid-cac  |
| NIE-YOB-DAM-24-001   | NIGERIA | DAMATURU  | guid-dam  |
| NIE-YOB-DAM-24-002   | NIGERIA | POTISKUM  | guid-pot  |
| NIE-YOB-DAM-24-009   | NIGERIA | NA        | NA        |
| NIE-KAN-XYZ-24-001   | NIGERIA | NA        | NA        |

Five rows are missing their district (and four their GUID); one is
missing its country. Each gap is recoverable from a *different* kind of
evidence — and one or two are deliberately **not** recoverable, so you
can see what the function does when it cannot be sure.

## One call

You hand
[`polished::impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
the data, tell it which columns hold the EPID, the year, and each admin
level, and (optionally) give it a country crosswalk for resolving
Admin0. It returns a list.

``` r

country_ref <- tibble::tibble(
  code = c("NIE", "AGO"),
  name = c("NIGERIA", "ANGOLA"),
  iso3 = c("NGA", "AGO")
)

result <- polished::impute_geo_from_epid(
  cases,
  epid_var = "epid",
  year_var = "year_onset",
  admin0_var = "adm0",
  admin1_var = "adm1",
  admin2_var = "adm2",
  guid_vars = c(adm2 = "adm2_guid"),
  country_ref = country_ref,
  year_window = 1
)
```

The console summary (shown when `verbose = TRUE`, the default) reports,
per admin level, how many cells were missing and how each gap was
closed. The detail lives in the returned object, which we now unpack.

## What `impute_geo_from_epid()` returns

The result is a named list with four elements:

| Element | What it is                                                     |
|---------|----------------------------------------------------------------|
| `$data` | your input, with gaps filled and a provenance column per level |
| `$qa`   | a one-row-per-level audit of what was filled and what wasn’t   |
| `$ref`  | the lookups it built, exposed so you can inspect the evidence  |
| `$meta` | the settings the run used                                      |

`$data` — the filled table, with provenance

`$data` is your original data frame, **unchanged except that blank cells
are filled**, plus one new `<column>_source` column for every level it
touched. That companion column is the audit trail: it records *how* each
cell got its value.

``` r

result$data |>
  dplyr::select(epid, adm2, adm2_source, adm2_guid)
```

| epid                 | adm2      | adm2_source  | adm2_guid |
|:---------------------|:----------|:-------------|:----------|
| NIE-BOR-MMC-24-001   | MAIDUGURI | original     | guid-mmc  |
| NIE-BOR-MMC-24-014   | MAIDUGURI | prefix_match | guid-mmc  |
| NIE-BOR-MMC-24-001CC | MAIDUGURI | self_ref     | guid-mmc  |
| NIE-BOR-JER-23-007   | JERE      | original     | guid-jere |
| NIE-BOR-JER-24-021   | JERE      | prefix_match | guid-jere |
| AGO-LUA-CAC-24-002   | CACUACO   | original     | guid-cac  |
| NIE-YOB-DAM-24-001   | DAMATURU  | original     | guid-dam  |
| NIE-YOB-DAM-24-002   | POTISKUM  | original     | guid-pot  |
| NIE-YOB-DAM-24-009   | NA        | unresolved   | NA        |
| NIE-KAN-XYZ-24-001   | NA        | unresolved   | NA        |

Every `<col>_source` is a factor with these levels:

| Provenance | Meaning |
|----|----|
| `original` | the cell already had a value — left untouched |
| `self_ref` | filled from another row with the **exact same EPID** |
| `prefix_match` | filled from rows sharing the EPID’s geographic **prefix** |
| `reference` | filled from an external lookup table you supplied |
| `country_prefix` | Admin0 filled from the country code via the crosswalk |
| `unresolved` | was missing and **could not be filled** — left `NA` |

Reading the district column above, row by row:

- `…MMC-24-014` and `…JER-24-021` were filled by **`prefix_match`** — a
  sibling case in the same district (the `JER` donor was a year earlier,
  reached because we set `year_window = 1`).
- `…MMC-24-001CC`, a **contact**, was filled by **`self_ref`**: contacts
  reuse their case’s EPID with a trailing marker, so it collapses onto
  case `001` and inherits its district.
- `…DAM-24-009` is **`unresolved`**: the `NIE-YOB-DAM` code points at
  *two* different districts in the donors (Damaturu and Potiskum), so
  the function refuses to guess.
- `…KAN-XYZ-24-001` is **`unresolved`** too: nothing else in the data
  shares its prefix, and no external reference was given.

The two key guarantees are visible here: a present value is **never
overwritten** (everything that started with a district still reads
`original`), and an ambiguous or unsupported gap is **never invented** —
it stays `NA` and is labelled `unresolved`, so silent fabrication is
impossible.

Admin0 tells the same story through a different strategy:

``` r

result$data |>
  dplyr::filter(adm0_source != "original") |>
  dplyr::select(epid, adm0, adm0_source)
```

| epid               | adm0   | adm0_source    |
|:-------------------|:-------|:---------------|
| AGO-LUA-CAC-24-002 | ANGOLA | country_prefix |

The Angola row had no country name; `country_prefix` recovered it from
the `AGO` code via the crosswalk.

`$qa` — the audit, one row per level

`$qa` is the reconciliation table. It has one row per column the
function tried to fill, and it accounts for **every** missing cell.

``` r

result$qa
```

| level | column | n_missing_before | n_filled_self_ref | n_filled_prefix_match | n_filled_reference | n_filled_country_prefix | n_ambiguous | n_unresolved | pct_resolved |
|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| admin0 | adm0 | 1 | 0 | 0 | 0 | 1 | 0 | 0 | 1.0 |
| admin1 | adm1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1.0 |
| admin2 | adm2 | 5 | 1 | 2 | 0 | 0 | 1 | 2 | 0.6 |
| adm2 | adm2_guid | 5 | 1 | 2 | 0 | 0 | 1 | 2 | 0.6 |

Column by column:

| QA column | What it counts |
|----|----|
| `level` / `column` | the admin level and the data column it refers to |
| `n_missing_before` | blank cells in that column when the run started |
| `n_filled_self_ref` | filled from the same exact EPID |
| `n_filled_prefix_match` | filled from a shared prefix |
| `n_filled_reference` | filled from the external reference table |
| `n_filled_country_prefix` | Admin0 filled from the country code |
| `n_ambiguous` | gaps left blank because the evidence disagreed |
| `n_unresolved` | gaps still blank after every strategy |
| `pct_resolved` | share of the originally-missing cells that got filled |

The counts always reconcile — every missing cell is either filled by
exactly one strategy or left unresolved:

``` r

result$qa |>
  dplyr::transmute(
    column,
    n_missing_before,
    filled = n_filled_self_ref + n_filled_prefix_match +
      n_filled_reference + n_filled_country_prefix,
    n_unresolved,
    reconciles = n_missing_before == filled + n_unresolved
  )
```

| column    | n_missing_before | filled | n_unresolved | reconciles |
|:----------|-----------------:|-------:|-------------:|:-----------|
| adm0      |                1 |      1 |            0 | TRUE       |
| adm1      |                0 |      0 |            0 | TRUE       |
| adm2      |                5 |      3 |            2 | TRUE       |
| adm2_guid |                5 |      3 |            2 | TRUE       |

`n_ambiguous` is a **diagnostic, not a separate bucket**: an ambiguous
cell is also counted in `n_unresolved` (it was missing and stayed
missing). It is broken out so you can tell “we had no evidence” apart
from “we had conflicting evidence” — the latter is where a manual review
or a better reference table pays off.

`$ref` — the evidence it built

To fill from sibling rows, the function first builds
most-recent-per-EPID lookups. It hands them back in `$ref` so you can
audit *what* it considered authoritative:

``` r

result$ref$admin2
```

| .epid_norm         | adm2      |
|:-------------------|:----------|
| AGO-LUA-CAC-24-002 | CACUACO   |
| NIE-BOR-JER-23-007 | JERE      |
| NIE-BOR-JER-24-021 | JERE      |
| NIE-BOR-MMC-24-001 | MAIDUGURI |
| NIE-BOR-MMC-24-014 | MAIDUGURI |
| NIE-YOB-DAM-24-001 | DAMATURU  |
| NIE-YOB-DAM-24-002 | POTISKUM  |

Each row is one EPID and the most recent non-blank value seen for it.
This is the table that powers the `self_ref` step; inspecting it is the
quickest way to see why a given EPID resolved the way it did.

`$meta` — the settings used

`$meta` records exactly how the run was configured — useful for logging
a pipeline so a result is reproducible.

``` r

result$meta
#> $strategies
#> [1] "self_ref"       "prefix_match"   "reference"      "country_prefix"
#> 
#> $prefix_length
#> [1] 11
#> 
#> $year_window
#> [1] 1
#> 
#> $reference_used
#> [1] FALSE
#> 
#> $country_ref_used
#> [1] TRUE
```

## The strategies, and when each earns its keep

[`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
tries strategies in order and stops as soon as a cell is filled. You
control which run, and in what order, with `strategies`.

| Strategy | Use it when… | Evidence |
|----|----|----|
| `self_ref` | the same case (or its contacts) appears more than once | another row with the **identical** EPID |
| `prefix_match` | different cases sit in the same district | rows sharing the `CCC-PPP-DDD` **prefix**, within `year_window` |
| `reference` | the data has *no* names of its own to borrow | an external `epid`- or `prefix`-keyed table you pass in |
| `country_prefix` | only the country is missing | the `country_ref` code → name crosswalk |

Two knobs shape the matching:

- **`year_window`** widens prefix-matching to neighbouring years (we
  used `1` above so a 2023 donor could fill a 2024 case). `0` keeps it
  strict.
- **`prefix_length`** sets how much of the EPID must agree. The default
  `11` covers `CCC-PPP-DDD`; shorten it to match on country+province
  only.

When a prefix maps to two districts,
[`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
first tries a **parent tie-break** — if the row’s province is known, it
keeps only candidates from that province. It declares a gap ambiguous
only when even that cannot single out one answer (as with the Yobe row
above).

### Filling only from an external table

If an extract carries EPIDs but no usable names at all, skip the in-data
strategies and lean entirely on a reference table keyed on `epid` or
`prefix`:

``` r

nameless <- tibble::tibble(
  epid = c("NIE-BOR-MMC-24-031", "AGO-LUA-CAC-24-050"),
  adm2 = c(NA_character_, NA_character_)
)
district_lookup <- tibble::tibble(
  prefix = c("NIE-BOR-MMC", "AGO-LUA-CAC"),
  adm2 = c("MAIDUGURI", "CACUACO")
)
polished::impute_geo_from_epid(
  nameless,
  admin0_var = NULL, admin1_var = NULL, admin2_var = "adm2",
  guid_vars = NULL,
  reference = district_lookup,
  strategies = "reference",
  verbose = FALSE
)$data |>
  dplyr::select(epid, adm2, adm2_source)
```

| epid               | adm2      | adm2_source |
|:-------------------|:----------|:------------|
| NIE-BOR-MMC-24-031 | MAIDUGURI | reference   |
| AGO-LUA-CAC-24-050 | CACUACO   | reference   |

## Running it twice is safe

Because filled cells become `original` on the next pass, re-running the
cleaner on its own output changes nothing — handy when it sits inside a
pipeline that may execute more than once.

``` r

second <- polished::impute_geo_from_epid(
  result$data,
  epid_var = "epid",
  year_var = "year_onset",
  admin0_var = "adm0",
  admin1_var = "adm1",
  admin2_var = "adm2",
  guid_vars = c(adm2 = "adm2_guid"),
  country_ref = country_ref,
  year_window = 1,
  verbose = FALSE
)
identical(second$data$adm2, result$data$adm2)
#> [1] TRUE
```

## Building blocks (advanced)

[`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
is the function you will reach for. The cascade is assembled from
smaller exported pieces, exposed for the rare cases where you want to
parse or match EPIDs yourself:

- [`epid_split()`](https://truenomad.github.io/polished/reference/epid_split.md),
  [`epid_country_code()`](https://truenomad.github.io/polished/reference/epid_country_code.md),
  [`epid_prefix()`](https://truenomad.github.io/polished/reference/epid_prefix.md),
  [`epid_strip_contact()`](https://truenomad.github.io/polished/reference/epid_strip_contact.md)
  — pure parsers that pull codes out of an EPID.
- [`build_admin_ref()`](https://truenomad.github.io/polished/reference/build_admin_ref.md),
  [`build_prefix_ref()`](https://truenomad.github.io/polished/reference/build_prefix_ref.md)
  — the lookups behind `self_ref` and `prefix_match`.
- [`resolve_epid_country()`](https://truenomad.github.io/polished/reference/resolve_epid_country.md)
  — the code → country resolver behind `country_prefix`.

You almost never need these directly; see each function’s help page
(e.g.
[`?epid_prefix`](https://truenomad.github.io/polished/reference/epid_prefix.md))
for details.

``` r

polished::epid_country_code("NIE-BOR-MMC-24-001")
#> [1] "NIE"
polished::epid_prefix("NIE-BOR-MMC-24-001", length = 11)
#> [1] "NIE-BOR-MMC"
polished::epid_strip_contact("NIE-BOR-MMC-24-001CC")
```

| epid_base          | contact_code |
|:-------------------|:-------------|
| NIE-BOR-MMC-24-001 | CC           |
