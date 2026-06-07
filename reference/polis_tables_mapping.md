# POLIS table catalogue

Static mapping of the tables
[`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
supports.

## Usage

``` r
polis_tables_mapping
```

## Format

A data.frame with one row per supported table and columns:

- table_name:

  Short identifier used by `tables = "..."` and as the canonical
  filename stem.

- endpoint:

  OData endpoint suffix appended to
  `https://extranet.who.int/polis/api/v2/`.

- date_field:

  Column used for both the OData filter and the dedup tiebreaker.

## Details

Each `date_field` is the "update" column the package uses when
filtering. Probes against POLIS confirmed each value is 100%-populated
AND clustered post-2010 (records were imported into POLIS then), so a
filter on this field catches every row in the table including pre-2000
legacy records. The clinical/event columns (`CaseDate`, `VirusDate`,
`CollectionDate`) are skipped because they contain pre-2000 legacy dates
that fall outside typical user-supplied ranges.
