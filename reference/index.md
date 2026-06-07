# Package index

## Download POLIS data

The download entry point and the table catalogue it fetches: a one-call
pull that works around POLIS’s pagination and date-filter quirks, with
on-disk caching, resume, and post-download completeness verification.

- [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
  : Download POLIS tables
- [`polis_tables_mapping`](https://truenomad.github.io/polished/reference/polis_tables_mapping.md)
  : POLIS table catalogue

## Impute geography from EPIDs

The main entry point: a provenance-stamped cascade that fills missing
admin names and GUIDs from EPIDs, without overwriting present values or
fabricating on ambiguity.

- [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
  : Recover administrative geography from the EPID

## Building blocks

The exported pieces the cascade is assembled from — parsers, the
sibling-record lookups, and the country resolver. You rarely call these
directly.

- [`epid_split()`](https://truenomad.github.io/polished/reference/epid_split.md)
  : Split an EPID into its component segments
- [`epid_country_code()`](https://truenomad.github.io/polished/reference/epid_country_code.md)
  : Extract the country code from an EPID
- [`epid_prefix()`](https://truenomad.github.io/polished/reference/epid_prefix.md)
  : Geographic prefix used for prefix-matching
- [`epid_strip_contact()`](https://truenomad.github.io/polished/reference/epid_strip_contact.md)
  : Separate a contact EPID from its base case EPID
- [`build_admin_ref()`](https://truenomad.github.io/polished/reference/build_admin_ref.md)
  : Build an EPID -\> admin-value reference (most-recent-per-EPID)
- [`build_prefix_ref()`](https://truenomad.github.io/polished/reference/build_prefix_ref.md)
  : Build a (prefix, year) -\> unique admin-value reference
- [`resolve_epid_country()`](https://truenomad.github.io/polished/reference/resolve_epid_country.md)
  : Resolve an EPID country code to a country name
