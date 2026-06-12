# polished 0.1.0

* Initial development version.
* Extended `calc_polio_indicators()` from 4 to the full WHO POLIS indicator
  catalogue (~62 indicators across AFP, Stool, Dose, Timeliness, Lab, ES,
  Virus, SIA and Composite families), driven by DRY generators and a unified
  registry that `available_indicators()` reads (queryable by `family`). New
  optional source inputs (`virus`, `es`, `sia`, `lab`, `admin_units`); missing
  sources/columns skip with a warning rather than erroring.
* `get_polis_data()` now supports the `population` reference table. It carries no
  usable update date, so it is pulled whole in a single Id-paginated pass with no
  date/region filter (`date_field = NA` in `polis_tables_mapping`). Note: the
  endpoint/field still need a live-API sanity check before relying on it.
* Added the EPID-driven geography cleaner: `impute_geo_from_epid()` plus the
  `epid_split()` / `epid_country_code()` / `epid_prefix()` /
  `epid_strip_contact()` parsers, the `build_admin_ref()` /
  `build_prefix_ref()` reference builders, and `resolve_epid_country()`.
