# Changelog

## polished 0.2.0

CRAN release: 2020-09-29

## polished 0.1.0

CRAN release: 2020-07-01

- Initial development version.
- Unified `raw_*` / `polished_*` naming end to end:
  [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
  writes each table under its `raw_*` stem (new `file_stem` column on
  `polis_tables_mapping`) and migrates files written under the old bare
  table name in place on the next run (no re-download).
  [`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
  reads `raw_*` inputs and writes `polished_*` outputs, with each
  output’s format following its source file.
- [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  now runs end to end: it cleans human specimens via
  [`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md),
  passes a configured `shape` to every cleaner for admin reconciliation,
  and computes the surveillance indicators
  ([`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md))
  using a configured `population` denominator.
  [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  gains `population` and `shape` reference handles.
- Added per-stream data-quality checks —
  [`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md),
  [`checks_es()`](https://truenomad.github.io/polished/reference/checks_es.md),
  [`checks_sia()`](https://truenomad.github.io/polished/reference/checks_sia.md),
  [`checks_virus()`](https://truenomad.github.io/polished/reference/checks_virus.md),
  [`checks_hum_spec()`](https://truenomad.github.io/polished/reference/checks_hum_spec.md)
  — and
  [`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md),
  a styled one-tab-per-check workbook.
  [`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
  writes a `checks_<dataset>.xlsx` per output.
- [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
  gains `prune_parts` (delete the resume cache after a verified write,
  rebuilt from the canonical next run) and self-heals a corrupt
  canonical by rebuilding it from the intact parts.
- Extended
  [`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
  from 4 to the full WHO POLIS indicator catalogue (~62 indicators
  across AFP, Stool, Dose, Timeliness, Lab, ES, Virus, SIA and Composite
  families), driven by DRY generators and a unified registry that
  [`available_indicators()`](https://truenomad.github.io/polished/reference/available_indicators.md)
  reads (queryable by `family`). New optional source inputs (`virus`,
  `es`, `sia`, `lab`, `admin_units`); missing sources/columns skip with
  a warning rather than erroring.
- [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
  now supports the `population` reference table. It carries no usable
  update date, so it is pulled whole in a single Id-paginated pass with
  no date/region filter (`date_field = NA` in `polis_tables_mapping`).
  Note: the endpoint/field still need a live-API sanity check before
  relying on it.
- Added the EPID-driven geography cleaner:
  [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
  plus the
  [`epid_split()`](https://truenomad.github.io/polished/reference/epid_split.md)
  /
  [`epid_country_code()`](https://truenomad.github.io/polished/reference/epid_country_code.md)
  /
  [`epid_prefix()`](https://truenomad.github.io/polished/reference/epid_prefix.md)
  /
  [`epid_strip_contact()`](https://truenomad.github.io/polished/reference/epid_strip_contact.md)
  parsers, the
  [`build_admin_ref()`](https://truenomad.github.io/polished/reference/build_admin_ref.md)
  /
  [`build_prefix_ref()`](https://truenomad.github.io/polished/reference/build_prefix_ref.md)
  reference builders, and
  [`resolve_epid_country()`](https://truenomad.github.io/polished/reference/resolve_epid_country.md).
