# Changelog

## polished 0.2.0

CRAN release: 2020-09-29

- Added
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md),
  the POLIS population cleaner: it turns the raw population reference
  into adm0/adm1/adm2 under-5 / under-15 / all-ages denominators,
  optionally reconciled against a WorldPop input and rolled up by
  boundary validity. This is the base for the rate indicators.
  [`checks_pop()`](https://truenomad.github.io/polished/reference/checks_pop.md)
  and a `checks_pop` workbook tab add its per-stream data-quality
  checks.
- [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  and
  [`clean_pop()`](https://truenomad.github.io/polished/reference/clean_pop.md)
  gain `pop_source` to choose the `<age>_pop` denominator:
  `"reconciled"` (default — a trusted POLIS value, else WorldPop, else
  the district -\> province -\> country ladder), `"polis"`, or
  `"worldpop"`. The chosen value keeps `<age>_pop_polis` and
  `<age>_pop_wp` alongside it so every source stays inspectable.
- Added
  [`init_polis_pipeline()`](https://truenomad.github.io/polished/reference/init_polis_pipeline.md),
  a full pipeline-project scaffold: the domain-numbered layout
  (`01_data`, `02_scripts`, `03_outputs`), a wired `.Rprofile` carrying
  the `cfg` manifest, a `.gitignore`, and runnable download / process
  scripts, so the project runs end to end once the boundary layers are
  dropped in. `renv = TRUE` pins package versions for collaborators. It
  is distinct from the lighter
  [`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md).
- [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  now emits a lean `detections` table alongside `virus` — a
  per-detection projection of the positives table (epid, adm0-adm2 +
  adm2 GUID, latitude/longitude, the virus label, vtype, emergence
  group, surveillance type/source, and dates) that recomputes nothing.
- Added
  [`polis_dictionary()`](https://truenomad.github.io/polished/reference/polis_dictionary.md),
  a data dictionary for the raw and cleaned tables.
- Added citation metadata: a `CITATION.cff` (GitHub’s “Cite this
  repository”) and `inst/CITATION`, so `citation("polished")` returns a
  proper reference.

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
