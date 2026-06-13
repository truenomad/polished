# polished 0.1.0

* Initial development version.
* Unified `raw_*` / `polished_*` naming end to end: `get_polis_data()` writes
  each table under its `raw_*` stem (new `file_stem` column on
  `polis_tables_mapping`) and migrates files written under the old bare table
  name in place on the next run (no re-download). `run_pipeline_dir()` reads
  `raw_*` inputs and writes `polished_*` outputs, with each output's format
  following its source file.
* `run_pipeline()` now runs end to end: it cleans human specimens via
  `clean_human_spec()`, passes a configured `shape` to every cleaner for admin
  reconciliation, and computes the surveillance indicators
  (`calc_polio_indicators()`) using a configured `population` denominator.
  `polis_config()` gains `population` and `shape` reference handles.
* Added per-stream data-quality checks — `checks_afp()`, `checks_es()`,
  `checks_sia()`, `checks_virus()`, `checks_hum_spec()` — and
  `write_checks_excel()`, a styled one-tab-per-check workbook. `run_pipeline_dir()`
  writes a `checks_<dataset>.xlsx` per output.
* `get_polis_data()` gains `prune_parts` (delete the resume cache after a verified
  write, rebuilt from the canonical next run) and self-heals a corrupt canonical
  by rebuilding it from the intact parts.
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
