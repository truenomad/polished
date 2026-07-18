# polished 0.2.0

* Added `clean_afp_diagnosis()`, a `clean_afp()` step that harmonises the AFP
  clinical diagnosis. POLIS scatters the clinical cause across four fields
  (`diagnosis_final`, the ICD-10 `diagnosis_other`, and the bilingual free-text
  `diagnosis_other_specified` / `provisional_diagnosis`); it coalesces them, in
  priority order with a confirmed-polio override, into a single
  `diagnosis_harmonised` (plus a `diagnosis_source` provenance), then derives
  `diagnosis_class` and the `is_non_afp` flag that separates reported non-AFP
  illness (malaria, sepsis, malnutrition, ...) from the acute-flaccid-paralysis
  differentials, the 60-day `residual_paralysis` outcome and the
  `febrile_asymmetric_onset` flag. The mapping ships as three reviewable
  reference tables exposed by `polis_afp_diagnosis_lookup()` (free-text
  keywords, multilingual), `polis_afp_icd10()` (ICD-10 prefixes) and
  `polis_afp_diagnosis_class()` (diagnosis -> class).
* Added `clean_pop()`, the POLIS population cleaner: it turns the raw population
  reference into adm0/adm1/adm2 under-5 / under-15 / all-ages denominators,
  optionally reconciled against a WorldPop input and rolled up by boundary
  validity. This is the base for the rate indicators. `checks_pop()` and a
  `checks_pop` workbook tab add its per-stream data-quality checks.
* `polis_config()` and `clean_pop()` gain `pop_source` to choose the
  `<age>_pop` denominator: `"reconciled"` (default — a trusted POLIS value,
  else WorldPop, else the district -> province -> country ladder), `"polis"`,
  or `"worldpop"`. The chosen value keeps `<age>_pop_polis` and `<age>_pop_wp`
  alongside it so every source stays inspectable.
* Added `init_polis_pipeline()`, a full pipeline-project scaffold: the
  domain-numbered layout (`01_data`, `02_scripts`, `03_outputs`), a wired
  `.Rprofile` carrying the `cfg` manifest, a `.gitignore`, and runnable
  download / process scripts, so the project runs end to end once the boundary
  layers are dropped in. `renv = TRUE` pins package versions for collaborators.
  It is distinct from the lighter `init_polis_project()`.
* `run_pipeline()` now emits a lean `detections` table alongside `virus` — a
  per-detection projection of the positives table (epid, adm0-adm2 + adm2 GUID,
  latitude/longitude, the virus label, vtype, emergence group, surveillance
  type/source, and dates) that recomputes nothing.
* Added `polis_dictionary()`, a data dictionary for the raw and cleaned tables.
* Added citation metadata: a `CITATION.cff` (GitHub's "Cite this repository")
  and `inst/CITATION`, so `citation("polished")` returns a proper reference.

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
