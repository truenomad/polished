# polished (development version)

* Population rate denominators now include districts without reported cases
  when a complete administrative lookup is supplied. Explicit year-specific
  parent mappings take precedence; ambiguous mappings are rejected.
* SIA deduplication selects the latest subactivity revision and preserves
  timestamp precision. ES coordinate and human-specimen collection checks now
  use the cleaner output names. Unavailable checks report `not_run` and their
  missing columns instead of disappearing from the summary.
* `polis_config(reference_date = ...)` controls date-dependent cleaning,
  indicators and checks consistently. Population years and the calculation
  date invalidate caches. Disabled caches avoid input hashing; enabled caches
  reuse reference fingerprints within a run.
* Download caches track query scope and revisions. Same-count edits,
  replacements and deletions are reconciled. Changed scopes and legacy caches
  with unknown filters require a fresh pull. Failed pulls retain the previous
  complete file and resumable checkpoints.
* Downloads write immutable pages followed by one year compaction. Progress
  uses validated metadata. Failed file renames abort without advancing the
  committed cursor. Unchanged revision-based snapshots avoid full data reads
  and partition rebuilding.
* Population snapshots expire after one day by default; set
  `reference_refresh_days = 0` for every-call refresh. Tables with only event
  dates are refreshed in full because those dates cannot identify edits.
* Downloading, directory discovery, cleaning and output loading share format
  support, including `.rda`. Output writes use atomic replacement. Download
  documentation now states the actual memory requirements and `raw_*` paths.

# polished 0.2.2

* `get_polis_data()` gains `verify_years` (default `3L`). The post-download
  completeness check, the slowest step on large tables, now covers only the
  most recent calendar years instead of the whole range back to `min_date`.
  Pass `verify_years = NULL` for the full-range check.
* Read timeouts are now retried. `httr2::req_retry()` defaults
  `retry_on_failure = FALSE`, so `max_tries` never covered transport errors
  and a single timeout aborted the run. `POLIS_TIMEOUT_SECONDS` overrides
  the 120-second default.
* A year whose worker fails is requeued up to three times instead of ending
  the download outright. Parts are checkpointed, so each retry resumes from
  the last Id.
* A local copy slightly ahead of the declared row count keeps its cache;
  rows POLIS retires between pulls are not corruption. Only an excess beyond
  1% clears the year parts, and the canonical file now survives until its
  replacement is written.

# polished 0.2.1

* `get_polis_data()` no longer treats a local copy that is larger than the
  server's declared row count as up to date. That test used `>=`, so a table
  carrying rows POLIS had since retired, or duplicates from an interrupted
  merge, reported itself complete and was skipped on every subsequent run --
  the surplus kept the condition true, so the table could never refresh again
  without `force = TRUE`. The count is now compared with `==`, and a local
  copy holding more rows than declared clears its cache and refetches.

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
