# Package index

## Download POLIS data

The download entry point and the table catalogue it fetches: a one-call
pull that works around POLIS’s pagination and date-filter quirks, with
on-disk caching, resume, and post-download completeness verification.

- [`get_polis_data()`](https://truenomad.github.io/polished/reference/get_polis_data.md)
  : Download POLIS tables
- [`polis_tables_mapping`](https://truenomad.github.io/polished/reference/polis_tables_mapping.md)
  : POLIS table catalogue

## Configuration & pipeline

The shared settings object every cleaner takes, and the orchestrators
that run the whole cleaning set in one call (in memory, or from a
directory of raw files).

- [`polis_config()`](https://truenomad.github.io/polished/reference/polis_config.md)
  : Build a POLIS pipeline configuration
- [`polis_active_config()`](https://truenomad.github.io/polished/reference/polis_active_config.md)
  : The session-active POLIS configuration
- [`print(`*`<polis_config>`*`)`](https://truenomad.github.io/polished/reference/print.polis_config.md)
  : Print method for POLIS configuration
- [`run_pipeline()`](https://truenomad.github.io/polished/reference/run_pipeline.md)
  : Run the POLIS cleaning pipeline in memory
- [`run_pipeline_dir()`](https://truenomad.github.io/polished/reference/run_pipeline_dir.md)
  : Run the cleaning pipeline from a directory of raw files

## Clean the surveillance streams

One cleaner per POLIS stream. Each standardises names, sanitises dates,
derives the analytic variables, cleans geography and dedups to one row
per POLIS id.
[`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
is the exception: it builds the positives dataset from the
already-cleaned AFP and ES outputs.

- [`clean_afp()`](https://truenomad.github.io/polished/reference/clean_afp.md)
  : Clean POLIS AFP case data
- [`clean_human_spec()`](https://truenomad.github.io/polished/reference/clean_human_spec.md)
  : Clean POLIS human specimen (laboratory) data
- [`clean_es()`](https://truenomad.github.io/polished/reference/clean_es.md)
  : Clean POLIS environmental surveillance data
- [`clean_sia()`](https://truenomad.github.io/polished/reference/clean_sia.md)
  : Clean POLIS SIA (campaign) data
- [`clean_virus()`](https://truenomad.github.io/polished/reference/clean_virus.md)
  : Build the POLIS virus (positives) dataset from cleaned cases and ES

## Virus classification

The shared decoder that turns the raw POLIS virus fields into the
standard WPV / cVDPV / aVDPV / iVDPV vocabulary used across every
stream.

- [`clean_afp_classification()`](https://truenomad.github.io/polished/reference/clean_afp_classification.md)
  : Derive the fused AFP virus type and analytic classification
- [`clean_es_classification()`](https://truenomad.github.io/polished/reference/clean_es_classification.md)
  : Derive the AFP-style virus classification and detection flags for ES

## Reference tables

The packaged lookups the cleaners are driven by: the raw-to-canonical
column crosswalk, the data dictionary (raw or cleaned schema) it backs,
and the country grouping/risk reference.

- [`polis_crosswalk()`](https://truenomad.github.io/polished/reference/polis_crosswalk.md)
  : POLIS column crosswalk
- [`polis_dictionary()`](https://truenomad.github.io/polished/reference/polis_dictionary.md)
  : POLIS data dictionary (raw or cleaned schema)
- [`polis_country_lookup()`](https://truenomad.github.io/polished/reference/polis_country_lookup.md)
  : Country reference lookup shipped with the package

## Recover & reconcile geography

Fill missing admin names and GUIDs without overwriting present values or
fabricating on ambiguity (from EPIDs, from a district shape, or from
coordinates), plus the admin-name fixers.

- [`impute_geo_from_epid()`](https://truenomad.github.io/polished/reference/impute_geo_from_epid.md)
  : Recover administrative geography from the EPID
- [`reconcile_admin_guids()`](https://truenomad.github.io/polished/reference/reconcile_admin_guids.md)
  : Reconcile case admin names and GUIDs against the long district shape
- [`impute_geo_from_coords()`](https://truenomad.github.io/polished/reference/impute_geo_from_coords.md)
  : Recover missing admin from coordinates, in place
- [`impute_missing_coords()`](https://truenomad.github.io/polished/reference/impute_missing_coords.md)
  : Place a random point inside the district polygon for cases missing
  coordinates
- [`fix_geo_names()`](https://truenomad.github.io/polished/reference/fix_geo_names.md)
  : Normalise admin names on a cleaned data frame
- [`polis_fix_geo_names()`](https://truenomad.github.io/polished/reference/polis_fix_geo_names.md)
  : Apply geographic name fixes to a character vector
- [`polis_geo_name_fixes()`](https://truenomad.github.io/polished/reference/polis_geo_name_fixes.md)
  : Geographic name-fix lookup table

## Clean administrative spatial data

Read WHO ADM0/ADM1/ADM2 boundary layers from any format, standardise the
names and repair the geometry, then write cleaned shapes plus a
year-expanded long shape, with point-to-admin recovery.

- [`process_spatial()`](https://truenomad.github.io/polished/reference/process_spatial.md)
  : Clean WHO administrative spatial data
- [`create_long_shape()`](https://truenomad.github.io/polished/reference/create_long_shape.md)
  : Expand admin shapes to one row per active year
- [`get_admin_info_from_coords()`](https://truenomad.github.io/polished/reference/get_admin_info_from_coords.md)
  : Recover administrative info for point data via a spatial join

## EPID building blocks

The exported pieces the EPID-geography cascade is assembled from:
parsers, the sibling-record lookups, and the country resolver. You
rarely call these directly.

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

## Records, dedup & types

The shared primitives the cleaners compose from: column standardisation
and ordering, keep-latest dedup, the business-key tripwire, synonym
remapping, full-pull reconcile, and column type inference.

- [`standardise_names()`](https://truenomad.github.io/polished/reference/standardise_names.md)
  : Standardise POLIS column names
- [`order_columns()`](https://truenomad.github.io/polished/reference/order_columns.md)
  : Order columns: identifiers, then location, then time, then
  everything else
- [`polis_upsert()`](https://truenomad.github.io/polished/reference/polis_upsert.md)
  : Upsert by Id, keeping the latest record
- [`collapse_business_key()`](https://truenomad.github.io/polished/reference/collapse_business_key.md)
  : Collapse business-key duplicates, keeping the latest record
- [`flag_ambiguous()`](https://truenomad.github.io/polished/reference/flag_ambiguous.md)
  : Flag (do not drop) rows whose business key spans multiple Ids
- [`remap_synonyms()`](https://truenomad.github.io/polished/reference/remap_synonyms.md)
  : Remap merged EPIDs to their canonical value
- [`reconcile()`](https://truenomad.github.io/polished/reference/reconcile.md)
  : Prune records absent from a full pull (reconcile)
- [`auto_parse_types()`](https://truenomad.github.io/polished/reference/auto_parse_types.md)
  : Infer column types after cleaning, then optionally layer factor
  detection
- [`detect_factors()`](https://truenomad.github.io/polished/reference/detect_factors.md)
  : Detect factor-like character columns (low-cardinality only)

## Indicators

Compute the WHO POLIS surveillance indicator catalogue (NPAFP rate,
stool adequacy, timeliness, dose history, environmental, virus, SIA and
composite families) from cleaned case / ES / virus / SIA / lab tables.
Call
[`available_indicators()`](https://truenomad.github.io/polished/reference/available_indicators.md)
to browse the catalogue, optionally by `family`.

- [`calc_polio_indicators()`](https://truenomad.github.io/polished/reference/calc_polio_indicators.md)
  : Calculate polio surveillance indicators (the POLIS indicator
  catalogue)
- [`available_indicators()`](https://truenomad.github.io/polished/reference/available_indicators.md)
  : Dictionary of available polio surveillance indicators

## Data-quality checks

Per-dataset checks that flag duplicates, blank keys, unreconciled GUIDs,
out-of-range values and date-ordering problems straight from the cleaned
tables, exported as a styled Excel workbook (one tab per check), plus
the ES-specific quality helpers.

- [`checks_afp()`](https://truenomad.github.io/polished/reference/checks_afp.md)
  : Run AFP data-quality checks
- [`checks_es()`](https://truenomad.github.io/polished/reference/checks_es.md)
  : Run environmental-surveillance data-quality checks
- [`checks_hum_spec()`](https://truenomad.github.io/polished/reference/checks_hum_spec.md)
  : Run human-specimen data-quality checks
- [`checks_sia()`](https://truenomad.github.io/polished/reference/checks_sia.md)
  : Run SIA data-quality checks
- [`checks_virus()`](https://truenomad.github.io/polished/reference/checks_virus.md)
  : Run virus/positives data-quality checks
- [`write_checks_excel()`](https://truenomad.github.io/polished/reference/write_checks_excel.md)
  : Write a checks result to an Excel workbook
- [`es_missingness()`](https://truenomad.github.io/polished/reference/es_missingness.md)
  : Summarise missingness in key ES surveillance variables
- [`validate_es_sites()`](https://truenomad.github.io/polished/reference/validate_es_sites.md)
  : Flag ES site names absent from a reference site list

## Other surveillance processing

Independent-monitoring (IM), LQAS campaign-monitoring, and SIA
round-quality processors.

- [`process_im()`](https://truenomad.github.io/polished/reference/process_im.md)
  : Process raw Independent Monitoring (IM) data into missed-children
  rates
- [`process_lqas()`](https://truenomad.github.io/polished/reference/process_lqas.md)
  : Process raw LQAS lots into classifications and district pass rates
- [`process_sia_quality()`](https://truenomad.github.io/polished/reference/process_sia_quality.md)
  : Process the POLIS SIA campaign-quality tables (LQAS + IM)

## Project workspace

Set up a standard on-disk project (raw / processed / cache zones),
stream the cleaning pipeline into it, and read partitioned slices back
out.

- [`init_polis_project()`](https://truenomad.github.io/polished/reference/init_polis_project.md)
  : Create (or re-open) a data project
- [`load_polished()`](https://truenomad.github.io/polished/reference/load_polished.md)
  : Read a country / period slice of the polished outputs
- [`project_path()`](https://truenomad.github.io/polished/reference/project_path.md)
  : Build a path inside a project zone
- [`clear_cache()`](https://truenomad.github.io/polished/reference/clear_cache.md)
  : Clear a project's regenerable cache
- [`print(`*`<polis_project>`*`)`](https://truenomad.github.io/polished/reference/print.polis_project.md)
  : Print a polis_project
