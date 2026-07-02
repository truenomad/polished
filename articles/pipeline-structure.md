# Pipeline structure

An interactive mock of a project scaffolded by
[`init_polis_pipeline()`](https://truenomad.github.io/polished/reference/init_polis_pipeline.md)
— expand the folders and click a **script** to open it (several can be
open at once; the `01_data` / `03_outputs` folders are structure only
and don’t open).

Exactly what
`init_polis_pipeline("emro_polis", regions = "EMRO", renv = TRUE)`
produces: the `.Rprofile` **manifest** defines the data-directory roots
once and builds `cfg` from them, so the two `02_scripts/` and the
cleaners all share the same paths. See *Scaffolding a pipeline project*
for the step-by-step version.

Explorer

emro_polis

01_data

1a_shapefiles

raw

who_polio_global_gdb.gdb

processed

spatial_global_adm0.qs2

spatial_global_adm1.qs2

spatial_global_adm2.qs2

1b_population

worldpop

raw

all/global_pop_2020.tif

u5/global_total_00_04_2020.tif

u15/global_total_00_14_2020.tif

processed

global_worldpop_total_pop.qs2

global_worldpop_u5_pop.qs2

global_worldpop_u15_pop.qs2

polis_pop

raw

raw_population.qs2

processed

cleaned denominators

1c_polis

raw

raw_afp.qs2

raw_es.qs2

raw_hum_spec.qs2

raw_activity.qs2

raw_sub_activity.qs2

raw_im.qs2

raw_lqas.qs2

processed

data

polished_afp.qs2

polished_es.qs2

polished_virus.qs2

polished_detections.qs2

polished_indicators.qs2

polished_pop_adm2.qs2

checks

checks_afp.xlsx

checks_es.xlsx

checks_pop.xlsx

cache

regenerable cache

1d_vaccination

raw

coverage inputs (optional)

processed

derived tables

02_scripts

2a_download_data.R

2b_process_data.R

03_outputs

plots

figures (.png / .pdf)

tables

exported summaries (.csv / .xlsx)

.Rprofile

.gitignore

renv.lock
