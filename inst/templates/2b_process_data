##################  Process all GPEI data  ####################################

cli::cli_h1("Process all GPEI data")

# Paths (shapes_raw / shapes_proc / wp_raw / wp_proc) and `cfg` come from the
# project manifest defined in .Rprofile.

## ---------------------------------------------------------------------------##
# Spatial -- process shapefiles ------------------------------------------------
## ---------------------------------------------------------------------------##
# Clean the raw WHO polio GDB layers into snake_case sf tables. Runs first:
# everything downstream keys off it -- the WorldPop extraction below and
# run_pipeline()/clean_pop() via cfg$shape.

polished::process_spatial(
  input_path = shapes_raw,
  output_dir = shapes_proc,
  layers = c(
    adm0 = "who_polio_global_gdb_adm0",
    adm1 = "who_polio_global_gdb_adm1",
    adm2 = "who_polio_global_gdb_adm2"
  ),
  output_format = "qs2"
)

## ---------------------------------------------------------------------------##
# WorldPop -- extract annual rasters to district (adm2) counts -----------------
## ---------------------------------------------------------------------------##
# Sum each annual WorldPop raster to adm2 with the processed shape, one call per
# age scope. clean_pop() then consumes these pre-extracted adm2 tables (set on
# cfg$worldpop), so the package never touches a raster. Delete the files to
# force a re-extraction.

wp_files <- c(
  u15 = file.path(wp_proc, "global_worldpop_u15_pop.qs2"),
  u5 = file.path(wp_proc, "global_worldpop_u5_pop.qs2"),
  all = file.path(wp_proc, "global_worldpop_total_pop.qs2")
)

if (!all(file.exists(wp_files))) {
  adm2_shp <- sntutils::read(
    file.path(shapes_proc, "spatial_global_adm2.qs2")
  ) |>
    sf::st_as_sf()

  # one annual raster -> one district x year count column, age-band specific
  extract_worldpop <- function(sub, pattern, value_col) {
    sntutils::process_raster_collection(
      directory = file.path(wp_raw, sub),
      pattern = pattern,
      shapefile = adm2_shp,
      aggregations = "sum",
      id_cols = c(
        "who_region",
        "iso_3_code",
        "adm0",
        "adm1",
        "adm2",
        "adm2_guid"
      )
    ) |>
      dplyr::mutate("{value_col}" := as.integer(sum)) |>
      sntutils::auto_parse_types() |>
      dplyr::select(-file_name, -sum)
  }

  sntutils::write(
    extract_worldpop("u15", "global_total_00_14_\\d{4}\\.tif$", "u15_pop"),
    file_path = wp_files[["u15"]]
  )
  sntutils::write(
    extract_worldpop("u5", "global_total_00_04_\\d{4}\\.tif$", "u5_pop"),
    file_path = wp_files[["u5"]]
  )
  sntutils::write(
    extract_worldpop("all", "global_pop_.*\\.tif$", "total_pop"),
    file_path = wp_files[["all"]]
  )
} else {
  cli::cli_alert_info("worldpop adm2 extracts already present -- skipping")
}

## ---------------------------------------------------------------------------##
# POLIS -- clean every stream, incl. population --------------------------------
## ---------------------------------------------------------------------------##
# One call reads the raw_* tables, cleans each stream (AFP / ES / SIA / LQAS /
# IM / virus / indicators AND population via polished::clean_pop()), scopes,
# writes the polished_* outputs (incl. polished_pop_adm0/adm1/adm2), and caches.

out <- polished::run_pipeline(cfg = cfg)

# load the cleaned set back (country / year slices available via the args) -----
cleaned <- polished::load_polished()

invisible(gc())
cli::cli_rule(
  left = "All processing complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
