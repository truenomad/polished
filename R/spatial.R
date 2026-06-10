# =============================================================================
# Clean WHO administrative spatial data
#
# Reads the GLOBAL_ADM0 / GLOBAL_ADM1 / GLOBAL_ADM2 admin layers from any source
# sf can open -- an Esri geodatabase (.gdb), a GeoPackage (.gpkg), or a folder
# of standalone shapefiles (.shp) / GeoJSON (.geojson) -- standardises their
# columns (clean names, parsed start/end dates, a normalised country name, a
# lower-case `shape` geometry column), runs a battery of validity / duplicate
# checks, reprojects to a target CRS, and writes both the cleaned per-level
# shapes and a year-expanded "long" shape used for temporal joins.
# process_spatial() is the thin orchestrator; everything below it is a small,
# single-purpose helper reused across the three admin levels.
#
# Two standalone utilities ride along: create_long_shape() (expand a shape table
# to one row per active year) and get_admin_info_from_coords() (recover admin
# names/GUIDs for point data via a spatial join), both useful on their own.
# =============================================================================

#' Clean WHO administrative spatial data
#'
#' Reads the country (ADM0), province (ADM1) and district (ADM2) boundary layers
#' from any source [sf][sf::st_read] can open and turns each into a cleaned,
#' validity-checked, reprojected `sf` object written to `output_dir`. For ADM1
#' and ADM2 a year-expanded "long" shape (one row per active year) is also
#' written, ready for temporal point-in-polygon joins.
#'
#' `input_path` may be any of:
#' \itemize{
#'   \item an Esri geodatabase (`.gdb`) or GeoPackage (`.gpkg`) holding the
#'     three admin layers, matched by name via `layers`;
#'   \item a folder of standalone files -- shapefiles (`.shp`), GeoPackages
#'     (`.gpkg`) or GeoJSON (`.geojson` / `.json`) -- one per level, matched by
#'     filename via `layers`.
#' }
#'
#' Per layer the cleaner:
#' \itemize{
#'   \item reads the layer and snake_cases its columns (via janitor);
#'   \item parses `startdate` / `enddate` to [Date] and derives `year_start` /
#'     `year_end`;
#'   \item normalises the Cote d'Ivoire country name;
#'   \item renames the admin name columns to the package's canonical `adm0` /
#'     `adm1` / `adm2`, the level GUID column to `adm0_guid` / `adm1_guid` /
#'     `adm2_guid`, and the geometry column to `shape`;
#'   \item runs validity, empty-geometry and duplicate (GUID and name) checks,
#'     writing a CSV per issue found into a `checks/` subfolder;
#'   \item when `fix_issues = TRUE`, repairs geometries: drops Z/M, makes
#'     invalid geometries valid, and removes sliver holes and sliver polygon
#'     parts below `sliver_area`;
#'   \item reprojects to `crs` when `transform` is `TRUE`.
#' }
#'
#' @param input_path Path to the spatial source: a `.gdb` / `.gpkg` dataset, or
#'   a folder of standalone `.shp` / `.gpkg` / `.geojson` files.
#' @param output_dir Directory for cleaned outputs and the `checks/` subfolder.
#' @param layers Named character vector mapping admin levels to a layer name
#'   (for a `.gdb` / `.gpkg`) or a filename stem (for a folder of files). Names
#'   must be a subset of `adm0` / `adm1` / `adm2`; only the levels present are
#'   processed, so pass a single entry to clean one level. Matching is
#'   case-insensitive, preferring an exact match and otherwise the substring
#'   match. Defaults to all three WHO `GLOBAL_ADM*` layers.
#' @param transform Whether to reproject each layer to `crs`. The reprojection
#'   is skipped automatically when a layer's CRS is already equivalent to `crs`
#'   (ignoring cosmetic CRS-label differences), so no coordinates are swept
#'   needlessly. Default `TRUE`.
#' @param crs Target CRS as an EPSG code. Default `4326` (WGS84).
#' @param fix_issues Whether to repair geometries (in addition to flagging
#'   them): drop Z/M dimensions, make invalid geometries valid
#'   ([sf::st_make_valid()]), and strip sliver holes and sliver polygon parts
#'   smaller than `sliver_area`. Real enclaves, lakes and islands above the
#'   threshold are kept, and a feature never loses its largest part. Set `FALSE`
#'   for the fastest run (flag-only). Default `TRUE`.
#' @param sliver_area Area threshold in square metres below which interior holes
#'   and detached polygon parts are treated as digitising artifacts and removed
#'   when `fix_issues = TRUE`. Default `1e4` (1 hectare).
#' @param output_format Serialization format for the cleaned shapes, `"rds"` or
#'   `"qs2"`. `"qs2"` writes and reads large geometries far faster (needs the
#'   qs2 package). Default `"rds"`.
#' @param verbose Whether to print a cli progress summary. Default `TRUE`.
#'
#' @return `output_dir`, invisibly. The function's outputs are the files it
#'   writes (`{ext}` is `output_format`):
#' \itemize{
#'   \item `spatial_global_{level}.{ext}`: cleaned per-level shapes;
#'   \item `spatial_{level}_long_shape.{ext}`: year-expanded shapes (ADM1, ADM2);
#'   \item `checks/spatial_*_{level}_*.csv`: any validity/duplicate issues.
#' }
#'
#' @examples
#' \dontrun{
#' # an Esri geodatabase with the GLOBAL_ADM* layers
#' process_spatial("path/to/who.gdb", output_dir = "outputs/spatial")
#'
#' # a folder of shapefiles named adm0.shp / adm1.shp / adm2.shp
#' process_spatial(
#'   "path/to/shapefiles",
#'   output_dir = "outputs/spatial",
#'   layers = c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2")
#' )
#' }
#'
#' @export
process_spatial <- function(
  input_path,
  output_dir,
  layers = c(
    adm0 = "GLOBAL_ADM0",
    adm1 = "GLOBAL_ADM1",
    adm2 = "GLOBAL_ADM2"
  ),
  transform = TRUE,
  crs = 4326,
  fix_issues = TRUE,
  sliver_area = 1e4,
  output_format = "rds",
  verbose = TRUE
) {
  # ---- validate -------------------------------------------------------------
  if (!is.character(input_path) || length(input_path) != 1L) {
    cli::cli_abort("{.arg input_path} must be a single path string.")
  }
  if (!file.exists(input_path)) {
    cli::cli_abort("Spatial source {.file {input_path}} does not exist.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L) {
    cli::cli_abort("{.arg output_dir} must be a single path string.")
  }
  valid_levels <- c("adm0", "adm1", "adm2")
  if (length(layers) == 0L || is.null(names(layers))) {
    cli::cli_abort(
      "{.arg layers} must be a named vector of admin levels to process."
    )
  }
  unknown_levels <- setdiff(names(layers), valid_levels)
  if (length(unknown_levels) > 0L) {
    cli::cli_abort(c(
      "{.arg layers} has unknown level name{?s}: {.val {unknown_levels}}.",
      "i" = "Valid level{?s}: {.val {valid_levels}}."
    ))
  }
  valid_formats <- c("rds", "qs2")
  if (!is.character(output_format) || !output_format %in% valid_formats) {
    cli::cli_abort(
      "{.arg output_format} must be one of {.val {valid_formats}}."
    )
  }
  if (
    !is.numeric(sliver_area) || length(sliver_area) != 1L || sliver_area < 0
  ) {
    cli::cli_abort("{.arg sliver_area} must be a single non-negative number.")
  }

  # ---- set up the checks folder ---------------------------------------------
  checks_dir <- file.path(output_dir, "checks")
  if (!dir.exists(checks_dir)) {
    dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # ---- read, check, reproject and write each requested level ----------------
  for (level in intersect(valid_levels, names(layers))) {
    if (isTRUE(verbose)) {
      cli::cli_process_start("Processing {.field {level}} shapes")
    }
    shapes <- .spatial_read_layer(input_path, layers[[level]]) |>
      .spatial_rename_guid(level)
    if (identical(level, "adm1")) {
      shapes <- .spatial_somalia_fix(shapes)
    }
    .spatial_process_level(
      shapes,
      level,
      transform = transform,
      crs = crs,
      checks_dir = checks_dir,
      output_dir = output_dir,
      fix_issues = fix_issues,
      sliver_area = sliver_area,
      verbose = verbose,
      output_format = output_format
    )
    if (isTRUE(verbose)) {
      cli::cli_process_done(msg_done = "Processed and checked {.field {level}}")
    }
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Wrote cleaned spatial data to {.file {output_dir}}."
    )
  }
  invisible(output_dir)
}

#' Read and standardise one admin layer from any spatial source
#'
#' Resolves `pattern` to a dataset + layer via `.spatial_locate()`, reads it,
#' snake_cases its columns, parses the start/end dates and their years,
#' normalises the Cote d'Ivoire country name, and renames the geometry column to
#' `shape`.
#'
#' @keywords internal
#' @noRd
.spatial_read_layer <- function(input_path, pattern) {
  located <- .spatial_locate(input_path, pattern)
  admin_sf_raw <- if (is.na(located$layer)) {
    sf::st_read(dsn = located$dsn, quiet = TRUE)
  } else {
    sf::st_read(dsn = located$dsn, layer = located$layer, quiet = TRUE)
  }
  admin_sf <- janitor::clean_names(admin_sf_raw)

  # standardise the geometry column to `shape`, re-pointing the active-geometry
  # attribute after the rename so callers can rely on either name resolving to
  # the same geometry (sf operations always follow the active column).
  geometry_col <- attr(admin_sf, "sf_column")
  if (!identical(geometry_col, "shape")) {
    admin_sf <- dplyr::rename(admin_sf, shape = dplyr::all_of(geometry_col))
    sf::st_geometry(admin_sf) <- "shape"
  }

  # adopt the package's canonical admin-name columns (adm0 / adm1 / adm2); the
  # *_guid columns already match the canonical names.
  admin_sf |>
    dplyr::rename(dplyr::any_of(c(
      adm0 = "adm0_name",
      adm1 = "adm1_name",
      adm2 = "adm2_name"
    ))) |>
    dplyr::mutate(
      startdate = as.Date(startdate),
      enddate = as.Date(enddate),
      year_start = lubridate::year(startdate),
      year_end = lubridate::year(enddate),
      adm0 = dplyr::case_when(
        stringr::str_detect(adm0, "IVOIRE") ~ "COTE D IVOIRE",
        .default = adm0
      )
    )
}

#' Resolve a level pattern to a dataset + layer
#'
#' Handles two shapes of `input_path`: a multilayer container (a `.gdb`
#' directory or a `.gpkg` / `.gdb` file), where `pattern` selects a layer; or a
#' folder of standalone files, where `pattern` selects a file by stem. Returns a
#' list with `dsn` and `layer` (`NA` for single-layer sources, so the caller
#' omits the `layer` argument to [sf::st_read()]).
#'
#' @keywords internal
#' @noRd
.spatial_locate <- function(input_path, pattern) {
  spatial_exts <- c("shp", "gpkg", "geojson", "json")

  # a multilayer container: read the layer straight out of it
  if (.spatial_is_container(input_path)) {
    available <- sf::st_layers(input_path)$name
    layer_name <- .spatial_pick(available, pattern, "layer", input_path)
    return(list(dsn = input_path, layer = layer_name))
  }

  # a folder of standalone files: pick the file whose stem matches
  if (!dir.exists(input_path)) {
    cli::cli_abort(
      "{.arg input_path} {.file {input_path}} is not a directory or dataset."
    )
  }
  files <- list.files(input_path, full.names = TRUE)
  files <- files[tolower(tools::file_ext(files)) %in% spatial_exts]
  if (length(files) == 0L) {
    cli::cli_abort(c(
      "No spatial files found in {.file {input_path}}.",
      "i" = "Expected file{?s} with extension {.val {spatial_exts}}."
    ))
  }
  stems <- tools::file_path_sans_ext(basename(files))
  chosen <- files[match(
    .spatial_pick(stems, pattern, "file", input_path),
    stems
  )]
  list(dsn = chosen, layer = .spatial_inner_layer(chosen, pattern))
}

#' Is `input_path` a multilayer container (.gdb dir, or .gpkg/.gdb file)?
#'
#' @keywords internal
#' @noRd
.spatial_is_container <- function(input_path) {
  if (dir.exists(input_path)) {
    return(grepl("\\.gdb$", input_path, ignore.case = TRUE))
  }
  tolower(tools::file_ext(input_path)) %in% c("gpkg", "gdb")
}

#' Pick the best match for `pattern` among `choices`, or abort
#'
#' Prefers a case-insensitive exact match, falling back to a substring match;
#' on multiple substring hits the last in sorted order wins (most recent stem).
#'
#' @keywords internal
#' @noRd
.spatial_pick <- function(choices, pattern, what, source_path) {
  exact <- choices[tolower(choices) == tolower(pattern)]
  hit <- if (length(exact) > 0L) {
    exact
  } else {
    choices[grepl(tolower(pattern), tolower(choices), fixed = TRUE)]
  }
  if (length(hit) == 0L) {
    cli::cli_abort(c(
      "No {what} in {.file {source_path}} matches {.val {pattern}}.",
      "i" = "Available: {.val {choices}}."
    ))
  }
  sort(hit, decreasing = TRUE)[[1]]
}

#' Inner layer name for a standalone file, or `NA` for single-layer files
#'
#' Shapefiles and GeoJSON are single-layer (`NA`); a multilayer GeoPackage in a
#' folder still needs its layer selected by `pattern`.
#'
#' @keywords internal
#' @noRd
.spatial_inner_layer <- function(path, pattern) {
  if (!tolower(tools::file_ext(path)) %in% c("gpkg", "gdb")) {
    return(NA_character_)
  }
  layers <- sf::st_layers(path)$name
  if (length(layers) <= 1L) {
    return(NA_character_)
  }
  .spatial_pick(layers, pattern, "layer", path)
}

#' Rename the level GUID column to `adm{level}_guid`
#'
#' @keywords internal
#' @noRd
.spatial_rename_guid <- function(data, level) {
  guid_col <- paste0(level, "_guid")
  if ("guid" %in% names(data)) {
    data <- dplyr::rename(data, !!guid_col := guid)
  }
  data
}

#' Apply the Somalia ADM1 end-date special case
#'
#' One Somalia province shape carries an open-ended `enddate`; cap its
#' `year_end` at 2021 so the coordinate joins do not treat it as currently valid.
#' A no-op when the GUID columns are absent.
#'
#' @keywords internal
#' @noRd
.spatial_somalia_fix <- function(data) {
  if (!all(c("adm0_guid", "adm1_guid", "year_end") %in% names(data))) {
    return(data)
  }
  somalia_guid <- "B5FF48B9-7282-445C-8CD2-BEFCE4E0BDA7"
  province_guid <- "EE73F3EA-DD35-480F-8FEA-5904274087C4"
  hit <- .geo_guid_key(data$adm0_guid) == somalia_guid &
    .geo_guid_key(data$adm1_guid) == province_guid
  data$year_end <- dplyr::if_else(
    dplyr::coalesce(hit, FALSE),
    2021,
    data$year_end
  )
  data
}

#' Check, reproject and write one admin level
#'
#' @keywords internal
#' @noRd
.spatial_process_level <- function(
  data,
  level,
  transform,
  crs,
  checks_dir,
  output_dir,
  fix_issues,
  sliver_area,
  verbose,
  output_format
) {
  .spatial_check(data, level, checks_dir)

  # repair geometries on the cleaned output (audit above captured the originals)
  if (isTRUE(fix_issues)) {
    data <- .spatial_repair(data, level, sliver_area, verbose)
  }

  # reproject only when the source CRS is genuinely different -- st_transform
  # sweeps every coordinate, so skipping it is a large win on data already in
  # the target CRS (the WHO layers are WGS84 but labelled so that an exact
  # comparison misses it; see .spatial_crs_equivalent()).
  if (isTRUE(transform) && !.spatial_crs_equivalent(data, crs)) {
    data <- sf::st_transform(data, crs)
  }

  .polis_write(
    data,
    file.path(output_dir, paste0("spatial_global_", level, ".", output_format))
  )

  if (level %in% c("adm1", "adm2")) {
    long_shape <- create_long_shape(data, level, checks_dir = checks_dir)
    .polis_write(
      long_shape,
      file.path(
        output_dir,
        paste0("spatial_", level, "_long_shape.", output_format)
      )
    )
  }

  invisible(data)
}

#' Repair geometries: drop Z/M, make valid, strip slivers
#'
#' Runs the geometry-repair cascade for one level under fast planar GEOS:
#' drops Z/M dimensions, makes invalid geometries valid, then removes interior
#' holes and detached polygon parts smaller than `sliver_area` (a feature never
#' loses its largest part). Reports the counts via cli.
#'
#' @keywords internal
#' @noRd
.spatial_repair <- function(data, level, sliver_area, verbose) {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  data <- sf::st_zm(data, drop = TRUE, what = "ZM")

  n_invalid <- sum(!sf::st_is_valid(data), na.rm = TRUE)
  if (n_invalid > 0L) {
    data <- sf::st_make_valid(data)
  }

  holes <- .spatial_drop_sliver_holes(data, sliver_area)
  parts <- .spatial_drop_sliver_parts(holes$data, sliver_area)

  if (isTRUE(verbose)) {
    cli::cli_alert_success(c(
      "Repaired {.field {level}}: fixed {n_invalid} ",
      "invalid geometr{?y/ies}, removed {holes$n} sliver ",
      "hole{?s} and {parts$n} sliver part{?s}."
    ))
  }
  parts$data
}

#' Remove interior holes smaller than a threshold
#'
#' Drops interior rings whose area is below `sliver_area`, keeping holes (real
#' enclaves/lakes) at or above it. Only features that actually carry a hole are
#' rebuilt, so the common no-hole feature is never copied. Returns the repaired
#' data and the number of holes removed.
#'
#' @keywords internal
#' @noRd
.spatial_drop_sliver_holes <- function(data, sliver_area) {
  if (sliver_area <= 0) {
    return(list(data = data, n = 0L))
  }
  crs <- sf::st_crs(data)
  geometry <- sf::st_geometry(data)
  with_holes <- which(vapply(geometry, .spatial_feature_holes, integer(1)) > 0L)
  if (length(with_holes) == 0L) {
    return(list(data = data, n = 0L))
  }

  new_geometry <- geometry
  n_removed <- 0L
  for (i in with_holes) {
    filtered <- .spatial_filter_feature_holes(geometry[[i]], sliver_area, crs)
    new_geometry[[i]] <- filtered$geom
    n_removed <- n_removed + filtered$removed
  }
  list(data = sf::st_set_geometry(data, new_geometry), n = n_removed)
}

#' Count interior rings (holes) in one geometry
#' @keywords internal
#' @noRd
.spatial_feature_holes <- function(geom) {
  per_polygon <- function(polygon) max(length(polygon) - 1L, 0L)
  if (inherits(geom, "POLYGON")) {
    per_polygon(geom)
  } else if (inherits(geom, "MULTIPOLYGON")) {
    sum(vapply(geom, per_polygon, integer(1)), na.rm = TRUE)
  } else {
    0L
  }
}

#' Area in square metres via a planar equal-area projection
#'
#' `sliver_area` is a threshold in m^2, so area must be real m^2. Computing that
#' on lon/lat needs either s2 (which can error on degenerate rings) or the
#' `lwgeom` package (an undeclared, often-absent extra) -- so instead a
#' geographic geometry is projected to the global equal-area CRS EPSG:6933 and
#' the area taken with planar GEOS (s2 off). That depends only on PROJ + GEOS,
#' which `sf` always carries, and is robust to degenerate input. Already-metric
#' or CRS-less geometry is measured directly. sf's first-use linking banner and
#' the "built under R version" warning are wrapped away. Returns a plain numeric.
#'
#' @keywords internal
#' @noRd
.spatial_area <- function(x) {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  if (!is.na(sf::st_crs(x)) && sf::st_is_longlat(x)) {
    x <- suppressWarnings(suppressMessages(sf::st_transform(x, 6933)))
  }
  suppressWarnings(suppressMessages(as.numeric(sf::st_area(x))))
}

#' Rebuild one feature, dropping interior rings below `sliver_area`
#' @keywords internal
#' @noRd
.spatial_filter_feature_holes <- function(geom, sliver_area, crs) {
  filter_rings <- function(rings) {
    if (length(rings) <= 1L) {
      return(list(rings = rings, removed = 0L))
    }
    holes <- rings[-1]
    areas <- .spatial_area(sf::st_sfc(
      lapply(holes, function(ring) sf::st_polygon(list(ring))),
      crs = crs
    ))
    keep <- areas >= sliver_area
    list(rings = c(rings[1], holes[keep]), removed = sum(!keep, na.rm = TRUE))
  }

  if (inherits(geom, "POLYGON")) {
    result <- filter_rings(unclass(geom))
    list(geom = sf::st_polygon(result$rings), removed = result$removed)
  } else if (inherits(geom, "MULTIPOLYGON")) {
    polygons <- lapply(unclass(geom), filter_rings)
    list(
      geom = sf::st_multipolygon(lapply(polygons, function(p) p$rings)),
      removed = sum(
        vapply(polygons, function(p) p$removed, integer(1)),
        na.rm = TRUE
      )
    )
  } else {
    list(geom = geom, removed = 0L)
  }
}

#' Remove detached polygon parts smaller than a threshold
#'
#' Explodes multipart features, drops parts below `sliver_area`, but always
#' keeps each feature's largest part so no feature is emptied, then recombines.
#' Single-part features are untouched. Returns the repaired data and the number
#' of parts removed.
#'
#' @keywords internal
#' @noRd
.spatial_drop_sliver_parts <- function(data, sliver_area) {
  geometry <- sf::st_geometry(data)
  is_multipart <- vapply(
    geometry,
    function(geom) inherits(geom, "MULTIPOLYGON") && length(geom) > 1L,
    logical(1)
  )
  if (sliver_area <= 0 || !any(is_multipart)) {
    return(list(data = data, n = 0L))
  }

  # Homogenise to MULTIPOLYGON before exploding: st_cast() to POLYGON on a mixed
  # POLYGON/MULTIPOLYGON column keeps only each feature's first part instead of
  # splitting it. Casting to MULTIPOLYGON first makes the explode complete.
  data[[".feature_id"]] <- seq_len(nrow(data))
  multipart <- tryCatch(
    suppressWarnings(sf::st_cast(data, "MULTIPOLYGON")),
    error = function(e) NULL
  )
  data[[".feature_id"]] <- NULL
  if (is.null(multipart)) {
    return(list(data = data, n = 0L))
  }
  parts <- suppressWarnings(sf::st_cast(multipart, "POLYGON", warn = FALSE))
  part_area <- .spatial_area(parts)
  feature_id <- parts[[".feature_id"]]
  max_area <- tapply(
    part_area,
    feature_id,
    function(x) max(x, na.rm = TRUE)
  )[as.character(feature_id)]
  keep <- part_area >= sliver_area | part_area >= max_area
  n_removed <- sum(!keep, na.rm = TRUE)
  if (n_removed == 0L) {
    return(list(data = data, n = 0L))
  }

  # rebuild only the features that lost a part, leaving the rest untouched
  part_geometry <- sf::st_geometry(parts)
  new_geometry <- geometry
  for (id in unique(feature_id[!keep])) {
    kept_rows <- which(feature_id == id & keep)
    new_geometry[[id]] <- sf::st_combine(part_geometry[kept_rows])[[1]]
  }
  list(data = sf::st_set_geometry(data, new_geometry), n = n_removed)
}

#' Test whether two CRSes are equivalent for reprojection purposes
#'
#' Returns `TRUE` when transforming `data` to `crs` would not move any
#' coordinate, so the (expensive) [sf::st_transform()] call can be skipped.
#' Beyond exact equality it tolerates cosmetic CRS differences (a differing CRS
#' name or vertical-unit token) by comparing only the horizontal `+proj` /
#' `+datum` / `+ellps` proj4 tokens. Conservative: any uncertainty returns
#' `FALSE`, so at worst a harmless extra transform runs.
#'
#' @keywords internal
#' @noRd
.spatial_crs_equivalent <- function(data, crs) {
  crs_data <- sf::st_crs(data)
  crs_target <- sf::st_crs(crs)
  if (isTRUE(crs_data == crs_target)) {
    return(TRUE)
  }
  horizontal_tokens <- function(crs_obj) {
    proj4 <- crs_obj$proj4string
    if (is.null(proj4) || is.na(proj4)) {
      return(NA_character_)
    }
    tokens <- strsplit(trimws(proj4), "\\s+")[[1]]
    paste(
      sort(grep("^\\+(proj|datum|ellps)=", tokens, value = TRUE)),
      collapse = " "
    )
  }
  tokens_data <- horizontal_tokens(crs_data)
  tokens_target <- horizontal_tokens(crs_target)
  !is.na(tokens_data) &&
    !is.na(tokens_target) &&
    identical(tokens_data, tokens_target)
}

#' Run validity and duplicate checks on one admin level
#'
#' Writes one CSV per issue found (invalid geometries, empty geometries,
#' duplicate GUIDs, duplicate names) into `checks_dir`, and warns when
#' duplicates are present. The geometry scan uses fast planar GEOS.
#'
#' @keywords internal
#' @noRd
.spatial_check <- function(data, level, checks_dir) {
  guid_col <- paste0(level, "_guid")
  name_cols <- switch(
    level,
    adm0 = "adm0",
    adm1 = c("adm0", "adm1"),
    adm2 = c("adm0", "adm1", "adm2")
  )

  # Flag geometry issues with planar GEOS rather than spherical s2: it is
  # several times faster on global high-resolution polygons and is the
  # conventional notion of validity (ring self-intersection) for a check that
  # only flags rows for review. Restore the caller's s2 setting afterwards.
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  # invalid geometries --------------------------------------------------------
  invalid_mask <- !sf::st_is_valid(data)
  if (any(invalid_mask, na.rm = TRUE)) {
    invalid_shapes <- data |>
      dplyr::slice(which(invalid_mask)) |>
      sf::st_drop_geometry() |>
      dplyr::select(
        dplyr::all_of(c(name_cols, guid_col, "year_start", "year_end"))
      ) |>
      dplyr::arrange(adm0)
    .polis_write(
      invalid_shapes,
      file.path(checks_dir, paste0("spatial_invalid_", level, "_shapes.csv"))
    )
  }

  # empty geometries ----------------------------------------------------------
  empty_mask <- sf::st_is_empty(data)
  if (any(empty_mask, na.rm = TRUE)) {
    empty_shapes <- data |>
      dplyr::slice(which(empty_mask)) |>
      sf::st_drop_geometry() |>
      dplyr::arrange(adm0)
    .polis_write(
      empty_shapes,
      file.path(checks_dir, paste0("spatial_empty_", level, "_shapes.csv"))
    )
  }

  # duplicates ----------------------------------------------------------------
  flat <- sf::st_drop_geometry(data)

  dupe_guid <- flat |>
    dplyr::group_by(.data[[guid_col]]) |>
    dplyr::filter(dplyr::n() > 1L) |>
    dplyr::ungroup() |>
    dplyr::arrange(adm0, .data[[guid_col]])

  name_group_cols <- c(name_cols, "year_start", "year_end")
  dupe_name <- flat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(name_group_cols))) |>
    dplyr::filter(dplyr::n() > 1L) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::across(dplyr::all_of(name_group_cols)))

  if (nrow(dupe_guid) > 0L || nrow(dupe_name) > 0L) {
    cli::cli_alert_warning(
      "Duplicate {.field {level}} shapes found -- inspect the checks folder."
    )
  }
  if (nrow(dupe_guid) > 0L) {
    .polis_write(
      dupe_guid,
      file.path(checks_dir, paste0("spatial_duplicate_", level, "_guid.csv"))
    )
  }
  if (nrow(dupe_name) > 0L) {
    .polis_write(
      dupe_name,
      file.path(checks_dir, paste0("spatial_duplicate_", level, "_name.csv"))
    )
  }

  invisible(NULL)
}

#' Expand admin shapes to one row per active year
#'
#' Turns a shape table with `year_start` / `year_end` validity windows into a
#' long table with one row per administrative unit per active year, spanning each
#' unit's earliest start to the current year (a unit stays matchable in every
#' year from its start; `year_end` does not close the span here), plus a `9999`
#' sentinel year used to match records with an unknown year. When `checks_dir` is
#' supplied, any administrative unit with more than one shape active in the same
#' year is written to a CSV for manual review.
#'
#' @param data An `sf` object (or data frame) carrying the admin name/GUID
#'   columns plus `year_start` and `year_end`.
#' @param level Administrative level, `"adm1"` or `"adm2"`. Determines the
#'   grouping columns.
#' @param checks_dir Optional directory for the multiple-shape check CSV. When
#'   `NULL` (default) no check file is written.
#'
#' @return A [tibble][tibble::tibble] with the grouping columns plus
#'   `active_year`, `year_start` and `year_end`, one row per unit-year.
#'
#' @examples
#' shapes <- tibble::tibble(
#'   adm0_guid = "g0", adm0 = "NIGERIA",
#'   adm1_guid = "g1", adm1 = "BORNO",
#'   year_start = 2018, year_end = 2020
#' )
#' create_long_shape(shapes, "adm1")
#'
#' @export
create_long_shape <- function(data, level, checks_dir = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or sf object.")
  }
  if (nrow(data) == 0L) {
    cli::cli_abort("{.arg data} has zero rows.")
  }
  required_cols <- c("year_start", "year_end")
  missing_req <- setdiff(required_cols, names(data))
  if (length(missing_req) > 0L) {
    cli::cli_abort(
      "{.arg data} is missing required column{?s}: {.var {missing_req}}."
    )
  }
  if (!level %in% c("adm1", "adm2")) {
    cli::cli_abort("{.arg level} must be one of {.val adm1} or {.val adm2}.")
  }

  flat <- sf::st_drop_geometry(data)
  current_year <- lubridate::year(Sys.Date())

  group_cols <- if (identical(level, "adm1")) {
    c("adm0_guid", "adm0", "adm1", "adm1_guid")
  } else {
    c("adm0_guid", "adm0", "adm1", "adm1_guid", "adm2", "adm2_guid")
  }

  # one (min start, max end) window per unit, then expand to one row per year.
  # Expansion is done by index replication rather than a list-column + unnest,
  # which is an order of magnitude faster on the global ADM2 table.
  windows <- flat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      year_start = min(year_start, na.rm = TRUE),
      year_end = max(year_end, na.rm = TRUE),
      .groups = "drop"
    )

  # Enumerate active years from each unit's earliest start up to the present
  # only -- never past the current year. Open-ended shapes carry an end-year
  # sentinel (e.g. 9999); enumerating up to it would spawn thousands of phantom
  # rows per unit. The catch-all 9999 year is appended separately so unknown-year
  # records still match. Non-finite starts (all-NA groups) fall back to the
  # current year to avoid runaway sequences.
  year_from <- windows$year_start
  year_from[!is.finite(year_from)] <- current_year
  year_to <- pmax(year_from, current_year)
  active_years <- Map(
    function(from, to) c(seq.int(from, to, by = 1L), 9999L),
    year_from,
    year_to
  )
  long_shape <- windows[rep(seq_len(nrow(windows)), lengths(active_years)), ] |>
    dplyr::mutate(active_year = unlist(active_years, use.names = FALSE)) |>
    dplyr::relocate(active_year, .before = year_start) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, "active_year"))))

  # multiple shapes active in the same year -----------------------------------
  check_cols <- if (identical(level, "adm1")) {
    c("adm0", "adm1", "active_year")
  } else {
    c("adm0", "adm1", "adm2", "active_year")
  }
  shape_issues <- long_shape |>
    dplyr::count(
      dplyr::across(dplyr::all_of(check_cols)),
      name = "no_of_shapes"
    ) |>
    dplyr::filter(no_of_shapes > 1L) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(check_cols)))

  if (nrow(shape_issues) > 0L && !is.null(checks_dir)) {
    year_range <- paste(
      min(shape_issues$active_year, na.rm = TRUE),
      max(shape_issues$active_year, na.rm = TRUE),
      sep = "_"
    )
    .polis_write(
      shape_issues,
      file.path(
        checks_dir,
        paste0("spatial_shape_multiple_", level, "_", year_range, ".csv")
      )
    )
  }

  long_shape
}

#' Recover administrative info for point data via a spatial join
#'
#' Builds point geometries from a data frame's longitude/latitude columns,
#' spatially joins them to a district (`adm2`) shape table, optionally filtering
#' the join to shapes whose validity window (`year_start`..`year_end`) contains
#' the point's year, and fills any missing admin names/GUIDs from the matched
#' shape. Points that match more than one shape (e.g. on a boundary) are dropped
#' rather than guessed.
#'
#' @param data A data frame with `lon_var` and `lat_var` columns, and optionally
#'   `adm0` and a `year_col`.
#' @param shp_adm2 An `sf` object of district boundaries carrying the canonical
#'   admin name columns (`adm0` / `adm1` / `adm2`) and GUID columns, and, for
#'   temporal filtering, `year_start` / `year_end`.
#' @param year_col Optional name of the year column in `data`. When supplied and
#'   present, the join is filtered to temporally valid shapes. Default `NULL`.
#' @param lon_var Longitude column name. Default `"longitude"`.
#' @param lat_var Latitude column name. Default `"latitude"`.
#' @param crs CRS of the input coordinates as an EPSG code. Default `4326`.
#'
#' @return A data frame (geometry dropped) with imputed `adm1` / `adm1_guid` /
#'   `adm2` / `adm2_guid` where they were missing, restricted to rows with an
#'   unambiguous district match.
#'
#' @examples
#' \dontrun{
#' get_admin_info_from_coords(case_points, shp_adm2, year_col = "year_onset")
#' }
#'
#' @export
get_admin_info_from_coords <- function(
  data,
  shp_adm2,
  year_col = NULL,
  lon_var = "longitude",
  lat_var = "latitude",
  crs = 4326
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  if (nrow(data) == 0L) {
    cli::cli_abort("{.arg data} has zero rows.")
  }
  if (!inherits(shp_adm2, "sf")) {
    cli::cli_abort("{.arg shp_adm2} must be an {.cls sf} object.")
  }
  required_shp_cols <- c(
    "adm0",
    "adm1",
    "adm2",
    "adm0_guid",
    "adm1_guid",
    "adm2_guid"
  )
  missing_shp_cols <- setdiff(required_shp_cols, names(shp_adm2))
  if (length(missing_shp_cols) > 0L) {
    cli::cli_abort(
      "{.arg shp_adm2} is missing column{?s}: {.var {missing_shp_cols}}."
    )
  }
  required <- c(lon_var, lat_var)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing coordinate column{?s}: {.val {missing_cols}}.")
  }

  # points with valid coordinates ---------------------------------------------
  data_sf <- data |>
    dplyr::filter(!is.na(.data[[lon_var]]) & !is.na(.data[[lat_var]])) |>
    sf::st_as_sf(coords = c(lon_var, lat_var), crs = crs) |>
    sf::st_make_valid()
  # explicit point id: st_join replicates a point's row when it matches several
  # polygons; grouping on this (not on st_join's rownames, which carry no `.`
  # suffix for a tibble) is how ambiguous multi-matches are dropped below.
  data_sf[[".point_id"]] <- seq_len(nrow(data_sf))

  admin_cols <- c(
    "adm0",
    "adm1",
    "adm2",
    "adm0_guid",
    "adm1_guid",
    "adm2_guid"
  )

  # prepared shapes (planar, repaired) -- no adm0 pre-filter, so a point whose
  # own adm0 is missing (the case most needing recovery) is not excluded.
  shp_prepared <- shp_adm2 |>
    sf::st_transform(3857) |>
    sf::st_buffer(0) |>
    dplyr::select(dplyr::all_of(c(admin_cols, "year_start", "year_end")))

  use_year <- !is.null(year_col) && year_col %in% names(data)

  # spatial join (optionally temporally filtered) -----------------------------
  if (use_year) {
    data_with_admin <- data_sf |>
      sf::st_transform(3857) |>
      sf::st_join(shp_prepared, suffix = c("", "_shp")) |>
      dplyr::filter(
        is.na(year_start) |
          is.na(year_end) |
          is.na(.data[[year_col]]) |
          (.data[[year_col]] >= year_start & .data[[year_col]] <= year_end)
      ) |>
      sf::st_transform(crs)
  } else {
    data_with_admin <- data_sf |>
      sf::st_transform(3857) |>
      sf::st_join(
        dplyr::select(shp_prepared, dplyr::all_of(admin_cols)),
        suffix = c("", "_shp")
      ) |>
      sf::st_transform(crs)
  }

  # impute missing admin values, drop ambiguous matches -----------------------
  data_with_admin |>
    sf::st_drop_geometry() |>
    dplyr::mutate(
      adm1 = dplyr::if_else(
        is.na(adm1) & !is.na(adm0) & adm0 == adm0_shp,
        adm1_shp,
        adm1
      ),
      adm1_guid = dplyr::if_else(
        is.na(adm1_guid) & !is.na(adm0) & adm0 == adm0_shp,
        adm1_guid_shp,
        adm1_guid
      ),
      adm2 = dplyr::if_else(
        is.na(adm2) & !is.na(adm1) & !is.na(adm1_shp) & adm1 == adm1_shp,
        adm2_shp,
        adm2
      ),
      adm2_guid = dplyr::if_else(
        is.na(adm2_guid) & !is.na(adm1) & !is.na(adm1_shp) & adm1 == adm1_shp,
        adm2_guid_shp,
        adm2_guid
      )
    ) |>
    dplyr::filter(!is.na(adm2_guid)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(".point_id"))) |>
    dplyr::filter(dplyr::n() == 1L) |>
    dplyr::ungroup() |>
    dplyr::select(
      -dplyr::ends_with("_shp"),
      -dplyr::any_of(c("year_start", "year_end", ".point_id"))
    )
}

#' Recover missing admin from coordinates, in place
#'
#' For cases still missing `adm2`/`adm2_guid` that carry valid coordinates, a
#' point-in-polygon join against the district polygons (`shape_adm2`, the
#' `spatial_global_adm2` layer from [process_spatial()]) recovers `adm1`/`adm2`
#' and their GUIDs, matched to the case's onset year. Unlike
#' [get_admin_info_from_coords()] every row is kept -- only the missing cells of
#' unambiguously matched cases are filled.
#'
#' @param data A case data frame with coordinate and admin columns.
#' @param shape_adm2 An `sf` object of district polygons carrying the canonical
#'   admin name + GUID columns and `year_start`/`year_end`.
#' @param year_var Onset-year column for temporal filtering. Default
#'   `"year_onset"`.
#' @param lon_var,lat_var Coordinate columns. Default `"longitude"` /
#'   `"latitude"`.
#' @param target Admin columns whose `NA` marks a case as needing recovery: a
#'   case is recovered when *any* of them is missing. Default
#'   `c("adm1", "adm2", "adm1_guid", "adm2_guid")`, so a present-but-stale GUID
#'   no longer blocks recovery of a missing name.
#' @param verbose Emit a cli summary. Default `TRUE`.
#'
#' @return `data` with `adm1`/`adm2`/`adm1_guid`/`adm2_guid` filled where the
#'   coordinates resolved to a single district; all rows retained.
#'
#' @examples
#' \dontrun{
#' shp <- qs2::qs_read("spatial_global_adm2.qs2")
#' impute_geo_from_coords(cases, shp)
#' }
#'
#' @export
impute_geo_from_coords <- function(
  data,
  shape_adm2,
  year_var = "year_onset",
  lon_var = "longitude",
  lat_var = "latitude",
  target = c("adm1", "adm2", "adm1_guid", "adm2_guid"),
  verbose = TRUE
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  if (!inherits(shape_adm2, "sf")) {
    cli::cli_abort("{.arg shape_adm2} must be an {.cls sf} object.")
  }
  target <- intersect(target, names(data))
  if (!all(c(lon_var, lat_var) %in% names(data)) || length(target) == 0L) {
    return(data)
  }

  has_coord <- !is.na(data[[lon_var]]) &
    !is.na(data[[lat_var]]) &
    !(data[[lon_var]] %in% 0 & data[[lat_var]] %in% 0)
  # a case needs recovery if any targeted admin level (name or GUID) is missing
  incomplete <- Reduce(`|`, lapply(target, function(col) is.na(data[[col]])))
  need <- incomplete & has_coord
  if (!any(need)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("No cases need coordinate-based admin recovery.")
    }
    return(data)
  }

  idx <- which(need)
  fill_cols <- intersect(
    c("adm0", "adm1", "adm2", "adm0_guid", "adm1_guid", "adm2_guid"),
    intersect(names(data), names(shape_adm2))
  )

  # planar GEOS for a fast, robust point-in-polygon on global boundaries
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  yr <- if (year_var %in% names(data)) {
    suppressWarnings(as.integer(data[[year_var]][idx]))
  } else {
    rep(NA_integer_, length(idx))
  }
  pts <- sf::st_as_sf(
    data.frame(
      .crow = idx,
      .yr = yr,
      .lon = data[[lon_var]][idx],
      .lat = data[[lat_var]][idx]
    ),
    coords = c(".lon", ".lat"),
    crs = 4326
  )
  shp <- shape_adm2 |>
    dplyr::select(dplyr::all_of(c(fill_cols, "year_start", "year_end"))) |>
    sf::st_transform(3857) |>
    sf::st_buffer(0)
  pts <- sf::st_transform(pts, 3857)

  # trust the geometry: a point inside a district IS that district (no admin-name
  # gating, no adm0 pre-filter -- those break on spelling/accent/case mismatch).
  joined <- suppressWarnings(suppressMessages(
    sf::st_join(pts, shp, left = FALSE)
  )) |>
    sf::st_drop_geometry()

  # 1. country anchor FIRST: a case is only matched to a district in its own
  # country (via adm0_guid, robust to name spelling). This resolves disputed
  # territories -- e.g. a Pakistan case over Kashmir keeps the Pakistan polygon
  # and drops the overlapping India ones before the ambiguity check.
  if ("adm0_guid" %in% names(data) && "adm0_guid" %in% names(joined)) {
    case_g0 <- .geo_guid_key(data[["adm0_guid"]][joined$.crow])
    shp_g0 <- .geo_guid_key(joined[["adm0_guid"]])
    joined <- joined[
      is.na(case_g0) | (!is.na(shp_g0) & case_g0 == shp_g0),
      ,
      drop = FALSE
    ]
  }
  # 2. year filter only disambiguates: when a point matches several distinct
  # districts, keep the year-valid one; a sole match is used regardless of its
  # year window (which is often just a digitisation date, not a real start).
  joined$.year_ok <- is.na(joined$year_start) |
    is.na(joined$year_end) |
    is.na(joined$.yr) |
    (joined$.yr >= joined$year_start & joined$.yr <= joined$year_end)
  resolved <- joined |>
    dplyr::group_by(.crow) |>
    dplyr::filter(dplyr::n_distinct(adm2_guid) == 1L | .year_ok) |>
    dplyr::filter(dplyr::n_distinct(adm2_guid) == 1L) |>
    dplyr::slice(1L) |>
    dplyr::ungroup()
  crow <- resolved$.crow
  if (nrow(resolved) == 0L) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Coordinates resolved no additional districts.")
    }
    return(data)
  }

  # When the district was missing, adopt the matched district's whole adm1/adm2
  # chain (so the hierarchy stays coherent -- a coords-derived adm2 carries its
  # own adm1, not the stale one). When only a GUID was missing, fill that cell.
  adm2_was_na <- if ("adm2" %in% names(data)) {
    is.na(data[["adm2"]][crow])
  } else {
    rep(TRUE, length(crow))
  }
  chain_cols <- intersect(
    c("adm1", "adm2", "adm1_guid", "adm2_guid"),
    fill_cols
  )
  coord_filled <- logical(nrow(data))
  for (col in chain_cols) {
    vals <- resolved[[col]]
    if (stringr::str_detect(col, "_guid$")) {
      vals <- .geo_guid_canon(vals)
    }
    cur <- data[[col]][crow]
    take <- (adm2_was_na | is.na(cur)) & !is.na(vals)
    data[[col]][crow[take]] <- vals[take]
    coord_filled[crow[take]] <- TRUE
  }
  # stamp provenance on genuinely-unresolved rows we just filled from coordinates
  if ("geo_source" %in% names(data)) {
    data <- dplyr::mutate(
      data,
      geo_source = dplyr::if_else(
        coord_filled &
          (is.na(.data$geo_source) | .data$geo_source == "unresolved"),
        "coord_match",
        .data$geo_source
      )
    )
  }
  if (isTRUE(verbose)) {
    n_fixed <- .polis_big_num(nrow(resolved))
    cli::cli_alert_success(
      "Recovered admin for {n_fixed} cases from coordinates."
    )
  }
  data
}

utils::globalVariables(c(
  ".data",
  "guid",
  "shape",
  "startdate",
  "enddate",
  "year_start",
  "year_end",
  "active_year",
  "no_of_shapes",
  "int_part",
  "row_names",
  "adm0",
  "adm1",
  "adm2",
  "adm0_guid",
  "adm1_guid",
  "adm2_guid",
  "adm0_shp",
  "adm1_shp",
  "adm2_shp",
  "adm1_guid_shp",
  "adm2_guid_shp"
))


# EPID-driven geography cleaner.
#
# An EPID is a hierarchical case identifier conventionally shaped as
# COUNTRY-PROVINCE-DISTRICT-YEAR-SERIAL (e.g. "NIE-BOS-XYZ-24-001"). The
# leading three characters are a country code; subsequent dash-delimited
# segments encode province/district abbreviations. The EPID therefore acts
# as a recovery key for administrative geography when admin names/GUIDs are
# missing.
#
# Two capabilities are kept separate:
#   (A) parse the EPID into its component codes (pure, no reference data);
#   (B) infer/clean admin names + GUIDs via a provenance-stamped cascade.
#
# Everything here is standalone: it operates on any data frame carrying an
# `epid` column and never reaches into the rest of the package, except for an
# optional name-canonicalisation hook resolved at call time.

# ---------------------------------------------------------------------
# Pure parsers (capability A)
# ---------------------------------------------------------------------

#' Split an EPID into its component segments
#'
#' Splits each EPID on `sep` into one column per element of `parts`,
#' preserving `NA` and never erroring on malformed input.
#'
#' @param epid Character vector of EPID strings.
#' @param sep Single-character delimiter between segments. Default `"-"`.
#' @param parts Character vector naming the output columns, in order. Default
#'   `c("country", "province", "district", "year", "serial")`.
#' @param extra How to treat segments beyond `length(parts)`: `"drop"`
#'   (default) discards them; `"merge"` collapses the remainder into the last
#'   column.
#' @param fill How to pad EPIDs with fewer segments than `parts`. Only
#'   `"right"` is supported: missing trailing segments become `NA`.
#' @return A [tibble][tibble::tibble] with one character column per `parts`
#'   element. Blank/`NA` EPIDs yield an all-`NA` row.
#' @examples
#' epid_split(c("NIE-BOS-XYZ-24-001", "AGO-LUA", NA))
#' epid_split("NIE-BOS-XYZ-24-001-EXTRA", extra = "merge")
#' @export
epid_split <- function(
  epid,
  sep = "-",
  parts = c("country", "province", "district", "year", "serial"),
  extra = "drop",
  fill = "right"
) {
  if (!is.character(sep) || length(sep) != 1L || !nzchar(sep)) {
    cli::cli_abort("{.arg sep} must be a single non-empty string.")
  }
  if (!extra %in% c("drop", "merge")) {
    cli::cli_abort("{.arg extra} must be one of {.val drop} or {.val merge}.")
  }
  if (!identical(fill, "right")) {
    cli::cli_abort("{.arg fill} only supports {.val right}.")
  }

  trimmed <- trimws(as.character(epid))
  pieces <- stringr::str_split(trimmed, stringr::fixed(sep))
  n_parts <- length(parts)

  columns <- lapply(seq_len(n_parts), function(position) {
    vapply(
      pieces,
      function(segments) {
        if (length(segments) < position) {
          return(NA_character_)
        }
        value <- if (
          identical(extra, "merge") &&
            position == n_parts &&
            length(segments) > n_parts
        ) {
          paste(segments[position:length(segments)], collapse = sep)
        } else {
          segments[[position]]
        }
        if (is.na(value) || !nzchar(trimws(value))) NA_character_ else value
      },
      character(1)
    )
  })
  names(columns) <- parts

  result <- tibble::as_tibble(columns)
  blank_epid <- is.na(trimmed) | !nzchar(trimmed)
  result[blank_epid, ] <- NA_character_
  result
}

#' Extract the country code from an EPID
#'
#' Returns the leading country code: the first run of `n` word-characters,
#' matching how upstream systems parse the code.
#'
#' @param epid Character vector of EPID strings.
#' @param n Number of leading word-characters that form the code. Default `3`.
#' @param upper Whether to upper-case the result. Default `TRUE`.
#' @return Character vector of country codes (`NA` where none is found).
#' @examples
#' epid_country_code(c("NIE-BOS-XYZ-24-001", "ago-lua-01", NA))
#' @export
epid_country_code <- function(epid, n = 3, upper = TRUE) {
  trimmed <- trimws(as.character(epid))
  pattern <- sprintf("\\w{%d}", as.integer(n))
  code <- stringr::str_extract(trimmed, pattern)
  if (isTRUE(upper)) {
    code <- toupper(code)
  }
  code
}

#' Geographic prefix used for prefix-matching
#'
#' Returns the leading `length` characters of the normalised EPID
#' (trimmed, whitespace-collapsed, upper-cased) -- the country+province+
#' district stem used to recover geography from sibling records.
#'
#' @param epid Character vector of EPID strings.
#' @param length Number of leading characters in the prefix. Default `11`.
#' @return Character vector of prefixes (`NA` where the EPID is blank).
#' @examples
#' epid_prefix(c("NIE-BOS-XYZ-24-001", NA))
#' @export
epid_prefix <- function(epid, length = 11) {
  normalised <- .epid_normalise(epid)
  prefix <- substr(normalised, 1L, as.integer(length))
  prefix[.epid_blank(prefix)] <- NA_character_
  prefix
}

#' Separate a contact EPID from its base case EPID
#'
#' Contact records reuse a case EPID with a trailing contact marker
#' (`C`, `CC`, `HC`, or `C` followed by digits). This splits the marker off
#' so contacts collapse onto their parent case for matching.
#'
#' @param epid Character vector of EPID strings.
#' @return A [tibble][tibble::tibble] with `epid_base` (marker removed) and
#'   `contact_code` (the extracted marker, `NA` when absent).
#' @examples
#' epid_strip_contact(c("NIE-BOS-XYZ-24-001", "NIE-BOS-XYZ-24-001CC"))
#' @export
epid_strip_contact <- function(epid) {
  normalised <- .epid_normalise(epid)
  marker <- "[-_ ]?(CC|HC|C[0-9]*)$"
  contact_code <- stringr::str_extract(normalised, marker)
  contact_code <- stringr::str_remove(contact_code, "^[-_ ]")
  contact_code[.epid_blank(contact_code)] <- NA_character_
  base <- stringr::str_remove(normalised, marker)
  base[.epid_blank(base)] <- NA_character_
  tibble::tibble(epid_base = base, contact_code = contact_code)
}

# ---------------------------------------------------------------------
# Normalisation helpers
# ---------------------------------------------------------------------

#' Fast EPID base key for exact matching
#'
#' The upper-cased, trimmed EPID with any trailing contact marker removed -- the
#' same base [epid_strip_contact()] returns, but in one trim + one substitution
#' rather than the full normalise plus multi-pass extract/remove, for hot
#' exact-match joins over millions of rows. Blank maps to `NA`.
#'
#' @param epid Character vector.
#' @return Character vector of base keys (`NA` where blank).
#' @keywords internal
#' @noRd
.epid_base_key <- function(epid) {
  key <- toupper(trimws(as.character(epid)))
  key <- sub("[-_ ]?(CC|HC|C[0-9]*)$", "", key)
  key[!nzchar(key)] <- NA_character_
  key
}

#' Normalise an EPID/string for matching
#'
#' Trims, collapses internal whitespace, upper-cases, and maps blanks to
#' `NA`.
#'
#' @param epid Character vector.
#' @return Normalised character vector.
#' @keywords internal
#' @noRd
.epid_normalise <- function(epid) {
  normalised <- trimws(as.character(epid))
  normalised <- gsub("\\s+", " ", normalised)
  normalised <- toupper(normalised)
  normalised[!nzchar(normalised)] <- NA_character_
  normalised
}

#' Test for blank (`NA`/empty/whitespace) values
#'
#' @param value Atomic vector.
#' @return Logical vector, `TRUE` where blank.
#' @keywords internal
#' @noRd
.epid_blank <- function(value) {
  value <- as.character(value)
  # equivalent to `!nzchar(trimws(value))` over the same whitespace set, but a
  # single grepl pass instead of trimws's two sub() passes -- this helper runs
  # over the full column many times across the fill cascade, so it is hot.
  is.na(value) | !grepl("[^ \t\r\n]", value)
}

#' Whole-number scalar guards
#'
#' @param value Candidate value.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.is_count <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value >= 1 &&
    value == as.integer(value)
}

#' @param value Candidate value.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.is_nonneg_int <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value >= 0 &&
    value == as.integer(value)
}

# ---------------------------------------------------------------------
# Reference builders (capability B inputs)
# ---------------------------------------------------------------------

#' Build an EPID -> admin-value reference (most-recent-per-EPID)
#'
#' For each EPID, takes the most recent non-blank value of `admin_col`
#' (ties broken deterministically). Used to fill missing admin values from
#' other records that share the exact same EPID.
#'
#' @param data Data frame containing `epid_var`, `year_var`, and `admin_col`.
#' @param admin_col Name of the admin column to summarise.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Recency column name. Default `"year"`.
#' @return A [tibble][tibble::tibble] with columns `epid_var` and `admin_col`,
#'   one row per EPID.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("A-1", "A-1", "B-2"),
#'   year = c(2023, 2024, 2024),
#'   district = c(NA, "BOSSO", "LUANDA")
#' )
#' build_admin_ref(cases, "district")
#' @export
build_admin_ref <- function(
  data,
  admin_col,
  epid_var = "epid",
  year_var = "year"
) {
  required <- c(epid_var, year_var, admin_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing column{?s}: {.val {missing_cols}}.")
  }
  # Most-recent non-blank value per EPID via a base-R order + !duplicated()
  # first-per-group, not dplyr::group_by() |> slice(): on a full pull the EPIDs
  # are near-unique, so a grouped slice over millions of singleton groups is the
  # single slowest step in the cascade. Ordering by (epid, year desc, value asc)
  # and taking the first row of each EPID is equivalent and far cheaper.
  epid <- as.character(data[[epid_var]])
  value <- as.character(data[[admin_col]])
  year <- data[[year_var]]
  keep <- !.epid_blank(value)
  epid <- epid[keep]
  value <- value[keep]
  year <- year[keep]
  ord <- order(epid, -xtfrm(year), value, method = "radix")
  epid <- epid[ord]
  value <- value[ord]
  first <- !duplicated(epid)
  tibble::tibble(!!epid_var := epid[first], !!admin_col := value[first])
}

#' Build a (prefix, year) -> unique admin-value reference
#'
#' For each geographic prefix and year, returns the admin value only when it
#' is unambiguous (exactly one distinct non-blank value); otherwise the value
#' is `NA` and `n_candidates` records how many distinct values competed.
#'
#' @param data Data frame containing `epid_var`, `year_var`, and `admin_col`.
#' @param admin_col Name of the admin column to summarise.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Year column name. Default `"year"`.
#' @param prefix_length Prefix length passed to [epid_prefix()]. Default `11`.
#' @return A [tibble][tibble::tibble] with columns `prefix`, `year`,
#'   `n_candidates`, and `admin_col`.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2", "NIE-BOS-BBB-1"),
#'   year = c(2024, 2024, 2024),
#'   district = c("BOSSO", "BOSSO", "BIRNI")
#' )
#' build_prefix_ref(cases, "district", prefix_length = 7)
#' @export
build_prefix_ref <- function(
  data,
  admin_col,
  epid_var = "epid",
  year_var = "year",
  prefix_length = 11
) {
  required <- c(epid_var, year_var, admin_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing column{?s}: {.val {missing_cols}}.")
  }
  data |>
    dplyr::filter(
      !.epid_blank(.data[[admin_col]]),
      !.epid_blank(.data[[epid_var]])
    ) |>
    dplyr::transmute(
      prefix = epid_prefix(.data[[epid_var]], prefix_length),
      year = .data[[year_var]],
      value = as.character(.data[[admin_col]])
    ) |>
    dplyr::filter(!is.na(prefix)) |>
    dplyr::group_by(prefix, year) |>
    dplyr::summarise(
      n_candidates = dplyr::n_distinct(value, na.rm = TRUE),
      value = dplyr::if_else(
        n_candidates == 1L,
        dplyr::first(sort(value)),
        NA_character_
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(!!admin_col := value) |>
    dplyr::arrange(prefix, year)
}

# ---------------------------------------------------------------------
# Country resolution (capability A -> name)
# ---------------------------------------------------------------------

#' Resolve an EPID country code to a country name
#'
#' Maps the 3-character country code (see [epid_country_code()]) to a name
#' and ISO3 via a caller-supplied crosswalk, matching the code against either
#' the crosswalk's code or ISO3 column. Resolution never guesses: a code that
#' maps to more than one distinct name is flagged ambiguous and left `NA`.
#'
#' Temporal validity is the caller's responsibility -- pre-filter `ref` to the
#' period of interest (or pass a `region` with `region_var`) before calling.
#'
#' @param epid Character vector of EPID strings.
#' @param ref Optional crosswalk data frame. When `NULL`, the raw code is
#'   returned with `resolved = FALSE` (no fabrication).
#' @param region Optional region value to filter `ref` by (needs
#'   `region_var`).
#' @param code_var Crosswalk column holding the country code. Default
#'   `"code"`.
#' @param name_var Crosswalk column holding the country name. Default
#'   `"name"`.
#' @param iso3_var Crosswalk column holding the ISO3 code. Default `"iso3"`.
#' @param region_var Optional crosswalk column holding the region. Default
#'   `NULL`.
#' @return A [tibble][tibble::tibble] row-aligned to `epid` with columns
#'   `code`, `name`, `iso3`, `n_matches`, `ambiguous`, `resolved`.
#' @examples
#' crosswalk <- tibble::tibble(
#'   code = c("NIE", "AGO"),
#'   name = c("NIGERIA", "ANGOLA"),
#'   iso3 = c("NGA", "AGO")
#' )
#' resolve_epid_country(c("NIE-BOS-1", "AGO-LUA-1"), ref = crosswalk)
#' @export
resolve_epid_country <- function(
  epid,
  ref = NULL,
  region = NULL,
  code_var = "code",
  name_var = "name",
  iso3_var = "iso3",
  region_var = NULL
) {
  code <- epid_country_code(epid, n = 3, upper = TRUE)

  if (is.null(ref)) {
    return(tibble::tibble(
      code = code,
      name = NA_character_,
      iso3 = NA_character_,
      n_matches = 0L,
      ambiguous = FALSE,
      resolved = FALSE
    ))
  }

  required <- c(code_var, name_var)
  missing_cols <- setdiff(required, names(ref))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "{.arg ref} is missing column{?s}: {.val {missing_cols}}.",
      "i" = "Set {.arg code_var}/{.arg name_var} to match your crosswalk."
    ))
  }

  ref_use <- ref
  if (!is.null(region) && !is.null(region_var) && region_var %in% names(ref)) {
    ref_use <- ref_use[
      toupper(trimws(ref_use[[region_var]])) == toupper(region),
      ,
      drop = FALSE
    ]
  }

  has_iso3 <- iso3_var %in% names(ref_use)
  code_name_tbl <- tibble::tibble(
    key = toupper(trimws(ref_use[[code_var]])),
    name = as.character(ref_use[[name_var]]),
    iso3 = if (has_iso3) as.character(ref_use[[iso3_var]]) else NA_character_
  )
  if (has_iso3) {
    code_name_tbl <- dplyr::bind_rows(
      code_name_tbl,
      tibble::tibble(
        key = toupper(trimws(ref_use[[iso3_var]])),
        name = as.character(ref_use[[name_var]]),
        iso3 = as.character(ref_use[[iso3_var]])
      )
    )
  }
  summary_ref <- code_name_tbl |>
    dplyr::filter(!.epid_blank(key)) |>
    dplyr::distinct() |>
    dplyr::group_by(key) |>
    dplyr::summarise(
      n_matches = dplyr::n_distinct(name, na.rm = TRUE),
      name = dplyr::if_else(n_matches == 1L, dplyr::first(name), NA_character_),
      iso3 = dplyr::if_else(n_matches == 1L, dplyr::first(iso3), NA_character_),
      .groups = "drop"
    )

  resolved <- tibble::tibble(code = code) |>
    dplyr::left_join(summary_ref, by = dplyr::join_by(code == key))
  resolved |>
    dplyr::mutate(
      n_matches = dplyr::coalesce(n_matches, 0L),
      ambiguous = n_matches > 1L,
      resolved = n_matches == 1L
    )
}

# ---------------------------------------------------------------------
# Imputation cascade (capability B)
# ---------------------------------------------------------------------

#' Pick the single defensible candidate, else NA
#'
#' Returns the unique candidate value; on a tie, restricts to candidates whose
#' parent matches the row's known parent and accepts only if that is unique.
#' Never guesses.
#'
#' @param values Candidate values.
#' @param parents Parent values aligned to `values`.
#' @param this_parent The row's own (normalised) parent value.
#' @return A single character value (`NA` on ambiguity).
#' @keywords internal
#' @noRd
.epid_resolve_candidates <- function(values, parents, this_parent) {
  present <- values[!is.na(values)]
  distinct_values <- unique(present)
  if (length(distinct_values) == 1L) {
    return(distinct_values)
  }
  if (length(distinct_values) == 0L) {
    return(NA_character_)
  }
  if (!is.na(this_parent) && nzchar(this_parent)) {
    matched <- !is.na(values) & !is.na(parents) & parents == this_parent
    parent_values <- unique(values[matched])
    if (length(parent_values) == 1L) {
      return(parent_values)
    }
  }
  NA_character_
}

#' Prefix-match candidates for the still-missing rows
#'
#' Joins the missing rows' distinct `(prefix, year, parent)` keys to known rows
#' sharing the geographic prefix within `year_window`, resolves each key via
#' `.epid_resolve_candidates()`, then maps the result back onto every row that
#' carried the key. Resolving per distinct key (not per row) bounds the prefix
#' join so a high-frequency prefix cannot explode it.
#'
#' @param data Working data frame (carries `.epid_prefix`).
#' @param value_col Column being filled.
#' @param parent_col Parent admin column for tie-breaking, or `NULL`.
#' @param year_var Year column name.
#' @param year_window Allowed +/- year distance.
#' @param still_mask Logical mask of rows still missing.
#' @return A list with `value` and `ambiguous`, each length `nrow(data)`.
#' @keywords internal
#' @noRd
.epid_prefix_match <- function(
  data,
  value_col,
  parent_col,
  year_var,
  year_window,
  still_mask
) {
  n_row <- nrow(data)
  empty <- list(
    value = rep(NA_character_, n_row),
    ambiguous = rep(FALSE, n_row)
  )

  known_mask <- !.epid_blank(data[[value_col]]) & !is.na(data[[".epid_prefix"]])
  miss_mask <- still_mask & !is.na(data[[".epid_prefix"]])
  if (!any(known_mask) || !any(miss_mask)) {
    return(empty)
  }

  known_tbl <- tibble::tibble(
    k_prefix = data[[".epid_prefix"]][known_mask],
    k_year = data[[year_var]][known_mask],
    k_parent = if (is.null(parent_col)) {
      NA_character_
    } else {
      toupper(trimws(as.character(data[[parent_col]][known_mask])))
    },
    k_value = as.character(data[[value_col]][known_mask])
  )
  known_tbl <- dplyr::distinct(known_tbl)

  # Collapse the missing side to its distinct (prefix, year, parent) keys before
  # the prefix join. Every row sharing a key sees the same candidate set and so
  # resolves identically; resolving per key rather than per row keeps a
  # high-frequency prefix (one prefix shared by tens of thousands of missing
  # rows) from exploding the join into millions of pairs.
  miss_rows <- tibble::tibble(
    row_id = which(miss_mask),
    m_prefix = data[[".epid_prefix"]][miss_mask],
    m_year = data[[year_var]][miss_mask],
    m_parent = if (is.null(parent_col)) {
      NA_character_
    } else {
      toupper(trimws(as.character(data[[parent_col]][miss_mask])))
    }
  )
  miss_keys <- dplyr::distinct(miss_rows, m_prefix, m_year, m_parent)

  joined <- dplyr::inner_join(
    miss_keys,
    known_tbl,
    by = dplyr::join_by(m_prefix == k_prefix),
    relationship = "many-to-many"
  )
  # NA-safe year filter: a missing or known row with an unknown year still
  # matches on prefix (any year); a logical NA here would drop a valid pair.
  year_diff <- abs(joined$m_year - joined$k_year)
  joined <- joined[is.na(year_diff) | year_diff <= year_window, , drop = FALSE]
  if (nrow(joined) == 0L) {
    return(empty)
  }

  resolved <- joined |>
    dplyr::group_by(m_prefix, m_year, m_parent) |>
    dplyr::summarise(
      accepted = .epid_resolve_candidates(
        k_value,
        k_parent,
        dplyr::first(m_parent)
      ),
      n_distinct_value = dplyr::n_distinct(k_value, na.rm = TRUE),
      .groups = "drop"
    )

  # Map each key's resolution back onto every row that carried it (NA keys match
  # NA in a dplyr join, so unknown-year/parent rows still pick up their result).
  per_row <- dplyr::left_join(
    miss_rows,
    resolved,
    by = dplyr::join_by(m_prefix, m_year, m_parent)
  )

  value <- rep(NA_character_, n_row)
  ambiguous <- rep(FALSE, n_row)
  value[per_row$row_id] <- per_row$accepted
  ambiguous[per_row$row_id] <- is.na(per_row$accepted) &
    !is.na(per_row$n_distinct_value) &
    per_row$n_distinct_value > 1L
  list(value = value, ambiguous = ambiguous)
}

#' Look an external reference table up by EPID or prefix
#'
#' @param data Working data frame (carries `.epid_norm` and `.epid_prefix`).
#' @param reference External reference data frame, keyed on `epid` or
#'   `prefix`.
#' @param value_col Column to pull from `reference`.
#' @return Character vector, length `nrow(data)` (`NA` where unresolved or
#'   ambiguous).
#' @keywords internal
#' @noRd
.epid_reference_lookup <- function(data, reference, value_col) {
  n_row <- nrow(data)
  if (!value_col %in% names(reference)) {
    return(rep(NA_character_, n_row))
  }
  if ("epid" %in% names(reference)) {
    ref_key <- .epid_normalise(reference[["epid"]])
    data_key <- data[[".epid_norm"]]
  } else if ("prefix" %in% names(reference)) {
    ref_key <- toupper(trimws(as.character(reference[["prefix"]])))
    data_key <- data[[".epid_prefix"]]
  } else {
    return(rep(NA_character_, n_row))
  }

  lookup_tbl <- tibble::tibble(
    k = ref_key,
    v = as.character(reference[[value_col]])
  ) |>
    dplyr::filter(!.epid_blank(k), !.epid_blank(v)) |>
    dplyr::distinct() |>
    dplyr::group_by(k) |>
    dplyr::summarise(
      n = dplyr::n_distinct(v, na.rm = TRUE),
      v = dplyr::if_else(n == 1L, dplyr::first(v), NA_character_),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(v))

  lut <- stats::setNames(lookup_tbl$v, lookup_tbl$k)
  unname(lut[data_key])
}

#' Fill one target column through the imputation cascade
#'
#' Applies the requested strategies in order to a single column, stamping the
#' provenance of every fill. Never overwrites a present value; never fabricates
#' on ambiguity.
#'
#' @param data Working data frame.
#' @param value_col Column to fill.
#' @param parent_col Parent admin column (for prefix tie-break), or `NULL`.
#' @param is_admin0 Whether this is the top admin level.
#' @param is_name Whether this is a name column (vs a GUID).
#' @param strategies Ordered strategy vector.
#' @param reference External reference data frame, or `NULL`.
#' @param country_ref Country crosswalk, or `NULL`.
#' @param year_var Year column name.
#' @param prefix_length Prefix length.
#' @param year_window Allowed +/- year distance for prefix-matching.
#' @param canonicalise Whether to canonicalise filled name cells.
#' @return A list with `data`, `source` (provenance vector), and `counts`.
#' @keywords internal
#' @noRd
.epid_fill_target <- function(
  data,
  value_col,
  parent_col,
  is_admin0,
  is_name,
  strategies,
  reference,
  country_ref,
  year_var,
  prefix_length,
  year_window,
  canonicalise
) {
  data[[value_col]] <- as.character(data[[value_col]])
  blank_before <- .epid_blank(data[[value_col]])
  source <- dplyr::if_else(blank_before, NA_character_, "original")
  counts <- list(
    n_missing_before = sum(blank_before, na.rm = TRUE),
    self_ref = 0L,
    prefix_match = 0L,
    reference = 0L,
    country_prefix = 0L,
    n_ambiguous = 0L
  )

  for (strategy in strategies) {
    still <- .epid_blank(data[[value_col]])
    if (!any(still)) {
      break
    }

    if (identical(strategy, "self_ref")) {
      ref_tbl <- build_admin_ref(
        data,
        value_col,
        epid_var = ".epid_norm",
        year_var = year_var
      )
      lut <- stats::setNames(ref_tbl[[value_col]], ref_tbl[[".epid_norm"]])
      candidate <- unname(lut[data[[".epid_norm"]]])
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "self_ref"
      counts$self_ref <- sum(fill, na.rm = TRUE)
    } else if (identical(strategy, "prefix_match")) {
      matched <- .epid_prefix_match(
        data,
        value_col,
        parent_col,
        year_var,
        year_window,
        still
      )
      fill <- still & !is.na(matched$value)
      data[[value_col]][fill] <- matched$value[fill]
      source[fill] <- "prefix_match"
      counts$prefix_match <- sum(fill, na.rm = TRUE)
      counts$n_ambiguous <- counts$n_ambiguous +
        sum(still & matched$ambiguous & is.na(matched$value), na.rm = TRUE)
    } else if (
      identical(strategy, "reference") &&
        !is.null(reference) &&
        value_col %in% names(reference)
    ) {
      candidate <- .epid_reference_lookup(data, reference, value_col)
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "reference"
      counts$reference <- sum(fill, na.rm = TRUE)
    } else if (
      identical(strategy, "country_prefix") &&
        is_admin0 &&
        is_name &&
        !is.null(country_ref)
    ) {
      country <- resolve_epid_country(data[[".epid_norm"]], ref = country_ref)
      candidate <- country$name
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "country_prefix"
      counts$country_prefix <- sum(fill, na.rm = TRUE)
    }
  }

  if (isTRUE(canonicalise) && isTRUE(is_name)) {
    canon_fn <- tryCatch(
      utils::getFromNamespace("polis_fix_geo_names", "polished"),
      error = function(e) NULL
    )
    if (!is.null(canon_fn)) {
      fillable <- source %in%
        c("self_ref", "prefix_match", "reference", "country_prefix")
      if (any(fillable)) {
        fixed <- tryCatch(
          canon_fn(data[[value_col]][fillable]),
          error = function(e) NULL
        )
        if (!is.null(fixed) && length(fixed) == sum(fillable, na.rm = TRUE)) {
          data[[value_col]][fillable] <- as.character(fixed)
        }
      }
    }
  }

  source <- dplyr::if_else(is.na(source), "unresolved", source)
  counts$n_unresolved <- sum(source == "unresolved", na.rm = TRUE)
  list(data = data, source = source, counts = counts)
}

#' Build the per-level target specification
#'
#' @param admin0_var Admin0 name column, or `NULL` to skip.
#' @param admin1_var Admin1 name column, or `NULL` to skip.
#' @param admin2_var Admin2 name column, or `NULL` to skip.
#' @param guid_vars Named character vector of GUID columns, or `NULL`.
#' @return A list of target specification lists, one per column to fill.
#' @keywords internal
#' @noRd
.epid_targets <- function(admin0_var, admin1_var, admin2_var, guid_vars) {
  targets <- list()
  if (!is.null(admin0_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin0_var,
        level = "admin0",
        parent = NULL,
        is_admin0 = TRUE,
        is_name = TRUE
      ))
    )
  }
  if (!is.null(admin1_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin1_var,
        level = "admin1",
        parent = admin0_var,
        is_admin0 = FALSE,
        is_name = TRUE
      ))
    )
  }
  if (!is.null(admin2_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin2_var,
        level = "admin2",
        parent = admin1_var,
        is_admin0 = FALSE,
        is_name = TRUE
      ))
    )
  }
  parent_of <- list(adm0 = NULL, adm1 = admin0_var, adm2 = admin1_var)
  for (key in names(guid_vars)) {
    parent <- parent_of[[key]]
    targets <- c(
      targets,
      list(list(
        col = unname(guid_vars[[key]]),
        level = key,
        parent = parent,
        is_admin0 = identical(key, "adm0"),
        is_name = FALSE
      ))
    )
  }
  targets
}

#' Recover administrative geography from the EPID
#'
#' Fills missing administrative names (and optionally GUIDs) using the EPID as
#' a recovery key, through an ordered, provenance-stamped cascade. Only blank
#' cells are filled; present values are never overwritten and nothing is
#' fabricated on ambiguity.
#'
#' The cascade, per admin level (Admin0, then Admin1, then Admin2):
#' \describe{
#'   \item{original}{Value already present -- kept.}
#'   \item{self_ref}{Most-recent non-blank value for the exact same EPID
#'     elsewhere in `data`.}
#'   \item{prefix_match}{Unique value among records sharing the geographic
#'     prefix within `year_window` years; a parent-level tie-break is applied
#'     before declaring ambiguity.}
#'   \item{reference}{An external `reference` table keyed on EPID or prefix.}
#'   \item{country_prefix}{Admin0 only -- the country code resolved via
#'     `country_ref`.}
#' }
#' Any cell still blank after the cascade is labelled `unresolved`.
#'
#' @param data Non-empty data frame carrying an EPID column.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Year/onset column name. Default `"year_onset"`.
#' @param admin0_var,admin1_var,admin2_var Admin name columns; `NULL` skips a
#'   level. Defaults `"adm0"`/`"adm1"`/`"adm2"`.
#' @param guid_vars Named character vector mapping levels (`adm0`/`adm1`/
#'   `adm2`) to GUID columns to fill. Default `c(adm2 = "adm2_guid")`; set
#'   `NULL` to fill no GUIDs.
#' @param reference Optional external table (keyed on `epid` or `prefix`) of
#'   admin names/GUIDs, for data with no names of its own. Default `NULL`.
#' @param country_ref Optional country code -> name/ISO3 crosswalk. Default
#'   `NULL`.
#' @param strategies Ordered subset of `c("self_ref", "prefix_match",
#'   "reference", "country_prefix")`. Default uses all four.
#' @param prefix_length Prefix length for prefix-matching. Default `11`.
#' @param year_window Allowed +/- year distance when prefix-matching. Default
#'   `0`.
#' @param sep EPID segment delimiter. Default `"-"`.
#' @param canonicalise Whether to canonicalise filled name cells when a
#'   canonicaliser is available. Default `TRUE`.
#' @param audit Whether to build the per-level self-reference tables returned in
#'   `$ref` (for inspection). Default `TRUE`; set `FALSE` to skip the extra
#'   reference passes when the audit handle is not needed.
#' @param verbose Whether to print a cli summary. Default `TRUE`.
#' @return A named list:
#' \describe{
#'   \item{data}{`data` with filled admin/GUID columns plus a
#'     `<col>_source` provenance factor per filled column.}
#'   \item{ref}{The self-reference lookups built (for audit).}
#'   \item{qa}{A tibble of per-level fill counts and `pct_resolved`.}
#'   \item{meta}{The settings used.}
#' }
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002", "AGO-LUA-BBB-24-001"),
#'   year_onset = c(2024, 2024, 2024),
#'   adm0 = c("NIGERIA", NA, "ANGOLA"),
#'   adm1 = c("BORNO", NA, "LUANDA"),
#'   adm2 = c("BOSSO", NA, "LUANDA"),
#'   adm2_guid = c("g-bosso", NA, "g-luanda")
#' )
#' result <- impute_geo_from_epid(cases, verbose = FALSE)
#' result$qa
#' @export
impute_geo_from_epid <- function(
  data,
  epid_var = "epid",
  year_var = "year_onset",
  admin0_var = "adm0",
  admin1_var = "adm1",
  admin2_var = "adm2",
  guid_vars = c(adm2 = "adm2_guid"),
  reference = NULL,
  country_ref = NULL,
  strategies = c("self_ref", "prefix_match", "reference", "country_prefix"),
  prefix_length = 11,
  year_window = 0,
  sep = "-",
  canonicalise = TRUE,
  audit = TRUE,
  verbose = TRUE
) {
  known_strategies <- c(
    "self_ref",
    "prefix_match",
    "reference",
    "country_prefix"
  )

  # validate ----------------------------------------------------------
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  if (length(strategies) == 0L) {
    cli::cli_abort("{.arg strategies} must name at least one strategy.")
  }
  bad_strategies <- setdiff(strategies, known_strategies)
  if (length(bad_strategies) > 0L) {
    cli::cli_abort(c(
      "Unknown {.arg strategies} value{?s}: {.val {bad_strategies}}.",
      "i" = "Valid strategies: {.val {known_strategies}}."
    ))
  }
  if (!.is_count(prefix_length)) {
    cli::cli_abort("{.arg prefix_length} must be a positive whole number.")
  }
  if (!.is_nonneg_int(year_window)) {
    cli::cli_abort("{.arg year_window} must be a non-negative whole number.")
  }
  if (!is.character(sep) || length(sep) != 1L || !nzchar(sep)) {
    cli::cli_abort("{.arg sep} must be a single non-empty string.")
  }
  if (!is.null(guid_vars)) {
    bad_levels <- setdiff(names(guid_vars), c("adm0", "adm1", "adm2"))
    if (is.null(names(guid_vars)) || length(bad_levels) > 0L) {
      cli::cli_abort(c(
        "{.arg guid_vars} must be named with {.val adm0}/{.val adm1}/{.val adm2}.",
        "x" = if (length(bad_levels) > 0L)
          "Bad name{?s}: {.val {bad_levels}}." else NULL
      ))
    }
  }

  targets <- .epid_targets(admin0_var, admin1_var, admin2_var, guid_vars)
  if (length(targets) == 0L) {
    cli::cli_abort("No target columns: set at least one admin or GUID column.")
  }

  needs_year <- any(c("self_ref", "prefix_match") %in% strategies)
  target_cols <- vapply(targets, function(target) target$col, character(1))
  needed <- c(epid_var, target_cols)
  if (needs_year) {
    needed <- c(needed, year_var)
  }
  missing_cols <- setdiff(unique(needed), names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Missing required column{?s}: {.val {missing_cols}}.",
      "i" = "Set the matching {.arg *_var} argument(s) or add the column(s)."
    ))
  }

  if (!is.null(reference)) {
    if (!is.data.frame(reference)) {
      cli::cli_abort("{.arg reference} must be a data frame or {.code NULL}.")
    }
    if (!any(c("epid", "prefix") %in% names(reference))) {
      cli::cli_abort(c(
        "{.arg reference} needs an {.val epid} or {.val prefix} key column.",
        "i" = "Plus a value column named like the admin/GUID column to fill."
      ))
    }
  }
  if (!is.null(country_ref) && "country_prefix" %in% strategies) {
    if (!is.data.frame(country_ref)) {
      cli::cli_abort("{.arg country_ref} must be a data frame or {.code NULL}.")
    }
    missing_country <- setdiff(c("code", "name"), names(country_ref))
    if (length(missing_country) > 0L) {
      cli::cli_abort(
        "{.arg country_ref} is missing column{?s}: {.val {missing_country}}."
      )
    }
  }

  # normalise ---------------------------------------------------------
  work <- data
  work[[".epid_norm"]] <- .epid_normalise(
    epid_strip_contact(work[[epid_var]])$epid_base
  )
  # `.epid_norm` is already normalised, so take the prefix by a plain substring
  # rather than epid_prefix() (which would re-run .epid_normalise over every row).
  epid_pre <- substr(work[[".epid_norm"]], 1L, as.integer(prefix_length))
  epid_pre[.epid_blank(epid_pre)] <- NA_character_
  work[[".epid_prefix"]] <- epid_pre

  # run cascade per target --------------------------------------------
  qa_rows <- list()
  for (target in targets) {
    filled <- .epid_fill_target(
      work,
      value_col = target$col,
      parent_col = target$parent,
      is_admin0 = target$is_admin0,
      is_name = target$is_name,
      strategies = strategies,
      reference = reference,
      country_ref = country_ref,
      year_var = year_var,
      prefix_length = prefix_length,
      year_window = year_window,
      canonicalise = canonicalise
    )
    work <- filled$data
    work[[paste0(target$col, "_source")]] <- factor(
      filled$source,
      levels = c(
        "original",
        "self_ref",
        "prefix_match",
        "reference",
        "country_prefix",
        "unresolved"
      )
    )
    counts <- filled$counts
    qa_rows[[target$col]] <- tibble::tibble(
      level = target$level,
      column = target$col,
      n_missing_before = counts$n_missing_before,
      n_filled_self_ref = counts$self_ref,
      n_filled_prefix_match = counts$prefix_match,
      n_filled_reference = counts$reference,
      n_filled_country_prefix = counts$country_prefix,
      n_ambiguous = counts$n_ambiguous,
      n_unresolved = counts$n_unresolved,
      pct_resolved = dplyr::if_else(
        counts$n_missing_before == 0L,
        1,
        (counts$n_missing_before - counts$n_unresolved) /
          counts$n_missing_before
      )
    )
  }
  qa <- dplyr::bind_rows(qa_rows)

  # audit references --------------------------------------------------
  # Built only for the returned `$ref` audit handle; skip when `audit = FALSE`
  # (e.g. the AFP cleaner, which discards it) to avoid two full reference passes.
  ref_audit <- list()
  if (isTRUE(audit) && needs_year) {
    if (!is.null(admin1_var)) {
      ref_audit$admin1 <- build_admin_ref(
        work,
        admin1_var,
        epid_var = ".epid_norm",
        year_var = year_var
      )
    }
    if (!is.null(admin2_var)) {
      ref_audit$admin2 <- build_admin_ref(
        work,
        admin2_var,
        epid_var = ".epid_norm",
        year_var = year_var
      )
    }
  }

  # ambiguous country codes (for the warning) -------------------------
  ambiguous_codes <- character(0)
  if (!is.null(country_ref) && "country_prefix" %in% strategies) {
    country <- resolve_epid_country(work[[".epid_norm"]], ref = country_ref)
    ambiguous_codes <- unique(country$code[
      country$ambiguous & !is.na(country$code)
    ])
  }

  out_data <- work |>
    dplyr::select(-dplyr::any_of(c(".epid_norm", ".epid_prefix")))

  meta <- list(
    strategies = strategies,
    prefix_length = as.integer(prefix_length),
    year_window = as.integer(year_window),
    reference_used = !is.null(reference),
    country_ref_used = !is.null(country_ref)
  )

  if (isTRUE(verbose)) {
    .epid_report(qa, strategies, prefix_length, year_window, ambiguous_codes)
  }

  list(data = out_data, ref = ref_audit, qa = qa, meta = meta)
}

#' Print the cli summary for an imputation run
#'
#' @keywords internal
#' @noRd
.epid_report <- function(
  qa,
  strategies,
  prefix_length,
  year_window,
  ambiguous_codes
) {
  cli::cli_rule(left = "EPID geo imputation")
  cli::cli_text("Strategies (in order): {.val {strategies}}.")
  cli::cli_text(
    "Prefix length {.val {prefix_length}} \u00b7 year window {.val {year_window}}."
  )
  cli::cli_h3("Results by level")
  for (i in seq_len(nrow(qa))) {
    row <- qa[i, ]
    cli::cli_bullets(c(
      "*" = paste0(
        "{.field {row$column}} ({row$level}): {row$n_missing_before} ",
        "missing \u2192 self_ref {row$n_filled_self_ref}, ",
        "prefix_match {row$n_filled_prefix_match}, ",
        "reference {row$n_filled_reference}, ",
        "country_prefix {row$n_filled_country_prefix}; ",
        "ambiguous {row$n_ambiguous}, unresolved {row$n_unresolved} ",
        "({round(row$pct_resolved * 100)}% resolved)"
      )
    ))
  }
  if (length(ambiguous_codes) > 0L) {
    cli::cli_alert_warning(
      "Country code{?s} mapping to >1 name: {.val {ambiguous_codes}}."
    )
  }
  total_unresolved <- sum(qa$n_unresolved, na.rm = TRUE)
  if (total_unresolved > 0L) {
    cli::cli_alert_warning(
      "{total_unresolved} cell{?s} left unresolved \u2014 not fabricated."
    )
  } else {
    cli::cli_alert_success("All targeted missing cells resolved.")
  }
  invisible(NULL)
}

utils::globalVariables(c(
  ".data",
  ".epid_norm",
  ".epid_prefix",
  "prefix",
  "year",
  "value",
  "n_candidates",
  "key",
  "name",
  "iso3",
  "n_matches",
  "row_id",
  "k_value",
  "k_parent",
  "m_parent",
  "accepted",
  "n_distinct_value",
  "k",
  "v",
  "n"
))


# =============================================================================
# Geographic name fixes (data-driven)
#
# preprocess() historically hard-coded a handful of country/province name
# normalisations as inline `if_else` / `str_replace_all` calls scattered across
# the pipeline (Cote d'Ivoire, Pakistan NWFP, Iran, the United Kingdom). This
# module lifts them into a single maintainable lookup table shipped with the
# package (`inst/extdata/geo_name_fixes.csv`) plus a small applier, so the rules
# are visible, testable and extensible without touching pipeline code.
#
# Note: this covers *name normalisations* only. Genuinely conditional business
# rules (e.g. the Congo-2010 / Nigeria-2011 classification overrides, the
# Sudan-2011 exclusion) are not name fixes and remain rule-based in
# preprocess().
# =============================================================================

#' Geographic name-fix lookup table
#'
#' Returns the curated table of geographic name normalisations applied during
#' preprocessing, read from `inst/extdata/geo_name_fixes.csv`. Each row is one
#' rule:
#' \describe{
#'   \item{`field`}{Logical field the rule targets: `"adm0_name"`,
#'     `"adm1_name"` or `"location"`.}
#'   \item{`match_type`}{How to match: `"contains"` (replace the whole value if
#'     the pattern occurs anywhere), `"exact"` (replace only on an exact match),
#'     or `"substr"` (replace the matched substring in place).}
#'   \item{`pattern`}{Literal string to match (not a regex).}
#'   \item{`replacement`}{Replacement value.}
#'   \item{`note`}{Why the rule exists.}
#' }
#'
#' @return A tibble of name-fix rules.
#' @examples
#' polis_geo_name_fixes()
#' @export
polis_geo_name_fixes <- function() {
  path <- .polis_extdata_path("geo_name_fixes.csv")
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
}

#' Apply geographic name fixes to a character vector
#'
#' Applies every rule in [polis_geo_name_fixes()] whose `field` matches the
#' requested `field`, in table order, to a vector of names. Matching is literal
#' (not regex). `NA`s are preserved.
#'
#' @param x A character vector of geographic names.
#' @param field Which rule set to apply: `"adm0_name"`, `"adm1_name"` or
#'   `"location"`.
#' @param fixes The lookup table (default [polis_geo_name_fixes()]). Pass a
#'   filtered/extended table to customise.
#'
#' @return `x` with the matching fixes applied.
#' @examples
#' polis_fix_geo_names(c("REPUBLIQUE DE COTE D'IVOIRE", "NIGERIA"), "adm0_name")
#' polis_fix_geo_names(c("KHYBER PAKHTOON", "FATA", "SINDH"), "adm1_name")
#' @export
polis_fix_geo_names <- function(x, field, fixes = polis_geo_name_fixes()) {
  .polis_apply_geo_name_fixes(x, field, fixes)
}

#' Internal vectorised applier for geo name fixes.
#' @keywords internal
#' @noRd
.polis_apply_geo_name_fixes <- function(
  x,
  field,
  fixes = polis_geo_name_fixes()
) {
  x <- as.character(x)
  valid_fields <- c("adm0_name", "adm1_name", "location")
  if (!field %in% valid_fields) {
    cli::cli_abort(c(
      "Unknown {.arg field}: {.val {field}}",
      "i" = "Valid fields: {.val {valid_fields}}"
    ))
  }
  rules <- fixes[!is.na(fixes$field) & fixes$field == field, , drop = FALSE]
  if (nrow(rules) == 0) {
    return(x)
  }

  for (i in seq_len(nrow(rules))) {
    pat <- rules$pattern[i]
    rep <- rules$replacement[i]
    type <- rules$match_type[i]

    if (type == "contains") {
      hit <- !is.na(x) & stringr::str_detect(x, stringr::fixed(pat))
      x[hit] <- rep
    } else if (type == "exact") {
      hit <- !is.na(x) & x == pat
      x[hit] <- rep
    } else if (type == "substr") {
      x <- stringr::str_replace_all(x, stringr::fixed(pat), rep)
    } else {
      cli::cli_abort("Unknown {.field match_type}: {.val {type}}")
    }
  }
  x
}

#' Normalise admin names on a cleaned data frame
#'
#' Convenience wrapper used by every cleaner: applies the country-level
#' (`adm0_name`) fixes to the `adm0` column and the province-level (`adm1_name`)
#' fixes to the `adm1` column, when present. Columns are expected to already
#' carry canonical names (post [standardise_names()]).
#'
#' @param data A data frame with canonical `adm0`/`adm1` columns.
#' @param fixes The lookup table (default [polis_geo_name_fixes()]).
#'
#' @return `data` with admin names normalised.
#'
#' @export
fix_geo_names <- function(data, fixes = polis_geo_name_fixes()) {
  if ("adm0" %in% names(data)) {
    data$adm0 <- .polis_apply_geo_name_fixes(data$adm0, "adm0_name", fixes)
  }
  if ("adm1" %in% names(data)) {
    data$adm1 <- .polis_apply_geo_name_fixes(data$adm1, "adm1_name", fixes)
  }
  data
}

# =============================================================================
# Reconcile case geography against the cleaned district shapes
#
# Two case-side geo fixers that consume the outputs of process_spatial():
#
#   reconcile_admin_guids()  attribute-only. Validates and corrects a case
#                            table's admin names + GUIDs against the long ADM2
#                            attribute table (spatial_adm2_long_shape), the
#                            authority for which GUIDs/names are valid per year.
#                            No geometry, so it is fast on millions of rows.
#
#   impute_missing_coords()  geometry. Places a random point inside the case's
#                            district polygon (from spatial_global_adm2) for
#                            cases whose coordinates are missing or (0, 0).
#
# Both are standalone and composable; clean_afp() runs reconcile_admin_guids()
# when a `shape` is supplied. Coordinate imputation stays a separate, opt-in
# step because it needs the (large) polygon layer, not the flat attribute table.
# =============================================================================

#' Reconcile case admin names and GUIDs against the long district shape
#'
#' Validates and, where possible, corrects the admin GUIDs and names on a case
#' table against the authoritative long ADM2 attribute table produced by
#' [process_spatial()] / [create_long_shape()] (one row per district per active
#' year, with the parent ADM0/ADM1 names and GUIDs). For each case:
#' \enumerate{
#'   \item If the ADM2 GUID matches a district valid in the case's onset year,
#'     the shape is authoritative: missing/mismatched admin names and parent
#'     GUIDs are filled from it.
#'   \item Otherwise, if the ADM0+ADM1+ADM2 names unambiguously identify one
#'     district that year, its GUIDs are adopted (`guid_corrected_from_name`).
#'   \item Otherwise the row is left unchanged and flagged `unresolved`.
#' }
#' GUIDs are compared case- and brace-insensitively (`{ABC}` == `abc`) and
#' emitted lower-case without braces. A missing onset year matches the `9999`
#' catch-all rows in the shape.
#'
#' @param data A case data frame (e.g. the output of [clean_afp()]).
#' @param shape The long ADM2 attribute table: a data frame with `adm0`/`adm1`/
#'   `adm2`, `adm0_guid`/`adm1_guid`/`adm2_guid` and `active_year` (an `sf`
#'   object is accepted and its geometry dropped).
#' @param year_var Case onset-year column. Default `"year_onset"`.
#' @param guid_vars Named (`adm0`/`adm1`/`adm2`) case GUID columns. Default
#'   `c(adm0 = "adm0_guid", adm1 = "adm1_guid", adm2 = "adm2_guid")`.
#' @param name_vars Named (`adm0`/`adm1`/`adm2`) case admin-name columns.
#'   Default `c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2")`.
#' @param sink Optional file path; when set, the per-row reconciliation flags
#'   for changed/unresolved rows are written there as CSV.
#' @param verbose Emit a cli summary. Default `TRUE`.
#'
#' @return `data` with reconciled admin name/GUID columns and an added
#'   `geo_source` factor (`guid_match` / `guid_corrected_from_name` /
#'   `unresolved`). A `reconcile_qa` attribute carries per-country issue counts.
#'
#' @examples
#' shape <- data.frame(
#'   adm0 = "NIGERIA", adm1 = "BORNO", adm2 = "BOSSO",
#'   adm0_guid = "{A0}", adm1_guid = "{A1}", adm2_guid = "{A2}",
#'   active_year = 2024
#' )
#' cases <- data.frame(
#'   adm0 = "NIGERIA", adm1 = NA, adm2 = NA,
#'   adm0_guid = "a0", adm1_guid = NA, adm2_guid = "a2",
#'   year_onset = 2024
#' )
#' reconcile_admin_guids(cases, shape, verbose = FALSE)[, c("adm1", "adm2")]
#'
#' @export
reconcile_admin_guids <- function(
  data,
  shape,
  year_var = "year_onset",
  guid_vars = c(adm0 = "adm0_guid", adm1 = "adm1_guid", adm2 = "adm2_guid"),
  name_vars = c(adm0 = "adm0", adm1 = "adm1", adm2 = "adm2"),
  sink = NULL,
  verbose = TRUE
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  if (inherits(shape, "sf")) {
    shape <- sf::st_drop_geometry(shape)
  }
  g <- guid_vars
  nm <- name_vars
  needed_shape <- c(
    nm[["adm0"]],
    nm[["adm1"]],
    nm[["adm2"]],
    "adm0_guid",
    "adm1_guid",
    "adm2_guid",
    "active_year"
  )
  missing_shape <- setdiff(needed_shape, names(shape))
  if (length(missing_shape) > 0L) {
    cli::cli_abort(
      "{.arg shape} is missing column{?s}: {.var {missing_shape}}."
    )
  }
  needed_case <- c(g, nm, year_var)
  missing_case <- setdiff(unname(needed_case), names(data))
  if (length(missing_case) > 0L) {
    cli::cli_abort("{.arg data} is missing column{?s}: {.var {missing_case}}.")
  }

  lookups <- .geo_shape_lookups(shape, nm)
  out <- .geo_apply_reconcile(data, lookups, g, nm, year_var)

  qa <- .geo_reconcile_qa(out, g, nm)
  attr(out, "reconcile_qa") <- qa
  if (is.character(sink) && nzchar(sink)) {
    changed <- out[out$geo_source != "guid_match", , drop = FALSE]
    readr::write_csv(changed, sink)
  }
  if (isTRUE(verbose)) {
    tab <- table(out$geo_source)
    cli::cli_alert_info(
      "Reconciled {nrow(out)} case{?s}: \\
      {tab[['guid_match']] %||% 0L} by GUID, \\
      {tab[['guid_corrected_from_name']] %||% 0L} corrected from name, \\
      {tab[['unresolved']] %||% 0L} unresolved."
    )
  }
  out
}

#' Build the by-GUID and unambiguous by-name district lookups from the shape
#'
#' Hash-based `distinct`/`count`/`semi_join` rather than `group_by` +
#' `slice_max`/`n_distinct`, which is far faster on the ~1.2M-row long ADM2
#' table while giving identical lookups.
#' @noRd
.geo_shape_lookups <- function(shape, nm) {
  base <- dplyr::tibble(
    k2 = .geo_guid_key(shape[["adm2_guid"]]),
    year = suppressWarnings(as.integer(shape[["active_year"]])),
    s_adm0 = shape[[nm[["adm0"]]]],
    s_adm1 = shape[[nm[["adm1"]]]],
    s_adm2 = shape[[nm[["adm2"]]]],
    s_g0 = .geo_guid_canon(shape[["adm0_guid"]]),
    s_g1 = .geo_guid_canon(shape[["adm1_guid"]]),
    s_g2 = .geo_guid_canon(shape[["adm2_guid"]])
  )

  # by-GUID, year-specific: one row per (district, year)
  by_guid <- dplyr::distinct(base, k2, year, .keep_all = TRUE)

  # any-year fallback: most recent real (non-9999) boundary per district
  by_guid_any <- base |>
    dplyr::filter(year != 9999L) |>
    dplyr::arrange(dplyr::desc(year)) |>
    dplyr::distinct(k2, .keep_all = TRUE)

  # unambiguous name -> single district: keep (adm0, adm1, adm2, year) combos
  # that map to exactly one GUID, an ambiguous name never overwrites a GUID.
  single <- base |>
    dplyr::distinct(s_adm0, s_adm1, s_adm2, year, s_g2) |>
    dplyr::count(s_adm0, s_adm1, s_adm2, year, name = ".n") |>
    dplyr::filter(.n == 1L)
  by_name <- base |>
    dplyr::distinct(s_adm0, s_adm1, s_adm2, year, .keep_all = TRUE) |>
    dplyr::semi_join(
      single,
      by = dplyr::join_by(s_adm0, s_adm1, s_adm2, year)
    )

  list(by_guid = by_guid, by_guid_any = by_guid_any, by_name = by_name)
}

#' Resolve each case row against the GUID then name lookups
#' @noRd
.geo_apply_reconcile <- function(data, lookups, g, nm, year_var) {
  d <- data
  d$.row <- seq_len(nrow(d))
  d$.yr <- dplyr::coalesce(suppressWarnings(as.integer(d[[year_var]])), 9999L)
  d$.k2 <- .geo_guid_key(d[[g[["adm2"]]]])

  gm <- dplyr::left_join(
    dplyr::tibble(.row = d$.row, k2 = d$.k2, year = d$.yr),
    lookups$by_guid,
    by = dplyr::join_by(k2, year)
  )
  gm_any <- dplyr::left_join(
    dplyr::tibble(.row = d$.row, k2 = d$.k2),
    lookups$by_guid_any,
    by = dplyr::join_by(k2)
  )
  nmatch <- dplyr::left_join(
    dplyr::tibble(
      .row = d$.row,
      s_adm0 = d[[nm[["adm0"]]]],
      s_adm1 = d[[nm[["adm1"]]]],
      s_adm2 = d[[nm[["adm2"]]]],
      year = d$.yr
    ),
    lookups$by_name,
    by = dplyr::join_by(s_adm0, s_adm1, s_adm2, year)
  )

  by_guid_hit <- !is.na(gm$s_g2)
  by_any_hit <- !by_guid_hit & !is.na(gm_any$s_g2)
  by_name_hit <- !by_guid_hit & !by_any_hit & !is.na(nmatch$s_g2)

  pick <- function(col) {
    dplyr::case_when(
      by_guid_hit ~ gm[[col]],
      by_any_hit ~ gm_any[[col]],
      by_name_hit ~ nmatch[[col]],
      .default = NA_character_
    )
  }
  d[[nm[["adm0"]]]] <- dplyr::coalesce(pick("s_adm0"), d[[nm[["adm0"]]]])
  d[[nm[["adm1"]]]] <- dplyr::coalesce(pick("s_adm1"), d[[nm[["adm1"]]]])
  d[[nm[["adm2"]]]] <- dplyr::coalesce(pick("s_adm2"), d[[nm[["adm2"]]]])
  d[[g[["adm0"]]]] <- dplyr::coalesce(
    pick("s_g0"),
    .geo_guid_canon(d[[g[["adm0"]]]])
  )
  d[[g[["adm1"]]]] <- dplyr::coalesce(
    pick("s_g1"),
    .geo_guid_canon(d[[g[["adm1"]]]])
  )
  d[[g[["adm2"]]]] <- dplyr::coalesce(
    pick("s_g2"),
    .geo_guid_canon(d[[g[["adm2"]]]])
  )
  d$geo_source <- dplyr::case_when(
    by_guid_hit ~ "guid_match",
    by_any_hit ~ "guid_match_other_year",
    by_name_hit ~ "guid_corrected_from_name",
    .default = "unresolved"
  )
  d$.row <- NULL
  d$.yr <- NULL
  d$.k2 <- NULL
  d
}

#' Per-country reconciliation issue counts
#' @noRd
.geo_reconcile_qa <- function(out, g, nm) {
  dplyr::tibble(
    adm0 = out[[nm[["adm0"]]]],
    geo_source = out$geo_source,
    na_adm1 = is.na(out[[nm[["adm1"]]]]),
    na_adm2 = is.na(out[[nm[["adm2"]]]])
  ) |>
    dplyr::group_by(adm0) |>
    dplyr::summarise(
      n = dplyr::n(),
      unresolved = sum(geo_source == "unresolved", na.rm = TRUE),
      corrected = sum(geo_source == "guid_corrected_from_name", na.rm = TRUE),
      missing_adm1 = sum(na_adm1, na.rm = TRUE),
      missing_adm2 = sum(na_adm2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(unresolved > 0L | corrected > 0L | missing_adm2 > 0L) |>
    dplyr::arrange(dplyr::desc(unresolved))
}

#' Place a random point inside the district polygon for cases missing coordinates
#'
#' For each case whose coordinates are missing or `(0, 0)`, draws a random point
#' uniformly within its district (`adm2`) polygon, so downstream maps and
#' point-in-polygon work have a usable location. Cases with valid coordinates are
#' untouched. A district whose polygon cannot be sampled falls back to its
#' centroid buffered by `fallback_buffer` metres.
#'
#' @param data A case data frame carrying an ADM2 GUID and coordinate columns.
#' @param shape_adm2 An `sf` object of district polygons with an `adm2_guid`
#'   column (the `spatial_global_adm2` layer from [process_spatial()]).
#' @param guid_var Case ADM2 GUID column. Default `"adm2_guid"`.
#' @param lon_var,lat_var Case coordinate columns. Default `"longitude"` /
#'   `"latitude"`.
#' @param shape_guid_var ADM2 GUID column in `shape_adm2`. Default
#'   `"adm2_guid"`.
#' @param seed Integer seed for reproducible sampling. Default `1234`.
#' @param fallback_buffer Buffer radius in metres for the centroid fallback when
#'   a polygon cannot be sampled. Default `3000`.
#' @param verbose Emit a cli summary. Default `TRUE`.
#'
#' @return `data` with `lon_var`/`lat_var` filled for sampled rows and a logical
#'   `coord_imputed` column marking them.
#'
#' @examples
#' \dontrun{
#' shp <- qs2::qs_read("spatial_global_adm2.qs2")
#' impute_missing_coords(cases, shp)
#' }
#'
#' @export
impute_missing_coords <- function(
  data,
  shape_adm2,
  guid_var = "adm2_guid",
  lon_var = "longitude",
  lat_var = "latitude",
  shape_guid_var = "adm2_guid",
  seed = 1234,
  fallback_buffer = 3000,
  verbose = TRUE
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  if (!inherits(shape_adm2, "sf")) {
    cli::cli_abort("{.arg shape_adm2} must be an {.cls sf} object.")
  }
  miss_cols <- setdiff(c(guid_var, lon_var, lat_var), names(data))
  if (length(miss_cols) > 0L) {
    cli::cli_abort("{.arg data} is missing column{?s}: {.var {miss_cols}}.")
  }
  if (!shape_guid_var %in% names(shape_adm2)) {
    cli::cli_abort("{.arg shape_adm2} has no {.var {shape_guid_var}} column.")
  }

  data$coord_imputed <- FALSE
  need <- (is.na(data[[lon_var]]) | is.na(data[[lat_var]])) |
    (data[[lon_var]] %in% 0 & data[[lat_var]] %in% 0)
  need <- need & !is.na(data[[guid_var]])
  if (!any(need)) {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("No cases need coordinate imputation.")
    }
    return(data)
  }

  # one polygon per district, keyed on the normalised GUID
  shp <- shape_adm2
  shp$.k <- .geo_guid_key(shp[[shape_guid_var]])
  shp <- shp[!duplicated(shp$.k), , drop = FALSE]

  counts <- table(.geo_guid_key(data[[guid_var]][need]))
  sampled <- .geo_sample_points(
    shp,
    counts,
    seed,
    fallback_buffer,
    verbose
  )

  filled <- .geo_fill_sampled(
    data,
    need,
    guid_var,
    lon_var,
    lat_var,
    sampled
  )
  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "Imputed coordinates for {sum(filled$coord_imputed)} case{?s}."
    )
  }
  filled
}

#' Draw `n` random points per district GUID, with a centroid-buffer fallback
#' @noRd
.geo_sample_points <- function(shp, counts, seed, fallback_buffer, verbose) {
  set.seed(seed)
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  guids <- names(counts)
  result <- vector("list", length(guids))
  names(result) <- guids
  for (k in guids) {
    poly <- shp[shp$.k == k, , drop = FALSE]
    if (nrow(poly) == 0L) {
      next
    }
    n <- as.integer(counts[[k]])
    pts <- tryCatch(
      suppressWarnings(suppressMessages(sf::st_sample(poly, n, exact = TRUE))),
      error = function(e) NULL
    )
    if (is.null(pts) || length(pts) < n) {
      centroid <- suppressWarnings(sf::st_centroid(
        poly,
        of_largest_polygon = TRUE
      ))
      buffered <- sf::st_buffer(centroid, dist = fallback_buffer)
      pts <- suppressWarnings(suppressMessages(sf::st_sample(buffered, n)))
    }
    coords <- sf::st_coordinates(sf::st_transform(
      sf::st_sfc(
        pts,
        crs = sf::st_crs(shp)
      ),
      4326
    ))
    result[[k]] <- coords[seq_len(min(n, nrow(coords))), , drop = FALSE]
  }
  result
}

#' Write the sampled coordinates back onto the cases that needed them
#' @noRd
.geo_fill_sampled <- function(data, need, guid_var, lon_var, lat_var, sampled) {
  keys <- .geo_guid_key(data[[guid_var]])
  cursor <- stats::setNames(rep(0L, length(sampled)), names(sampled))
  idx <- which(need)
  for (i in idx) {
    k <- keys[i]
    pts <- sampled[[k]]
    if (is.null(pts)) {
      next
    }
    j <- cursor[[k]] + 1L
    if (j > nrow(pts)) {
      next
    }
    cursor[[k]] <- j
    data[[lon_var]][i] <- pts[j, "X"]
    data[[lat_var]][i] <- pts[j, "Y"]
    data$coord_imputed[i] <- TRUE
  }
  data
}

utils::globalVariables(c(
  "k2",
  "year",
  "s_adm0",
  "s_adm1",
  "s_adm2",
  "s_g0",
  "s_g1",
  "s_g2",
  "geo_source",
  "na_adm1",
  "na_adm2",
  "unresolved",
  "corrected",
  "missing_adm2"
))
