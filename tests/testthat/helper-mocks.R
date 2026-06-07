# Test helpers for the POLIS download engine. Synthetic data only.

# Build a fake POLIS OData JSON body for a list of records.
fake_polis_body <- function(records = list(), count = NULL) {
  out <- list(value = records)
  if (!is.null(count)) {
    out[["@odata.count"]] <- count
  }
  out
}

# Make a fake "case row" with all the columns the workers depend on.
fake_case_row <- function(id, date = "2024-06-15") {
  list(
    Id = id,
    EPID = paste0("TEST-", id),
    CaseDate = date,
    LastUpdateDate = date,
    WHORegion = "AFRO",
    CountryISO3Code = "NGA"
  )
}

# Wrap a function so it returns a saved data.frame regardless of input.
make_stub_body_response <- function(records, count = length(records)) {
  function(url, polis_api_key, ...) {
    fake_polis_body(records = records, count = count)
  }
}

# Make a temporary polis_folder with a few preexisting part files
# (helper for resume-from-disk tests).
seed_parts <- function(polis_folder, table_name, year, df, ext = "rds") {
  parts_dir <- file.path(polis_folder, "data", ".parts", table_name)
  dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)
  part_file <- file.path(parts_dir, sprintf("year_%d.%s", year, ext))
  saveRDS(df, part_file)
  part_file
}
