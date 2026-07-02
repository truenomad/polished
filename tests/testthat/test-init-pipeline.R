test_that("init_polis_pipeline scaffolds the full tree, .Rprofile and scripts", {
  root <- withr::local_tempdir()
  proj <- init_polis_pipeline(
    root,
    regions = c("EMRO", "AFRO"),
    start_year = 2015,
    quiet = TRUE
  )

  # domain dirs (incl. the deep worldpop age-band + polis processed sub-dirs)
  expect_true(dir.exists(file.path(root, "01_data/1c_polis/processed/data")))
  expect_true(dir.exists(file.path(root, "01_data/1c_polis/processed/cache")))
  expect_true(dir.exists(file.path(
    root,
    "01_data/1b_population/worldpop/raw/u15"
  )))
  expect_true(dir.exists(file.path(
    root,
    "01_data/1b_population/polis_pop/raw"
  )))
  expect_true(dir.exists(file.path(root, "01_data/1d_vaccination/processed")))
  expect_true(dir.exists(file.path(root, "03_outputs/plots")))

  # generated files
  for (f in c(
    ".Rprofile",
    ".gitignore",
    ".here",
    "02_scripts/2a_download_data.R",
    "02_scripts/2b_process_data.R"
  )) {
    expect_true(file.exists(file.path(root, f)), info = f)
  }

  # all generated R parses, and no placeholder leaked through
  for (f in c(
    ".Rprofile",
    "02_scripts/2a_download_data.R",
    "02_scripts/2b_process_data.R"
  )) {
    expect_silent(parse(file.path(root, f)))
    expect_false(any(grepl("{{", readLines(file.path(root, f)), fixed = TRUE)))
  }
  rp <- readLines(file.path(root, ".Rprofile"))
  expect_true(any(grepl('regions = c("EMRO", "AFRO")', rp, fixed = TRUE)))
})

test_that("the generated .Rprofile builds a valid cfg", {
  # sourcing the .Rprofile calls polis_config(), which registers the session
  # active config -- snapshot + restore it so this test doesn't leak into others
  prev_active <- .polis_config_store$active
  withr::defer(.polis_config_store$active <- prev_active)

  root <- withr::local_tempdir()
  init_polis_pipeline(
    root,
    regions = "EMRO",
    pop_source = "worldpop",
    quiet = TRUE
  )
  e <- new.env()
  withr::with_dir(root, sys.source(file.path(root, ".Rprofile"), envir = e))
  expect_s3_class(e$cfg, "polis_config")
  expect_equal(e$cfg$regions, "EMRO")
  expect_equal(e$cfg$pop_source, "worldpop")
  # cfg points at paths the scaffold actually created
  expect_true(dir.exists(e$cfg$output_dir))
})

test_that("domains controls which 01_data domains are created", {
  root <- withr::local_tempdir()
  init_polis_pipeline(root, domains = c("shapefiles", "polis"), quiet = TRUE)
  expect_true(dir.exists(file.path(root, "01_data/1a_shapefiles/raw")))
  expect_true(dir.exists(file.path(root, "01_data/1c_polis/raw")))
  expect_false(dir.exists(file.path(root, "01_data/1b_population")))
  expect_false(dir.exists(file.path(root, "01_data/1d_vaccination")))
})

test_that("init_polis_pipeline does not clobber existing files without overwrite", {
  root <- withr::local_tempdir()
  init_polis_pipeline(root, quiet = TRUE)
  writeLines("# edited by hand", file.path(root, ".Rprofile"))

  # default re-run keeps the edited file
  init_polis_pipeline(root, quiet = TRUE)
  expect_identical(readLines(file.path(root, ".Rprofile")), "# edited by hand")

  # overwrite = TRUE regenerates it
  init_polis_pipeline(root, overwrite = TRUE, quiet = TRUE)
  expect_gt(length(readLines(file.path(root, ".Rprofile"))), 1L)
})

test_that("renv = FALSE keeps the guarded autoloader (renv-ready, not required)", {
  root <- withr::local_tempdir()
  init_polis_pipeline(root, quiet = TRUE)
  rp <- readLines(file.path(root, ".Rprofile"))
  expect_true(any(grepl(
    'if (file.exists("renv/activate.R")) source("renv/activate.R")',
    rp,
    fixed = TRUE
  )))
  expect_false(dir.exists(file.path(root, "renv")))
})

test_that("renv = TRUE sets up renv with a single, clean autoloader", {
  skip_if_not_installed("renv")
  root <- withr::local_tempdir()
  # quiet = FALSE so the renv "next steps" success message is exercised too
  suppressMessages(init_polis_pipeline(root, renv = TRUE, quiet = FALSE))

  expect_true(file.exists(file.path(root, "renv/activate.R")))
  expect_true(file.exists(file.path(root, "renv.lock")))

  rp <- readLines(file.path(root, ".Rprofile"))
  # exactly one renv autoloader (no duplicate), guarded line gone, manifest kept
  expect_equal(sum(grepl('source("renv/activate.R")', rp, fixed = TRUE)), 1L)
  expect_false(any(grepl('file.exists("renv/activate.R")', rp, fixed = TRUE)))
  expect_true(any(grepl("polis_config", rp, fixed = TRUE)))
  expect_silent(parse(file.path(root, ".Rprofile")))
})

test_that("renv = TRUE without renv installed warns and stays renv-ready", {
  # stub the availability check so this branch is exercised whether or not renv
  # is actually installed in the test library
  local_mocked_bindings(.polis_renv_available = function() FALSE)

  root <- withr::local_tempdir()
  # quiet = FALSE also exercises the success message on the do_renv = FALSE path
  expect_message(
    init_polis_pipeline(root, renv = TRUE, quiet = FALSE),
    "renv"
  )

  # renv setup was skipped: no renv/ scaffold, no lockfile ...
  expect_false(dir.exists(file.path(root, "renv")))
  expect_false(file.exists(file.path(root, "renv.lock")))
  # ... and the .Rprofile keeps the guarded (renv-optional) autoloader
  rp <- readLines(file.path(root, ".Rprofile"))
  expect_true(any(grepl(
    'if (file.exists("renv/activate.R")) source("renv/activate.R")',
    rp,
    fixed = TRUE
  )))
})

test_that("init_polis_pipeline rejects a bad root and bad pop_source", {
  expect_error(init_polis_pipeline(character(0), quiet = TRUE), "root")
  expect_error(
    init_polis_pipeline(
      withr::local_tempdir(),
      pop_source = "nope",
      quiet = TRUE
    )
  )
})
