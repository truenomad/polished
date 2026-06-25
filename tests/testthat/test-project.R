testthat::test_that("init_polis_project scaffolds, re-opens, and guards; path/clear helpers work", {
  root <- file.path(withr::local_tempdir(), "proj")
  proj <- polished::init_polis_project(root, quiet = TRUE)

  testthat::expect_s3_class(proj, "polis_project")
  testthat::expect_true(all(dir.exists(unlist(proj))))
  testthat::expect_setequal(
    basename(unlist(proj[c(
      "raw",
      "processed",
      "validation",
      "cache",
      "logs"
    )])),
    c("raw", "processed", "validation", "cache", "logs")
  )
  # gitignore written, ignoring the bulky/regenerable zones
  gi <- readLines(file.path(root, ".gitignore"))
  testthat::expect_true(all(c("raw/", "cache/", "logs/") %in% gi))

  # idempotent re-open: no error, same paths, existing files untouched
  writeLines("keep", file.path(proj$raw, "keep.txt"))
  proj2 <- polished::init_polis_project(root, quiet = TRUE)
  testthat::expect_identical(proj2, proj)
  testthat::expect_true(file.exists(file.path(proj$raw, "keep.txt")))

  # project_path builds under a zone; print returns invisibly
  testthat::expect_identical(
    polished::project_path(proj, "processed", "sia"),
    file.path(proj$processed, "sia")
  )
  testthat::expect_identical(polished::project_path(proj), proj$root)
  testthat::expect_identical(testthat::expect_invisible(print(proj)), proj)

  # clear_cache only ever touches cache/ (verbose path exercises the message)
  writeLines("x", file.path(proj$cache, "c.txt"))
  writeLines("y", file.path(proj$processed, "p.txt"))
  testthat::expect_message(polished::clear_cache(proj), "Cleared")
  testthat::expect_equal(length(list.files(proj$cache)), 0L)
  testthat::expect_true(file.exists(file.path(proj$processed, "p.txt")))

  # a relative root is made absolute against the working directory (verbose)
  withr::with_dir(withr::local_tempdir(), {
    rel <- polished::init_polis_project("rel_proj")
    testthat::expect_true(dir.exists(rel$processed))
  })

  # guards
  testthat::expect_error(polished::init_polis_project(""), "non-empty")
  testthat::expect_error(polished::project_path(list()), "polis_project")
  testthat::expect_error(polished::project_path(proj, "nope"), "zone")
  testthat::expect_error(polished::clear_cache(list()), "polis_project")
})
