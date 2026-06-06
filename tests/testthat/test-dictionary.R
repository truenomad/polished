# dictionary engine: rename, select, order

test_that("every shipped dictionary has the required columns", {
  for (ds in c("afp", "es", "virus", "sia")) {
    dict <- polis_dictionary(ds)
    expect_named(dict, c("raw", "clean", "note"), info = ds)
    expect_true(all(!is.na(dict$clean)), info = ds)
  }
})

test_that("rename_canonical maps raw to clean and leaves others alone", {
  dict <- polis_dictionary("es")
  d <- tibble::tibble(admin0_name = "NIGERIA", junk = 1)
  out <- rename_canonical(d, dict)
  expect_true("adm0" %in% names(out))
  expect_true("junk" %in% names(out))
})

test_that("select_canonical drops non-canonical columns by default", {
  dict <- polis_dictionary("es")
  d <- tibble::tibble(adm0 = "X", junk = 1)
  expect_named(select_canonical(d, dict), "adm0")
  expect_named(
    select_canonical(d, dict, keep_raw_cols = TRUE),
    c("adm0", "junk")
  )
})

test_that("order_columns orders id -> location -> time -> other", {
  cfg <- polis_config()
  d <- tibble::tibble(
    other = 1,
    date_onset = 1,
    adm0 = 1,
    id = 1
  )
  expect_equal(
    names(order_columns(d, cfg$column_roles)),
    c("id", "adm0", "date_onset", "other")
  )
})
