# Crosswalk-driven name standardisation + column ordering.

testthat::test_that("standardise_names applies crosswalk overrides then janitor", {
  raw <- data.frame(
    PoNS_OnSetDate = 1,
    Admin0Name = "X",
    DateOnset = 2,
    RandomCol = 3,
    check.names = FALSE
  )
  out <- polished::standardise_names(raw)
  testthat::expect_true("pons_on_set_date" %in% names(out)) # crosswalk
  testthat::expect_false("po_ns_on_set_date" %in% names(out))
  testthat::expect_true("adm0" %in% names(out)) # Admin0Name -> adm0
  testthat::expect_true("date_onset" %in% names(out))
  testthat::expect_true("random_col" %in% names(out)) # janitor handles rest
})

testthat::test_that("polis_crosswalk returns the lookup with Snake_Name", {
  cw <- polished::polis_crosswalk()
  testthat::expect_true(all(
    c("Table", "API_Name", "Snake_Name") %in% names(cw)
  ))
  testthat::expect_gt(nrow(cw), 0L)
})

testthat::test_that("admin name/guid columns are named identically", {
  cw <- polished::polis_crosswalk()
  tables <- c("Case", "EnvSample", "Virus", "SubActivity")
  admin_map <- function(tbl) {
    rows <- cw[
      cw$Table == tbl & grepl("^Admin[0-9](Name|GUID|Guid)$", cw$API_Name),
    ]
    # key the canonical Snake_Name by admin level + kind, so the comparison is
    # about naming style, not raw API casing (GUID vs Guid).
    kind <- dplyr::if_else(grepl("Name$", rows$API_Name), "name", "guid")
    level <- sub("^Admin([0-9]).*", "\\1", rows$API_Name)
    stats::setNames(rows$Snake_Name, paste0(level, "_", kind))
  }
  maps <- lapply(tables, admin_map)
  for (m in maps[-1]) {
    testthat::expect_identical(
      m[order(names(m))],
      maps[[1]][order(names(maps[[1]]))]
    )
  }
  # and the canonical form is the short adm<level> / adm<level>_guid
  testthat::expect_setequal(
    unname(maps[[1]]),
    c("adm0", "adm1", "adm2", "adm0_guid", "adm1_guid", "adm2_guid")
  )
})

testthat::test_that("order_columns orders id -> location -> time -> other", {
  cfg <- polished::polis_config()
  d <- tibble::tibble(other = 1, date_onset = 1, adm0 = 1, id = 1)
  testthat::expect_equal(
    names(polished::order_columns(d, cfg$column_roles)),
    c("id", "adm0", "date_onset", "other")
  )
})

testthat::test_that("polis_dictionary returns raw vs clean schema views", {
  raw <- polished::polis_dictionary("raw")
  clean <- polished::polis_dictionary("clean")

  # both are the same tidy 3-column dictionary: stream, column, label
  testthat::expect_named(raw, c("data_type", "column_name", "label"))
  testthat::expect_named(clean, c("data_type", "column_name", "label"))
  # raw column_name is the API field (no blanks); clean is the canonical name
  testthat::expect_true(all(!is.na(raw$column_name) & nzchar(raw$column_name)))

  # clean carries derived + indicator columns the raw schema cannot
  testthat::expect_true("measurement" %in% clean$column_name)
  testthat::expect_true("npafp_rate" %in% clean$column_name)
  # clean excludes the raw age sources the cleaner drops and the raw poliovirus
  # fields outside the curated clean_virus() subset
  testthat::expect_false(
    any(
      c("calculated_age_in_month", "person_age_in_months") %in%
        clean$column_name
    )
  )
  testthat::expect_false(
    "pons_passage" %in% clean$column_name[clean$data_type == "Virus"]
  )

  # defaults to clean; filters by table; rejects an unknown type
  testthat::expect_identical(polished::polis_dictionary(), clean)
  testthat::expect_true(all(
    polished::polis_dictionary("raw", table = "Case")$data_type == "Case"
  ))
  testthat::expect_error(polished::polis_dictionary("nope"))
})
