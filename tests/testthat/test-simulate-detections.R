detection_inputs <- function() {
  admin <- simulate_admin_frame(n_provinces = 4, n_districts = 5)
  list(
    afp = simulate_afp_cases(admin, n_positive = 10),
    es = simulate_es_samples(
      admin, n_sites = 10, positive_types = c("WPV 1", "cVDPV 2"),
      positive_type_probs = c(0.7, 0.3)
    )
  )
}

test_that("simulate_detections returns the expected columns and types", {
  inputs <- detection_inputs()
  detections <- simulate_detections(inputs$afp, inputs$es)
  classes <- column_classes(detections)

  expect_s3_class(detections, "tbl_df")
  expect_equal(
    names(detections),
    c(
      "epid", "surveillance_type", "source", "country_iso3code", "adm0",
      "adm1", "adm2", "adm0_guid", "adm1_guid", "adm2_guid", "latitude",
      "longitude", "vtype", "classification_all", "measurement",
      "emergence_group", "nt_changes", "virus_cluster", "virus_is_orphan",
      "vdpv_classification_change_date", "virus_date", "year_onset",
      "month_onset", "notification_date", "report_date", "source_system"
    )
  )
  expect_true(all(classes[grepl("_date$", names(classes))] == "Date"))
  expect_equal(unname(classes[c("year_onset", "month_onset")]),
               c("integer", "integer"))
  expect_equal(unname(classes["virus_is_orphan"]), "logical")
})

test_that("simulate_detections agrees with the positive cases and samples", {
  inputs <- detection_inputs()
  detections <- simulate_detections(inputs$afp, inputs$es)

  positive_cases <- dplyr::filter(
    inputs$afp, grepl("WPV|VDPV", classification_all)
  )
  positive_samples <- dplyr::filter(
    inputs$es, grepl("WPV|VDPV", classification_all)
  )

  expect_equal(nrow(detections),
               nrow(positive_cases) + nrow(positive_samples))
  expect_setequal(
    detections$epid, c(positive_cases$epid, positive_samples$sample_id)
  )

  joined <- detections |>
    dplyr::filter(source == "AFP") |>
    dplyr::inner_join(positive_cases, by = "epid", suffix = c("", "_case"))

  expect_equal(joined$virus_date, joined$paralysis_onset_date)
  expect_equal(joined$adm2_guid, joined$adm2_guid_case)
  expect_equal(joined$classification_all, joined$classification_all_case)

  environmental <- detections |>
    dplyr::filter(source == "Environmental") |>
    dplyr::inner_join(
      positive_samples, by = c(epid = "sample_id"), suffix = c("", "_es")
    )

  expect_equal(environmental$virus_date, environmental$collection_date)
  expect_equal(environmental$latitude, environmental$site_y_coordinate)
})

test_that("simulate_detections uses the pipeline's source labels", {
  inputs <- detection_inputs()
  detections <- simulate_detections(inputs$afp, inputs$es)

  expect_true(all(detections$source %in% c("AFP", "Environmental")))
  expect_true(all(
    (detections$source == "Environmental") ==
      (detections$surveillance_type == "environmental")
  ))
  expect_true(all(detections$source_system == "POLIS"))
  expect_true(all(
    grepl("^WPV|^cVDPV|^VDPV|^aVDPV|^iVDPV", detections$vtype)
  ))
})

test_that("simulate_detections sets the report date by virus type", {
  inputs <- detection_inputs()
  detections <- simulate_detections(inputs$afp, inputs$es)
  wild <- dplyr::filter(detections, grepl("^WPV", classification_all))
  derived <- dplyr::filter(detections, grepl("VDPV", classification_all))

  expect_equal(wild$report_date, wild$notification_date)
  expect_equal(
    derived$report_date,
    dplyr::coalesce(
      derived$vdpv_classification_change_date, derived$notification_date
    )
  )
})

test_that("simulate_detections gives the same table for the same inputs", {
  inputs <- detection_inputs()

  expect_identical(simulate_detections(inputs$afp, inputs$es),
                   simulate_detections(inputs$afp, inputs$es))
})

test_that("simulate_detections rejects tables of the wrong shape", {
  inputs <- detection_inputs()

  expect_error(simulate_detections(data.frame(x = 1), inputs$es),
               "simulate_afp_cases")
  expect_error(simulate_detections(inputs$afp, inputs$afp),
               "simulate_es_samples")
})
