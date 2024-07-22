# Tests for prep_dummy_data() ----
n <- 50
data <- prep_dummy_data(n)
test_that("prep_dummy_data() returns a named list of data frames", {
  expect_type(data, "list")
  purrr::walk(data, ~ expect_true("data.frame" %in% class(.x)))
  expect_named(data)
})

test_that("prep_dummy_data() restricts number of subjects according to n parameter", {
  expect_equal(nrow(data$adsl), n)
  purrr::walk(2:length(data), ~ expect_true(all(data[[.x]]$USUBJID %in% data$adsl$USUBJID)))
})

test_that("prep_dummy_data() adds a flag for AEREL", {
  expect_true(all(data$adae$AERELFLG %in% c("Y", "N")))
})


# Tests for default_basic_info() ----
test_that("default_basic_info() returns a named list with default settings ", {
  outcome <- default_basic_info()
  expected <- list(data = "adsl", trt_start = "TRTSDT", trt_end = "TRTEDT", icf_date = "RFICDT")

  expect_equal(outcome, expected)
})


# Tests for default_mapping() ----
test_that("default_mapping() returns a named list with default settings", {
  outcome <- default_mapping()
  expected <- list(
    adsl = list(
      "Treatment Start" = set_event("TRTSDT"),
      "Treatment End" = set_event("TRTEDT"),
      "Informed Consent" = set_event("RFICDT")
    ),
    adae = list(
      "Adverse Events" = set_event("AESTDTC", "AEENDTC", detail_var = "AEDECOD")
    ),
    adcm = list(
      "Concomitant Medications" = set_event("CMSTDTC", "CMENDTC", detail_var = "CMDECOD")
    )
  )

  expect_equal(outcome, expected)
})


# Tests for default_drug_admin() ----
test_that("default_drug_admin() returns a named list with default settings", {
  outcome <- default_drug_admin()
  expected <- list(
    name = "exp",
    start_var = "EXSTDTC",
    end_var = "EXENDTC",
    detail_var = "EXTRT",
    label = "Drug Administration",
    exp_dose = "EXDOSE",
    exp_dose_unit = "EXDOSU"
  )

  expect_equal(outcome, expected)
})
