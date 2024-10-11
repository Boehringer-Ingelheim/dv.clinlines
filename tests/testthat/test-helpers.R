# Tests for set_basic_info() ----
test_that("set_basic_info() returns a named list", {
  outcome <- set_basic_info(
    subject_level_dataset_name = "adsl",
    trt_start_var = "start",
    trt_end_var = "end",
    icf_date_var = "consent"
  )
  expected <- list(
    subject_level_dataset_name = "adsl",
    trt_start_var = "start",
    trt_end_var = "end",
    icf_date_var = "consent"
  )
  expect_equal(outcome, expected)
})


# Tests for set_event() ----
test_that("set_event() returns a named list", {
  outcome <- set_event(
    start_dt_var = "start date", end_dt_var = "end date",
    start_dy_var = "start day", end_dy_var = "end day", detail_var = "some details"
  )
  expected <- list(
    start_dt_var = "start date", end_dt_var = "end date",
    start_dy_var = "start day", end_dy_var = "end day", detail_var = "some details"
  )
  expect_equal(outcome, expected)
})


# Tests for set_drug_admin() ----
test_that("set_drug_admin() returns a named list", {
  outcome <- set_drug_admin(
    dataset_name = "exp", start_var = "start date", end_var = "end date",
    label = "some label", detail_var = "some details", dose_var = "dose", dose_unit_var = "unit"
  )
  expected <- list(
    dataset_name = "exp", start_var = "start date", end_var = "end date",
    detail_var = "some details", label = "some label", dose_var = "dose", dose_unit_var = "unit"
  )
  expect_equal(outcome, expected)
})
