# Tests for local_filters() ----
ns <- shiny::NS("test")
outcome <- local_filters(ns)

test_that("local_filters() returns a list of Shiny UI elements", {
  expect_type(outcome, "list")

  exp_names <- c("serious_AE", "soc", "pref_term", "drug_rel_AE")
  expect_named(outcome, exp_names)
})
