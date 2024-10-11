# Tests for mod_local_filter_UI() ----
test_that("mod_local_filter_UI() returns a shiny.tag list", {
  filter_list <- list("soc", "pref_term", "drug_rel_ae", "serious_ae")
  ui <- mod_local_filter_UI("filter_tests", filter_list = filter_list)

  expect_equal(class(ui), "shiny.tag")
  expect_type(ui, "list")
})

test_that(
  "mod_local_filter_UI() returns only UI elements for local filters defined" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    # two filters
    filter_list <- list("soc_var", "pref_term_var")
    ui <- mod_local_filter_UI("filter_tests", filter_list = filter_list)

    expect_equal(length(ui$children[[1]]), length(filter_list))
    expect_named(ui$children[[1]], gsub("_var", "", as.character(filter_list)), ignore.order = TRUE)

    # no filters
    filter_list <- NULL
    ui <- mod_local_filter_UI("filter_tests", filter_list = filter_list)

    expect_equal(ui$attribs$`data-display-if`, "false")
    expect_equal(length(ui$children[[1]]), length(filter_list))
    expect_named(ui$children[[1]], as.character(filter_list), ignore.order = TRUE)
  }
)


# Tests for mod_local_filter_server() ----
data_list <- add_ids(prep_dummy_data(50))

filter_data <- data_list$adae %>%
  dplyr::mutate(subject_id = USUBJID) %>%
  dplyr::select(
    dplyr::all_of(c("set_id", "subject_id", "AESTDTC", "AEENDTC", "AEDECOD", "AESER", "AESOC", "AEREL"))
  )

data <- shiny::reactive({
  prep_data(data_list) %>%
    dplyr::left_join(
      filter_data,
      by = c("set_id" = "set_id", "subject_id" = "subject_id", "start_dt_var" = "AESTDTC", "end_dt_var" = "AEENDTC")
    )
})

changed <- shiny::reactive(1)
filter <- list("soc", "drug_rel_ae")
server <- function(id) {
  mod_local_filter_server(
    module_id = id,
    filter = filter,
    joined_data = data,
    changed = changed
  )
}

test_that(
  "mod_local_filter_server() returns the same data frame as received, only filtered by adverse events" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$AE_filter),
  {
    shiny::testServer(server, {
      session$setInputs(soc = "GASTROINTESTINAL DISORDERS", drug_rel_ae = "NONE")
      outcome <- session$returned()

      expect_equal(colnames(outcome), colnames(joined_data()))
      expect_true("data.frame" %in% class(outcome))
    })
  }
)
