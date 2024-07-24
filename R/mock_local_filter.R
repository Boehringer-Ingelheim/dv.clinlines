#' Mock UI for the local filter module
#'
#' @param request Required parameter for bookmarking.
#'
#' @return Shiny UI for the local filters module.
#' @keywords internal
mock_local_filter_UI <- function(request) { # nolint
  ui <- shiny::fluidPage(
    shiny::bookmarkButton(),
    mod_local_filter_UI(
      "filter",
      list("soc", "pref_term", "drug_rel_AE", "serious_AE")
    ),
    shiny::tableOutput("table")
  )

  return(ui)
}


#' Mock server for the local filter module
#'
#' Uses adsl, adae, and adcm datasets of roche data.
#'
#' @param input,output,session Internal parameters for shiny.
#' @keywords internal
mock_local_filter_server <- function(input, output, session) {
  initial_data <- shiny::reactive({
    data_list <- prep_dummy_data()
    data_list$adae <- data_list$adae %>%
      dplyr::mutate(
        # Note: conversion below is only for mock purpose and changes the data
        AEREL = dplyr::if_else(AEREL == "NONE", "N", "Y")
      )
    data_list <- purrr::set_names(names(data_list)) %>%
      purrr::map(function(x) {
        dplyr::mutate(
          data_list[[x]],
          set_id = paste0(seq_len(nrow(data_list[[x]])), "_", x)
        )
      })

    data <- prep_data(data_list)

    if (TRUE) {
      filter_data <- data_list$adae %>%
        dplyr::mutate(
          subject_id = .data$USUBJID
        ) %>%
        dplyr::select(
          .data$set_id, .data$subject_id, .data$AESTDTC, .data$AEENDTC,
          .data$AEDECOD, .data$AESER, .data$AESOC, .data$AEREL
        )

      data <- dplyr::left_join(
        data, filter_data,
        by = c(
          "set_id" = "set_id",
          "subject_id" = "subject_id",
          "start_dt_var" = "AESTDTC",
          "end_dt_var" = "AEENDTC"
        )
      )
    }

    data
  })

  filtered_data <- mod_local_filter_server(
    "filter",
    filter = list(ae_filter = list(
      data_name = "adae", soc = "AESOC", pref_term = "AEDECOD", drug_rel_AE = "AEREL", serious_AE = "AESER"
    )),
    joined_data = initial_data,
    changed = shiny::reactive(1)
  )

  output$table <- shiny::renderTable({
    filtered_data() %>% dplyr::filter(.data$set == "adae")
  })
}


#' Run local filters mock
#'
#' \code{mock_local_filter_app()} runs the local filter module with
#' dummy data.
#' @keywords internal
#'
mock_local_filter_app <- function() {
  shiny::shinyApp(
    ui = mock_local_filter_UI,
    server = mock_local_filter_server,
    enableBookmarking = "url"
  )
}
