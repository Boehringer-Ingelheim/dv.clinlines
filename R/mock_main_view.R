#' Mock UI for main view module of clinlines
#'
#' @return Shiny UI for the main view module.
#' @keywords internal
mock_main_view_UI <- function() { #nolint

  ui_list <- mod_main_view_UI("main_view") #nolint

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(ui_list$sidebar, width = 2),
      shiny::mainPanel(ui_list$main_panel, width = 10)
    )
  )
}


#' Mock server for main view module of clinlines
#'
#' Uses adsl, adae, and adcm datasets of roche data.
#'
#' @param input,output,session Internal parameters for shiny.
#' @keywords internal
mock_main_view_server <- function(input, output, session) {
  data <- shiny::reactive(prep_data(add_ids(prep_dummy_data()), mapping = default_mapping()[-3]))
  changed <- shiny::reactive(1)
  colors <- shiny::reactive(color_lookup(unique(data()$group)))
  mod_main_view_server("main_view", initial_data = data, changed = changed, colors_groups = colors)
}


#' Run main view mock app
#'
#' \code{mock_main_view_app()} runs the main view module with dummy data.
#' @keywords internal
#'
mock_main_view_app <- function() {
  shiny::shinyApp(ui = mock_main_view_UI(), server = mock_main_view_server)
}
