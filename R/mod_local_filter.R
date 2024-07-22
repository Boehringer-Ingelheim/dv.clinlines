#' Create user interface for the local filter shiny module of \pkg{dv.clinlines}
#'
#' @param module_id A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_local_filter_server()}.
#' @param filter_list List of filter names that indicate which local filters to
#'   display.
#'
#' @return A shiny \code{uiOutput} element.
#' @keywords internal
#'
mod_local_filter_UI <- function(module_id, filter_list = NULL) { #nolint
  ns <- shiny::NS(module_id)

  all_filters <- local_filters(ns)

  if (!is.null(filter_list)) {
    cond <- "true"
  } else {
    cond <- "false"
  }

  ui <- shiny::conditionalPanel(
    condition = cond,
    all_filters[which(names(all_filters) %in% filter_list)],
    ns = ns
  )

  return(ui)
}


#' Create server for local filter shiny module of \pkg{dv.clinlines}
#'
#' @param module_id A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_local_filter_UI()}.
#' @param joined_data A metaReactive dataset as provided by \code{prep_data()}.
#' @param changed A reactive whose actualization indicates if the filters should be reset to default values.
#' @inheritParams mod_clinical_timelines_server
#'
#' @return A data frame filtered by local adverse event filters.
#' @keywords internal
#'
mod_local_filter_server <- function(module_id, filter, joined_data, changed) {

  shiny::moduleServer(
    module_id,
    function(input, output, session) {

      ns <- session$ns

      # No need for reactivity as this serves merely as a storage
      cache <- list(
        soc_choices = NULL,
        pt_choices = NULL,
        restored = FALSE,
        pt_input = NULL
      )
      only_filters <- filter$ae_filter[!names(filter$ae_filter) %in% c("data_name", "label")]
      # No need for reactivity as this never changes during a session
      status_filters <- get_filter_status(
        names(local_filters(session$ns)),
        names(only_filters)
      )

      # Choices for preferred terms (pt's) depend on selected soc's
      pt_choices <- shiny::reactive({
        if (status_filters[["pref_term"]]) {
          set_pts(
            status_filters = status_filters,
            soc = input$soc,
            prepped_data = joined_data(),
            filter = filter$ae_filter
          )
        }
      })


      # Set initial choices
      # Explicitely reset filters at dataset change
      shiny::observeEvent(changed(), {

        # Use bookmarking values if available
        if (!is.null(cache$sae_choice)) {
          selected_sae <- cache$sae_choice
          selected_rel <- cache$rel_choice
          cache$sae_choice <<- NULL
          cache$rel_choice <<- NULL
        } else {
          selected_sae <- "all"
          selected_rel <- "all"
        }

        shiny::updateRadioButtons(session, "serious_AE", selected = selected_sae)
        shiny::updateRadioButtons(session, "drug_rel_AE", selected = selected_rel)

        if (status_filters[["soc"]]) {
          choices <- sort(unique(joined_data()[[filter$ae_filter$soc]]))

          shiny::updateSelectizeInput(
            inputId = "soc",
            choices = choices,
            selected = NULL
          )

          # Store choices for bookmarking purposes
          cache$soc_choices <<- choices
        }

        if (status_filters[["pref_term"]]) {

          shiny::updateSelectizeInput(
            inputId = "pref_term",
            choices = pt_choices()
          )
        }
      })


      shiny::observeEvent(input$soc, {

        if (status_filters[["pref_term"]]) {

          # In case we restore from bookmarking we need to update both, choices and
          # selected - otherwise selected pt filters will be overwritten and set to NULL
          # after soc filter is restored

          if (cache$restored) {
            cache$restored <<- FALSE # reset
            shiny::updateSelectizeInput(inputId = "pref_term", choices = pt_choices(), selected = cache$pt_input)
          } else {
            shiny::updateSelectizeInput(inputId = "pref_term", choices = pt_choices())
          }

          # Store choices for bookmarking purposes
          cache$pt_choices <<- pt_choices()
        }

      }, ignoreNULL = FALSE)


      # Choices need to be saved explicitely
      shiny::onBookmark(function(state) {
        state$values$soc_choices <- cache$soc_choices
        state$values$pt_choices <- cache$pt_choices
      })


      # Transfer bookmarked values to reactiveValues for later usage
      shiny::onRestore(function(state) {
        cache$soc_choices <<- state$values$soc_choices
        cache$pt_choices <<- state$values$pt_choices
        cache$pt_input <<- state$input$pref_term
        cache$sae_choice <<- state$input$serious_AE
        cache$rel_choice <<- state$input$drug_rel_AE
      })


      shiny::onRestored(function(state) {
        cache$restored <<- TRUE # remember that restoring happened

        shiny::updateSelectizeInput(
          inputId = ns("soc"),
          choices = cache$soc_choices,
          selected = state$input$soc
        )

        shiny::updateSelectizeInput(
          inputId = ns("pref_term"),
          choices = cache$pt_choices,
          selected = state$input$pref_term
        )
      })


      filtered_data <- shiny::reactive({

        if (status_filters[["serious_AE"]]) {
          shiny::req(input$serious_AE)
        }

        if (status_filters[["drug_rel_AE"]]) {
          shiny::req(input$drug_rel_AE)
        }

        filter_data(
          data = joined_data(),
          status_filters = status_filters,
          input_filters = list(
            serious_AE = input$serious_AE,
            soc = input$soc,
            pref_term = input$pref_term,
            drug_rel_AE = input$drug_rel_AE
          ),
          filter = filter
        )
      })

      return(filtered_data)
    }
  )
}
