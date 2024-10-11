#' UI storage of available local adverse event filters
#'
#' @param ns Namespace (character).
#'
#' @return A list UI elements that represents all available local adverse event filters
#' @keywords internal
#'
local_filters <- function(ns) {
  ui_list <- list(
    serious_ae = shiny::radioButtons(
      ns("serious_ae"),
      label = "Serious Adverse Events?",
      choices = c("Yes" = "Y", "No" = "N", "All" = "all"),
      selected = "all"
    ),
    soc = shiny::selectizeInput(
      ns("soc"),
      label = "System Organ Class",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All SOC's selected")
    ),
    pref_term = shiny::selectizeInput(
      ns("pref_term"),
      label = "Preferred Term",
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "All PT's selected")
    ),
    drug_rel_ae = shiny::radioButtons(
      ns("drug_rel_ae"),
      label = "Drug related Adverse Events?",
      choices = c("Yes" = "Y", "No" = "N", "All" = "all"),
      selected = "all"
    )
  )

  return(ui_list)
}
