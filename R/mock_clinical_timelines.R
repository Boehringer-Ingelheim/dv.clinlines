#' Mock UI for clinlines module
#'
#' @param id A unique identifier.
#'
#' @return Shiny UI for the clinlines module.
#' @keywords internal
mock_clinical_timelines_UI <- function(id = NULL) { # nolint
  # If it is a character then we are using it as a module
  ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))

  ui <- shiny::fluidPage(
    theme = bslib::bs_theme(version = "4"),
    shiny::tags$h1("BI Clinical Timelines", class = "mod-title"),
    mod_clinical_timelines_UI(
      ns("clin_tl"),
      list("serious_ae", "soc", "pref_term", "drug_rel_ae")
    )
  )

  return(ui)
}


#' Mock server for clinlines module
#'
#' Uses adsl, adae, and adcm data of roche dataset.
#'
#' @param input,output,session Internal parameters for shiny.
#' @keywords internal
mock_clinical_timelines_server <- function(input, output, session) {
  data_list <- shiny::reactive(prep_dummy_data())

  mod_clinical_timelines_server(
    "clin_tl",
    data_name = shiny::reactive("dummyData"),
    data_list,
    basic_info = list(
      subject_level_dataset_name = "adsl",
      trt_start_var = "TRTSDT",
      trt_end_var = "TRTEDT",
      icf_date_var = "RFICDT"
    ),
    mapping = list(
      adsl = list(
        "Treatment Start" = list(
          start_dt_var = "TRTSDT",
          end_dt_var = NULL,
          start_dy_var = NULL,
          end_dy_var = NULL,
          detail_var = NULL
        ),
        "Treatment End" = list(
          start_dt_var = "TRTEDT",
          end_dt_var = NULL,
          start_dy_var = NULL,
          end_dy_var = NULL,
          detail_var = NULL
        )
      ),
      adae = list(
        "Adverse Events" = list(
          start_dt_var = "AESTDTC",
          end_dt_var = "AEENDTC",
          start_dy_var = NULL,
          end_dy_var = NULL,
          detail_var = "AEDECOD"
        )
      )
    ),
    drug_admin = list(
      dataset_name = "exp",
      start_var = "EXSTDTC",
      end_var = "EXENDTC",
      detail_var = "EXTRT",
      label = "Drug Administration",
      dose_var = "EXDOSE",
      dose_unit_var = "EXDOSU"
    ),
    subjid_var = "USUBJID",
    filter = list(
      ae_filter = list(
        dataset_name = "adae",
        label = "Adverse Events",
        soc_var = "AESOC",
        serious_ae_var = "AESER",
        pref_term_var = "AEDECOD",
        drug_rel__var = "AEREL"
      )
    ),
    ms = 50
  )
}



#' Run clinlines mock
#'
#' \code{mock_clinical_timelines_app()} runs the \pkg{dv.clinlines} module
#' with dummy data. Local adverse event filters included.
#'
mock_clinical_timelines_app <- function() {
  shiny::shinyApp(
    ui = mock_clinical_timelines_UI,
    server = mock_clinical_timelines_server
  )
}
