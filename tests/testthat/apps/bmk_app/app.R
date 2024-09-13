use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.clinlines)
}

bmk_ui <- function(id) {
  ns <- ifelse(is.character(id), shiny::NS(id), shiny::NS(NULL))

  shiny::fluidPage(
    shiny::bookmarkButton(),
    mod_clinical_timelines_UI(ns("mod"), list("serious_ae_var", "soc_var", "pref_term_var", "drug_rel_ae_var"))
  )
}

bmk_server <- function(input, output, session) {
  # For updating bookmark url automatically
  shiny::observe({
    shiny::reactiveValuesToList(input)
    session$doBookmark()
  })
  shiny::onBookmarked(shiny::updateQueryString)

  data_list <- shiny::reactive(dv.clinlines:::prep_dummy_data())

  mod_clinical_timelines_server(
    "mod",
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
        drug_rel_ae_var = "AEREL"
      )
    ),
    ms = 50
  )
}

shiny::shinyApp(
  ui = bmk_ui,
  server = bmk_server,
  enableBookmarking = "url"
)
