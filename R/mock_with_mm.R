#' Run an example for Clinical Timelines integrated in the module manager
#'
#' Launches an example app that shows a Clinical Timelines module integrated in the
#' module manager surface. Displays data from the \pkg{pharmaverseadam} package.
#'
#' @export
mock_clinlines_with_mm_app <- function() {
  # Specifiy dataset list for module manager
  dataset_list <- list(
    dummyData1 = prep_dummy_data(20),
    dummyData2 = prep_dummy_data(200)
  )


  # Define module list for module manager
  module_list <- list(
    "Clinical Timelines" = mod_clinical_timelines(
      module_id = "mod1",
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
          ),
          "Informed Consent" = list(
            start_dt_var = "RFICDT",
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
        trt_var = "EXTRT",
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
          drug_rel_ae_var = "AERELFLG"
        )
      ),
      ms = 50,
      default_plot_settings = list(
        x_param = "day",
        start_day = -5,
        boxheight_val = 60
      )
      # nolint start: commented_code_linter
      # color_palette = c(
      #   "Treatment End" = "blue",
      #   "Treatment Start" = "red",
      #   "Drug Administration: PLACEBO" = "yellow",
      #   "Drug Administration: XANOMELINE" = "green",
      #   "Informed Consent" = "purple",
      #   "Adverse Events" = "orange"
      # )
      # nolint end
    )
  )

  # Run app
  dv.manager::run_app(
    data = dataset_list,
    module_list = module_list,
    filter_data = "adsl"
  )
}
