use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.clinlines)
}

# Specify datasets
dummyData1 <- dv.clinlines:::prep_dummy_data(20)  #nolint
dummyData2 <- dv.clinlines:::prep_dummy_data(200) #nolint

# Define module
clinlines <- dv.clinlines::mod_clinical_timelines(
  module_id = "mod",
  basic_info = list(
    data = "adsl",
    trt_start = "TRTSDT",
    trt_end = "TRTEDT",
    icf_date = "RFICDT"
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
    name = "exp",
    start_var = "EXSTDTC",
    end_var = "EXENDTC",
    detail_var = "EXTRT",
    label = "Drug Administration",
    exp_dose = "EXDOSE",
    exp_dose_unit = "EXDOSU"
  ),
  subjid_var = "USUBJID",
  filter = list(
    ae_filter = list(
      data_name = "adae",
      label = "Adverse Events",
      soc = "AESOC",
      serious_AE = "AESER",
      pref_term = "AEDECOD",
      drug_rel_AE = "AERELFLG"
    )
  ),
  default_plot_settings = list(x_param = "date", start_day = -5, boxheight_val = 50),
  ms = 50
)

# Run app with Module Manager
dv.manager::run_app(
  data = list("dummyData1" = dummyData1, "dummyData2" = dummyData2),
  module_list = list("Clinical Timelines" = clinlines),
  filter_data = "adsl"
)
