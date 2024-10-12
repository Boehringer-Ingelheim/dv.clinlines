# This is a test template for modules that select a subject ID and send it to dv.papo.

# In order to fit it to your module, it needs three pieces of information:
# 1) An instance of the module you want to test, parameterized to produce valid output and not trigger a `shiny::req`.
mod <- mod_clinical_timelines(
  module_id = "mod",
  basic_info = list(
    subject_level_dataset_name = "adsl", trt_start_var = "TRTSDT", trt_end_var = "TRTEDT", icf_date_var = "RFICDT"
  ),
  mapping = list(
    adsl = list(
      "Treatment Start" = list(
        start_dt_var = "TRTSDT", end_dt_var = NULL, start_dy_var = NULL, end_dy_var = NULL, detail_var = NULL
      ),
      "Treatment End" = list(
        start_dt_var = "TRTEDT", end_dt_var = NULL, start_dy_var = NULL, end_dy_var = NULL, detail_var = NULL
      ),
      "Informed Consent" = list(
        start_dt_var = "RFICDT", end_dt_var = NULL, start_dy_var = NULL, end_dy_var = NULL, detail_var = NULL
      )
    ),
    adae = list(
      "Adverse Events" = list(
        start_dt_var = "AESTDTC", end_dt_var = "AEENDTC", start_dy_var = NULL, end_dy_var = NULL, detail_var = "AEDECOD"
      )
    )
  ),
  drug_admin = list(
    dataset_name = "exp", start_var = "EXSTDTC", end_var = "EXENDTC", detail_var = "EXTRT",
    label = "Drug Administration", dose_var = "EXDOSE", dose_unit_var = "EXDOSU"
  ),
  subjid_var = "USUBJID",
  filter = list(
    ae_filter = list(
      dataset_name = "adae", label = "Adverse Events", soc_var = "AESOC", serious_ae_var = "AESER",
      pref_term_var = "AEDECOD", drug_rel_ae_var = "AERELFLG"
    )
  ),
  ms = 50,
  default_plot_settings = list(x_param = "day", start_day = -5, boxheight_val = 60)
)

# 2) Data matching the previous parameterization.
data <- dv.clinlines:::prep_dummy_data()

# 3) Fully namespaced input ID that, when set to a subject ID value, should make the module send dv.papo a message.
trigger_input_id <- "mod-main_view-debug_select_subject"

source("message_papo-common.R", local = TRUE)
