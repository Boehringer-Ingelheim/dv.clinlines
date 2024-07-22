use_load_all <- isTRUE(as.logical(Sys.getenv("TEST_LOCAL"))) | isTRUE(as.logical(Sys.getenv("TEST_PUSH")))
if (use_load_all) {
  devtools::load_all("../../../../", quiet = TRUE)
} else {
  library(dv.clinlines)
}

# Create large datasets
n <- 5
adsl <- data.frame()
adae <- data.frame()
for (i in 1:n) {
  manipulated_adsl <- pharmaverseadam::adsl %>%
    dplyr::mutate(USUBJID = paste0(i, USUBJID))
  manipulated_adae <- pharmaverseadam::adae %>%
    dplyr::mutate(USUBJID = paste0(i, USUBJID))
  adsl <- dplyr::bind_rows(adsl, manipulated_adsl)
  adae <- dplyr::bind_rows(adae, manipulated_adae)
}

# Prepare data
adsl <- adsl %>%
  dplyr::mutate(
    TRTSDT = lubridate::ymd_hm(.data$TRTSDT, truncated = 2),
    TRTEDT = lubridate::ymd_hm(.data$TRTEDT, truncated = 2),
    RFICDT = lubridate::ymd_hm(.data$RFSTDTC, truncated = 2)
  )

adae <- adae %>%
  dplyr::mutate(
    AESTDTC = lubridate::ymd_hm(.data$ASTDT, truncated = 2),
    AEENDTC = lubridate::ymd_hm(.data$AENDT, truncated = 2),
    AERELFLG = dplyr::case_when(
      AEREL %in% c("REMOTE", "NONE") ~ "N",
      AEREL %in% c("POSSIBLE", "PROBABLE") ~ "Y",
      TRUE ~ ""
    )
  ) %>%
  dplyr::filter(.data$USUBJID %in% adsl$USUBJID)

exp <- pharmaverseadam::adex %>%
  dplyr::mutate(
    EXSTDTC = lubridate::ymd_hm(.data$EXSTDTC, truncated = 2),
    EXENDTC = lubridate::ymd_hm(.data$EXENDTC, truncated = 2),
    subject_id = .data$USUBJID
  ) %>%
  dplyr::filter(.data$USUBJID %in% adsl$USUBJID)


data_list <- list(adsl = adsl, adae = adae, exp = exp)


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
        start_dt_var = "TRTSDT"
      ),
      "Treatment End" = list(
        start_dt_var = "TRTEDT"
      )
    ),
    adae = list(
      "Adverse Events" = list(
        start_dt_var = "AESTDTC",
        end_dt_var = "AEENDTC",
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
  subjid_var = "USUBJID"
)

# Run app with Module Manager
dv.manager::run_app(
  data = list("my_data" = data_list),
  module_list = list("Clinical Timelines" = clinlines),
  filter_data = "adsl"
)
