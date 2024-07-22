#' Prepare dummy data
#'
#' Modifiy pharmaverseadam's adsl, adae, adcm, and exp dummy data for easy use within
#' \pkg{dv.clinlines}.
#'
#' @param n Number of rows to select from the adsl dataset. The first n rows will be
#' taken. Used to reduce runtime during development.
#'
#' @return A list of three data frames.
#'
#' @importFrom rlang .data
#'
#' @keywords internal
#'

prep_dummy_data <- function(n = 200) {
  adsl_info <- pharmaverseadam::adsl[1:n, ] %>%
    dplyr::mutate(
      TRTSDT = lubridate::ymd_hm(.data$TRTSDT, truncated = 2),
      TRTEDT = lubridate::ymd_hm(.data$TRTEDT, truncated = 2),
      RFICDT = lubridate::ymd_hm(.data$RFSTDTC, truncated = 2)
    )

  adae_info <- pharmaverseadam::adae %>%
    dplyr::mutate(
      AESTDTC = lubridate::ymd_hm(.data$ASTDT, truncated = 2),
      AEENDTC = lubridate::ymd_hm(.data$AENDT, truncated = 2),
      AERELFLG = dplyr::case_when(
        AEREL %in% c("REMOTE", "NONE") ~ "N",
        AEREL %in% c("POSSIBLE", "PROBABLE") ~ "Y",
        TRUE ~ ""
      )
    ) %>%
    dplyr::filter(.data$USUBJID %in% adsl_info$USUBJID)

  adcm_info <- pharmaverseadam::adcm %>%
    dplyr::filter(!is.na(.data$CMSTDTC)) %>%
    dplyr::mutate(
      CMSTDTC = lubridate::ymd_hm(.data$CMSTDTC, truncated = 4),
      CMENDTC = lubridate::ymd_hm(.data$CMENDTC, truncated = 4)
    ) %>%
    dplyr::filter(.data$USUBJID %in% adsl_info$USUBJID)

  exp_info <- pharmaverseadam::adex %>%
    dplyr::mutate(
      EXSTDTC = lubridate::ymd_hm(.data$EXSTDTC, truncated = 2),
      EXENDTC = lubridate::ymd_hm(.data$EXENDTC, truncated = 2),
      subject_id = .data$USUBJID
    ) %>%
    dplyr::filter(.data$USUBJID %in% adsl_info$USUBJID)

  exp_empty <- pharmaverseadam::adex %>%
    dplyr::filter(.data$USUBJID == "+") %>%
    dplyr::mutate(
      EXSTDTC = lubridate::ymd_hm(.data$EXSTDTC),
      EXENDTC = lubridate::ymd_hm(.data$EXENDTC),
      subject_id = .data$USUBJID
    )

  return(list(adsl = adsl_info, adae = adae_info, adcm = adcm_info, exp = exp_info, exp2 = exp_empty))
}



#' Set default values for \code{basic_info}
#'
#' @return A list that points to an ADSL dataset and assigns the following elements:
#' \itemize{
#'   \item{\code{TRTSDT} as treatment start variable}
#'   \item{\code{TRTEDT} as treatment end variable}
#'   \item{\code{RFICDT} as informed consent date variable}
#' }
#'
#' @export
#'
default_basic_info <- function() {
  return(
    set_basic_info(
      data = "adsl",
      trt_start = "TRTSDT",
      trt_end = "TRTEDT",
      icf_date = "RFICDT"
    )
  )
}

#' Set default values for \code{mapping}
#'
#' @return A list that contains lists of events for ADSL, ADAE, and ADCM datasets. Events
#' defined are:
#' \itemize{
#'   \item{For ADSL: Treatment Start (\code{TRTSDT} variable), Treatment End (\code{TRTEDT}
#'     variable), and Informed Consent (\code{RFICDT} variable)}
#'   \item{For ADAE: Adverse Events with \code{AESTDTC} (start_dt_var), \code{AEENDTC}
#'     (end_dt_var), and \code{AEDECOD} (detail_var)}
#'   \item{For ADCM: Concomitant Medications with \code{CMSTDTC} (start_dt_var),
#'     \code{CMENDTC} (end_dt_var), and \code{CMDECOD} (detail_var)}
#' }
#'
#' @export
#'
default_mapping <- function() {
  return(
    list(
      adsl = list(
        "Treatment Start" = set_event("TRTSDT"),
        "Treatment End" = set_event("TRTEDT"),
        "Informed Consent" = set_event("RFICDT")
      ),
      adae = list(
        "Adverse Events" = set_event("AESTDTC", "AEENDTC", detail_var = "AEDECOD")
      ),
      adcm = list(
        "Concomitant Medications" = set_event("CMSTDTC", "CMENDTC", detail_var = "CMDECOD")
      )
    )
  )
}

#' Set default values for \code{drug_admin}
#'
#' @return A list that points to an ex domain dataset and assigns the following elements:
#' \itemize{
#'   \item{\code{EXSTDTC} as start variable}
#'   \item{\code{EXENDTC} as end variable}
#'   \item{\code{EXTRT} as detail variable}
#'   \item{\code{Drug Administration} as label}
#'   \item{\code{EXDOSE} as exposure dosage}
#'   \item{\code{EXDOSU} as dose unit}
#' }
#'
#' @export
#'
default_drug_admin <- function() {
  return(
    list(
      name = "exp",
      start_var = "EXSTDTC",
      end_var = "EXENDTC",
      detail_var = "EXTRT",
      label = "Drug Administration",
      exp_dose = "EXDOSE",
      exp_dose_unit = "EXDOSU"
    )
  )
}
