#' Create a list to be assigned to the \code{basic_info} parameter
#'
#' Helper function to support configuration of a Clinical Timelines module.
#'
#' @param subject_level_dataset_name Character name of the subject level analysis dataset (e.g. "adsl", "dm")
#'   as it is called in the \code{data_list} parameter.
#' @param trt_start_var Character name of the variable that contains treatment start dates.
#'   Must be present in the data frame mentioned in the \code{subject_level_dataset_name} element.
#' @param trt_end_var Character name of the variable that contains treatment end dates.
#'   Must be present in the data frame mentioned in the \code{subject_level_dataset_name} element.
#' @param icf_date_var Character name of the variable that contains informed consent dates.
#'   Must be present in the data frame mentioned in the \code{subject_level_dataset_name} element.
#'
#' @return A list that could directly be used as input for the \code{basic_info} parameter
#'   of \code{mod_clinical_timelines()} and \code{mod_clinical_timelines_server()}.
#' @export
#'
set_basic_info <- function(subject_level_dataset_name, trt_start_var, trt_end_var, icf_date_var) {
  return(
    list(
      subject_level_dataset_name = subject_level_dataset_name,
      trt_start_var = trt_start_var,
      trt_end_var = trt_end_var,
      icf_date_var = icf_date_var
    )
  )
}


#' Create a list to be inserted into a second level list of the \code{mapping} parameter
#'
#' Helper function to support configuration of a Clinical Timelines module.
#'
#' @param start_dt_var Character name of the variable that contains either the event start
#'   dates (for interval events) or merely timepoints (e.g. milestones). The variable name
#'   must be present in the data frame under which the event is listed.
#' @param end_dt_var Character name of the variable that contains the event end dates.
#'   Needs to be provided for interval events, but set to NULL for timepoints. The
#'   variable name must be present in the data frame under which the event is listed.
#' @param start_dy_var  Similar to start_dt_var, but refers to the study relative days
#'   (instead of dates). The variable name must be present in the dataset under which the
#'   event is listed. Can be set to NULL to let the module calculate the study days
#'   according to SDTM standard rules.
#' @param end_dy_var Similar to end_dt_var, but refers to the study relative days
#'   (instead of dates). The variable name must be present in the dataset under which the
#'   event is listed. Can be set to NULL to let the module calculate the study days
#'   according to SDTM standard rules.
#' @param detail_var Character name of the variable that contains further descriptive
#'   information that shall be displayed for the event. Can be set to NULL for no further
#'   information.
#'
#' @return A list that could directly be used as third level list for the \code{mapping}
#'   parameter of \code{mod_clinical_timelines()} and
#'   \code{mod_clinical_timelines_server()}.
#' @export
#'
set_event <- function(start_dt_var,
                      end_dt_var = NULL,
                      start_dy_var = NULL,
                      end_dy_var = NULL,
                      detail_var = NULL) {
  return(
    list(
      start_dt_var = start_dt_var,
      end_dt_var = end_dt_var,
      start_dy_var = start_dy_var,
      end_dy_var = end_dy_var,
      detail_var = detail_var
    )
  )
}


#' Create a list to be assigned to the \code{drug_admin} parameter
#'
#' Helper function to support configuration of a Clinical Timelines module.
#'
#' @param dataset_name Character name of the data frame that holds drug administration data
#'   (e.g. ex domain) as it is called in the \code{data_list} parameter.
#' @param trt_var Character name of the variable that contains the treatment name.
#'   Must be present in the data frame mentioned in the \code{dataset_name} element.
#' @param start_var Character name of the variable that contains the start dates
#'   (e.g. exposure start dates). Must be present in the data frame mentioned in the
#'   \code{dataset_name} element.
#' @param end_var Character name of the variable that contains the end dates
#'   (e.g. exposure start dates). Must be present in the data frame mentioned in the
#'   \code{dataset_name} element.
#' @param detail_var Character name of the variable that contains the treatment
#' information. Must exist in the dataset mentioned in the \code{dataset_name} element.
#' @param label Free-text character label for the drug administration event.
#' @param dose_var Character name of the variable that contains the dosis level
#' information. Must exist in the dataset mentioned in the \code{dataset_name} element.
#' @param dose_unit_var Character name of the variable that contains the dosis unit.
#' Must exist in the dataset mentioned in the \code{dataset_name} element.
#'
#' @return A list that could directly be used as input for the \code{drug_admin} parameter
#'   of \code{mod_clinical_timelines()} and \code{mod_clinical_timelines_server()}.
#' @export
#'
set_drug_admin <- function(dataset_name,
                           trt_var,
                           start_var,
                           end_var,
                           detail_var,
                           label,
                           dose_var,
                           dose_unit_var) {
  return(
    list(
      dataset_name = dataset_name,
      trt_var = trt_var,
      start_var = start_var,
      end_var = end_var,
      detail_var = detail_var,
      label = label,
      dose_var = dose_var,
      dose_unit_var = dose_unit_var
    )
  )
}
