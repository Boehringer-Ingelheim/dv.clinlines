#' Get status of each available filter
#'
#' Checks for each available filter if it was chosen to be displayed in the
#' user interface of \pkg{dv.clinlines}.
#'
#' @param all_filters Character vector of all available filter names.
#' @param chosen_filters List of filters to be displayed.
#'
#' @return Named vector that contains for each available filter if it was
#' chosen by the user to be displayed (TRUE) or not (FALSE).
#' @keywords internal
get_filter_status <- function(all_filters, chosen_filters) {
  all_filters <- paste0(all_filters, "_var")
  check_filters(filter_list = chosen_filters, filter_names = all_filters)
  status <- all_filters %in% chosen_filters
  names(status) <- gsub("_var", "", all_filters)

  return(status)
}


#' Set choices for Preferred Term filter
#'
#' @param status_filters Named boolean vector as returned
#'   by \code{get_filter_status()}.
#' @param soc Character vector of System Organ Classes.
#' @param prepped_data Data frame as returned by \code{prep_data()}.
#' @inheritParams mod_local_filter_server
#'
#' @return Character vector of PT's.
#' @keywords internal
set_pts <- function(status_filters, soc, prepped_data, filter) {
  if (status_filters[["soc"]] && !is.null(soc)) {
    pt_choices <- prepped_data %>%
      dplyr::filter(.data[[filter$soc_var]] %in% soc) %>%
      dplyr::select(tidyselect::all_of(filter$pref_term_var)) %>%
      dplyr::distinct() %>%
      dplyr::arrange()
  } else {
    pt_choices <- sort(unique(prepped_data[[filter$pref_term_var]]))
  }
}


#' Filter data according to local filter settings
#'
#' @param data A data frame as provided by \code{prep_data}.
#' @param input_filters A list of inputs from local filters.
#' @inheritParams set_pts
#'
#' @return The received data frame filtered according to local filter settings.
#'
#' @keywords internal
filter_data <- function(data, status_filters, input_filters, filter) {
  # .data$set != info_ae$name ensures to also display non-AE data
  if (status_filters[["serious_ae"]]) {
    if (input_filters$serious_ae != "all") {
      data <- data %>% dplyr::filter(
        .data[[filter$ae_filter$serious_ae_var]] == input_filters$serious |
          .data$set != filter$ae_filter$dataset_name
      )
    }
  }

  if (status_filters[["soc"]]) {
    if (!is.null(input_filters$soc)) {
      data <- data %>% dplyr::filter(
        .data[[filter$ae_filter$soc_var]] %in% input_filters$soc |
          .data$set != filter$ae_filter$dataset_name
      )
    }
  }

  if (status_filters[["pref_term"]]) {
    if (!is.null(input_filters$pref_term)) {
      data <- data %>% dplyr::filter(
        .data[[filter$ae_filter$pref_term_var]] %in% input_filters$pref_term |
          .data$set != filter$ae_filter$dataset_name
      )
    }
  }

  if (status_filters[["drug_rel_ae"]]) {
    if (input_filters$drug_rel_ae != "all") {
      data <- data %>% dplyr::filter(
        .data[[filter$ae_filter$drug_rel_ae_var]] == input_filters$drug_rel_ae |
          .data$set != filter$ae_filter$dataset_name
      )
    }
  }

  return(data)
}

#' Check if the user specified adverse event filters are available
#'
#' \code{check_filters} produces an error if the filter names can't be found
#' among the available local adverse event filters.
#'
#' @param filter_list A string vector of filter names specified by the user.
#' @param filter_names A string vector of available filters.
#' @keywords internal
check_filters <- function(filter_list, filter_names) {
  if (!all(filter_list %in% filter_names)) {
    stop(
      paste0(
        "Specified filters must be one of the following: ",
        paste0(filter_names, collapse = ", ")
      )
    )
  }
}
