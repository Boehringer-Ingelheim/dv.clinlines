#' Prepare a data frame to be used to plot the clinical timelines
#'
#' \code{prep_data()} combines subject level and other user specified event datasets into
#' one data frame that can be used to plot the clinical timelines without local filtering.
#'
#' @param data_list A named list of data frames that provides a subject level dataset
#' and further datasets with event specific data. Usually obtained from module manager.
#' @inheritParams mod_clinical_timelines_server
#' @inherit mod_clinical_timelines details
#'
#' @importFrom rlang :=
#'
#' @return A data frame that can directly be used to plot the clinical
#' timelines.
#' @keywords internal
prep_data <- function(data_list,
                      basic_info = default_basic_info(),
                      mapping = default_mapping(),
                      drug_admin = default_drug_admin(),
                      subjid_var = "USUBJID",
                      filter = NULL) {

  if (is.null(drug_admin)) {
    empty_drug_admin <- data.frame(
      subjects = character(),
      treatment = character(),
      start = lubridate::ymd_hm(),
      end = lubridate::ymd_hm(),
      details = character(),
      dose = character(),
      unit = character()
    ) %>%
      dplyr::rename(!!subjid_var := "subjects")

    data_list <- append(data_list, list(no_da = empty_drug_admin))

    drug_admin <- list(
      dataset_name = "no_da",
      trt_var = "treatment",
      start_var = "start",
      end_var = "end",
      detail_var = "details",
      label = "",
      dose_var = "dose",
      dose_unit_var = "unit"
    )
  }

  # Check if datasets names of mapping exist in data_list
  if (!all(names(mapping) %in% names(data_list))) {
    rlang::abort(
      message = c(
        "Clinical Timelines: You tried to define events for a dataset
            that do not exist in your data list.",
        x = paste0(
          "You have defined '",
          paste(names(mapping), collapse = ", "),
          "' inside your mapping list."
        ),
        i = paste0(
          "Your data list contains ",
          paste(names(data_list), collapse = ", "),
          "."
        ),
        i = "Have you spelled dataset names correctly in your mapping list?"
      )
    )
  }

  data_list <- add_ids(data_list)

  # Extract relevant data of a subject level dataset (e.g. adsl, dm, ...)
  basics_list <- set_basics(
    data_list,
    basic_info = basic_info,
    subjid_var = subjid_var
  )

  df_events <- set_events_intern(
    data_list,
    mapping = mapping,
    subjid_var = subjid_var
  )

  df_exp_intervals <- set_exp_intervals(
    data_list,
    mapping = drug_admin,
    subjid_var = subjid_var
  )

  one_data <- combine_data(
    list(df_events, df_exp_intervals),
    basics_list$data,
    basics_list$trt_start,
    basics_list$trt_end,
    icf_date = basics_list$icf_date,
    subjid_var = "subject_id"
  )

  initial_data <- one_data %>%
    complete_events(basics_list$trt_start, basics_list$trt_end)

  # Add additional information for local ae filters
  if (!is.null(filter)) {
    filter_set <- set_filter_dataset(
      filter = filter,
      data_list = data_list,
      mapping = mapping,
      subjid_var = subjid_var
    )

    initial_data <- dplyr::left_join(
      initial_data, filter_set,
      by = c(
        "set_id" = "set_id",
        "subject_id" = "subject_id" # fixed on purpose!
      )
    )
  }

  # Add minimum date and minimum day information to truncate x-axis correctly also after dataset switch
  date_vars <- c(
    "start_exp", "end_exp", "resetted", "earliest",
    unlist(mapping)[grepl("dt_var$", names(unlist(mapping)))]
  )
  date_vars <- date_vars[date_vars %in% names(initial_data)]
  day_vars <- c("start_dy_var", "end_dy_var", "start_exp_day", "end_exp_day")
  day_vars <- day_vars[day_vars %in% names(initial_data)]
  date_min <- lapply(initial_data[date_vars], function(x) {
    if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  })
  initial_data$date_min <- do.call(min, date_min[!is.na(date_min)])
  day_min <- lapply(initial_data[day_vars], function(x) {
    if (all(is.na(x))) NA else min(x, na.rm = TRUE)
  })
  initial_data$day_min <- do.call(min, day_min[!is.na(day_min)])

  return(initial_data)
}


#' Add unique identifiers to each dataset in a list of datasets
#'
#' @param data_list A list of named datasets.
#'
#' @keywords internal
#' @return A list of the same datasets but with an additional column set_id that serves
#'   as row ID's.
add_ids <- function(data_list) {
  data_list <- names(data_list) %>%
    purrr::set_names() %>%
    purrr::map(function(x) {
      dplyr::mutate(
        data_list[[x]],
        set_id = paste0(seq_len(nrow(data_list[[x]])), "_", x)
      )
    })

  return(data_list)
}


#' Set information about subject level analysis data (e.g. adsl, dm, ...)
#'
#' @inheritParams prep_data
#'
#' @return A list containing the following elements:
#' \itemize{
#' \item{\code{data}: A data frame that includes subject level data as
#'   provided in \code{data_list} with name for the subject identifier column fixed to
#'   \code{subjid_var}.}
#' \item{\code{trt_start}: Character name of the variable that contains
#' treatment start dates as provided in \code{basic_info}.}
#' \item{\code{trt_end}: Character name of the variable that contains
#' treatment end dates as provided in \code{basic_info}.}
#' \item{\code{icf_date}: Character name of the variable that contains
#' informed consent dates as provided in \code{basic_info}.}
#' }
#' @keywords internal
set_basics <- function(data_list, basic_info = default_basic_info(), subjid_var) {
  # Extract subject level dataset from data_list
  data <- data_list[[basic_info$subject_level_dataset_name]]

  check_names(
    data,
    var_names = c(basic_info$trt_start_var, basic_info$trt_end_var, basic_info$icf_date_var),
    subjid_var = subjid_var
  )
  check_date_type(data, c(basic_info$trt_start_var, basic_info$trt_end_var, basic_info$icf_date_var))

  return(
    list(
      data = dplyr::rename(data, dplyr::all_of(c(subject_id = subjid_var))),
      trt_start = basic_info$trt_start_var,
      trt_end = basic_info$trt_end_var,
      icf_date = basic_info$icf_date_var
    )
  )
}


#' Set and prepare interval data
#'
#' Gathers data needed for the plot and transforms them
#' to a consistent structure.
#'
#' @inheritParams prep_data
#'
#' @details
#' Please refer to the Details section of \code{mod_clinical_timelines()} for further
#' instructions on how to define a proper mapping.
#'
#' @return A data frame including the following columns:
#' \itemize{
#' \item{\code{subject_id}: A unique subject identifier column. Its name is specified by
#' \code{subjid_var}.}
#' \item{\code{start_dt_var}, \code{end_dt_var}: Start and end dates of the event.}
#' \item{\code{detail_var}: Further information to the events.}
#' \item{\code{set}: Name of the dataset the data origins from.}
#' \item{\code{set_id}: Row ID's of the related dataset.}
#' \item{\code{group}: Label for the event types.}
#' \item{\code{arrow_right}: Flag that indicates whether the event is ongoing/ended
#'   after the specified time range or not.}
#' }
#'
#' @keywords internal
set_events_intern <- function(data_list, mapping = default_mapping(), subjid_var) {
  # Mapping contains one or multiple event definitions per dataset
  per_set <- names(mapping) %>%
    purrr::map(function(set) { # Per dataset

      df <- data_list[[set]]
      per_event <- names(mapping[[set]]) %>%
        purrr::map(function(event) { # Per event within one dataset

          sub_df <- df %>%
            # Elements of mapping[[set]][[event]]: "start_dt_var" "end_dt_var" "start_dy_var" "end_dy_var" "detail_var"
            dplyr::select(dplyr::all_of(c(subjid_var, unlist(mapping[[set]][[event]]), "set_id"))) %>%
            dplyr::mutate(group = rep(event))

          if ("end_dt_var" %in% colnames(sub_df)) { # I.e. if it's an interval event

            sub_df <- sub_df %>%
              dplyr::mutate(

                # Missing flags
                start_missing = dplyr::if_else(is.na(.data$start_dt_var), TRUE, FALSE),
                end_missing = dplyr::if_else(is.na(.data$end_dt_var), TRUE, FALSE),

                # Arrow flags to indicate open intervals (basic setup which is enhanced in the later processing)
                arrow_left = .data$start_missing,
                arrow_right = .data$end_missing
              )
          } else {
            sub_df <- sub_df %>%
              dplyr::mutate(
                start_missing = FALSE,
                end_missing = FALSE,
                arrow_left = .data$start_missing,
                arrow_right = .data$end_missing
              )
          }

          return(sub_df)
        })

      set_df <- dplyr::bind_rows(per_event) %>%
        dplyr::mutate(set = set)

      return(set_df)
    })

  event_df <- dplyr::bind_rows(per_set) %>%
    dplyr::rename(dplyr::all_of(c(subject_id = subjid_var)))

  return(event_df)
}



#' Set and prepare interval for exp data
#'
#' Gathers data needed for the plotting of drug administration periods and transforms them
#' to a consistent structure.
#'
#' @param mapping A list of named character strings that describes which variables to
#'   use to display drug administration events.
#' @inheritParams prep_data
#'
#' @details
#' The \code{mapping} list must contain the following elements:
#' \itemize{
#'   \item{\code{name}: Character name of the dataset that holds drug administration data
#'     (e.g. ex domain), as it is called in the datalist that is provided to the
#'     \pkg{modulemanager}.}
#'   \item{\code{start_var}: Character name of the variable that contains the start dates
#'     (e.g. exposure start dates) which must be present in the dataset mentioned in the
#'     \code{name} element.}
#'   \item{\code{end_var}: Character name of the variable that contains the end dates
#'     (e.g. exposure end dates) which must be present in the dataset mentioned in the
#'     \code{name} element.}
#'   \item{\code{detail_var}: Character name of the variable that contains the treatment
#'     information. Must exist in the dataset mentioned in the \code{name} element.}
#'   \item{\code{label}: Free-text character label for the drug administration event.}
#'   \item{\code{dose_var}: Character name of the variable that contains the dosis level
#'     information. Must exist in the dataset mentioned in the \code{name} element.}
#'   \item{\code{dose_unit_var}: Character name of the variable that contains the dosis
#'     unit. Must exist in the dataset mentioned in the \code{name} element.}
#' }
#'
#'
#' @return A data frame including the following columns:
#' \itemize{
#' \item{\code{subject_id}: A unique subject identifier column. Its name is specified by
#' \code{subjid_var}.}
#' \item{\code{start_exp}, \code{end_exp}: Start and end dates of the event.}
#' \item{\code{detail_var}: Information about drug name and dosage.}
#' \item{\code{set}: Name of the dataset the data origins from.}
#' \item{\code{set_id}: Row ID's of the related dataset.}
#' \item{\code{dose_var}: Indicates whether the dose of the treatment has increased,
#' decreased or stayed the same compared to the last dose at the same subject.}
#'  \item{\code{group}: Label for the event types.}
#' }
#'
#' @keywords internal
#'
set_exp_intervals <- function(data_list, mapping = default_drug_admin(), subjid_var) {
  col_list <- mapping[!names(mapping) %in% c("dataset_name")]

  cols <- c(col_list$start_var, col_list$end_var, col_list$detail_var, col_list$trt_var)
  data <- data_list[[mapping$dataset_name]]

  check_names(data, cols, subjid_var)
  check_date_type(data, c(col_list$start_var, col_list$end_var))

  data <- data %>%
    dplyr::group_by(get(subjid_var), get(col_list$trt_var)) %>%
    dplyr::mutate(
      exp_dose = dplyr::case_when(
        is.na(dplyr::lag(get(col_list$dose_var))) ~ "start/equal",
        dplyr::lag(get(col_list$dose_var)) == get(col_list$dose_var) ~ "start/equal",
        dplyr::lag(get(col_list$dose_var)) < get(col_list$dose_var) ~ "increase",
        dplyr::lag(get(col_list$dose_var)) > get(col_list$dose_var) ~ "decrease"
      )
    ) %>%
    dplyr::ungroup()
  interval_df <- data %>%
    dplyr::mutate(
      detail_var = paste(
        .data[[col_list$detail_var]], "-",
        .data[[col_list$dose_var]],
        .data[[col_list$dose_unit_var]]
      ),
      trt_var = .data[[col_list$trt_var]]
    ) %>%
    dplyr::select(
      tidyselect::all_of(c(subjid_var, cols[1:2], "set_id", "exp_dose", "detail_var", "trt_var"))
    ) %>%
    dplyr::mutate(
      group = dplyr::if_else(
        !is.na(.data[["trt_var"]]),
        paste0(col_list$label, ": ", .data[["trt_var"]]),
        NA
      )
    ) %>%
    dplyr::rename(
      start_exp = tidyselect::all_of(col_list$start_var),
      end_exp = tidyselect::all_of(col_list$end_var)
    ) %>%
    dplyr::mutate(set = mapping$dataset_name) %>%
    dplyr::rename(dplyr::all_of(c(subject_id = subjid_var)))

  return(interval_df)
}


#' Combine subject level, event, and drug administration data frames
#'
#' Given a list of data frames, \code{combine_data()} merges them into one data
#' frame and adds subject level specific columns.
#'
#' @param df_list A list of data frames as delivered by \code{set_events_intern()} and
#'   \code{set_exp_intervals()}.
#' @param basic_info_df A data frame containing subject level data as extracted by
#' \code{set_basics()}.
#' @param trtstart Character name of the treatment start column as extracted
#' by \code{set_basics()}.
#' @param trtend Character name of the treatment end column as extracted by
#' \code{set_basics()}.
#' @param icf_date Character name of the informed consent date column of the
#' subject level dataset as extracted by \code{set_basics()}.
#' @inheritParams prep_data
#'
#'
#' @return A data frame in which the columns of all data frames of the df_list
#' are combined and with the following additional columns:
#' \itemize{
#' \item{A treatment start date column as specified through the trtstart
#' parameter.}
#' \item{A treatment end date column as specified through the trtend
#' parameter.}
#' \item{\code{earliest}: Holds the date of the earliest event per subject,
#' which is always the informed consent date.}
#' }
#'
#' @importFrom rlang .data
#' @keywords internal
#'
combine_data <- function(df_list,
                         basic_info_df,
                         trtstart,
                         trtend,
                         icf_date,
                         subjid_var) {
  # Set informed consent date as earliest event
  basic_data <- basic_info_df %>%
    dplyr::select(
      dplyr::all_of(c(subjid_var, trtstart, trtend, "earliest" = icf_date))
    )

  # Combine interval and timepoint data
  one_data <- dplyr::bind_rows(df_list) %>%
    dplyr::left_join(basic_data)

  return(one_data)
}


#' Adapt data frame structure for plotting purposes
#'
#' \code{complete_events()} ensures all dates necessary for the main plot, i.e. start and
#' and end dates/days per event are available and of respective type. If no study day
#' columns were determined at app configuration, the function calculates them
#' according to SDTM rules.
#'
#' @param combined_data A data frame as provided by \code{combine_data()}.
#' @param trt_start Character name of the treatment start column as extracted
#'   from \code{set_basics}.
#' @param trt_end Character name of the treatment end column as extracted
#'   from \code{set_basics}.
#'
#' @return A data frame as provided via the combined_data parameter with
#' additional columns:
#' \itemize{
#' \item{\code{start_dy_var}: Event start days relative to study start.}
#' \item{\code{end_dy_var}: Event end days relative to study start.}
#' \item{\code{start_exp_day}: Start of time exp intervals in days relative to study
#' start.}
#' \item{\code{end_exp_day}: End of time exp intervals in days relative to study start.}
#' }
#'
#' @keywords internal

complete_events <- function(combined_data, trt_start, trt_end) {
  # Check if columns exist & add them if not, 'cause they're needed below
  combined_data$end_dt_var <- if ("end_dt_var" %in% colnames(combined_data)) {
    combined_data$end_dt_var
  } else {
    as.POSIXct(NA_real_)
  }

  combined_data$start_dy_var <- if ("start_dy_var" %in% colnames(combined_data)) {
    combined_data$start_dy_var
  } else {
    NA_real_
  }

  combined_data$end_dy_var <- if ("end_dy_var" %in% colnames(combined_data)) {
    combined_data$end_dy_var
  } else {
    NA_real_
  }

  # Set dates & days for plotting purposes later
  df <- combined_data %>%
    dplyr::filter(!is.na(.data[[trt_start]])) %>% # do not display screening failures
    dplyr::mutate(
      # Set missing start dates to informed consent date (stored in "earliest" col)
      start_dt_var = dplyr::if_else(.data$start_missing, .data$earliest, .data$start_dt_var),

      # Impute missing end dates
      # If treatment end is available, put this date in
      # If not, assume that it is ongoing and put today in
      end_dt_var = dplyr::if_else(
        .data$end_missing, # boolean column - TRUE indicates open interval
        dplyr::if_else(
          !is.na(.data[[trt_end]]),
          lubridate::ymd_hm(lubridate::as_date(.data[[trt_end]]), truncated = 2),
          Sys.time()
        ),
        .data$end_dt_var
      ),
      resetted = lubridate::ymd_hm(lubridate::as_date(.data[[trt_start]]), truncated = 2),

      # Calculate days where not available
      start_dy_var = dplyr::if_else(
        is.na(.data$start_dy_var),
        dplyr::case_when(
          is.na(.data$start_dt_var) ~ NA_real_,
          # as.numeric converts datetime to seconds // 86400 sec = 1 day
          .data$start_dt_var >= resetted ~ (as.numeric(.data$start_dt_var) - as.numeric(resetted) + 86400) / 86400,
          .data$start_dt_var < resetted ~ (as.numeric(.data$start_dt_var) - as.numeric(resetted)) / 86400
        ),
        .data$start_dy_var
      ),
      end_dy_var = dplyr::if_else(
        is.na(.data$end_dy_var),
        dplyr::case_when(
          is.na(.data$end_dt_var) ~ NA_real_,
          .data$end_dt_var >= resetted ~ (as.numeric(.data$end_dt_var) - as.numeric(resetted) + 86400) / 86400,
          .data$end_dt_var < resetted ~ (as.numeric(.data$end_dt_var) - as.numeric(resetted)) / 86400
        ),
        .data$end_dy_var
      ),
      start_exp_day = dplyr::case_when(
        is.na(.data$start_exp) ~ NA_real_,
        .data$start_exp >= resetted ~ (as.numeric(.data$start_exp) - as.numeric(resetted) + 86400) / 86400,
        .data$start_exp < resetted ~ (as.numeric(.data$start_exp) - as.numeric(resetted)) / 86400
      ),
      end_exp_day = dplyr::case_when(
        is.na(.data$end_exp) ~ NA_real_,
        .data$end_exp >= resetted ~ (as.numeric(.data$end_exp) - as.numeric(resetted) + 86400) / 86400,
        .data$end_exp < resetted ~ (as.numeric(.data$end_exp) - as.numeric(resetted)) / 86400
      )
    ) # end of mutate()
  return(df)
}



#' Create a dataset for local adverse event filters
#'
#' @inheritParams prep_data
#'
#' @details
#' The list provided to \code{mapping} must follow a strict hierarchy. It must contain
#' one entry per dataset/domain that serves as basis for the events. These entries need to
#' be named according to the names of the datalist that is provided to the
#' \pkg{modulemanager}. The entries by oneself must again be lists. \cr
#' These second level lists contain the variable names that are needed to plot the events,
#' gathered in yet another lists, which are named according to the labels that shall be
#' assigned to each event, and that contain the following elements each:
#' \itemize{
#'  \item{\code{start_dt_var}: Character name of the variable that contains
#'     either the event start dates (for interval events) or merely timepoints (e.g.
#'     milestones). The variable name must be present in the dataset under which the
#'     event is listed.}
#'  \item{\code{end_dt_var}: Character name of the variable that contains
#'     the event end dates. Needs to be provided for interval events, but set to
#'     \code{NULL} for timepoints. The variable name must be present in the dataset under
#'     which the event is listed.}
#'  \item{\code{start_dy_var}: Similar to \code{start_dt_var}, but refers to the study
#'     relative days (instead of dates). The variable name must be present in the dataset
#'     under which the event is listed. Can be set to \code{NULL} to let the module
#'     calculate the study days according to SDTM standard rules.}
#'  \item{\code{end_dy_var}: Similar to \code{end_dt_var}, but refers to the study
#'     relative days (instead of dates). The variable name must be present in the dataset
#'     under which the event is listed. Can be set to \code{NULL} to let the module
#'     calculate the study days according to SDTM standard rules.}
#'  \item{\code{detail_var}: Character name of the variable that contains further
#'     descriptive information that shall be displayed for the event. Can be set to
#'     \code{NULL} for no further information.}
#' }
#'
#' The structure of the \code{mapping} parameters for one single event is mentioned below.
#' It is possible to define multiple events for one dataset, and multiple datasets in
#' the mapping list.
#'
#' \code{mapping = list(}\cr
#'   \verb{  }\code{<data name> = list}(\cr
#'   \verb{  }\verb{  }\code{<event label> = list}(\cr
#'   \verb{  }\verb{  }\verb{  }\code{start_dt_var = <variable name>,}\cr
#'   \verb{  }\verb{  }\verb{  }\code{end_dt_var = <variable name or NULL>,}\cr
#'   \verb{  }\verb{  }\verb{  }\code{start_dy_var = <variable name or NULL>,}\cr
#'   \verb{  }\verb{  }\verb{  }\code{end_dy_var = <variable name or NULL>,}\cr
#'   \verb{  }\verb{  }\verb{  }\code{detail_var = <variable name or NULL>}\cr
#'   \verb{  }\verb{  }\code{)}\cr
#'   \verb{  }\code{)}\cr
#'   \code{)}
#'
#' @return A subset of the adverse event dataset containing all columns needed
#' for local adverse event filters.
#'
#' @keywords internal
#'
set_filter_dataset <- function(filter, data_list, mapping, subjid_var) {
  # Check for inconsistencies on modul definition side
  if (length(filter$ae_filter) < 3) stop("No ae_filter defined for Clinical Timelines!")
  if (!filter$ae_filter$dataset_name %in% names(data_list)) {
    msg <- paste0(
      "Clinical Timelines (dv.clinlines) cannot find ",
      filter$ae_filter$dataset_name,
      " in your data list."
    )

    stop(msg)
  }

  only_filters <- filter$ae_filter[!names(filter$ae_filter) %in% c("dataset_name", "label")]

  check_names(data_list[[filter$ae_filter$dataset_name]],
    unlist(only_filters), # drop dataset_name
    subjid_var = subjid_var
  )

  ae_info <- mapping[[filter$ae_filter$dataset_name]][[filter$ae_filter$label]]

  filter_dataset <- data_list[[filter$ae_filter$dataset_name]] %>%
    dplyr::select(
      dplyr::all_of(
        c(
          "set_id",
          "subject_id" = subjid_var,
          ae_info$start_dt_var,
          ae_info$end_dt_var,
          ae_info$detail_var,
          unlist(only_filters, use.names = FALSE)
        )
      )
    )

  return(filter_dataset)
}


#' Check if the user specified variable names exist in the dataset
#'
#' \code{check_names} produces an error if the variables can't be found in the
#' data frame.
#'
#' @param df A data frame.
#' @param var_names A character vector of column names to be checked.
#' @inheritParams prep_data
#'
#' @return If all variables were found in the data frame, the function returns
#' the data frame invisibly.
#' @keywords internal
check_names <- function(df, var_names, subjid_var) {
  error <- FALSE
  # Variable check
  var_check <- purrr::map_chr(c(var_names, subjid_var), function(x) {
    if (x %in% names(df)) {
      msg <- NA
    } else {
      error <<- TRUE
      msg <- x
    }

    return(msg)
  })

  if (error) {
    msg <- c(
      "dv.clinlines: Can't find user defined variable(s) in the dataset.",
      x = "You've tried to set the following variable(s):",
      x = paste0("`", var_check[!is.na(var_check)], "`", collapse = ", "),
      i = "Have you spelled their names correctly?"
    )
    rlang::abort(msg)
  } else {
    return(invisible(df))
  }
}


#' Check if the specified columns are of type Date
#'
#' \code{check_date_type} produces an error if data in the variables is not of type
#' Date.
#'
#' @param df A data frame.
#' @param var_names A string vector of column names to be checked.
#'
#' @return If all variables are of type Date, the function returns
#' the data frame invisibly.
#' @keywords internal
check_date_type <- function(df, var_names) {
  error <- FALSE

  date_check <- purrr::map_chr(var_names, function(x) {
    if (lubridate::is.POSIXct(df[[x]])) {
      msg <- NA
    } else {
      error <<- TRUE
      msg <- x
    }

    return(msg)
  })


  if (error) {
    msg <- c(
      "dv.clinlines: Variable(s) holding dates must be of type date-time.",
      x = "The following variable(s) are not of type date-time:",
      x = paste0("`", date_check[!is.na(date_check)], "`", collapse = ", "),
      i = 'Type "?lubridate" or "?POSIXct" into your console to learn more about date-times.'
    )
    rlang::abort(msg)
  } else {
    return(invisible(df))
  }
}


#' Produce a warning for non-available receiver names
#'
#' @param receiver_id Character string defining the module that should receive a subject identifier
#'   from clinlines.
#' @param module_ids Vector of characters defining all available module IDs.
#'
#' @return Logical outcome of the test invisible.
#' @keywords internal
#'
check_receiver <- function(receiver_id, module_ids) {
  if (!is.null(receiver_id) && !receiver_id %in% module_ids) {
    rlang::warn(
      message = c(
        "Clinical Timelines: You tried to point to a receiver module
              that does not exist in your module list.",
        x = paste0("You have set '", receiver_id, "' as receiver_id."),
        i = paste0(
          "Your module list contains ",
          paste(module_ids, collapse = ", "),
          "."
        ),
        i = "Have you spelled receiver_id correctly?"
      )
    )

    return(invisible(FALSE))
  }

  return(invisible(TRUE))
}
