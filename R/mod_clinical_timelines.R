#' Create user interface for \pkg{dv.clinlines} module
#'
#' \code{mod_clinical_timelines_UI} acts as central for managing user interfaces of
#' main view and local filter modules.
#'
#' @param module_id `[character(1)]`
#'
#' A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_clinical_timelines_server()}.
#' @param filter_list `[list(character(1)+) | NULL]`
#'
#' List of filter names that indicate which local filters to
#'   display.
#' @param x_param `[character(1) | NULL]`
#'
#' Either "date" or "day" defining if the x axis shows the date or
#' study days as initial setting at app launch. Defaults to "day".
#' @param boxheight_val `[integer(1) | NULL]`
#'
#' A value between 30 and 150 defining the initial height of
#' the individual timeline plot boxes at app launch. Defaults to 60.
#'
#' @return A shiny UI \code{tagList} that contains a \code{sidebarLayout} object.
#'
#' @seealso [mod_clinical_timelines()] and [mod_clinical_timelines_server()]
#' @export
mod_clinical_timelines_UI <- function(module_id, # nolint
                                      filter_list = NULL,
                                      x_param = "day",
                                      boxheight_val = 60) {
  # Check validity of arguments
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_string(module_id, min.chars = 0, add = ac) # Discuss min.chars = 1,
  checkmate::assert_list(filter_list, types = "character", null.ok = TRUE, add = ac)
  checkmate::assert_subset(
    unlist(filter_list),
    choices = c("soc_var", "serious_ae_var", "pref_term_var", "drug_rel_ae_var"),
    add = ac
  )
  checkmate::assert_string(x_param, null.ok = TRUE, add = ac)
  checkmate::assert_subset(x_param, choices = c("date", "day"), add = ac)
  checkmate::reportAssertions(ac)

  ns <- shiny::NS(module_id)

  ui_main <- mod_main_view_UI(ns("main_view"), x_param, boxheight_val)
  ui_filter <- mod_local_filter_UI(ns("filter"), filter_list)

  ui <- shiny::tagList(
    shiny::sidebarLayout(
      sidebarPanel = shiny::sidebarPanel(
        width = 2,
        ui_main$sidebar, ui_filter
      ),
      mainPanel = shiny::mainPanel(ui_main$main_panel)
    )
  )

  return(ui)
}


#' Create server for \pkg{dv.clinlines} module
#'
#' \code{mod_clinical_timelines_server} acts as central for managing the
#' communication between main view and local filter module.
#'
#' @param module_id `[character(1)]`
#'
#' A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_clinical_timelines_UI()}.
#' @param data_name `[shiny::reactive(character(1)]`
#'
#' A reactive string name of the selected set of dataset. Usually obtained
#'   from module manager.
#' @param dataset_list `[shiny::reactive(list(data.frame+))]`
#'
#' A reactive list of named datasets. Usually obtained from module
#'   manager.
#' @param start_day `[integer(1) | NULL]`
#'
#' A single integer indicating the lower x-axis limit in case of study day display.
#' Defaults to NULL, using the day of the earliest event to be displayed.
#' @param afmm_param `[list]`
#'
#' Named list of a selection of arguments from module manager. Expects
#'   at least two elements: \code{utils} and \code{module_names} defining a character vector
#'   whose entries have the corresponding module IDs as names.
#' @inheritParams mod_clinical_timelines
#' @inherit mod_clinical_timelines details
#' @seealso [mod_clinical_timelines()] and [mod_clinical_timelines_UI()],
#' [set_basic_info()], [set_event()], [set_drug_admin()]
#'
#' @return A reactive subject id string in a list for communication between modules with
#' an attribute code that contains the code to be used by the export functionality of
#' dv.manager.
#'
#' @export
mod_clinical_timelines_server <- function(module_id,
                                          data_name,
                                          dataset_list,
                                          basic_info = default_basic_info(),
                                          mapping = default_mapping(),
                                          drug_admin = default_drug_admin(),
                                          filter = NULL,
                                          subjid_var = "USUBJID",
                                          start_day = NULL,
                                          ms = 1000,
                                          receiver_id = NULL,
                                          afmm_param = NULL) {
  # Check validity of arguments
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_string(module_id, min.chars = 0, add = ac) # Discuss min.chars = 1,
  checkmate::assert_multi_class(data_name, c("reactive", "shinymeta_reactive"), add = ac)
  checkmate::assert_multi_class(dataset_list, c("reactive", "shinymeta_reactive"), add = ac)
  checkmate::assert_list(basic_info, types = "character", add = ac)
  checkmate::assert_subset(
    names(basic_info),
    choices = c(
      "subject_level_dataset_name",
      "trt_start_var",
      "trt_end_var",
      "icf_date_var"
    ),
    add = ac
  )
  checkmate::assert_list(mapping, types = "list", add = ac)
  checkmate::assert_character(unlist(mapping), add = ac)
  lapply(mapping, function(x) {
    lapply(x, function(y) {
      checkmate::assert_subset(
        names(unlist(y)),
        choices = c("start_dt_var", "end_dt_var", "start_dy_var", "end_dy_var", "detail_var")
      )
    })
  })
  checkmate::assert_list(drug_admin, types = "character", null.ok = TRUE, add = ac)
  checkmate::assert_subset(
    names(drug_admin),
    choices = c("dataset_name", "start_var", "end_var", "detail_var", "label", "dose_var", "dose_unit_var"),
    add = ac
  )
  checkmate::assert_list(filter, types = "list", null.ok = TRUE, add = ac)
  checkmate::assert_character(unlist(filter), null.ok = TRUE, add = ac)
  lapply(filter, function(x) {
    checkmate::assert_subset(
      names(unlist(x)),
      choices = c("dataset_name", "label", "soc_var", "serious_ae_var", "pref_term_var", "drug_rel_ae_var")
    )
  })
  checkmate::assert_string(subjid_var, min.chars = 1, add = ac)
  checkmate::assert_numeric(start_day, len = 1, null.ok = TRUE, add = ac)
  checkmate::assert_numeric(ms, len = 1, add = ac)
  checkmate::assert_string(receiver_id, min.chars = 1, null.ok = TRUE, add = ac)
  checkmate::assert_list(afmm_param, null.ok = TRUE, add = ac)
  checkmate::reportAssertions(ac)

  shiny::moduleServer(
    module_id,
    function(input, output, session) {
      # Check validity of reactive arguments
      v_dataset_list <- shiny::reactive({
        checkmate::assert_list(dataset_list(), types = "data.frame", null.ok = TRUE, names = "named")
        dataset_list()
      })
      v_data_name <- shiny::reactive({
        checkmate::assert_string(data_name(), min.chars = 1)
        data_name()
      })


      shiny::observe({
        shiny::req(afmm_param$module_names)

        # Check availability of receiver id
        check_receiver(receiver_id, names(afmm_param$module_names))
      })

      # Indicate if underlying dataset has changed
      vals <- shiny::reactiveValues(ds_name = NULL)
      changed <- shiny::reactiveVal(0)
      shiny::observeEvent(v_dataset_list(), {
        if (is.null(vals$ds_name) || vals$ds_name != v_data_name()) {
          changed(changed() + 1)
          vals$ds_name <- v_data_name()
        }
      })

      pre_data <- shiny::reactive({
        data <- prep_data(
          data_list = v_dataset_list(),
          basic_info = basic_info,
          mapping = mapping,
          drug_admin = drug_admin,
          subjid_var = subjid_var,
          filter = filter
        )

        data
      })

      # Set a fixed color for each group
      colors_groups <- shiny::reactive({
        if (nrow(pre_data()) > 0) color_lookup(unique(pre_data()$group))
      })

      # Add adverse event data that are relevant for filtering
      initial_data <- mod_local_filter_server(
        "filter",
        filter = filter,
        joined_data = pre_data,
        changed = changed
      )

      # Make sure current dataset name is transferred when bookmarking happens
      shiny::onBookmark(function(state) {
        state$values$ds_name <- v_data_name()
      })

      shiny::onRestore(function(state) {
        vals$ds_name <- state$values$ds_name
      })

      # Start main view
      main <- mod_main_view_server(
        "main_view",
        initial_data,
        changed,
        colors_groups,
        start_day,
        ms
      )

      shiny::observeEvent(main$subject(), {
        if (!receiver_id %in% names(afmm_param$module_names) && !is.null(receiver_id)) {
          shiny::showNotification(
            paste0("Can't find receiver module with ID ", receiver_id, "."),
            id = NULL,
            type = "message"
          )
        } else if (!is.null(receiver_id)) {
          afmm_param$utils$switch2(afmm_param$module_names[[receiver_id]])
        }
      })

      # Return code for export
      return(
        structure(
          list(subj_id = main$subject), # for communication with dv.papo
          code = main$code
        )
      )
    }
  )
}



#' Wrapper that serves as interface for defining an application in the \pkg{modulemanager}
#'
#' @param module_id `[character(1)]`
#'
#' A character string that serves as unique identifier for the module.
#' @param basic_info `[list(character(1)+)]`
#'
#' A list of four elements: \code{subject_level_dataset_name}, \code{trt_start_var},
#'   \code{trt_end_var}, and \code{icf_date_var}. Assigns the name
#'   of a subject level dataset and column names of treatment start and end, and informed
#'   consent variables.
#' @param mapping `[list(list(list(character(1)+)))]`
#'
#' A list of lists. It serves as instruction on which event to take from
#'   which data domain / dataset, and further from which variables to take the start and
#'   end values, etc., that will be plotted as clinical timelines. Elements need to follow
#'   a certain structure that is described in the Details section below.
#' @param drug_admin `[list(character(1)+) | NULL]`
#'
#' A list of named character strings that describes which variables to
#'   use to display drug administration events. Set it to NULL to deactivate them.
#' @param filter `[list(list(character(1)+)) | NULL]`
#'
#' A list that specifies information for local adverse events filters.
#'   Set to \code{NULL} (default) for no filters.
#' @param subjid_var `[character(1)]`
#'
#' Character name of the unique subject identifier column in
#'   all datasets (default is USUBJID). Must be a single value.
#' @param default_plot_settings `[list(character(1), integer(1), integer(1)) | NULL]`
#'
#' A named list containing three elements: \code{x_param} contains
#' either "date" or "day" defining if the x axis shows the date or
#' study days as default setting at app launch (defaults to "day"), \code{start_day}
#' contains an integer indicating the lower x-axis limit in case of study day display
#' (defaults to NULL, using the day of the earliest event to be displayed),
#' \code{boxheight_val} contains a value between 30 and 150 defining the initial height of
#' the individual timeline plot boxes at app launch (defaults to 60).
#'
#' @param ms `[numeric(1)]`
#'
#' Single numeric value indicating how many milliseconds to wait before the plot
#'   re-renders. Defaults to 1000.
#'
#' @param receiver_id `[character(1) | NULL]`
#'
#' Character string defining the ID of the module to which to send a subject ID. The
#'   module must exist in the module list. The default is NULL which disables communication.
#'
#'
#' @details
#' The \code{basic_info} list must contain the following elements:
#' \itemize{
#'   \item{\code{subject_level_dataset_name}: Character name of the subject level analysis dataset
#'     (e.g. "adsl", "dm") as it is called in the datalist that is provided to the
#'     \pkg{modulemanager}.}
#'   \item{\code{trt_start_var}: Character name of the variable that contains
#'     treatment start dates which must be present in the dataset mentioned in the
#'     \code{data} element.}
#'   \item{\code{trt_end_var}: Character name of the variable that contains
#'     treatment end dates which must be present in the dataset mentioned in the
#'     \code{data} element.}
#'   \item{\code{icf_date_var}: Character name of the variable that contains
#'     informed consent dates which must be present in the dataset mentioned in the
#'     \code{data} element.}
#' }
#'
#' \cr
#'
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
#' \cr
#'
#' If not NULL, the \code{drug_admin} list must contain the following elements:
#' \itemize{
#'   \item{\code{dataset_name}: Character name of the dataset that holds drug administration data
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
#' \cr
#'
#'
#' If not \code{NULL}, \code{filter} defines local filters.
#' So far, the following filters for adverse events are available:
#' \itemize{
#'   \item{\code{serious_ae_var}: Filter for serious adverse events.}
#'   \item{\code{soc_var}: Filter for system organ classes.}
#'   \item{\code{pref_term_var}: Filter for preferred terms.}
#'   \item{\code{drug_rel_ae_var}: Filter for drug related adverse events.}
#' }
#' The \code{filter} parameter must be a list that contains yet another list for adverse
#' events filters, that is named \code{ae_filter}, and that holds the following elements:
#' \itemize{
#'   \item{\code{dataset_name}: Character name of the adverse events dataset. Must be
#'     available in in the datalist that is provided to the \pkg{modulemanager}.}
#'   \item{\code{label}: Character value which is exactly the same as the name for the
#'   adverse events event defined in \code{mapping}.}
#'   \item{\code{serious_ae_var}: Character name of the adverse events variable that contains
#'     serious adverse events flags (Y/N).}
#'   \item{\code{soc_var}: Character name of the adverse events variable that contains
#'     system organ classes.}
#'   \item{\code{pref_term_var}: Character name of the adverse events variable that contains
#'     preferred terms.}
#'   \item{\code{drug_rel_ae_var}: Character name of the adverse events variable that contains
#'     drug related (causality) flags (Y/N).}
#' }
#'
#'
#' @return A list containing the following elements to be used by the
#' \pkg{modulemanager}:
#' \itemize{
#' \item{\code{ui}: A UI function of the \pkg{dv.clinlines} module.}
#' \item{\code{server}: A server function of the \pkg{dv.clinlines} module.}
#' \item{\code{module_id}: A unique identifier.}
#' }
#'
#' @seealso [mod_clinical_timelines_UI()], [mod_clinical_timelines_server()],
#'   [set_basic_info()], [set_event()], [set_drug_admin()]
#' @export
mod_clinical_timelines <- function(module_id,
                                   basic_info = default_basic_info(),
                                   mapping = default_mapping(),
                                   drug_admin = default_drug_admin(),
                                   filter = NULL,
                                   subjid_var = "USUBJID",
                                   default_plot_settings = list(
                                     x_param = "day",
                                     start_day = NULL,
                                     boxheight_val = 60
                                   ),
                                   ms = 1000,
                                   receiver_id = NULL) {
  # Check validity of arguments that won't be checked in UI/server
  ac <- checkmate::makeAssertCollection()
  checkmate::assert_list(default_plot_settings, null.ok = TRUE, add = ac)
  checkmate::assert_subset(names(default_plot_settings), choices = c("x_param", "start_day", "boxheight_val"), add = ac)
  checkmate::reportAssertions(ac)

  mod <- list(
    ui = function(id) {
      # Extract available adverse event filter names
      ae_filter <- !names(filter$ae_filter) %in% c("dataset_name", "label")

      # Select requested filters
      selected <- as.list(names(filter$ae_filter[ae_filter]))

      dv.clinlines::mod_clinical_timelines_UI(
        id,
        filter_list = selected,
        x_param = ifelse(is.null(default_plot_settings$x_param), "day", default_plot_settings$x_param),
        boxheight_val = ifelse(is.null(default_plot_settings$boxheight_val), 60, default_plot_settings$boxheight_val)
      )
    },
    server = function(afmm) {
      dv.clinlines::mod_clinical_timelines_server(
        module_id = module_id,
        # afmm$dataset_metadata$name holds the name of the currently selected set of dataset (dv.manager)
        data_name = afmm$dataset_metadata$name,
        dataset_list = shiny::reactive({
          needed_datasets <- unique(c(basic_info$subject_level_dataset_name, names(mapping), drug_admin$dataset_name))
          afmm$filtered_dataset()[needed_datasets]
        }),
        basic_info = basic_info,
        mapping = mapping,
        drug_admin = drug_admin,
        filter = filter,
        subjid_var = subjid_var,
        start_day = default_plot_settings$start_day,
        ms = ms,
        receiver_id = receiver_id,
        afmm_param = list(utils = afmm$utils, module_names = afmm$module_names)
      )
    },
    module_id = module_id
  )

  return(mod)
}

# clinical timelines module interface description ----
# TODO: Fill in
mod_clinical_timelines_API_docs <- list(
  "Clinical Timelines",
  module_id = "",
  basic_info = list(
    "",
    subject_level_dataset_name = "",
    trt_start_var = "",
    trt_end_var = "",
    icf_date_var = ""
  ),
  mapping = list(""),
  drug_admin = list(""),
  filter = list(""),
  subjid_var = "",
  default_plot_settings = list(""),
  ms = list(""),
  receiver_id = ""
)

mod_clinical_timelines_API_spec <- TC$group(
  module_id = TC$mod_ID(),
  basic_info = TC$group(
    subject_level_dataset_name = TC$dataset_name() |> TC$flag("subject_level_dataset_name"),
    trt_start_var = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())),
    trt_end_var = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime())),
    icf_date_var = TC$col("subject_level_dataset_name", TC$or(TC$date(), TC$datetime()))
  ) |> TC$flag("ignore"),
  mapping = TC$group() |> TC$flag("ignore"),
  drug_admin = TC$group() |> TC$flag("ignore"),
  filter = TC$group() |> TC$flag("ignore"),
  subjid_var = TC$character() |> TC$flag("optional", "ignore"), # TODO: Maybe TC$col("subject_level_dataset_name", TC$or(TC$factor(), TC$character())) |> TC$flag("subjid_var"),
  default_plot_settings = TC$group() |> TC$flag("ignore"),
  ms = TC$group() |> TC$flag("ignore"),
  receiver_id = TC$character() |> TC$flag("optional", "ignore")
) |> TC$attach_docs(mod_clinical_timelines_API_docs)

dataset_info_clinical_timelines <- function(basic_info, mapping, filter, ...) {
  # TODO: Should we do something with the `filter` parameter ?
  subject_level <- basic_info[["subject_level_dataset_name"]]
  all <- unique(c(subject_level, names(mapping)))
  return(list(all = all, subject_level = subject_level))
}

check_mod_clinical_timelines <- function(
    afmm, datasets, module_id, basic_info, mapping, drug_admin, filter, subjid_var, default_plot_settings, ms, receiver_id) {
  warn <- CM$container()
  err <- CM$container()

  # TODO: Replace this function with a generic one that performs the checks based on mod_clinical_timelines_API_spec.
  # Something along the lines of OK <- CM$check_API(mod_clinical_timelines_API_spec, args = match.call(), warn, err)

  OK <- check_mod_clinical_timelines_auto( # nolint
    afmm, datasets,
    module_id, basic_info, mapping, drug_admin, filter, subjid_var, default_plot_settings, ms, receiver_id,
    warn, err
  )

  # Checks that API spec does not (yet?) capture # TODO:

  res <- list(warnings = warn[["messages"]], errors = err[["messages"]])
  return(res)
}

mod_clinical_timelines <- CM$module(
  mod_clinical_timelines, check_mod_clinical_timelines, dataset_info_clinical_timelines
)
