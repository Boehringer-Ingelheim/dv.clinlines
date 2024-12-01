CL <- pack_of_constants( # nolint
  ERR_MSG = paste(
    "An error occurred, most likely due to the plot being too large.",
    "Please try reducing the number of subjects by adjusting the filter settings.",
    "If the issue persists, kindly contact the developer of this application and",
    "request checking the log files for more information about the error."
  ),
  VAL1 = "No data available, please adjust your time range settings.",
  VAL2 = "No event selected.",
  VAL3 = "No data available, please adjust your filter settings.",
  VAL4 = "Please select a start value prior to the end value of your time range."
)

#' Create user interface for the main view module of \pkg{dv.clinlines}
#'
#' @param module_id A unique ID string to create a namespace. Must match the ID of
#' \code{mod_main_view_server()}.
#'
#' @param x_param Either "date" or "day" defining if the x axis shows the date or
#' study days as initial setting at app launch. Defaults to "day".
#'
#' @param boxheight_val A value between 30 and 150 defining the initial height of
#' the individual timeline plot boxes at app launch. Defaults to 60.
#'
#' @return A list containing two entries:
#' \itemize{
#' \item{\code{sidebar}: A \code{tagList} of shiny input elements.}
#' \item{\code{main_panel}: A \code{div} element containing shiny UI elements.}
#' }
#'
#' @keywords internal
#'
mod_main_view_UI <- function(module_id, x_param = "day", boxheight_val = 60) { # nolint
  ns <- shiny::NS(module_id)

  ui <- list(
    sidebar = shiny::tagList(
      shinyjs::useShinyjs(),
      # Get screen hight for tooltip
      shiny::tags$head(
        shiny::tags$script(
          paste0(
            '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("', ns("screen_height"), '", window.innerHeight);
            });
            $(window).resize(function(e) {
              Shiny.onInputChange("', ns("screen_height"), '", window.innerHeight);
            });'
          )
        )
      ),
      shiny::h4("Adjust Plot"),
      shiny::hr(),

      # Select x-axis scale
      shinyWidgets::radioGroupButtons(
        inputId = ns("x_scale"),
        label = "Scale x-axis to",
        choices = c("Date" = "date", "Study day" = "day"),
        selected = x_param
      ),

      # Set time range input depending on x-axis scale
      shiny::conditionalPanel(
        condition = "input.x_scale == 'date'",
        shinyWidgets::airDatepickerInput(
          inputId = ns("date_range"),
          "Time range",
          range = TRUE,
          value = c("1800-01-01", "1900-01-01"),
          separator = " to ",
          timepicker = FALSE,
          dateFormat = "yyyy-M-dd",
          addon = "none"
        ),
        ns = ns
      ),
      shiny::conditionalPanel(
        condition = "input.x_scale == 'day'",
        shinyWidgets::numericRangeInput(
          inputId = ns("day_range"),
          label = "Time range",
          value = c(-1234, -1234)
        ),
        ns = ns
      ),

      # Select sorting type of y-axis
      shiny::selectInput(
        inputId = ns("y_sort"),
        label = "Sort y-axis",
        choices = list(
          "alphanumerically" = "alphanum",
          "by earliest event" = "earliest"
        ),
        selected = "alphanum"
      ),

      # Set height of boxes/rows
      shiny::sliderInput(ns("height"), "Box Height [Px]",
        min = 30, max = 150, value = boxheight_val, step = 10
      ),
      shiny::br(),
      shiny::h4("Filter Events"),
      shiny::hr(),

      # Filter subjects by event groups
      shinyWidgets::pickerInput(
        inputId = ns("filter_event"),
        label = "Event Type",
        choices = NULL,
        options = shinyWidgets::pickerOptions(actionsBox = TRUE),
        multiple = TRUE
      )
    ),
    main_panel = shiny::div(
      id = ns("main_plot_div"),
      style = "position:relative",

      # The following replaces all (!) error messages displayed in the UI by a custom message.
      # However, we assume that the only error message that could appear occasionally occurs
      # due to the plot being to big.
      shiny::tags[["style"]](
        paste0(
          "#", ns("main_plot"), ".shiny-output-error:not(.shiny-output-error-validation) {
             visibility: hidden
          }\n",
          "#", ns("main_plot_div"), ":has(.shiny-output-error)::after {
            content: '", CL$ERR_MSG, "';
          }",
          "#", ns("main_plot_div"), ":has(.shiny-output-error.shiny-output-error-validation)::after {
            content: '';
          }

          "
        )
      ),
      shiny::plotOutput(
        ns("main_plot"),
        click = shiny::clickOpts(ns("plot_click")),
        hover = shiny::hoverOpts(
          ns("plot_hover"),
          delayType = "debounce",
          delay = 500
        ),
        width = "120%", # if changed, there are also changes to be made in function create_tooltip
        height = "100%"
      ),
      shiny::uiOutput(ns("hover_info"))
    )
  )

  return(ui)
}



#' Server logic of the main view module of \pkg{dv.clinlines}
#'
#' @param module_id A unique ID string to create a namespace. Must match the ID of
#'   \code{mod_main_view_UI()}.
#' @param initial_data A metaReactive data frame returned by
#'   \code{mod_local_filter_server()}.
#' @param changed A reactive whose actualization indicates if the underlying dataset has changed.
#' @param colors_groups A reactive named vector holding hexcode colors.
#' @param start_day `[integer(1)]` A single integer indicating the lower x-axis limit in case of study day display.
#' Defaults to NULL, using the day of the earliest event to be displayed.
#' @param ms A single numeric value indicating how many milliseconds to be used for
#'   debouncing the main view plot.
#'
#' @return A list of reactives to be used to communicate with other DaVinci modules,
#'   if available.
#'
#' @keywords internal
#'
mod_main_view_server <- function(module_id, initial_data, changed,
                                 colors_groups, start_day = NULL, ms = 100) {
  shiny::moduleServer(
    module_id,
    function(input, output, session) {
      ns <- session$ns

      # No need for reactivity as this serves merely as a storage
      cache <- list(date_day_range = NULL)

      shiny::observeEvent(changed(), {
        # In case the date/day ranges are not already stored in the cache
        if (is.null(cache$date_day_range)) {
          # Use earliest day as lower x-axis limit if not specified explicitly
          if (is.null(start_day)) {
            start_day <- min(initial_data()$day_min)
          }

          cache$date_day_range <<- list(
            date = c(
              min(initial_data()$date_min),
              max(c(initial_data()$end_dt_var, initial_data()$end_exp), na.rm = TRUE)
            ),
            day = c(start_day, max(c(initial_data()$end_dy_var, initial_data()$end_exp_day), na.rm = TRUE))
          )
        }

        # Set time range choices
        shinyWidgets::updateAirDateInput(session, "date_range", value = cache$date_day_range$date)
        shinyWidgets::updateNumericRangeInput(session, "day_range", value = cache$date_day_range$day)


        # Set event groups
        # Use names of color_groups to get all groups
        groups <- names(colors_groups())
        shinyWidgets::updatePickerInput(session, "filter_event", choices = groups, selected = groups)
      })

      # Choices need to be saved explicitly
      shiny::onBookmark(function(state) {
        state$values$date_day_range <- list(date = input$date_range, day = input$day_range)
      })

      # Take over bookmarked values to cache for later usage
      shiny::onRestore(function(state) {
        cache$date_day_range <<- state$values$date_day_range
      })

      # Restore event groups after bookmarking
      shiny::onRestored(function(state) {
        event_choices <- names(colors_groups())
        event_selected <- if (is.null(state$input$filter_event)) {
          event_choices
        } else {
          state$input$filter_event
        }

        shinyWidgets::updatePickerInput(session, "filter_event", choices = event_choices, selected = event_selected)
      })

      shiny::observe({
        # Exclude inputs not needed for bookmarking
        shiny::setBookmarkExclude({
          c(
            "plot_click",
            "plot_hover",
            "screen_height",
            # Get shinyjs inputs
            names(input)[startsWith(names(input), "shinyjs")]
          )
        })
      })

      # Pick actual time range values
      time_range <- shiny::reactive({
        if (shiny::req(input$x_scale) == "date") {
          lubridate::ymd_hm(input$date_range, truncated = 2)
        } else {
          input$day_range
        }
      })

      # Adapt start/end date according to timeline limits
      plot_data <- shiny::reactive({
        shiny::req(
          length(time_range()) == 2, # wait until range is set completely
          time_range() != c("1800-01-01", "1900-01-01"), # wait until default was set
          time_range() != c(-1234, -1234)
        )

        create_plot_data(
          work_data = initial_data(),
          time_range = time_range(),
          filter_event = input$filter_event
        )
      })

      plot_obj <- shiny::reactive({
        shiny::req(
          length(time_range()) == 2, # wait until range is set completely
          time_range() != c("1800-01-01", "1900-01-01"), # wait until default was set
          time_range() != c(-1234, -1234)
        )

        plot <- create_main_plot(
          work_data = plot_data(),
          y_sort = input$y_sort,
          time_range = time_range(),
          colors = colors_groups()[unique(plot_data()$group)],
          height = input$height,
          subjid_var = "subject_id"
        )

        # Return both to avoid double rendering on input$height change
        list(plot = plot, height = input$height, filter_event = input$filter_event, time_range = time_range())
      })

      plot_obj_d <- shiny::debounce(plot_obj, ms)

      # Create plot on the main screen
      output$main_plot <- shiny::renderPlot(
        expr = {
          if (length(plot_obj_d()$plot) == 1) {
            shiny::validate(shiny::need(plot_obj_d()$plot != "No_start_data", CL$VAL1))
          } else {
            shiny::validate(shiny::need(!is.null(plot_obj_d()$filter_event), CL$VAL2))
            shiny::validate(shiny::need(nrow(plot_obj_d()$plot$data) != 0, CL$VAL3))
            shiny::validate(shiny::need(diff(plot_obj_d()$time_range) > 0, CL$VAL4))
          }

          plot_obj_d()$plot
        },
        height = function() {
          # Use height from plot_obj_d to avoid double rendering and add some space
          # for the legend (107 pt offset in general + 18 pt per legend line)
          n <- ifelse(is.character(plot_obj_d()$plot), 10, length(unique(plot_obj_d()$plot$data$subject_id)))
          plot_obj_d()$height * n + 107 + 18 * length(colors_groups())
        }
      )


      # y mouse position on window screen in percent
      y_screen_pct <- shiny::reactiveVal(NULL)
      shinyjs::onevent("mousemove", ns("main_plot_div"), asis = TRUE, function(event) {
        y_screen_pct(event$clientY / input$screen_height)
      })

      # Use input$plot_hover to avoid multiple rendering of tooltip through y_screen_pct()
      tooltip <- shiny::eventReactive(input$plot_hover, {
        # Use y position of the mouse to narrow ggdata down
        # (which is produced by a ggplot2 internal function)
        ggdata_y <- create_ggdata_y(plot_obj()$plot, input$plot_hover)

        hover_info <- create_hover_info(   # nolint
          input$plot_hover,
          ggdata_y,
          initial_data(),
          colors_groups(),
          input$x_scale,
          y_screen_pct = y_screen_pct(),
          time_range = shiny::req(time_range())
        )
      })

      # Create hover information for the main plot
      output$hover_info <- shiny::renderUI({
        shiny::wellPanel(
          style = tooltip()$style,
          shiny::p(
            shiny::HTML(tooltip()$message),
            shiny::HTML(tooltip()$df)
          ),
        )
      })

      # Get unique subject identifier from click
      subject_id <- shiny::reactive({
        shiny::req(input$plot_click)

        # Get y position of the click and find the matching subject identifier
        position <- round(input$plot_click$y)
        subject <- input$plot_click$domain$discrete_limits$y[[position]]
        subject
      })

      testing <- isTRUE(getOption("shiny.testmode"))
      if (testing) {
        subject_id_orig <- subject_id

        trigger <- shiny::reactiveVal(0)
        shiny::observeEvent(input[["debug_select_subject"]], trigger(trigger() + 1))
        subject_id <- shiny::reactive({
          res <- NULL
          if (trigger()) {
            res <- shiny::isolate(input[["debug_select_subject"]])
          } else {
            res <- subject_id_orig()
          }
          return(res)
        })
      }

      # For exchange with receiver module
      return(
        list(
          subject = subject_id,
          session = session,
          code = shiny::reactive(plot_obj()$plot) # for code export
        )
      )
    }
  )
}
