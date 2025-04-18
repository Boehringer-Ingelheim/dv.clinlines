#' Check if colors are valid
#'
#' Executes an error action in case of wrong color definitions.
#'
#' @param color_palette Vector containing strings that are meant to specify
#' a color, either as 6-digit hex color starting with the # symbol, or as
#' R color string like received when executing `colors()`.
#'
#' @keywords internal
#'
check_valid_color <- function(color_palette) {

  hex_colors <- color_palette[grepl("^#", color_palette)]
  no_colors <- hex_colors[!grepl("^#([A-Fa-f0-9]{6})$", hex_colors)]
  other_colors <- color_palette[!grepl("^#", color_palette)]
  no_colors <- c(no_colors, other_colors[!other_colors %in% colors()])

  if (length(no_colors) > 0) {
    stop(paste("Invalid color(s) in color_palette:", paste(no_colors, collapse = ", ")))
  }
}


#' Create color lookup table
#'
#' \code{color_lookup} returns a named vector of hex colors.
#'
#' @param groups Character vector of unique event types of the data to be
#'   plotted, e.g. returned by \code{combine_data}. Defines length and element
#'   names of the returned vector.
#'
#' @return A named vector of hexcode colors.
#' @keywords internal
#'
color_lookup <- function(groups, color_palette) {

  if (is.null(color_palette)) {
    colors_groups <- scales::hue_pal()(length(groups))

    # Avoid that neighbor intervals get similar colors
    set.seed(20220202)
    names(colors_groups) <- sample(groups)
  } else {
    missing_groups <- groups[!groups %in% names(color_palette)]
    grey_vec <- rep("grey", length(missing_groups))
    names(grey_vec) <- missing_groups
    colors_groups <- c(color_palette, grey_vec)
  }

  return(colors_groups)
}


#' Adapt time intervals for plotting purposes
#'
#' Modifies a data frame so that interval starts and ends are not outside
#' time_range boundaries.
#'
#' @param work_data Data frame returned by \code{prep_data()}.
#' @param time_range Datetime or numeric vector, length 2. Indicates limits of
#'   x-axis in main and detail plots.
#' @param filter_event Character name of selected event type. Can be one of the
#'   entries of \code{work_data$group}.
#'
#' @return Data frame with same structure as \code{work_data}. All start and end
#'   variables were modified according to the chosen time range, so that no date
#'   or day is outside this range. Events were filtered according to event type
#'   filter settings.
#'
#' @keywords internal
#'
create_plot_data <- function(work_data, time_range, filter_event) {
  if (lubridate::is.timepoint(time_range)) {
    first <- "start_dt_var"
    last <- "end_dt_var"
    first_exp <- "start_exp"
    last_exp <- "end_exp"
  } else {
    first <- "start_dy_var"
    last <- "end_dy_var"
    first_exp <- "start_exp_day"
    last_exp <- "end_exp_day"
    time_range <- as.numeric(time_range)
  }

  work_data <- work_data %>%
    dplyr::filter(.data$group %in% filter_event)

  if (nrow(work_data) == 0) {
    return(work_data)
  }

  work_data <- work_data %>%
    dplyr::mutate(
      # Add arrow positions for open intervals
      arrow_left = dplyr::case_when(
        !is.na(.data[[first]]) & is.na(.data[[last]]) ~ FALSE, # because then it's a timepoint
        !is.na(.data[[first]]) & .data[[first]] < time_range[1] & .data[[last]] > time_range[1] ~ TRUE,
        !is.na(.data[[first_exp]]) & .data[[first_exp]] < time_range[1] & .data[[last_exp]] > time_range[1] ~ TRUE,
        TRUE ~ .data[["arrow_left"]]
      ),
      arrow_right = dplyr::case_when(
        !is.na(.data[[first]]) & is.na(.data[[last]]) ~ FALSE, # because then it's a timepoint
        !is.na(.data[[last]]) & .data[[last]] > time_range[2] & .data[[first]] < time_range[2] ~ TRUE,
        (!is.na(.data[[last_exp]]) & .data[[last_exp]] > time_range[2] & .data[[first_exp]] < time_range[2]) ~ TRUE,
        TRUE ~ .data[["arrow_right"]]
      ),
      # For interval events, add columns to determine start/end of the lines
      xmin = dplyr::case_when(
        .data[[first]] < time_range[1] & !is.na(.data[[last]]) & .data[[last]] >= time_range[1] ~ time_range[1],
        .data[[first]] >= time_range[1] & .data[[first]] <= time_range[2] ~ .data[[first]],
        TRUE ~ NA
      ),
      xmax = dplyr::case_when(
        .data[[last]] <= time_range[2] & .data[[last]] >= time_range[1] ~ .data[[last]],
        .data[[last]] > time_range[2] & .data[[first]] <= time_range[2] ~ time_range[2],
        TRUE ~ NA
      ),
      xmin_exp = dplyr::case_when(
        .data[[first_exp]] < time_range[1] & !is.na(.data[[last_exp]]) &
          .data[[last_exp]] >= time_range[1] ~ time_range[1],
        .data[[first_exp]] >= time_range[1] & .data[[first_exp]] <= time_range[2] ~ .data[[first_exp]],
        TRUE ~ NA
      ),
      xmax_exp = dplyr::case_when(
        .data[[last_exp]] <= time_range[2] & .data[[last_exp]] >= time_range[1] ~ .data[[last_exp]],
        .data[[last_exp]] > time_range[2] & .data[[first_exp]] <= time_range[2] ~ time_range[2],
        TRUE ~ NA
      ),

      # Additional columns for geoms
      timepoints = dplyr::if_else(!is.na(.data$xmin) & is.na(.data$xmax), .data$xmin, NA),
      interval_points = dplyr::if_else(
        !is.na(.data$xmin) & !is.na(.data$xmax) & .data$xmin == .data$xmax,
        .data$xmin,
        NA
      )
    )

  return(work_data)
}


#' Create plot for main view of the \pkg{dv.clinlines} module
#'
#' \code{create_main_plot} returns a ggplot object in consideration of chosen
#' x-axis scale, y-axis sorting type, and time range.
#'
#' @param work_data Data frame returned by \code{create_plot_data()}
#' @param y_sort Character name of selected y-axis sorting type of the main
#'   plot. Can be either "alphanum" or "earliest".
#' @param colors  Named character vector of color hex codes. Must be named after
#'   event types. Must be of the same length as there are distinct event types to
#'   plot. Can be generated by a call to \code{color_lookup()}.
#' @param height Box/row height for each subject. Numeric.
#'   Must be a single value.
#' @inheritParams create_plot_data
#' @inheritParams prep_data
#'
#' @return A ggplot object - the main plot of  \pkg{dv.clinlines}.
#'
#' @importFrom rlang .data
#' @keywords internal

create_main_plot <- function(work_data,
                             y_sort,
                             time_range,
                             colors,
                             height,
                             subjid_var) {
  # Sort y aes according to user input
  if (y_sort == "alphanum") {
    sort_by <- subjid_var
  } else {
    sort_by <- "earliest"
  }

  # Set x scale and columns to be plotted
  if (lubridate::is.timepoint(time_range)) {
    x_scale <- ggplot2::scale_x_datetime(
      limits = c(time_range[1], time_range[2]),
      position = "top",
      sec.axis = ggplot2::dup_axis()
    )
    point_exp <- "start_exp"
    x_lab <- "Time range: Date"
  } else {
    x_scale <- ggplot2::scale_x_continuous(
      limits = c(time_range[1], time_range[2]),
      position = "top",
      sec.axis = ggplot2::dup_axis()
    )
    point_exp <- "start_exp_day"
    x_lab <- "Time range: Study Days"
  }


  if (nrow(work_data) == 0) {
    return(NULL)
  }

  if (all(is.na(work_data[["xmin"]])) && all(is.na(work_data[["xmin_exp"]]))) {
    return("No_start_data")
  }

  main_p <- work_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = stats::reorder(.data[[subjid_var]], dplyr::desc(get(sort_by))),
        color = .data$group
      )
    ) +
    # Draw intervals
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data[["xmin"]], xmax = .data[["xmax"]]),
      position = ggplot2::position_dodge2(0.9),
      linewidth = 2 / 140 * height,
      na.rm = TRUE
    ) +
    # Draw intervals that geom_linerange ignores
    ggplot2::geom_point(
      ggplot2::aes(x = .data[["interval_points"]]),
      position = ggplot2::position_dodge2(0.9),
      size = 2 / 140 * height,
      shape = 15,
      na.rm = TRUE
    ) +
    # Draw timepoints
    ggplot2::geom_point(
      ggplot2::aes(x = .data[["timepoints"]]),
      position = ggplot2::position_jitter(height = 0.1, width = 0),
      size = height / 20,
      na.rm = TRUE
    ) +
    x_scale

  # Only if Drug Administration is defined
  if (!all(is.na(work_data$trt_var))) {
    x <- sort(unique(stats::na.omit(work_data$exp_dose)))

    shapes <- dplyr::case_when(
      x == "decrease" ~ 25,
      x == "increase" ~ 24,
      x == "start/equal" ~ 23
    )
    names(shapes) <- x
    symbol_color <- colors[unique(work_data[!is.na(work_data$xmin_exp), ]$group)]

    trt_per_subject <- work_data |>
      dplyr::filter(.data[["group"]] %in% names(symbol_color)) |>
      dplyr::group_by(subject_id) |>
      dplyr::distinct(group) |>
      dplyr::count()

    if (any(trt_per_subject$n > 1)) {
      position <- 0.5 - (length(symbol_color) + 1) * 0.1
      for (i in seq_along(symbol_color)) {
        data <- main_p$data |>
          dplyr::filter(
            .data[["group"]] == names(symbol_color)[i]
          )
        # Transform symbol_color[i] into a vector to fix legend
        # (ggplot2 doing ggplot2 things...)
        symbol_colors <- rep(symbol_color[i], nrow(data))
        pos <- position + 0.1 * i
        main_p <- main_p +
          ggplot2::geom_linerange(
            data = data,
            ggplot2::aes(xmin = .data[["xmin_exp"]], xmax = .data[["xmax_exp"]], fill = group),
            color = symbol_colors,
            position = ggplot2::position_nudge(y = pos),
            linewidth = 2 / 140 * height,
            na.rm = TRUE
          ) +
          ggplot2::geom_point(
            data = data,
            ggplot2::aes(x = .data[[point_exp]], shape = .data[["exp_dose"]]),
            na.rm = TRUE,
            fill = symbol_colors,
            color = "black",
            position = ggplot2::position_nudge(y = pos),
            size = height / 20
          )
      }
    } else {
      symbol_colors <- sapply(work_data$group, function(x) {
        ifelse(x %in% names(symbol_color), symbol_color[[x]], NA)
      }, USE.NAMES = FALSE)

      # Add drug administration events
      main_p <- main_p +
        ggplot2::geom_linerange(
          ggplot2::aes(xmin = .data[["xmin_exp"]], xmax = .data[["xmax_exp"]]),
          color = symbol_colors,
          position = ggplot2::position_nudge(y = 0.35),
          linewidth = 2 / 140 * height,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = .data[[point_exp]], shape = .data[["exp_dose"]]),
          na.rm = TRUE,
          fill = symbol_colors,
          color = "black",
          position = ggplot2::position_nudge(y = 0.35),
          size = height / 20
        )
    }

    if (length(shapes) > 0) {
      main_p <- main_p +
        ggplot2::scale_shape_manual(
          name = "Dose Change:",
          values = shapes,
          na.translate = FALSE,
          breaks = x
        )
    }
  }

  # Add arrows for open intervals
  if (height >= 120) {
    main_p <- main_p +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data[["xmax"]],
          label = ifelse(.data$arrow_right, sprintf("\u2192"), "")
        ),
        position = ggplot2::position_dodge2(0.9),
        size = 5,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data[["xmin"]],
          label = ifelse(.data$arrow_left, sprintf("\u2190"), "")
        ),
        position = ggplot2::position_dodge2(0.9),
        size = 5,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data[["xmax_exp"]],
          label = ifelse(.data$arrow_right, sprintf("\u2192"), "")
        ),
        position = ggplot2::position_nudge(y = 0.35),
        size = 5,
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = .data[["xmin_exp"]],
          label = ifelse(.data$arrow_left, sprintf("\u2190"), "")
        ),
        position = ggplot2::position_nudge(y = 0.35),
        size = 5,
        na.rm = TRUE
      )
  }

  main_p <- main_p +
    # Layers for better appearance
    ggplot2::geom_hline(
      yintercept = seq(
        from = 1.5,
        to = length(unique(work_data[[subjid_var]])) - 0.5,
        by = ifelse(length(unique(work_data[[subjid_var]])) < 2, -1, 1)
      ),
      lwd = 0.5,
      color = "grey"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_text(face = "italic"),
      axis.title.x = ggplot2::element_text(size = 11, color = "grey"),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 15)
    ) +
    # Set colors manually to fix them
    ggplot2::scale_color_manual(name = "Event Type:", values = colors) +
    # Do not allow default expansion on top and bottom of the plot
    ggplot2::scale_y_discrete(expand = ggplot2::expansion()) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(ncol = 1, order = 1),
      shape = ggplot2::guide_legend(ncol = 1, order = 2)
    ) +
    ggplot2::labs(title = "Event Occurrence over Time") +
    ggplot2::xlab(x_lab)

  return(main_p)
}



#' Generates ggplot_build table and returns only rows with data near mouse cursor
#'
#' @param p ggplot object that represents the main plot.
#' @param hover Hover object of the main plot.
#'
#' @return A list of two data frames extracted from a call to
#' \code{ggplot2:::ggplot_build()}, and filtered by data points that lie within +/- 5 px
#' (on y-axis) from cursor position. One data frame represents interval data, the other
#' timepoints.
#'
#' @keywords internal
create_ggdata_y <- function(p, hover) {
  # Use ggplot2 internal to have access to the data producing the plot
  ggdata <- ggplot2:::ggplot_build(p)

  # Calculate y position of the mouse cursor +/- 5 pixels
  range_y_domain <- hover$domain$top - hover$domain$bottom
  range_y_px <- hover$range$bottom - hover$range$top

  domain_5px <- range_y_domain / range_y_px * 5
  range_hover_y <- c(hover$y - domain_5px, hover$y + domain_5px)

  # Restrict data points to those within the +/- 5 px range
  interval_data <- ggdata$data[[1]] %>%
    dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
    dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))

  timepoint_data <- ggdata$data[[2]] %>%
    dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
    dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))

  interval_point_data <- ggdata$data[[3]] %>%
    dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
    dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))



  if (length(ggdata$data) > 4) {
    df_list <- lapply(4:length(ggdata$data), function(x) {
      dataset_names <- names(ggdata$data[[x]])
      if ("y" %in% dataset_names && !("shape" %in% dataset_names)) {
        ret_data <- ggdata$data[[x]] %>%
          dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
          dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))

        if (nrow(ret_data) > 0) {
          ret_data
        } else {
          NULL
        }
      }
    })

    if (length(purrr::compact(df_list)) > 0) {
      interval_exp_data <- purrr::compact(df_list)[[1]]
    } else {
      interval_exp_data <- ggdata$data[[4]] %>%
        dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
        dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))
    }

  } else {
    # Drug_admin is always layer 4
    interval_exp_data <- ggdata$data[[4]] %>%
      dplyr::mutate(y = as.numeric(.data[["y"]])) %>% # to silence dplyr warning
      dplyr::filter(dplyr::between(.data[["y"]], range_hover_y[1], range_hover_y[2]))
  }

  return(list(interval_data, timepoint_data, interval_point_data, interval_exp_data))
}


#' Create info box for hovering over the main plot of \pkg{dv.clinlines}
#'
#' \code{create_hover_info} returns a shiny wellPanel with further event
#' information for the hovered-on subject in the main plot.
#'
#' @param hover Hover event of the main plot.
#' @param ggdata_y List of data frames as returned by \code{create_ggdata_y()}.
#' @param initial_data Data frame returned by \code{prep_data()}.
#' @param color_map Named vector of hexadecimal color codes, as returned by
#'   \code{color_lookup()}. Names represent event groups.
#' @param x_scale Character string containing either "date", or "day". Reveals current
#'   x-axis setting.
#' @param y_screen_pct Y mouse position on window screen (between 0 and 1)
#' @param time_range Datetime or numeric vector, length 2. Indicates limits of
#'   x-axis in main and detail plots.
#'
#' @return A shiny wellPanel object which displays each event type (group),
#'   timepoint date (moment), interval dates (start, end), and further details
#'   to the event (details) for the hovered subject of the main plot.
#' @importFrom rlang .data
#'
#' @keywords internal
create_hover_info <- function(hover, ggdata_y, initial_data, color_map, x_scale, y_screen_pct, time_range) {
  style <- create_tooltip(hover, y_screen_pct)

  # Get y position of the click & find the matching subject identifier
  position <- round(hover$y)
  subject <- hover$domain$discrete_limits$y[[position]]

  # Extract data near mouse cursor by narrowing event groups ( i.e. y direction) and
  # units (i.e. x direction) down to +/- 5px
  x_range <- calc_x_range(hover)
  rel_groups <- get_groups(ggdata_y, color_map, x_range)
  near_data <- filter_nearest(initial_data, subject, rel_groups, x_range, x_scale, time_range)

  if (nrow(near_data) == 0) {
    hover_df <- "Please move the cursor near an event start/end."
  } else {
    hover_df <- print(xtable::xtable(near_data), type = "html", print.results = FALSE)
  }

  # Actual tooltip
  hover_info <- list(
    style = style,
    message = paste0(
      "<i> Click for more details </i> <br/>",
      "Subject ID: ", subject, "<br/>"
    ),
    df = hover_df
  )

  return(hover_info)
}


#' Create tooltip at mouse position
#'
#' @inheritParams create_hover_info
#'
#' @return Style property for the tooltip
#' @keywords internal

create_tooltip <- function(hover, y_screen_pct) {
  # Calculate cursor position as percent
  left_pct <- (hover$x - hover$domain$left) /
    (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) /
    (hover$domain$top - hover$domain$bottom)

  # Distance from left and top side of the picture in pixels
  left_px <- hover$coords_css$x
  top_px <- hover$coords_css$y

  # Calculating zoom of browser as percent
  # Adding 1.2 because width of plot output is 120%
  zoom <- hover$img_css_ratio$x * 1.2 * 100
  # Distance from right of the picture in pixels minus the pixels of the zoom
  right_px <- hover$range$right - left_px - hover$range$right / zoom * (zoom - 100)

  # Calculating zoom of browser as percent
  zoom_bottom <- hover$img_css_ratio$x * 100
  # Distance from bottom of the picture in pixels minus the pixels of the zoom
  bottom_px <- hover$range$bottom - top_px - hover$range$bottom / zoom_bottom * (zoom_bottom - 100)

  # If mouse is on left side of the screen use position left else position right
  if (left_pct > 0.5) {
    pos_x <- paste0("right:", right_px + 6, "px;")
  } else {
    pos_x <- paste0("left:", left_px + 2, "px;")
  }

  # If mouse is on top side of the screen use position top else position bottom
  if (y_screen_pct > 0.5) {
    pos_y <- paste0("bottom:", bottom_px + 18, "px;")
  } else {
    pos_y <- paste0("top:", top_px + 2, "px;")
  }


  # Create style property for tooltip
  # z-index makes sure the tooltip will be on top
  style <- paste0(
    "position:absolute;
                    z-index:100;
                    background-color: rgba(190, 228, 253, 0.85);
                    color: rgba(127, 127, 127, 1);",
    pos_x, pos_y
  )

  return(style)
}



#' Calculate range of dates/study days that fall into cursor position +/- 5 px (x-axis)
#'
#' @inheritParams create_hover_info
#'
#' @return Numeric vector of length 2.
#' @keywords internal
calc_x_range <- function(hover) {
  # Calculate pixel range
  range_x_domain <- as.numeric(hover$domain$right - hover$domain$left)
  range_x_px <- hover$range$right - hover$range$left

  # Get number of pixels per unit
  px_unit <- range_x_px / range_x_domain

  hovered_x <- hover$x
  n_units <- 10 / px_unit

  # Dates that fall into cursor x position +/- 5 px
  x_range <- c(hovered_x - n_units, hovered_x + n_units)

  return(x_range)
}


#' Extract group names of events near cursor position
#'
#' @param x_range Vector of start and end values on x-axis level as provided by
#'   \code{calc_x_range}.
#' @inheritParams create_hover_info
#'
#' @return Character vector of event names.
#' @keywords internal
get_groups <- function(ggdata_y, color_map, x_range) {
  # Intervals
  ggdata_y_interv <- ggdata_y[[1]] %>%
    dplyr::mutate(
      # Assign event names based on the color
      event = purrr::map_chr(
        .data[["colour"]], ~ names(color_map[which(color_map == .x)])
      ),
      xmin = as.numeric(.data[["xmin"]]), # to silence dplyr warning
      xmax = as.numeric(.data[["xmax"]])
    ) %>%
    dplyr::filter(
      dplyr::between(.data[["xmin"]], x_range[1], x_range[2]) |
        dplyr::between(.data[["xmax"]], x_range[1], x_range[2])
    )

  # Timepoints
  ggdata_y_points <- ggdata_y[[2]] %>%
    dplyr::mutate(
      # assign event names based on the color
      event = purrr::map_chr(
        .data[["colour"]], ~ names(color_map[which(color_map == .x)])
      ),
      x = as.numeric(.data[["x"]]) # to silence dplyr warning
    ) %>%
    dplyr::filter(
      dplyr::between(.data[["x"]], x_range[1], x_range[2])
    )

  # Interval points
  ggdata_y_inter_pt <- ggdata_y[[3]] %>%
    dplyr::mutate(
      # assign event names based on the color
      event = purrr::map_chr(
        .data[["colour"]], ~ names(color_map[which(color_map == .x)])
      ),
      x = as.numeric(.data[["x"]]) # to silence dplyr warning
    ) %>%
    dplyr::filter(
      dplyr::between(.data[["x"]], x_range[1], x_range[2])
    )


  ggdata_y_interv_exp <- ggdata_y[[4]] %>%
    dplyr::mutate(
      # Assign event names based on the color
      event = purrr::map_chr(
        .data[["colour"]], ~ ifelse(.x != "<NA>", names(color_map[which(color_map == .x)]), NA)
      ),
      xmin = as.numeric(.data[["xmin"]]), # to silence dplyr warning
      xmax = as.numeric(.data[["xmax"]])
    ) %>%
    dplyr::filter(
      dplyr::between(.data[["xmin"]], x_range[1], x_range[2]) |
        dplyr::between(.data[["xmax"]], x_range[1], x_range[2])
    )

  groups <- unique(
    c(
      ggdata_y_interv[["event"]],
      ggdata_y_points[["event"]],
      ggdata_y_inter_pt[["event"]],
      ggdata_y_interv_exp[["event"]]
    )
  )

  return(groups)
}


#' Filter prepared event data by cursor position.
#'
#' @param subject Character sting of one unique subject identifier.
#' @param rel_groups Character vector of event names as returned by \code{get_groups()}.
#' @param x_range Numeric vector of x values as returned by \code{calc_x_range()}.
#' @inheritParams create_hover_info
#'
#' @return A data frame containing only those events of which start/end/timepoint lie
#' within an area determined by \code{rel_groups} and \code{x_range}.
#'
#' @keywords internal
filter_nearest <- function(initial_data, subject, rel_groups, x_range, x_scale, time_range) {
  if (x_scale == "date") {
    lower <- "start_dt_var"
    upper <- "end_dt_var"
    lower_exp <- "start_exp"
    upper_exp <- "end_exp"
    x_range <- as.POSIXct(x_range, origin = "1970-01-01")
  } else {
    lower <- "start_dy_var"
    upper <- "end_dy_var"
    lower_exp <- "start_exp_day"
    upper_exp <- "end_exp_day"
  }

  near_data <- initial_data %>%
    dplyr::filter(
      # Only display events near cursor (y position +/- 5px)
      .data[["subject_id"]] == subject,
      .data[["group"]] %in% rel_groups, # sufficient as events are plotted groupwise

      # Only display events near cursor (x position +/- 5px)
      dplyr::between(.data[[lower]], x_range[1], x_range[2]) |
        dplyr::between(.data[[upper]], x_range[1], x_range[2]) |
        dplyr::between(.data[[lower_exp]], x_range[1], x_range[2]) |
        dplyr::between(.data[[upper_exp]], x_range[1], x_range[2]) |
        (.data[[lower]] < time_range[[1]] & .data[[upper]] >= x_range[[1]]) |
        (.data[[upper]] > time_range[[2]] & .data[[lower]] <= x_range[[2]])
    ) %>%
    dplyr::mutate(
      # Display original values
      start_dt_var = dplyr::if_else(.data[["start_missing"]], "NA", as.character(.data[["start_dt_var"]])),
      end_dt_var = dplyr::if_else(.data[["end_missing"]], "NA", as.character(.data[["end_dt_var"]])),
      start_exp = as.character(.data[["start_exp"]]),
      end_exp = as.character(.data[["end_exp"]]),
      # Display days without digits
      dplyr::across(c("start_dy_var", "end_dy_var", "start_exp_day", "end_exp_day"), as.integer),
      # Display original values
      start_dy_var = dplyr::if_else(.data[["start_missing"]], "NA", as.character(.data[["start_dy_var"]])),
      end_dy_var = dplyr::if_else(.data[["end_missing"]], "NA", as.character(.data[["end_dy_var"]])),

      # Format table
      dates = collapse_cols(.data[["start_dt_var"]], .data[["end_dt_var"]], .data[["start_exp"]], .data[["end_exp"]]),
      days = collapse_cols(
        .data[["start_dy_var"]],
        .data[["end_dy_var"]],
        .data[["start_exp_day"]],
        .data[["end_exp_day"]]
      )
    ) %>%
    dplyr::select(tidyselect::all_of(c("detail_var", "dates", "days", "group"))) %>%
    dplyr::rename(
      dplyr::all_of(c("Details" = "detail_var", "Time Range" = "dates", "Study Days" = "days", "Event" = "group"))
    )

  return(near_data)
}


#' Collapse timepoint and interval columns of hover info table and remove empty rows
#'
#' @param col1 Column holding start dates/days from interval events.
#' @param col2 Column holding end dates/days from interval events.
#' @param col3 Column holding start dates/days from exp interval events.
#' @param col4 Column holding end dates/days from exp interval events.
#'
#' @return Character vector of collapsed dates/days.
#' @keywords internal
#'
collapse_cols <- function(col1, col2, col3, col4) {
  # Collapse col1 and col2 into one column
  step1 <- purrr::map2_chr(col1, col2, function(x, y) {
    if (is.na(x) & is.na(y)) {
      return(NA)
    } else if (!is.na(x) & is.na(y)) {
      return(x)
    } else {
      return(paste(x, "-", y))
    }
  })
  # Collapse col3 and col4 into one column
  step2 <- purrr::map2_chr(col3, col4, function(x, y) {
    if (is.na(x) & is.na(y)) {
      return(NA)
    } else if (!is.na(x) & is.na(y)) {
      return(x)
    } else {
      return(paste(x, "-", y))
    }
  })
  # Collapse col12 and col34 into one column
  step3 <- purrr::map2_chr(step1, step2, function(x, y) {
    if (is.na(x)) {
      return(y)
    } else {
      return(x)
    }
  })


  return(step3)
}
