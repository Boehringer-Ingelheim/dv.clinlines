# Tests for for create_plot_data() ----
df <- prep_data(add_ids(prep_dummy_data(n = 50)))

range_date <- as.POSIXct(c("2024-01-24 13:25:09 CET", "2024-02-02 13:25:09 CET"))
range_day <- c(12, 21)

plot_date <- create_plot_data(df, time_range = range_date, filter_event = unique(df$group))
plot_day <- create_plot_data(df, time_range = range_day, filter_event = "Informed Consent")

test_that("create_plot_data() returns only events to plot" %>%
  vdoc[["add_spec"]](specs$plot_specs$events), {
  expect_equal(unique(plot_day$group), "Informed Consent")
})

test_that(
  "create_plot_data() sets interval start values to time range start value" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$time_range),
  {
    expect_true(all(na.omit(plot_date$xmin) >= range_date[1]))
    expect_true(all(na.omit(plot_day$xmin) >= range_day[1]))
  }
)

test_that(
  "create_plot_data() sets interval end values to time range end value" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$time_range),
  {
    expect_true(all(na.omit(plot_date$xmax) <= range_date[2]))
    expect_true(all(na.omit(plot_day$xmax) <= range_day[2]))
  }
)

test_that("create_plot_data() returns a data frame", {
  expect_true("data.frame" %in% class(plot_date))
})

test_that(
  "create_plot_data() includes all the same subjects as the corresponding adsl data" %>%
    vdoc[["add_spec"]](specs$plot_specs$events),
  {
    expect_equal(unique(plot_date$subject_id), unique(df$subject_id))
    expect_equal(unique(plot_day$subject_id), unique(df$subject_id))
  }
)

test_that(
  "create_plot_data() only includes events that are specified by the event filter" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$event_type_filter),
  {
    expect_equal(unique(plot_day$group), "Informed Consent")
  }
)

test_that("create_plot_data() does not return rows if no event is selected" %>%
  vdoc[["add_spec"]](specs$sidebar_specs$event_type_filter), {
  plot_date <- create_plot_data(df, time_range = range_date, filter_event = NULL)
  expect_equal(length(plot_date$group), 0)
})


# Prepare next tests ----
df <- prep_data(
  add_ids(prep_dummy_data(n = 5)),
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
      ),
      "Informed Consent" = list(
        start_dt_var = "RFICDT",
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
  )
)

colors <- color_lookup(unique(df$group), NULL)
time_range <- lubridate::ymd_hm(c("2012-08-05 12:00", "2022-01-03 12:35"))
p_df <- create_plot_data(df, time_range, unique(df$group))
p <- create_main_plot(p_df, "earliest", time_range, colors, 50, "subject_id")

# How to get the following list:
# 1. Add dput(hover) within create_tooltip()
# 2. Run mock_with_mm() and make sure that the app is set-up in a way that all events like shown in p are displayed
# 3. Use global filter to filter for the 5 subjects as below
# 4. Switch to date, select the correct time range, sort y-axis correctly, and choose correct boxheight
# 4. Hover over an Adverse Event and wait until app is ready. Remove curser quickly!
# 5. Use last console output as hover variable
hover <- list(
  x = 1344401933.87767,
  y = 4.57800323024878,
  coords_css = list(x = 132.171875, y = 209.015625),
  coords_img = list(x = 132.171875, y = 209.015625),
  img_css_ratio = list(x = 1L, y = 1L),
  mapping = list(
    x = NULL,
    xmin = ".data[[\"xmin\"]]",
    xmax = ".data[[\"xmax\"]]",
    y = "stats::reorder(.data[[\"subject_id\"]], dplyr::desc(get(sort_by)))",
    colour = ".data$group"
  ),
  domain = list(
    left = 1329272640L,
    right = 1656020160L,
    bottom = 0.551323529411765,
    top = 5.44854838709677,
    discrete_limits = list(y = list("01-701-1034", "01-701-1033", "01-701-1015", "01-701-1028", "01-701-1023"))
  ),
  range = list(
    left = 78.4578339041096,
    right = 1238.5205479452,
    bottom = 412.849315068493,
    top = 164.947945205479
  ),
  log = list(x = NULL, y = NULL)
)

# Tests for create_ggdata_y() ----
y_range <- c(hover$y - 0.11, hover$y + 0.11) # assume y-distance of 0.11 per 5 px
ggdata_y <- create_ggdata_y(p, hover)

test_that("create_ggdata_y() returns only entries near hover object (y-axis wise)", {
  purrr::walk(ggdata_y[[1]]$y, ~ expect_gte(.x, y_range[1]))
  purrr::walk(ggdata_y[[1]]$y, ~ expect_lte(.x, y_range[2]))
})

test_that("create_ggdata_y() returns a list of data frames", {
  expect_type(ggdata_y, "list")
  purrr::walk(ggdata_y, ~ expect_true("data.frame" %in% class(.x)))
})


# Tests for calc_x_range() ----
test_that(
  "calc_x_range() returns a vector of x values for cursor position +/- ~10px",
  {
    px_range <- hover$range$right - hover$range$left
    domain_range <- hover$domain$right - hover$domain$left
    dom_10px <- domain_range / px_range * 10
    expected <- c(hover$x - dom_10px, hover$x + dom_10px)
    observed <- calc_x_range(hover)

    expect_equal(observed, expected, tolerance = 1)
    expect_length(observed, 2)
    expect_true(is.vector(observed))
  }
)


# Tests for get_groups() ----
ggdata_y <- create_ggdata_y(p, hover)
x_range <- calc_x_range(hover)

test_that("get_groups() returns group names of hovered-over events", {
  expected <- "Adverse Events"
  observed <- get_groups(ggdata_y, colors, x_range)

  expect_equal(observed, expected)
  expect_type(observed, "character")
  expect_true(is.vector(observed))
  expect_true(length(unique(observed)) == 1)
})


# Tests for filter_nearest() ----
subject <- "01-701-1023"
rel_groups <- "Adverse Events"
x_range <- calc_x_range(hover)
x_scale <- "date"

test_that("filter_nearest() returns a subset of initial_data", {
  expected <- data.frame(
    Details = c("ERYTHEMA", "ERYTHEMA", "ERYTHEMA", "ATRIOVENTRICULAR BLOCK SECOND DEGREE"),
    Time_Range = c("2012-08-07 - NA", "2012-08-07 - 2012-08-30", "2012-08-07 - 2012-08-30", "2012-08-26 - NA"),
    Study_Days = c("3 - NA", "3 - 26", "3 - 26", "22 - NA"),
    Event = rep("Adverse Events", times = 4)
  ) %>%
    dplyr::rename(dplyr::all_of(c("Time Range" = "Time_Range", "Study Days" = "Study_Days")))
  observed <- filter_nearest(df, subject, rel_groups, x_range, x_scale, time_range)

  expect_equal(as.character(observed$Details), expected$Details)
  expect_equal(as.character(observed$`Time Range`), expected$`Time Range`)
  expect_equal(as.character(observed$`Study Days`), expected$`Study Days`)
  expect_equal(as.character(observed$Event), expected$Event)
})


# Tests for collapse_cols() ----
test_that("collapse_cols() returns a single character vector", {
  observed <- collapse_cols(
    as.character(df$start_dt_var),
    as.character(df$end_dt_var),
    as.character(df$start_exp),
    as.character(df$end_exp)
  )

  expect_equal(length(observed), nrow(df))
  expect_type(observed, "character")
  expect_true(is.vector(observed))
})


# Tests for create_hover_info() ----
ggdata_y <- create_ggdata_y(p, hover)

test_that(
  "create_hover_info() returns a list that specifies the tooltip content" %>%
    vdoc[["add_spec"]](specs$plot_specs$hovering),
  {
    observed <- create_hover_info(hover, ggdata_y, df, colors, "date", 0.2, time_range)

    expect_equal(length(observed), 3)
    expect_named(observed, c("style", "message", "df"))
  }
)


# Tests for create tooltip() ----
test_that(
  "tooltip() returns a character vector that defines the style property" %>%
    vdoc[["add_spec"]](specs$plot_specs$hovering),
  {
    observed_top <- create_tooltip(hover, 0.3)
    expect_length(observed_top, 1)
    expect_type(observed_top, "character")
    expect_true(grepl("top", observed_top))
    expect_true(grepl("left", observed_top))

    hover2 <- hover
    hover2$x <- hover2$x * 2
    observed_bottom <- create_tooltip(hover2, 0.7)
    expect_true(grepl("bottom", observed_bottom))
    expect_true(grepl("right", observed_bottom))
  }
)


# Tests for create_main_plot() ----
test_that(
  "create_main_plot() returns a string when plot cannot be created due to settings" %>%
    vdoc[["add_spec"]](specs$plot_specs$errors),
  {
    df <- prep_data(add_ids(prep_dummy_data(n = 50)))

    range_date <- as.POSIXct(c("2024-01-24 13:25:09 CET", "2024-02-02 13:25:09 CET"))
    range_day <- c(12, 21)
    plot_data <- create_plot_data(df, time_range = range_date, filter_event = unique(df$group))

    plot <- create_main_plot(
      plot_data,
      y_sort = "alphanum",
      time_range = range_date,
      colors = colors,
      height = 50,
      subjid_var = "USUBJID"
    )
    expect_type(plot, "character")

    plot_null <- create_main_plot(
      data.frame(),
      y_sort = "alphanum",
      time_range = range_date,
      colors = colors,
      height = 50,
      subjid_var = "USUBJID"
    )
    expect_null(plot_null)
  }
)

test_that("create_main_plot() returns a plot object for plottable settings", {
  df <- prep_data(add_ids(prep_dummy_data(n = 50)))

  range_date <- lubridate::ymd_hm(c("2012-08-01T00:00", "2014-01-01T01:00"))
  range_day <- c(12, 21)
  plot_data <- create_plot_data(df, time_range = range_date, filter_event = unique(df$group))

  plot <- create_main_plot(
    plot_data,
    y_sort = "alphanum",
    time_range = range_date,
    colors = colors,
    height = 50,
    subjid_var = "subject_id"
  )
  expect_true("ggplot" %in% class(plot))
})

## create data for testServer tests
df <- prep_data(add_ids(prep_dummy_data(n = 2)))
## create color vector for testServer tests
colors <- color_lookup(c("Treatment Start", "Adverse Events"), NULL)
## set changed to an arbitrary value
changed <- shiny::reactive(1)
## create function to be used for testServer tests
server_func <- function(id, initial_data, changed, colors_groups, ms = 100) {
  mod_main_view_server(
    module_id = id, initial_data = initial_data, changed = changed, colors_groups = colors_groups, ms = ms
  )
}

test_that(
  "create_main_plot() displays events per subject and milestones as bullets/periods as horizontal lines" %>%
    vdoc[["add_spec"]](c(specs$plot_specs$events, specs$plot_specs$event_types)),
  {
    shiny::testServer(
      server_func,
      args = list(
        id = "main_view",
        initial_data = shiny::reactive({
          df
        }),
        changed = changed,
        colors_group = shiny::reactive({
          colors
        })
      ),
      {
        session$setInputs(
          date_range = c("2012-08-01T00:00", "2014-01-01T01:00"),
          day_range = c(12, 21),
          x_scale = "date",
          y_sort = "alphanum",
          height = 50,
          filter_event = c("Treatment Start", "Adverse Events")
        )

        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)
      }
    )
  }
)

test_that(
  "create_main_plot() returns a plot that scales the x-axis either as date or as numeric" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$date_day),
  {
    shiny::testServer(
      server_func,
      args = list(
        id = "main_view",
        initial_data = shiny::reactive({
          df
        }),
        changed = changed,
        colors_group = shiny::reactive({
          colors
        })
      ),
      {
        session$setInputs(
          date_range = c("2012-08-01T00:00", "2015-01-01T01:00"),
          day_range = c(0, 50),
          x_scale = "date",
          y_sort = "alphanum",
          height = 50,
          filter_event = c("Treatment Start", "Adverse Events")
        )

        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)

        session$setInputs(x_scale = "day")
        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)
      }
    )
  }
)

test_that(
  "create_main_plot() returns a plot that sorts the y-axis either alphanumerically or by earliest event" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$sorting),
  {
    shiny::testServer(
      server_func,
      args = list(
        id = "main_view",
        initial_data = shiny::reactive({
          df
        }),
        changed = changed,
        colors_group = shiny::reactive({
          colors
        })
      ),
      {
        session$setInputs(
          date_range = c("2012-08-01T00:00", "2015-01-01T01:00"),
          day_range = c(0, 50),
          x_scale = "date",
          y_sort = "alphanum",
          height = 50,
          filter_event = c("Treatment Start", "Adverse Events")
        )

        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)

        session$setInputs(y_sort = "earliest")
        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)
      }
    )
  }
)

test_that(
  "create_main_plot() returns a plot that determines vertical space per subject according to user settings" %>%
    vdoc[["add_spec"]](specs$sidebar_specs$boxheight),
  {
    shiny::testServer(
      server_func,
      args = list(
        id = "main_view",
        initial_data = shiny::reactive({
          df
        }),
        changed = changed,
        colors_group = shiny::reactive({
          colors
        })
      ),
      {
        session$setInputs(
          date_range = c("2012-08-15T00:00", "2015-01-01T01:00"),
          day_range = c(0, 50),
          x_scale = "date",
          y_sort = "alphanum",
          height = 30,
          filter_event = c("Treatment Start", "Adverse Events")
        )

        session$flushReact()
        expect_snapshot_output(plot_obj(), cran = TRUE)


        session$setInputs(height = 130)
        session$flushReact()
        # Test produces warnings regarding conversion failure of the arrows for open intervals.
        # Warnings are suppressed, since they do only appear in test server, not in the module server.
        suppressWarnings(
          expect_snapshot_output(plot_obj(), cran = TRUE)
        )
      }
    )
  }
)

test_that(
  "create_main_plot() returns a plot that indicates dose changes visually" %>%
    vdoc[["add_spec"]](specs$plot_specs$drug_admin_event),
  {
    df <- prep_data(add_ids(prep_dummy_data(n = 3)))
    colors <- color_lookup(c("Drug Administration", "Treatment Start"), NULL)

    shiny::testServer(
      server_func,
      args = list(
        id = "main_view",
        initial_data = shiny::reactive({
          df
        }),
        changed = changed,
        colors_group = shiny::reactive({
          colors
        })
      ),
      {
        session$setInputs(
          date_range = c("2012-08-15T00:00", "2015-01-01T01:00"),
          day_range = c(0, 50),
          x_scale = "date",
          y_sort = "alphanum",
          height = 50,
          filter_event = c("Treatment Start", "Drug Administration")
        )

        session$flushReact()

        expect_snapshot_output(plot_obj(), cran = TRUE)
      }
    )
  }
)


# Tests for check_valid_color() ----
test_that("check_valid_color() detects invalid colors", {
  color_palette <- c(
    "#1afe56", # hex color with 6 numbers/letters: valid
    "#AAFFBB", # hex color with 6 letters: valid
    "#225577", # hex color with 6 numbers: valid
    "#1e",     # hex color with only 2 numbers/letters: invalid
    "#lmnopq", # hex color with 6 invalid letters: invalid
    "blue",    # R color: valid
    "no_color" # no R color: invalid
  )
  expect_error(check_valid_color(color_palette), "^.*#1e, #lmnopq, no_color.*$")
})


# Tests for color_lookup() ----
test_that("color_lookup() returns a named vector of hex colors as default colors" %>%
  vdoc[["add_spec"]](specs$plot_specs$colors), {
  colors <- color_lookup(c("a", "b"), NULL)
  expect_named(colors, c("a", "b"), ignore.order = TRUE)
})

test_that("color_lookup() returns a customized color palette as is" %>%
  vdoc[["add_spec"]](specs$plot_specs$customizable_colors), {
  color_palette <- c("a" = "blue", "b" = "red")
  colors <- color_lookup(c("a", "b"), color_palette)
  expect_equal(colors, color_palette)
})

test_that("color_lookup() adds grey for missing events in customized color palette" %>%
  vdoc[["add_spec"]](specs$plot_specs$customizable_colors), {
  color_palette <- c("a" = "blue")
  colors <- color_lookup(c("a", "b"), color_palette)
  expected <- c("a" = "blue", b = "grey")
  expect_equal(colors, expected)
})
