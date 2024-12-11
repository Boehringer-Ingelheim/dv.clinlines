# Tests for mod_clinical_timelines_UI() ----
test_that("mod_clinical_timelines_UI() returns a shiny tagList", {
  ui <- mod_clinical_timelines_UI("test")

  expect_type(ui, "list")
  expect_true("shiny.tag.list" %in% class(ui))
})


# Tests for mod_main_view_server() ----
df <- prep_dummy_data(n = 5)
server_func <- function(id, data_name, dataset_list) {
  mod_clinical_timelines_server(module_id = id, data_name = data_name, dataset_list = dataset_list)
}

plot_click_li <- list(
  x = 1357205314.75161,
  y = 5.1280648515713,
  coords_css = list(x = 134.826721191406, y = 114.920455932617),
  coords_img = list(x = 148.313871467575, y = 126.412501525879),
  img_css_ratio = list(x = 1.10003321416547, y = 1.1),
  mapping = list(
    x = NULL,
    xmin = ".data[[\"xmin\"]]",
    xmax = ".data[[\"xmax\"]]",
    y = "stats::reorder(.data[[\"subject_id\"]], dplyr::desc(get(sort_by)))",
    colour = ".data$group"
  ),
  domain = list(
    left = 1340336160L,
    right = 1423686240L,
    bottom = 0.556428571428571,
    top = 5.444375,
    discrete_limits = list(y = list("01-701-1034", "01-701-1033", "01-701-1028", "01-701-1023", "01-701-1015"))
  ),
  range = list(
    left = 82.4231155552614,
    right = 407.987724892095,
    bottom = 371.570686068626,
    top = 109.450079979341
  ),
  log = list(x = NULL, y = NULL)
)

test_that("mod_main_view_server() returns the subject ID the user clicked on" %>%
  vdoc[["add_spec"]](specs$integration_specs$jumping), {
  shiny::testServer(
    server_func,
    args = list(
      id = "test",
      data_name = shiny::reactive("test"),
      dataset_list = shiny::reactive(df)
    ),
    {
      session$setInputs(
        `main_view-date_range` = c("2012-08-01T00:00", "2014-01-01T01:00"),
        `main_view-day_range` = c(12, 21),
        `main_view-x_scale` = "date",
        `main_view-y_sort` = "alphanum",
        `main_view-height` = 50,
        `main_view-filter_event` = c("Treatment Start", "Adverse Events"),
        `main_view-plot_click` = plot_click_li
      )

      session$flushReact()

      expect_equal(main$subject(), "01-701-1015")
    }
  )
})

test_that(
  "mod_main_view_server() runs even if there is no drug admin information",
  {
    df <- prep_dummy_data(n = 2)
    df <- df[names(df) != "exp"]

    server_func <- function(id, data_name, dataset_list) {
      mod_clinical_timelines_server(
        module_id = id,
        data_name = data_name,
        dataset_list = dataset_list,
        drug_admin = NULL
      )
    }

    expect_no_error(
      shiny::testServer(
        server_func,
        args = list(
          id = "test",
          data_name = shiny::reactive("test"),
          dataset_list = shiny::reactive(df)
        ),
        {
          session$flushReact()
        }
      )
    )
  }
)



# Tests using mm_app
app_dir <- "./apps/mm_app"

test_that("local filters are resetted (only) after dataset switch" %>%
  vdoc[["add_spec"]](specs$integration_specs$reset_filters), {
  app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_reset_local_filters")

  # Expected values
  default_filters <- c("sae" = "all", "soc" = NULL, "pt" = NULL, "rel" = "all")
  my_filters <- c(
    "sae" = "N",
    "soc" = "CARDIAC DISORDERS",
    "pt" = "ATRIOVENTRICULAR BLOCK SECOND DEGREE",
    "rel" = "N"
  )

  # Set local filters
  app$set_inputs(`mod-filter-serious_ae` = my_filters[["sae"]])
  app$set_inputs(`mod-filter-soc` = my_filters[["soc"]])
  app$wait_for_idle()
  app$set_inputs(`mod-filter-pref_term` = my_filters[["pt"]])
  app$set_inputs(`mod-filter-drug_rel_ae` = my_filters[["rel"]])
  app$wait_for_idle()

  # Dataset switch
  actual_before <- c(
    "sae" = app$get_value(input = "mod-filter-serious_ae"),
    "soc" = app$get_value(input = "mod-filter-soc"),
    "pt" = app$get_value(input = "mod-filter-pref_term"),
    "rel" = app$get_value(input = "mod-filter-drug_rel_ae")
  )
  app$set_inputs(selector = "dummyData2")
  app$wait_for_idle(duration = 3000, timeout = 15000) # cope with slow update under GH actions
  actual_after <- c(
    "sae" = app$get_value(input = "mod-filter-serious_ae"),
    "soc" = app$get_value(input = "mod-filter-soc"),
    "pt" = app$get_value(input = "mod-filter-pref_term"),
    "rel" = app$get_value(input = "mod-filter-drug_rel_ae")
  )

  # Tests
  testthat::expect_equal(actual_before, my_filters)
  testthat::expect_equal(actual_after, default_filters)

  app$stop()
})

test_that(
  "the default values for plot settings are displayed as specified at app launch" %>%
    vdoc[["add_spec"]](c(
      specs$default_values$plot_settings_x_param,
      specs$default_values$plot_settings_start_day,
      specs$default_values$plot_settings_boxheight_val
    )),
  {
    app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_default_plot_settings")

    # Get actual values of default parameters
    x_param_actual <- app$get_value(input = "mod-main_view-x_scale")
    start_day_actual <- app$get_value(input = "mod-main_view-day_range")[1]
    boxheight_val_actual <- app$get_value(input = "mod-main_view-height")[1]

    # Tests
    testthat::expect_equal(x_param_actual, "date")
    testthat::expect_equal(start_day_actual, -5)
    testthat::expect_equal(boxheight_val_actual, 50)

    app$stop()
  }
)

test_that("informative messages are visible in case a plot cannot be displayed" %>%
  vdoc[["add_spec"]](specs$plot_specs$errors), {
  app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_reset_local_filters")

  # Set input
  app$set_inputs(`mod-main_view-filter_event` = NULL)
  app$wait_for_idle()

  # Test
  testthat::expect_equal(app$get_value(output = "mod-main_view-main_plot")$message, CL$VAL2)

  # Set inputs
  app$set_inputs(`mod-main_view-filter_event` = "Adverse Events")
  app$set_inputs(`mod-filter-serious_ae` = "Y")
  app$wait_for_idle()

  # Test
  testthat::expect_equal(app$get_value(output = "mod-main_view-main_plot")$message, CL$VAL3)

  # The following messages are not tested since shinytest2 does not
  # support numericRangeInput from shinyWidgets:
  # - "No data available, please adjust your time range settings."
  # - "Please select a start value prior to the end value of your time range."

  app$stop()
})

test_that("bookmarking works as intended" %>% vdoc[["add_spec"]](specs$integration_specs$bookmarking), {
  # Original app
  app_dir <- "./apps/bmk_app"
  app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_bookmarking")
  app$set_inputs(`mod-main_view-x_scale` = "date")
  app$set_inputs(`mod-main_view-day_range` = c(0, 190))
  app$set_inputs(`mod-main_view-filter_event` = "Adverse Events")
  app$set_inputs(`mod-filter-serious_ae` = "N")
  app$set_inputs(`mod-filter-soc` = "CARDIAC DISORDERS")
  app$set_inputs(`mod-filter-pref_term` = "ATRIOVENTRICULAR BLOCK SECOND DEGREE")
  app$set_inputs(`mod-filter-drug_rel_ae` = "Y")
  app$wait_for_idle()

  # Bookmarked app
  bmk_url <- app$get_js("window.location.href")
  bookmark_app <- shinytest2::AppDriver$new(bmk_url)
  bookmark_app$wait_for_idle()

  # Compare app states
  app_input_values <- app$get_values()$input
  app_input_values <- app_input_values[lengths(app_input_values) != 0] # remove NULL entries
  bmk_input_values <- bookmark_app$get_values()$input
  bmk_input_values <- bmk_input_values[lengths(bmk_input_values) != 0] # remove NULL entries
  expect_identical(app_input_values, bmk_input_values)

  app$stop()
})

test_that("an informative error message gets displayed in case of the plot being too big" %>%
  vdoc[["add_spec"]](specs$plot_specs$errors), {
  app_dir <- "./apps/large_app"
  app <- shinytest2::AppDriver$new(app_dir = app_dir, name = "test_error_big_plot")
  app$wait_for_idle(timeout = 30000)
  Sys.sleep(1) # wait for error message being replaced

  # Verify that original error occurs
  original_error <- app$get_value(output = "mod-main_view-main_plot")$message
  original_error <- gsub("\n", "", original_error) # remove new line indicators
  expected_error <- paste0(
    "One or both dimensions exceed the maximum (50000px).",
    "- Use `options(ragg.max_dim = ...)` to change the max  ",
    "Warning: May cause the R session to crash"
  )
  expect_equal(original_error, expected_error)

  # Check if customized error message occurs
  actual_div <- app$get_html(selector = "#mod-main_view-main_plot_div")
  res <- grepl(CL$ERR_MSG, actual_div)
  expect_true(res)

  app$stop()
})
