# Tests for mod_main_view_UI() ----
test_that("mod_main_view_UI() returns a named shiny tagList" %>%
  vdoc[["add_spec"]](specs$output_specs$general_display), {
  ui <- mod_main_view_UI("test")

  expect_type(ui, "list")
  expect_named(ui, c("sidebar", "main_panel"))
  expect_true("shiny.tag.list" %in% class(ui$sidebar))
  expect_true("shiny.tag" %in% class(ui$main_panel))
})


# Tests for mod_main_view_server() ----
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

colors <- shiny::reactive(color_lookup(unique(df$group), NULL))
changed <- shiny::reactive(0)

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
  server_func <- function(id, initial_data, changed, colors_groups) {
    mod_main_view_server(
      module_id = id,
      initial_data = initial_data,
      changed = changed,
      colors_groups = colors_groups
    )
  }

  shiny::testServer(
    server_func,
    args = list(
      id = "main_view",
      initial_data = shiny::reactive(df),
      changed = changed,
      colors_group = colors
    ),
    {
      session$setInputs(plot_click = plot_click_li)
      expect_equal(subject_id(), "01-701-1015")
    }
  )
})
