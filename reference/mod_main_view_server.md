# Server logic of the main view module of dv.clinlines

Server logic of the main view module of dv.clinlines

## Usage

``` r
mod_main_view_server(
  module_id,
  initial_data,
  changed,
  colors_groups,
  start_day = NULL,
  ms = 100
)
```

## Arguments

- module_id:

  A unique ID string to create a namespace. Must match the ID of
  [`mod_main_view_UI()`](mod_main_view_UI.md).

- initial_data:

  A metaReactive data frame returned by
  [`mod_local_filter_server()`](mod_local_filter_server.md).

- changed:

  A reactive whose actualization indicates if the underlying dataset has
  changed.

- colors_groups:

  A reactive named vector holding hexcode colors.

- start_day:

  `[integer(1)]` A single integer indicating the lower x-axis limit in
  case of study day display. Defaults to NULL, using the day of the
  earliest event to be displayed.

- ms:

  A single numeric value indicating how many milliseconds to be used for
  debouncing the main view plot.

## Value

A list of reactives to be used to communicate with other DaVinci
modules, if available.
