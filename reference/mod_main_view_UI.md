# Create user interface for the main view module of dv.clinlines

Create user interface for the main view module of dv.clinlines

## Usage

``` r
mod_main_view_UI(module_id, x_param = "day", boxheight_val = 60)
```

## Arguments

- module_id:

  A unique ID string to create a namespace. Must match the ID of
  [`mod_main_view_server()`](mod_main_view_server.md).

- x_param:

  Either "date" or "day" defining if the x axis shows the date or study
  days as initial setting at app launch. Defaults to "day".

- boxheight_val:

  A value between 30 and 150 defining the initial height of the
  individual timeline plot boxes at app launch. Defaults to 60.

## Value

A list containing two entries:

- `sidebar`: A `tagList` of shiny input elements.

- `main_panel`: A `div` element containing shiny UI elements.
