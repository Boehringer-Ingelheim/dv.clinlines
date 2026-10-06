# Create user interface for dv.clinlines module

`mod_clinical_timelines_UI` acts as central for managing user interfaces
of main view and local filter modules.

## Usage

``` r
mod_clinical_timelines_UI(
  module_id,
  filter_list = NULL,
  x_param = "day",
  boxheight_val = 60
)
```

## Arguments

- module_id:

  `[character(1)]`

  A unique ID string to create a namespace. Must match the ID of
  [`mod_clinical_timelines_server()`](mod_clinical_timelines_server.md).

- filter_list:

  `[list(character(1)+) | NULL]`

  List of filter names that indicate which local filters to display.

- x_param:

  `[character(1) | NULL]`

  Either "date" or "day" defining if the x axis shows the date or study
  days as initial setting at app launch. Defaults to "day".

- boxheight_val:

  `[integer(1) | NULL]`

  A value between 30 and 150 defining the initial height of the
  individual timeline plot boxes at app launch. Defaults to 60.

## Value

A shiny UI `tagList` that contains a `sidebarLayout` object.

## See also

[`mod_clinical_timelines()`](mod_clinical_timelines.md) and
[`mod_clinical_timelines_server()`](mod_clinical_timelines_server.md)
