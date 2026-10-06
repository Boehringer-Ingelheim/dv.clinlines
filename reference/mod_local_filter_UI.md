# Create user interface for the local filter shiny module of dv.clinlines

Create user interface for the local filter shiny module of dv.clinlines

## Usage

``` r
mod_local_filter_UI(module_id, filter_list = NULL)
```

## Arguments

- module_id:

  A unique ID string to create a namespace. Must match the ID of
  [`mod_local_filter_server()`](mod_local_filter_server.md).

- filter_list:

  List of filter names that indicate which local filters to display.

## Value

A shiny `uiOutput` element.
