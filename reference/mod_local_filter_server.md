# Create server for local filter shiny module of dv.clinlines

Create server for local filter shiny module of dv.clinlines

## Usage

``` r
mod_local_filter_server(module_id, filter, joined_data, changed)
```

## Arguments

- module_id:

  A unique ID string to create a namespace. Must match the ID of
  [`mod_local_filter_UI()`](mod_local_filter_UI.md).

- filter:

  `[list(list(character(1)+)) | NULL]`

  A list that specifies information for local adverse events filters.
  Set to `NULL` (default) for no filters.

- joined_data:

  A metaReactive dataset as provided by [`prep_data()`](prep_data.md).

- changed:

  A reactive whose actualization indicates if the filters should be
  reset to default values.

## Value

A data frame filtered by local adverse event filters.
