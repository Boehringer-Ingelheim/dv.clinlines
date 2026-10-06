# Create info box for hovering over the main plot of dv.clinlines

`create_hover_info` returns a shiny wellPanel with further event
information for the hovered-on subject in the main plot.

## Usage

``` r
create_hover_info(
  hover,
  ggdata_y,
  initial_data,
  color_map,
  x_scale,
  y_screen_pct,
  time_range
)
```

## Arguments

- hover:

  Hover event of the main plot.

- ggdata_y:

  List of data frames as returned by
  [`create_ggdata_y()`](create_ggdata_y.md).

- initial_data:

  Data frame returned by [`prep_data()`](prep_data.md).

- color_map:

  Named vector of hexadecimal color codes, as returned by
  [`color_lookup()`](color_lookup.md). Names represent event groups.

- x_scale:

  Character string containing either "date", or "day". Reveals current
  x-axis setting.

- y_screen_pct:

  Y mouse position on window screen (between 0 and 1)

- time_range:

  Datetime or numeric vector, length 2. Indicates limits of x-axis in
  main and detail plots.

## Value

A shiny wellPanel object which displays each event type (group),
timepoint date (moment), interval dates (start, end), and further
details to the event (details) for the hovered subject of the main plot.
