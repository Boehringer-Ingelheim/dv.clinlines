# Generates ggplot_build table and returns only rows with data near mouse cursor

Generates ggplot_build table and returns only rows with data near mouse
cursor

## Usage

``` r
create_ggdata_y(p, hover)
```

## Arguments

- p:

  ggplot object that represents the main plot.

- hover:

  Hover object of the main plot.

## Value

A list of two data frames extracted from a call to
`ggplot2:::ggplot_build()`, and filtered by data points that lie within
+/- 5 px (on y-axis) from cursor position. One data frame represents
interval data, the other timepoints.
