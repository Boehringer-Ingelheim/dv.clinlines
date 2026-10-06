# Extract group names of events near cursor position

Extract group names of events near cursor position

## Usage

``` r
get_groups(ggdata_y, color_map, x_range)
```

## Arguments

- ggdata_y:

  List of data frames as returned by
  [`create_ggdata_y()`](create_ggdata_y.md).

- color_map:

  Named vector of hexadecimal color codes, as returned by
  [`color_lookup()`](color_lookup.md). Names represent event groups.

- x_range:

  Vector of start and end values on x-axis level as provided by
  `calc_x_range`.

## Value

Character vector of event names.
