# Filter prepared event data by cursor position.

Filter prepared event data by cursor position.

## Usage

``` r
filter_nearest(initial_data, subject, rel_groups, x_range, x_scale, time_range)
```

## Arguments

- initial_data:

  Data frame returned by [`prep_data()`](prep_data.md).

- subject:

  Character sting of one unique subject identifier.

- rel_groups:

  Character vector of event names as returned by
  [`get_groups()`](get_groups.md).

- x_range:

  Numeric vector of x values as returned by
  [`calc_x_range()`](calc_x_range.md).

- x_scale:

  Character string containing either "date", or "day". Reveals current
  x-axis setting.

- time_range:

  Datetime or numeric vector, length 2. Indicates limits of x-axis in
  main and detail plots.

## Value

A data frame containing only those events of which start/end/timepoint
lie within an area determined by `rel_groups` and `x_range`.
