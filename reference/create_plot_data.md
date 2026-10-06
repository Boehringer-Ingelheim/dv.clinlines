# Adapt time intervals for plotting purposes

Modifies a data frame so that interval starts and ends are not outside
time_range boundaries.

## Usage

``` r
create_plot_data(work_data, time_range, filter_event)
```

## Arguments

- work_data:

  Data frame returned by [`prep_data()`](prep_data.md).

- time_range:

  Datetime or numeric vector, length 2. Indicates limits of x-axis in
  main and detail plots.

- filter_event:

  Character name of selected event type. Can be one of the entries of
  `work_data$group`.

## Value

Data frame with same structure as `work_data`. All start and end
variables were modified according to the chosen time range, so that no
date or day is outside this range. Events were filtered according to
event type filter settings.
