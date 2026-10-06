# Adapt data frame structure for plotting purposes

`complete_events()` ensures all dates necessary for the main plot, i.e.
start and and end dates/days per event are available and of respective
type. If no study day columns were determined at app configuration, the
function calculates them according to SDTM rules.

## Usage

``` r
complete_events(combined_data, trt_start, trt_end)
```

## Arguments

- combined_data:

  A data frame as provided by [`combine_data()`](combine_data.md).

- trt_start:

  Character name of the treatment start column as extracted from
  `set_basics`.

- trt_end:

  Character name of the treatment end column as extracted from
  `set_basics`.

## Value

A data frame as provided via the combined_data parameter with additional
columns:

- `start_dy_var`: Event start days relative to study start.

- `end_dy_var`: Event end days relative to study start.

- `start_exp_day`: Start of time exp intervals in days relative to study
  start.

- `end_exp_day`: End of time exp intervals in days relative to study
  start.
