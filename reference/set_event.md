# Create a list to be inserted into a second level list of the `mapping` parameter

Helper function to support configuration of a Clinical Timelines module.

## Usage

``` r
set_event(
  start_dt_var,
  end_dt_var = NULL,
  start_dy_var = NULL,
  end_dy_var = NULL,
  detail_var = NULL
)
```

## Arguments

- start_dt_var:

  Character name of the variable that contains either the event start
  dates (for interval events) or merely timepoints (e.g. milestones).
  The variable name must be present in the data frame under which the
  event is listed.

- end_dt_var:

  Character name of the variable that contains the event end dates.
  Needs to be provided for interval events, but set to NULL for
  timepoints. The variable name must be present in the data frame under
  which the event is listed.

- start_dy_var:

  Similar to start_dt_var, but refers to the study relative days
  (instead of dates). The variable name must be present in the dataset
  under which the event is listed. Can be set to NULL to let the module
  calculate the study days according to SDTM standard rules.

- end_dy_var:

  Similar to end_dt_var, but refers to the study relative days (instead
  of dates). The variable name must be present in the dataset under
  which the event is listed. Can be set to NULL to let the module
  calculate the study days according to SDTM standard rules.

- detail_var:

  Character name of the variable that contains further descriptive
  information that shall be displayed for the event. Can be set to NULL
  for no further information.

## Value

A list that could directly be used as third level list for the `mapping`
parameter of [`mod_clinical_timelines()`](mod_clinical_timelines.md) and
[`mod_clinical_timelines_server()`](mod_clinical_timelines_server.md).
