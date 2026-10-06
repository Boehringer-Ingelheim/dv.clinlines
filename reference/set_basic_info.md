# Create a list to be assigned to the `basic_info` parameter

Helper function to support configuration of a Clinical Timelines module.

## Usage

``` r
set_basic_info(
  subject_level_dataset_name,
  trt_start_var,
  trt_end_var,
  icf_date_var
)
```

## Arguments

- subject_level_dataset_name:

  Character name of the subject level analysis dataset (e.g. "adsl",
  "dm") as it is called in the `data_list` parameter.

- trt_start_var:

  Character name of the variable that contains treatment start dates.
  Must be present in the data frame mentioned in the
  `subject_level_dataset_name` element.

- trt_end_var:

  Character name of the variable that contains treatment end dates. Must
  be present in the data frame mentioned in the
  `subject_level_dataset_name` element.

- icf_date_var:

  Character name of the variable that contains informed consent dates.
  Must be present in the data frame mentioned in the
  `subject_level_dataset_name` element.

## Value

A list that could directly be used as input for the `basic_info`
parameter of [`mod_clinical_timelines()`](mod_clinical_timelines.md) and
[`mod_clinical_timelines_server()`](mod_clinical_timelines_server.md).
