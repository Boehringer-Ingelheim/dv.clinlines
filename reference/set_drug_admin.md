# Create a list to be assigned to the `drug_admin` parameter

Helper function to support configuration of a Clinical Timelines module.

## Usage

``` r
set_drug_admin(
  dataset_name,
  trt_var,
  start_var,
  end_var,
  detail_var,
  label,
  dose_var,
  dose_unit_var
)
```

## Arguments

- dataset_name:

  Character name of the data frame that holds drug administration data
  (e.g. ex domain) as it is called in the `data_list` parameter.

- trt_var:

  Character name of the variable that contains the treatment name. Must
  be present in the data frame mentioned in the `dataset_name` element.

- start_var:

  Character name of the variable that contains the start dates (e.g.
  exposure start dates). Must be present in the data frame mentioned in
  the `dataset_name` element.

- end_var:

  Character name of the variable that contains the end dates (e.g.
  exposure start dates). Must be present in the data frame mentioned in
  the `dataset_name` element.

- detail_var:

  Character name of the variable that contains the treatment
  information. Must exist in the dataset mentioned in the `dataset_name`
  element.

- label:

  Free-text character label for the drug administration event.

- dose_var:

  Character name of the variable that contains the dosis level
  information. Must exist in the dataset mentioned in the `dataset_name`
  element.

- dose_unit_var:

  Character name of the variable that contains the dosis unit. Must
  exist in the dataset mentioned in the `dataset_name` element.

## Value

A list that could directly be used as input for the `drug_admin`
parameter of [`mod_clinical_timelines()`](mod_clinical_timelines.md) and
[`mod_clinical_timelines_server()`](mod_clinical_timelines_server.md).
