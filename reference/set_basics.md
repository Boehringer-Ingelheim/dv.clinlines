# Set information about subject level analysis data (e.g. adsl, dm, ...)

Set information about subject level analysis data (e.g. adsl, dm, ...)

## Usage

``` r
set_basics(data_list, basic_info = default_basic_info(), subjid_var)
```

## Arguments

- data_list:

  A named list of data frames that provides a subject level dataset and
  further datasets with event specific data. Usually obtained from
  module manager.

- basic_info:

  `[list(character(1)+)]`

  A list of four elements: `subject_level_dataset_name`,
  `trt_start_var`, `trt_end_var`, and `icf_date_var`. Assigns the name
  of a subject level dataset and column names of treatment start and
  end, and informed consent variables.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

## Value

A list containing the following elements:

- `data`: A data frame that includes subject level data as provided in
  `data_list` with name for the subject identifier column fixed to
  `subjid_var`.

- `trt_start`: Character name of the variable that contains treatment
  start dates as provided in `basic_info`.

- `trt_end`: Character name of the variable that contains treatment end
  dates as provided in `basic_info`.

- `icf_date`: Character name of the variable that contains informed
  consent dates as provided in `basic_info`.
