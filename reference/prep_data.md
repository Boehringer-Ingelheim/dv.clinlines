# Prepare a data frame to be used to plot the clinical timelines

`prep_data()` combines subject level and other user specified event
datasets into one data frame that can be used to plot the clinical
timelines without local filtering.

## Usage

``` r
prep_data(
  data_list,
  basic_info = default_basic_info(),
  mapping = default_mapping(),
  drug_admin = default_drug_admin(),
  subjid_var = "USUBJID",
  filter = NULL
)
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

- mapping:

  `[list(list(list(character(1)+)))]`

  A list of lists. It serves as instruction on which event to take from
  which data domain / dataset, and further from which variables to take
  the start and end values, etc., that will be plotted as clinical
  timelines. Elements need to follow a certain structure that is
  described in the Details section below.

- drug_admin:

  `[list(character(1)+) | NULL]`

  A list of named character strings that describes which variables to
  use to display drug administration events. Set it to NULL to
  deactivate them.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

- filter:

  `[list(list(character(1)+)) | NULL]`

  A list that specifies information for local adverse events filters.
  Set to `NULL` (default) for no filters.

## Value

A data frame that can directly be used to plot the clinical timelines.

## Details

The `basic_info` list must contain the following elements:

- `subject_level_dataset_name`: Character name of the subject level
  analysis dataset (e.g. "adsl", "dm") as it is called in the datalist
  that is provided to the modulemanager.

- `trt_start_var`: Character name of the variable that contains
  treatment start dates which must be present in the dataset mentioned
  in the `data` element.

- `trt_end_var`: Character name of the variable that contains treatment
  end dates which must be present in the dataset mentioned in the `data`
  element.

- `icf_date_var`: Character name of the variable that contains informed
  consent dates which must be present in the dataset mentioned in the
  `data` element.

\

The list provided to `mapping` must follow a strict hierarchy. It must
contain one entry per dataset/domain that serves as basis for the
events. These entries need to be named according to the names of the
datalist that is provided to the modulemanager. The entries by oneself
must again be lists.\
These second level lists contain the variable names that are needed to
plot the events, gathered in yet another lists, which are named
according to the labels that shall be assigned to each event, and that
contain the following elements each:

- `start_dt_var`: Character name of the variable that contains either
  the event start dates (for interval events) or merely timepoints (e.g.
  milestones). The variable name must be present in the dataset under
  which the event is listed.

- `end_dt_var`: Character name of the variable that contains the event
  end dates. Needs to be provided for interval events, but set to `NULL`
  for timepoints. The variable name must be present in the dataset under
  which the event is listed.

- `start_dy_var`: Similar to `start_dt_var`, but refers to the study
  relative days (instead of dates). The variable name must be present in
  the dataset under which the event is listed. Can be set to `NULL` to
  let the module calculate the study days according to SDTM standard
  rules.

- `end_dy_var`: Similar to `end_dt_var`, but refers to the study
  relative days (instead of dates). The variable name must be present in
  the dataset under which the event is listed. Can be set to `NULL` to
  let the module calculate the study days according to SDTM standard
  rules.

- `detail_var`: Character name of the variable that contains further
  descriptive information that shall be displayed for the event. Can be
  set to `NULL` for no further information.

The structure of the `mapping` parameters for one single event is
mentioned below. It is possible to define multiple events for one
dataset, and multiple datasets in the mapping list.

`mapping = list(`\
` ``<data name> = list`(\
` `` ``<event label> = list`(\
` `` `` ``start_dt_var = <variable name>,`\
` `` `` ``end_dt_var = <variable name or NULL>,`\
` `` `` ``start_dy_var = <variable name or NULL>,`\
` `` `` ``end_dy_var = <variable name or NULL>,`\
` `` `` ``detail_var = <variable name or NULL>`\
` `` ``)`\
` ``)`\
`)`

\

If not NULL, the `drug_admin` list must contain the following elements
(with the exception of `show_dose_info` being optional):

- `dataset_name`: Character name of the dataset that holds drug
  administration data (e.g. ex domain), as it is called in the datalist
  that is provided to the modulemanager.

- `trt_var`: Character name of the variable that contains the treatment
  name. Must exist in the dataset mentioned in the `dataset_name`
  element.

- `start_var`: Character name of the variable that contains the start
  dates (e.g. exposure start dates). Must exist in the dataset mentioned
  in the `dataset_name` element.

- `end_var`: Character name of the variable that contains the end dates
  (e.g. exposure end dates). Must exist in the dataset mentioned in the
  `dataset_name` element.

- `detail_var`: Character name of the variable that contains the
  treatment name to appear in the Details column of the hover box. Must
  exist in the dataset mentioned in the `dataset_name` element. Can be
  set to `NULL` for no treatment name. If `show_dose_info` is
  unspecified or set to true then dose information will also be added to
  the Details column.

- `label`: Free-text character label for the drug administration event.

- `dose_var`: Character name of the variable that contains the dose
  level information to be used for indication of increased/decreased
  dose, and added to Details column of the hover box. Must exist in the
  dataset mentioned in the `dataset_name` element.

- `dose_unit_var`: Character name of the variable that contains the dose
  unit to appear in the Details column of the hover box. Must exist in
  the dataset mentioned in the `dataset_name` element. Can be set to
  `NULL` for no unit.

- `show_dose_info`: Logical indicating whether or not to show dose level
  and unit in the Details column of the hover box.

\

If not `NULL`, `filter` defines local filters. So far, the following
filters for adverse events are available:

- `serious_ae_var`: Filter for serious adverse events.

- `soc_var`: Filter for system organ classes.

- `pref_term_var`: Filter for preferred terms.

- `drug_rel_ae_var`: Filter for drug related adverse events.

The `filter` parameter must be a list that contains yet another list for
adverse events filters, that is named `ae_filter`, and that holds the
following elements:

- `dataset_name`: Character name of the adverse events dataset. Must be
  available in in the datalist that is provided to the modulemanager.

- `label`: Character value which is exactly the same as the name for the
  adverse events event defined in `mapping`.

- `serious_ae_var`: Character name of the adverse events variable that
  contains serious adverse events flags (Y/N).

- `soc_var`: Character name of the adverse events variable that contains
  system organ classes.

- `pref_term_var`: Character name of the adverse events variable that
  contains preferred terms.

- `drug_rel_ae_var`: Character name of the adverse events variable that
  contains drug related (causality) flags (Y/N).
