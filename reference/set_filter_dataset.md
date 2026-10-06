# Create a dataset for local adverse event filters

Create a dataset for local adverse event filters

## Usage

``` r
set_filter_dataset(filter, data_list, mapping, subjid_var)
```

## Arguments

- filter:

  `[list(list(character(1)+)) | NULL]`

  A list that specifies information for local adverse events filters.
  Set to `NULL` (default) for no filters.

- data_list:

  A named list of data frames that provides a subject level dataset and
  further datasets with event specific data. Usually obtained from
  module manager.

- mapping:

  `[list(list(list(character(1)+)))]`

  A list of lists. It serves as instruction on which event to take from
  which data domain / dataset, and further from which variables to take
  the start and end values, etc., that will be plotted as clinical
  timelines. Elements need to follow a certain structure that is
  described in the Details section below.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

## Value

A subset of the adverse event dataset containing all columns needed for
local adverse event filters.

## Details

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
