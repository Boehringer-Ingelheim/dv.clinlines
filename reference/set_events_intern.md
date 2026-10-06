# Set and prepare interval data

Gathers data needed for the plot and transforms them to a consistent
structure.

## Usage

``` r
set_events_intern(data_list, mapping = default_mapping(), subjid_var)
```

## Arguments

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

A data frame including the following columns:

- `subject_id`: A unique subject identifier column. Its name is
  specified by `subjid_var`.

- `start_dt_var`, `end_dt_var`: Start and end dates of the event.

- `detail_var`: Further information to the events.

- `set`: Name of the dataset the data origins from.

- `set_id`: Row ID's of the related dataset.

- `group`: Label for the event types.

- `arrow_right`: Flag that indicates whether the event is ongoing/ended
  after the specified time range or not.

## Details

Please refer to the Details section of
[`mod_clinical_timelines()`](mod_clinical_timelines.md) for further
instructions on how to define a proper mapping.
