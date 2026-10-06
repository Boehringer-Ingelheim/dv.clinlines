# Combine subject level, event, and drug administration data frames

Given a list of data frames, `combine_data()` merges them into one data
frame and adds subject level specific columns.

## Usage

``` r
combine_data(df_list, basic_info_df, trtstart, trtend, icf_date, subjid_var)
```

## Arguments

- df_list:

  A list of data frames as delivered by
  [`set_events_intern()`](set_events_intern.md) and
  [`set_exp_intervals()`](set_exp_intervals.md).

- basic_info_df:

  A data frame containing subject level data as extracted by
  [`set_basics()`](set_basics.md).

- trtstart:

  Character name of the treatment start column as extracted by
  [`set_basics()`](set_basics.md).

- trtend:

  Character name of the treatment end column as extracted by
  [`set_basics()`](set_basics.md).

- icf_date:

  Character name of the informed consent date column of the subject
  level dataset as extracted by [`set_basics()`](set_basics.md).

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

## Value

A data frame in which the columns of all data frames of the df_list are
combined and with the following additional columns:

- A treatment start date column as specified through the trtstart
  parameter.

- A treatment end date column as specified through the trtend parameter.

- `earliest`: Holds the date of the earliest event per subject, which is
  always the informed consent date.
