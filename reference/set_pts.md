# Set choices for Preferred Term filter

Set choices for Preferred Term filter

## Usage

``` r
set_pts(status_filters, soc, prepped_data, filter)
```

## Arguments

- status_filters:

  Named boolean vector as returned by
  [`get_filter_status()`](get_filter_status.md).

- soc:

  Character vector of System Organ Classes.

- prepped_data:

  Data frame as returned by [`prep_data()`](prep_data.md).

- filter:

  `[list(list(character(1)+)) | NULL]`

  A list that specifies information for local adverse events filters.
  Set to `NULL` (default) for no filters.

## Value

Character vector of PT's.
