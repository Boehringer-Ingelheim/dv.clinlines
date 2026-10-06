# Filter data according to local filter settings

Filter data according to local filter settings

## Usage

``` r
filter_data(data, status_filters, input_filters, filter)
```

## Arguments

- data:

  A data frame as provided by `prep_data`.

- status_filters:

  Named boolean vector as returned by
  [`get_filter_status()`](get_filter_status.md).

- input_filters:

  A list of inputs from local filters.

- filter:

  `[list(list(character(1)+)) | NULL]`

  A list that specifies information for local adverse events filters.
  Set to `NULL` (default) for no filters.

## Value

The received data frame filtered according to local filter settings.
