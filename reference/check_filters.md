# Check if the user specified adverse event filters are available

`check_filters` produces an error if the filter names can't be found
among the available local adverse event filters.

## Usage

``` r
check_filters(filter_list, filter_names)
```

## Arguments

- filter_list:

  A string vector of filter names specified by the user.

- filter_names:

  A string vector of available filters.
