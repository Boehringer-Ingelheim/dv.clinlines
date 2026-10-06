# Get status of each available filter

Checks for each available filter if it was chosen to be displayed in the
user interface of dv.clinlines.

## Usage

``` r
get_filter_status(all_filters, chosen_filters)
```

## Arguments

- all_filters:

  Character vector of all available filter names.

- chosen_filters:

  List of filters to be displayed.

## Value

Named vector that contains for each available filter if it was chosen by
the user to be displayed (TRUE) or not (FALSE).
