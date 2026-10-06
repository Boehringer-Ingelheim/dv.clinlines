# Check if the specified columns are of type Date

`check_date_type` produces an error if data in the variables is not of
type Date.

## Usage

``` r
check_date_type(df, var_names)
```

## Arguments

- df:

  A data frame.

- var_names:

  A string vector of column names to be checked.

## Value

If all variables are of type Date, the function returns the data frame
invisibly.
