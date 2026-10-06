# Check if the user specified variable names exist in the dataset

`check_names` produces an error if the variables can't be found in the
data frame.

## Usage

``` r
check_names(df, var_names, subjid_var)
```

## Arguments

- df:

  A data frame.

- var_names:

  A character vector of column names to be checked.

- subjid_var:

  `[character(1)]`

  Character name of the unique subject identifier column in all datasets
  (default is USUBJID). Must be a single value.

## Value

If all variables were found in the data frame, the function returns the
data frame invisibly.
