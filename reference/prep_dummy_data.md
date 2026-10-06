# Prepare dummy data

Modifiy pharmaverseadam's adsl, adae, adcm, and exp dummy data for easy
use within dv.clinlines.

## Usage

``` r
prep_dummy_data(n = 200)
```

## Arguments

- n:

  Number of rows to select from the adsl dataset. The first n rows will
  be taken. Used to reduce runtime during development.

## Value

A list of three data frames.
