# Create color lookup table

`color_lookup` returns a named vector of hex colors.

## Usage

``` r
color_lookup(groups, color_palette)
```

## Arguments

- groups:

  Character vector of unique event types of the data to be plotted, e.g.
  returned by `combine_data`. Defines length and element names of the
  returned vector.

## Value

A named vector of hexcode colors.
