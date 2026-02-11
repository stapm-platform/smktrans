# Bin numeric variable

Converts a numeric variable to bins of specified number, where the
binned variable is also numeric and has the mid-points of the bins.

## Usage

``` r
bin_var(x, n_bins)
```

## Arguments

- x:

  Numeric variable to be binned

- n_bins:

  Integer - the number of bins to create

## Value

Returns a numeric binned variable

## Examples

``` r
bin_var(1:10, 3)
#>  [1] 3 3 3 3 3 8 8 8 8 8

```
