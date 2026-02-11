# Smooth age and period pattern in probability values

Applies a 2D moving average smooth to the trends over age and period,
and fills any remaining missing values by approximating the age pattern
within a particular year.

## Usage

``` r
p_smooth(data, value_var, window_size = 3)
```

## Arguments

- data:

  Data table containing "age", "year" and a variable bounded by 0 and 1.

- value_var:

  Character - the name of the variable to be smoothed.

- window_size:

  \- Integer - must be an odd number - the number of years covered by
  the moving average window.

## Value

Returns a data table the same as data but with smoothed probability
values.

## Examples

``` r
if (FALSE) { # \dontrun{
test_data <- data.frame(expand.grid(age = c(12:40, 42:89), year = 2001:2016))
setDT(test_data)
test_data[ , prob := runif(nrow(test_data))]
test_data[sample(1:nrow(test_data), 100, replace = F), prob := NA]

data_sm <- p_smooth(test_data, "prob", 3)
} # }
```
