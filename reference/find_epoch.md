# Find distinct epochs in a series

`find_epoch()` identifies the distinct epochs or periodicities present
in an object and returns them along with the most prevalent epoch that
meets a specified threshold.

## Usage

``` r
find_epoch(x, threshold = 0.9)

# S3 method for class 'numeric'
find_epoch(x, threshold = 0.9)

# S3 method for class 'hms'
find_epoch(x, threshold = 0.9)

# S3 method for class 'tbl_df'
find_epoch(x, threshold = 0.9)
```

## Arguments

- x:

  Any [`atomic`](https://rdrr.io/r/base/vector.html) vector, provided
  that the function has a method for handling it.

- threshold:

  (optional) A number, from `0` to `1`, indicating the minimum
  proportion that an epoch must have to be considered valid.
  `threshold = 1` means that the regularity of the time series must be
  strict (i.e., have just 1 periodicity) (default: `0.9`).

## Value

A [`list`](https://rdrr.io/r/base/list.html) object with the following
elements:

- `best_match`: A number indicating the epoch/periodicity above the
  `threshold` with greater prevalence in seconds. If none is find,
  `best_match` value will be equal as `as.numeric(NA)`.

- `prevalence`: a
  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) listing
  the unique epochs/periodicities found in `data` along with its
  proportions.

## Details

In rare cases where multiple periodicities have the same highest
prevalence above the threshold, `best_match` will return only one of
those values.

## See also

Other utility functions:
[`aggregate_index()`](https://danielvartan.github.io/actverse/reference/aggregate_index.md),
[`get_raw_data()`](https://danielvartan.github.io/actverse/reference/get_raw_data.md)

## Examples

``` r
data <-
  dplyr::tibble(
    index = c(
      as.POSIXct(
        seq(60, 5400, by = 60),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5430, 5490, by = 30),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5505, 5520, by = 15),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5530, 5540, by = 10),
        origin = lubridate::origin
      ),
      as.POSIXct(
        seq(5545, 5555, by = 5),
        origin = lubridate::origin
      )
    ),
    x = seq_along(timestamp)
  ) |>
  tsibble::tsibble(index = index)

find_epoch(data, 0.8)
#> $best_match
#> [1] 60
#> 
#> $prevalence
#> # A tibble: 5 × 2
#>   epoch proportion
#>   <dbl>      <dbl>
#> 1    60     0.899 
#> 2     5     0.0303
#> 3    30     0.0303
#> 4    10     0.0202
#> 5    15     0.0202
#> 

seq(1, 100, by = 5) |> find_epoch(threshold = 0.8)
#> $best_match
#> [1] 5
#> 
#> $prevalence
#> # A tibble: 1 × 2
#>   epoch proportion
#>   <dbl>      <dbl>
#> 1     5          1
#> 
```
