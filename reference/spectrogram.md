# Create a spectrogram plot based on Sokolove & Bushell's periodogram

`spectrogram()` computes a series of Sokolove & Bushell's (1978)
\\\chi^{2}\\ periodograms to visualize changes in periodicities across
intervals within a
[`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
object.

This function enables the detection of temporal variations in
rhythmicity by generating periodograms for consecutive or overlapping
time windows.

See the
[`periodogram()`](https://danielvartan.github.io/actverse/reference/periodogram.md)
function for details on periodogram computation.

## Usage

``` r
spectrogram(
  data,
  col,
  p_unit = "minutes",
  p_min = 1000,
  p_max = 2500,
  p_step = 1,
  int_unit = "days",
  int_n = 7,
  int_step = 720,
  alpha = 0.05,
  print = TRUE
)
```

## Arguments

- data:

  A [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
  object.

- col:

  A string indicating which column of `data` to use.

- p_unit:

  (optional) A string indicating at which time unit the index must be
  aggregated. By aggregating the index, this will change the time series
  interval and, consequently, its `p` periods. Valid values are:
  `“seconds”`, `“minutes”`, `“hours”`, `“days”`, `“weeks”`, `“months”`,
  `“quarters”`, and `“years”`) (default: `"minutes"`).

- p_min:

  (optional) An integer number indicating the minimum period (\\p\\),
  with the same unit as `p_unit`, to compute the test (e.g., if
  `p_unit = "minutes"`, `p_min = 1` means a period of 1 minute)
  (default: `1000`).

- p_max:

  (optional) An integer number indicating the maximum period (\\p\\),
  with the same unit as `p_unit`, to compute the test (default: `2500`).

- p_step:

  (optional) An integer number indicating the range of values that must
  be skipped between computing one test and the next (e.g., when
  `p_min == 1`, `p_max == 7`, and `p_step == 2`, the test periods will
  be `1`, `3`, `5`, and `7`) (default: `1`).

- int_unit:

  (optional) A string indicating the interval unit. Valid values are:
  `“seconds”`, `“minutes”`, `“hours”`, `“days”`, `“weeks”`, `“months”`,
  `“quarters”`, and `“years”`) (default: `"days"`).

- int_n:

  (optional) An integer number indicating the size of the intervals,
  with the same unit as `int_unit` (default: `7`).

- int_step:

  (optional) An integer number indicating the amount of epochs to
  advance at the end of each interval (default: `720`).

- alpha:

  (optional) A number, from `0` to `1`, indicating the significant level
  required for the peaks. The spectrogram plot only shows the
  significant peaks (default: `0.05`).

- print:

  (optional) A [`logical`](https://rdrr.io/r/base/logical.html) value
  indicating if the function must print the spectrogram plot (default:
  `TRUE`).

## Value

A [`list`](https://rdrr.io/r/base/list.html) object with the following
elements:

- `periodograms`: A [`list`](https://rdrr.io/r/base/list.html) object
  with the periodogram data for each interval. See
  [`periodogram()`](https://danielvartan.github.io/actverse/reference/periodogram.md)
  to learn more about the list elements.

- `spectrogram`: A
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
  with a heat map chart showing one periodogram per line (`q_p`) (y) by
  the period sequence (`p_seq`) (x).

## References

Sokolove, P. G., & Bushell, W. N. (1978). The chi square periodogram:
its utility for analysis of circadian rhythms. *Journal of Theoretical
Biology*, *72*(1), 131-160.
[doi:10.1016/0022-5193(78)90022-x](https://doi.org/10.1016/0022-5193%2878%2990022-x)
.

## See also

Other period analysis functions:
[`periodogram()`](https://danielvartan.github.io/actverse/reference/periodogram.md)

## Examples

``` r
if (curl::has_internet()) {
  file <- get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = tempdir(),
    file = "processed.txt"
  )

  data <- read_acttrust(
    file,
    tz = "America/Sao_Paulo"
  )

  spec <- spectrogram(data, "pim")
}
#> ℹ Reading data
#> ✔ Reading data [270ms]
#> 
#> ℹ Tidying data
#> ✔ Tidying data [348ms]
#> 
#> ℹ Validating data
#> ℹ Found 2 gap in the time series: 2021-04-26 03:14:00/2021-04-26 03:14:00 and 2021-05-01 17:34:00/2021-05-01 17:34:00 (showing up to a total of 5 values).
#> ℹ Validating data
#> ℹ Found 21 offwrist blocks in the time series. All values were set as NA.
#> ℹ Validating data
#> ✔ Validating data [18.2s]
#> 
#> ! data[[col]] has missing values. Results may diverge.
#> Computing periodograms ■■                                 3% | ETA: 33s
#> Computing periodograms ■■■                                7% | ETA: 31s
#> Computing periodograms ■■■■■■                            16% | ETA: 28s
#> Computing periodograms ■■■■■■■■■                         26% | ETA: 24s
#> Computing periodograms ■■■■■■■■■■■                       34% | ETA: 21s
#> Computing periodograms ■■■■■■■■■■■■■■                    43% | ETA: 19s
#> Computing periodograms ■■■■■■■■■■■■■■■■■                 53% | ETA: 15s
#> Computing periodograms ■■■■■■■■■■■■■■■■■■■■              62% | ETA: 12s
#> Computing periodograms ■■■■■■■■■■■■■■■■■■■■■■■           72% | ETA:  9s
#> Computing periodograms ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  6s
#> Computing periodograms ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      90% | ETA:  3s
#> Computing periodograms ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> 
```
