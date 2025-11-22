# Read, tidy, and validate an ActTrust file

`read_acttrust()` allows you to read, tidy, and validate an ActTrust
file in a consistent and easy manner. You can see the output data
structure in
[`?acttrust`](https://danielvartan.github.io/actverse/reference/acttrust.md).

ActTrust is a trademark of [Condor Instruments
Ltda](https://condorinst.com/en/).

## Usage

``` r
read_acttrust(
  file = tcltk::tk_choose.files(mult = FALSE),
  tz = "UTC",
  regularize = TRUE
)
```

## Arguments

- file:

  (optional) A string with the file path for the ActTrust data. If not
  assigned, a dialog window will open allowing the user to browse and
  select a file (default:
  [`tk_choose.files()`](https://rdrr.io/r/tcltk/tk_choose.files.html)).

- tz:

  (optional) A string that specifies which time zone to parse the
  dates/time with. The string must be a time zone that is recognized by
  the user's OS. For more information see
  [`?timezone`](https://rdrr.io/r/base/timezones.html) (default:
  `"UTC"`).

- regularize:

  (optional) A [`logical`](https://rdrr.io/r/base/logical.html) value
  indicating if the function must correct irregular intervals
  (**strongly recommended**). See more about it in the Details section
  (default: `TRUE`).

## Value

A [tsibble](https://tsibble.tidyverts.org/reference/tsibble.html)
object. The data structure can be found in
[`?acttrust`](https://danielvartan.github.io/actverse/reference/acttrust.md).

## Details

### `regularize` parameter

ActTrust data files may have uneven epochs or intervals due to small
drifts in the device's internal clock. These irregularities can affect
analyses. Setting `regularize = TRUE` in `read_acttrust()` will detect
and correct such issues by regularizing the time intervals.

Regularization is performed only if a clear epoch or periodicity is
found. During this process, values within each epoch are aggregated:
numeric variables are averaged, and categorical or integer variables are
assigned their most frequent value (mode).

Any gaps in the time series are filled with `NA`, and the corresponding
`state` is set to `9`.

### Offwrist data

`read_acttrust()` will convert all offwrist data (where `state == 4`) to
missing values (`NA`). These data points will remain classified as
offwrist in the `state` variable.

## See also

Other read/write functions:
[`write_acttrust()`](https://danielvartan.github.io/actverse/reference/write_acttrust.md)

## Examples

``` r
get_raw_data("acttrust.txt") |> read_acttrust()
#> ℹ Reading data
#> ✔ Reading data [59ms]
#> 
#> ℹ Tidying data
#> ✔ Tidying data [58ms]
#> 
#> ℹ Validating data
#> ✔ Validating data [670ms]
#> 
#> # A tsibble: 1,441 x 17 [1m] <UTC>
#>    timestamp             pim   tat   zcm orientation wrist_temperature
#>    <dttm>              <dbl> <dbl> <dbl>       <dbl>             <dbl>
#>  1 2021-04-24 04:14:00  7815   608   228           0              26.9
#>  2 2021-04-24 04:15:00  2661   160    64           0              27.2
#>  3 2021-04-24 04:16:00  3402   243    80           0              27.7
#>  4 2021-04-24 04:17:00  4580   317   125           0              27.9
#>  5 2021-04-24 04:18:00  2624   255    33           0              28.0
#>  6 2021-04-24 04:19:00  3929   246   105           0              28.1
#>  7 2021-04-24 04:20:00  5812   369   171           0              28.2
#>  8 2021-04-24 04:21:00  3182   270    54           0              28.4
#>  9 2021-04-24 04:22:00  6362   373   189           0              28.6
#> 10 2021-04-24 04:23:00  2621   159    64           0              28.7
#> # ℹ 1,431 more rows
#> # ℹ 11 more variables: external_temperature <dbl>, light <dbl>,
#> #   ambient_light <dbl>, red_light <dbl>, green_light <dbl>, blue_light <dbl>,
#> #   ir_light <dbl>, uva_light <dbl>, uvb_light <dbl>, event <dbl>, state <dbl>
```
