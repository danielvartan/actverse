# Aggregate the index of a `tsibble`

`aggregate_index()` allows you to aggregate the index of a
[`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html) object
by applying a specific function to its measured variables.

## Usage

``` r
aggregate_index(data, unit, fun = NULL, week_start = 1)
```

## Arguments

- data:

  A [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
  object.

- unit:

  A string indicating at which time unit the index must be aggregated.
  Valid values are: `“seconds”`, `“minutes”`, `“hours”`, `“days”`,
  `“weeks”`, `“months”`, `“quarters”`, and `“years”`) (default:
  `"minutes"`).

- fun:

  (optional) A [`function`](https://rdrr.io/r/base/is.function.html) to
  be applied to each measure variable of `data`. If `NULL`,
  `aggregate_index()` will apply its default function (see the Details
  section to learn more) (default: `NULL`).

- week_start:

  (optional) An integer number indicating the day on which the week
  starts (`1` for Monday and `7` for `Sunday`). This is only used when
  `unit` is set to `"weeks"` (default: `1`).

## Value

A [`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
object with the same columns as `data`, but with the index aggregated to
the specified time unit.

## Details

`aggregate_index()` is designed to simplify the regularization of
[`tsibble`](https://tsibble.tidyverts.org/reference/tsibble.html)
objects by aggregating their index to a specified time unit. For more
advanced or customized aggregation, consider using the `index_by()`
function from the [`tsibble`](https://tsibble.tidyverts.org/) package,
which offers greater flexibility and control.

### Default function

If the `fun` argument is `NULL`, `aggregate_index()` will use the
following function to transform each measured variable:

    function(x) {
        if (is.numeric(x) && !all(nchar(x) == 1, na.rm = TRUE)) {
            mean(x, na.rm = TRUE)
        } else {
            y <- x[which(!is.na(x))]
            unique <- unique(y)
            unique[which.max(tabulate(match(y, unique)))]
        }
    }

This function computes the mean for numeric variables and assigns the
most frequent value (mode) for non-numeric variables. If no mode is
found, it returns the first non-missing value of `x`.

## See also

Other utility functions:
[`find_epoch()`](https://danielvartan.github.io/actverse/reference/find_epoch.md),
[`get_raw_data()`](https://danielvartan.github.io/actverse/reference/get_raw_data.md)

## Examples

``` r
acttrust
#> # A tsibble: 1,441 x 17 [1m] <America/Sao_Paulo>
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

aggregate_index(acttrust, unit = "hour")
#> # A tsibble: 25 x 17 [1h] <America/Sao_Paulo>
#>    timestamp              pim    tat   zcm orientation wrist_temperature
#>    <dttm>               <dbl>  <dbl> <dbl>       <dbl>             <dbl>
#>  1 2021-04-24 04:00:00 2424.  182.   50.0            0              29.5
#>  2 2021-04-24 05:00:00 2631.  173.   64.1            0              30.0
#>  3 2021-04-24 06:00:00 1960.  139.   40.2            0              29.9
#>  4 2021-04-24 07:00:00 2453.  163.   56.7            0              29.9
#>  5 2021-04-24 08:00:00 1758.  128.   37.2            0              30.5
#>  6 2021-04-24 09:00:00 2008.  134.   45.3            0              31.0
#>  7 2021-04-24 10:00:00 3129.  192.   94.8            0              30.5
#>  8 2021-04-24 11:00:00  152.    9.72  4.48           0              32.3
#>  9 2021-04-24 12:00:00   72.9   4.18  2.1            0              29.6
#> 10 2021-04-24 13:00:00   79.8   6.22  1.33           0              31.4
#> # ℹ 15 more rows
#> # ℹ 11 more variables: external_temperature <dbl>, light <dbl>,
#> #   ambient_light <dbl>, red_light <dbl>, green_light <dbl>, blue_light <dbl>,
#> #   ir_light <dbl>, uva_light <dbl>, uvb_light <dbl>, event <dbl>, state <dbl>

aggregate_index(acttrust, unit = "day")
#> # A tsibble: 2 x 17 [1D]
#>   timestamp    pim   tat   zcm orientation wrist_temperature
#>   <date>     <dbl> <dbl> <dbl>       <dbl>             <dbl>
#> 1 2021-04-24 1657.  107.  43.8           0              30.8
#> 2 2021-04-25 2976.  198.  77.5           0              29.9
#> # ℹ 11 more variables: external_temperature <dbl>, light <dbl>,
#> #   ambient_light <dbl>, red_light <dbl>, green_light <dbl>, blue_light <dbl>,
#> #   ir_light <dbl>, uva_light <dbl>, uvb_light <dbl>, event <dbl>, state <dbl>

aggregate_index(acttrust, unit = "week")
#> # A tsibble: 1 x 17 [?]
#>   timestamp   pim   tat   zcm orientation wrist_temperature external_temperature
#>      <week> <dbl> <dbl> <dbl>       <dbl>             <dbl>                <dbl>
#> 1  2021 W16 1890.  123.  49.8           0              30.6                 29.6
#> # ℹ 10 more variables: light <dbl>, ambient_light <dbl>, red_light <dbl>,
#> #   green_light <dbl>, blue_light <dbl>, ir_light <dbl>, uva_light <dbl>,
#> #   uvb_light <dbl>, event <dbl>, state <dbl>

aggregate_index(acttrust, unit = "month")
#> # A tsibble: 1 x 17 [?]
#>   timestamp   pim   tat   zcm orientation wrist_temperature external_temperature
#>       <mth> <dbl> <dbl> <dbl>       <dbl>             <dbl>                <dbl>
#> 1  2021 Apr 1890.  123.  49.8           0              30.6                 29.6
#> # ℹ 10 more variables: light <dbl>, ambient_light <dbl>, red_light <dbl>,
#> #   green_light <dbl>, blue_light <dbl>, ir_light <dbl>, uva_light <dbl>,
#> #   uvb_light <dbl>, event <dbl>, state <dbl>

aggregate_index(acttrust, unit = "quarter")
#> # A tsibble: 1 x 17 [?]
#>   timestamp   pim   tat   zcm orientation wrist_temperature external_temperature
#>       <qtr> <dbl> <dbl> <dbl>       <dbl>             <dbl>                <dbl>
#> 1   2021 Q2 1890.  123.  49.8           0              30.6                 29.6
#> # ℹ 10 more variables: light <dbl>, ambient_light <dbl>, red_light <dbl>,
#> #   green_light <dbl>, blue_light <dbl>, ir_light <dbl>, uva_light <dbl>,
#> #   uvb_light <dbl>, event <dbl>, state <dbl>

aggregate_index(acttrust, unit = "year")
#> # A tsibble: 1 x 17 [?]
#>   timestamp   pim   tat   zcm orientation wrist_temperature external_temperature
#>       <dbl> <dbl> <dbl> <dbl>       <dbl>             <dbl>                <dbl>
#> 1      2021 1890.  123.  49.8           0              30.6                 29.6
#> # ℹ 10 more variables: light <dbl>, ambient_light <dbl>, red_light <dbl>,
#> #   green_light <dbl>, blue_light <dbl>, ir_light <dbl>, uva_light <dbl>,
#> #   uvb_light <dbl>, event <dbl>, state <dbl>
```
