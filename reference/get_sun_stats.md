# Get sun related statistics from different APIs

`get_sun_stats()` retrieves sun statistics using different models and
data sources.

At the moment, none of the `get_sun_stats()` methods uses real-world
data. All of them are based on models.

Each API have its peculiarities. We recommend checking the API
documentation for a better understanding of the mechanisms behind them
(see the Details section).

## Usage

``` r
get_sun_stats(
  latitude,
  longitude,
  date = Sys.Date(),
  tz = "UTC",
  method = "suncalc"
)
```

## Arguments

- latitude:

  A number indicating the latitude of the desired location in decimal
  degrees.

- longitude:

  A number indicating the longitude of the desired location in decimal
  degrees.

- date:

  (optional) A [`Date`](https://rdrr.io/r/base/as.Date.html) value
  indicating the moment in time (default:
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)).

- tz:

  (optional) A string indicating the time zone of the results. See
  [`timezones`](https://rdrr.io/r/base/timezones.html) to learn more
  (default: `"UTC"`).

- method:

  (optional) A string indicating which method or API to use. Valid
  values are: `"suncalc"` and `"sunrise-sunset.org"`. See the Details
  section to learn more (default: `"suncalc"`).

## Value

A [`list`](https://rdrr.io/r/base/list.html) object with the following
elements:

- `date`: A [`Date`](https://rdrr.io/r/base/as.Date.html) vector with
  the same value of the `date` parameter.

- `latitude`: A number with the same value of the `latitude` parameter.

- `longitude`: A number with the same value of the `longitude`
  parameter.

- `tz`: A string with the same value of the `tz` parameter.

- `sunrise_start`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when the top edge of the sun appears on the horizon.

- `sunrise_end`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when bottom edge of the sun touches the horizon.

- `golden_hour_end`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when the morning golden hour (soft light, best time for
  photography) ends.

- `solar_noon`: An [`hms`](https://hms.tidyverse.org/reference/hms.html)
  value indicating the moment when sun is in the highest position.

- `golden_hour_start`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when the evening golden hour (soft light, best time for
  photography) starts.

- `sunset_start`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when the bottom edge of the sun touches the horizon.

- `sunset_end`: An [`hms`](https://hms.tidyverse.org/reference/hms.html)
  value indicating the moment when the sun disappears below the horizon.
  This is also the moment when the evening civil twilight starts.

- `dusk`: An [`hms`](https://hms.tidyverse.org/reference/hms.html) value
  indicating the moment when the dusk starts. This is also the moment
  when the evening nautical twilight starts.

- `nautical_dusk`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when nautical dusk starts. This is also the moment when the
  evening astronomical twilight starts.

- `night_start`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when the night starts (dark enough for astronomical
  observations).

- `nadir`: An [`hms`](https://hms.tidyverse.org/reference/hms.html)
  value indicating the darkest moment of the night, i.e., when the sun
  is in the lowest position.

- `night_end`: An [`hms`](https://hms.tidyverse.org/reference/hms.html)
  value indicating the moment when the night ends. This is also the
  moment when the morning astronomical twilight starts.

- `nautical_dawn`: An
  [`hms`](https://hms.tidyverse.org/reference/hms.html) value indicating
  the moment when nautical dawn starts. This is also the moment when the
  morning nautical twilight starts.

- `dawn`: An [`hms`](https://hms.tidyverse.org/reference/hms.html) value
  indicating the moment when the dawn starts. This is also the moment
  when the morning nautical twilight ends and the morning civil twilight
  starts.

## Details

### `methods` argument

At the moment, `get_sun_stats()` can access the results of two APIs,
described below.

- `"suncalc"`: Use the sun statistics provided by the
  [`suncalc`](https://github.com/datastorm-open/suncalc) package.

- `"sunrise-sunset.org"`: Use the sun statistics provided by the
  [Sunrise-Sunset](https://sunrise-sunset.org/) API (requires an
  internet connection). Click [here](https://sunrise-sunset.org/api) to
  learn more.

The `"sunrise-sunset.org"` method tends to give a close, but usually
lower, result when compared with the `"suncalc"` method.

Please note that when using `method = "sunrise-sunset.org"` you need to
show attribution with a link to <https://sunrise-sunset.org>. Also note
that summer time adjustments are not included in the returned data when
using this method.

### Other statistics

The purpose of this function is to return basic statistics about the
sun. If you need other related statistics, we recommend checking the
following packages.

- [`cptec`](https://github.com/rpradosiqueira/cptec/): An Interface to
  the [CPTEC](https://www.cptec.inpe.br/)/
  [INPE](https://www.gov.br/inpe/) API.

- [`nasapower`](https://docs.ropensci.org/nasapower/): [NASA
  POWER](https://power.larc.nasa.gov/) API Client.

- [`rnoaa`](https://docs.ropensci.org/rnoaa/):
  [NOAA](https://www.noaa.gov/) Weather Data from R.

## See also

Other API functions:
[`get_from_zenodo()`](https://danielvartan.github.io/actverse/reference/get_from_zenodo.md)

## Examples

``` r
latitude <- -23.5489
longitude <- -46.6388
date <- Sys.Date()
tz <- "America/Sao_Paulo"

get_sun_stats(
  latitude = latitude,
  longitude = longitude,
  date = date,
  tz = tz,
  method = "suncalc"
)
#> $date
#> [1] "2025-11-22"
#> 
#> $latitude
#> [1] -23.5489
#> 
#> $longitude
#> [1] -46.6388
#> 
#> $tz
#> [1] "America/Sao_Paulo"
#> 
#> $sunrise_start
#> 05:13:24
#> 
#> $sunrise_end
#> 05:15:55
#> 
#> $golden_hour_end
#> 05:45:20
#> 
#> $solar_noon
#> 11:54:12
#> 
#> $golden_hour_start
#> 18:03:04
#> 
#> $sunset_start
#> 18:32:29
#> 
#> $sunset_end
#> 18:35:00
#> 
#> $dusk
#> 18:59:38
#> 
#> $nautical_dusk
#> 19:28:58
#> 
#> $night_start
#> 19:59:20
#> 
#> $nadir
#> 23:54:12
#> 
#> $night_end
#> 03:49:04
#> 
#> $nautical_dawn
#> 04:19:26
#> 
#> $dawn
#> 04:48:46
#> 

library(curl)

if (has_internet()) {
  get_sun_stats(
    latitude = latitude,
    longitude = longitude,
    date = date,
    tz = tz,
    method = "sunrise-sunset.org"
  )
}
#> $date
#> [1] "2025-11-22"
#> 
#> $latitude
#> [1] -23.5489
#> 
#> $longitude
#> [1] -46.6388
#> 
#> $tz
#> [1] "America/Sao_Paulo"
#> 
#> $sunrise_start
#> 05:04:03
#> 
#> $sunrise_end
#> NA
#> 
#> $golden_hour_end
#> NA
#> 
#> $solar_noon
#> 11:46:18
#> 
#> $golden_hour_start
#> NA
#> 
#> $sunset_start
#> NA
#> 
#> $sunset_end
#> 18:28:33
#> 
#> $dusk
#> 18:51:56
#> 
#> $nautical_dusk
#> 19:21:17
#> 
#> $night_start
#> 19:51:42
#> 
#> $nadir
#> NA
#> 
#> $night_end
#> 03:40:54
#> 
#> $nautical_dawn
#> 04:11:19
#> 
#> $dawn
#> 04:40:40
#> 
```
