#' Get sun related statistics from different APIs
#'
#' @description
#'
#' `get_sun_stats()` retrieves sun statistics using different models and
#' data sources.
#'
#' At the moment, none of the `get_sun_stats()` methods uses real-world data.
#' All of them are based on models.
#'
#' Each API have its peculiarities. We recommend checking the API documentation
#' for a better understanding of the mechanisms behind them (see the Details
#' section).
#'
#' @details
#'
#' ## `methods` argument
#'
#' At the moment, `get_sun_stats()` can access the results of two APIs,
#' described below.
#'
#' - `"suncalc"`: Use the sun statistics provided by the
#' [`suncalc`](https://github.com/datastorm-open/suncalc) package.
#' - `"sunrise-sunset.org"`: Use the sun statistics provided by the
#' [Sunrise-Sunset](https://sunrise-sunset.org/) API (requires an internet
#' connection). Click [here](https://sunrise-sunset.org/api) to learn more.
#'
#' The `"sunrise-sunset.org"` method tends to give a close, but usually lower,
#' result when compared with the `"suncalc"` method.
#'
#' Please note that when using `method = "sunrise-sunset.org"` you need to show
#' attribution with a link to \url{https://sunrise-sunset.org}. Also note that
#' summer time adjustments are not included in the returned data when using this
#' method.
#'
#' ## Other statistics
#'
#' The purpose of this function is to return basic statistics about the sun. If
#' you need other related statistics, we recommend checking the following
#' packages.
#'
#' - [`cptec`](https://github.com/rpradosiqueira/cptec/): An Interface to the
#' [CPTEC](https://www.cptec.inpe.br/)/
#' [INPE](https://www.gov.br/inpe/) API.
#' - [`nasapower`](https://docs.ropensci.org/nasapower/): [NASA
#'   POWER](https://power.larc.nasa.gov/) API Client.
#' - [`rnoaa`](https://docs.ropensci.org/rnoaa/): [NOAA](https://www.noaa.gov/)
#'   Weather Data from R.
#'
#' @param latitude A number indicating the latitude of the desired location in
#'   decimal degrees.
#' @param longitude A number indicating the longitude of the desired location in
#'   decimal degrees.
#' @param date (optional) A [`Date`][base::as.Date()] value indicating the
#'   moment in time (default: `Sys.Date()`).
#' @param tz (optional) A string indicating the time zone of the results. See
#'   [`timezones`][base::timezones] to learn more (default: `"UTC"`).
#' @param method (optional) A string indicating which method or API to use.
#'   Valid values are: `"suncalc"` and `"sunrise-sunset.org"`. See the Details
#'   section to learn more (default: `"suncalc"`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' - `date`: A [`Date`][base::as.Date()] vector with the same value of the
#' `date` parameter.
#' - `latitude`: A number with the same value of the `latitude` parameter.
#' - `longitude`: A number with the same value of the `longitude` parameter.
#' - `tz`: A string with the same value of the `tz` parameter.
#' - `sunrise_start`: An [`hms`][hms::hms()] value indicating the moment when
#' the top edge of the sun appears on the horizon.
#' - `sunrise_end`: An [`hms`][hms::hms()] value indicating the moment when
#' bottom edge of the sun touches the horizon.
#' - `golden_hour_end`: An [`hms`][hms::hms()] value indicating the moment when
#' the morning golden hour (soft light, best time for photography) ends.
#' - `solar_noon`: An [`hms`][hms::hms()] value indicating the moment when sun
#' is in the highest position.
#' - `golden_hour_start`: An [`hms`][hms::hms()] value indicating the moment
#' when the evening golden hour (soft light, best time for photography) starts.
#' - `sunset_start`: An [`hms`][hms::hms()] value indicating the moment when the
#' bottom edge of the sun touches the horizon.
#' - `sunset_end`: An [`hms`][hms::hms()] value indicating the moment when the
#' sun disappears below the horizon. This is also the moment when the evening
#' civil twilight starts.
#' - `dusk`: An [`hms`][hms::hms()] value indicating the moment when the dusk
#' starts. This is also the moment when the evening nautical twilight starts.
#' - `nautical_dusk`: An [`hms`][hms::hms()] value indicating the moment when
#' nautical dusk starts. This is also the moment when the evening astronomical
#' twilight starts.
#' - `night_start`: An [`hms`][hms::hms()] value indicating the moment when
#' the night starts (dark enough for astronomical observations).
#' - `nadir`: An [`hms`][hms::hms()] value indicating the darkest moment of the
#' night, i.e., when the sun is in the lowest position.
#' - `night_end`: An [`hms`][hms::hms()] value indicating the moment when the
#' night ends. This is also the moment when the morning astronomical twilight
#' starts.
#' - `nautical_dawn`:  An [`hms`][hms::hms()] value indicating the moment when
#' nautical dawn starts. This is also the moment when the morning nautical
#' twilight starts.
#' - `dawn`: An [`hms`][hms::hms()] value indicating the moment when the dawn
#' starts. This is also the moment when the morning nautical twilight ends and
#' the morning civil twilight starts.
#'
#' @family API functions
#'
#' @export
#'
#' @examples
#' latitude <- -23.5489
#' longitude <- -46.6388
#' date <- Sys.Date()
#' tz <- "America/Sao_Paulo"
#'
#' get_sun_stats(
#'   latitude = latitude,
#'   longitude = longitude,
#'   date = date,
#'   tz = tz,
#'   method = "suncalc"
#' )
#'
#' library(curl)
#'
#' if (has_internet()) {
#'   get_sun_stats(
#'     latitude = latitude,
#'     longitude = longitude,
#'     date = date,
#'     tz = tz,
#'     method = "sunrise-sunset.org"
#'   )
#' }
get_sun_stats <- function(
  latitude,
  longitude,
  date = Sys.Date(),
  tz = "UTC",
  method = "suncalc"
) {
  method_choices <- c("suncalc", "sunrise-sunset.org")

  checkmate::assert_number(latitude, lower = -90, upper = 90)
  checkmate::assert_number(longitude, lower = -180, upper = 180)
  checkmate::assert_date(date, len = 1, any.missing = FALSE)
  checkmate::assert_choice(tz, OlsonNames())
  checkmate::assert_choice(method, method_choices)

  if (method == "suncalc") {
    get_sun_stats_suncalc(
      latitude = latitude,
      longitude = longitude,
      date = date,
      tz = tz
    )
  } else if (method == "sunrise-sunset.org") {
    get_sun_stats_sunrise_sunset(
      latitude = latitude,
      longitude = longitude,
      date = date,
      tz = tz
    )
  }
}

get_sun_stats_suncalc <- function(
  latitude,
  longitude,
  date = Sys.Date(),
  tz = "UTC"
) {
  checkmate::assert_number(latitude, lower = -90, upper = 90)
  checkmate::assert_number(longitude, lower = -180, upper = 180)
  checkmate::assert_date(date, len = 1, any.missing = FALSE)
  checkmate::assert_choice(tz, OlsonNames())

  # R CMD Check variable bindings fix
  # nolint start
  sunrise <- sunriseEnd <- goldenHourEnd <- solarNoon <- NULL
  solarNoon <- goldenHour <- sunsetStart <- sunset <- NULL
  nauticalDusk <- nightEnd <- nauticalDawn <- night <- NULL
  where <- sunrise_start <- sunrise_end <- golden_hour_end <- NULL
  solar_noon <- golden_hour_start <- sunset_start <- sunset_end <- NULL
  dusk <- nautical_dusk <- night_start <- nadir <- NULL
  night_end <- nautical_dawn <- dawn <- lat <- lon <- NULL
  # nolint end

  suncalc::getSunlightTimes(
    date = date,
    lat = latitude,
    lon = longitude,
    tz = tz
  ) |>
    dplyr::mutate(tz = tz) |>
    dplyr::rename(
      latitude = lat,
      longitude = lon,
      sunrise_start = sunrise,
      sunrise_end = sunriseEnd,
      golden_hour_end = goldenHourEnd,
      solar_noon = solarNoon,
      golden_hour_start = goldenHour,
      sunset_start = sunsetStart,
      sunset_end = sunset,
      nautical_dusk = nauticalDusk,
      night_end = nightEnd,
      nautical_dawn = nauticalDawn,
      night_start = night
    ) |>
    dplyr::mutate(dplyr::across(
      where(lubridate::is.POSIXt),
      hms::as_hms
    )) |>
    dplyr::select(
      date, latitude, longitude, tz, sunrise_start, sunrise_end,
      golden_hour_end, solar_noon, golden_hour_start, sunset_start,
      sunset_end, dusk, nautical_dusk, night_start, nadir, night_end,
      nautical_dawn, dawn
    ) |>
    as.list()
}

get_sun_stats_sunrise_sunset <- function(
  latitude,
  longitude,
  date = Sys.Date(),
  tz = "UTC"
) {
  checkmate::assert_number(latitude, lower = -90, upper = 90)
  checkmate::assert_number(longitude, lower = -180, upper = 180)
  checkmate::assert_date(date, len = 1, any.missing = FALSE)
  checkmate::assert_choice(tz, OlsonNames())
  assert_internet()

  # R CMD Check variable bindings fix
  # nolint start
  sunrise <- sunset <- civil_twilight_end <- nautical_twilight_end <- NULL
  astronomical_twilight_end <- astronomical_twilight_begin <- NULL
  nautical_twilight_begin <- civil_twilight_begin <- NULL
  sunrise_start <- sunrise_end <- golden_hour_end <- solar_noon <- NULL
  golden_hour_start <- sunset_start <- sunset_end <- dusk <- NULL
  nautical_dusk <- night_start <- nadir <- night_end <- nautical_dawn <- NULL
  dawn <- lat <- lon <- NULL
  # nolint end

  get <-
    paste0(
      "https://api.sunrise-sunset.org/json?",
      "lat=",
      latitude,
      "&lng=",
      longitude,
      "&date=",
      date
    ) |>
    read_json()

  if (!get$status == "OK") {
    cli::cli_abort(paste0(
      "{.strong {cli::col_blue('sunrise-sunset.org')}} ",
      "returned a {.strong {cli::col_red('NOT OK')}} status. ",
      "This may be caused by wrong parameters or by API ",
      "shutdown."
    ))
  } else {
    get |>
      magrittr::extract2("results") |>
      dplyr::as_tibble() |>
      dplyr::mutate(dplyr::across(
        !dplyr::matches("^day_length$"),
        ~ lubridate::parse_date_time(.x, "IMS p") |>
          lubridate::with_tz(tzone = tz) |>
          hms::as_hms()
      )) |>
      dplyr::mutate(
        date = date,
        lat = latitude,
        lon = longitude,
        tz = tz,
        sunrise_end = hms::as_hms(NA),
        golden_hour_end = hms::as_hms(NA),
        golden_hour_start = hms::as_hms(NA),
        sunset_start = hms::as_hms(NA),
        nadir = hms::as_hms(NA)
      ) |>
      dplyr::rename(
        latitude = lat,
        longitude = lon,
        sunrise_start = sunrise,
        sunset_end = sunset,
        dusk = civil_twilight_end,
        nautical_dusk = nautical_twilight_end,
        night_start = astronomical_twilight_end,
        night_end = astronomical_twilight_begin,
        nautical_dawn = nautical_twilight_begin,
        dawn = civil_twilight_begin
      ) |>
      dplyr::select(
        date, latitude, longitude, tz, sunrise_start, sunrise_end,
        golden_hour_end, solar_noon, golden_hour_start, sunset_start,
        sunset_end, dusk, nautical_dusk, night_start, nadir, night_end,
        nautical_dawn, dawn
      ) |>
      as.list()
  }
}
