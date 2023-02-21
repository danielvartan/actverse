#' Get sun related statistics from different APIs
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `get_sun_stats()` allows you to get sun statistics using different APIs.
#'
#' At the moment, none of the `get_sun_stats()` methods use real world data. All
#' of them are based on models.
#'
#' Each API have its peculiarities. We recommend checking the API documentation
#' for a better understanding of the mechanisms behind them.
#'
#' @details
#'
#' ## Requirements
#'
#' When `method = "suncalc"`, `get_sun_stats()` will require the
#' [`suncalc`](https://github.com/datastorm-open/suncalc) package.
#'
#' When `method = "sunrise-sunset.org"`, `get_sun_stats()` will require an
#' internet connection and the [`curl`](https://github.com/jeroen/curl) and
#' [`jsonlite`](https://github.com/jeroen/jsonlite) packages.
#'
#' If you don't have any or one of the packages mentioned above, you can install
#' them with `install.packages("curl", "jsonlite", "suncalc")`.
#'
#' ## `methods` argument
#'
#' At the moment, `get_sun_stats()` can access the results of two APIs,
#' described below.
#'
#' * `"suncalc"`: Use the sun statistics provided by the
#' [`suncalc`](https://github.com/datastorm-open/suncalc) package.
#' * `"sunrise-sunset.org"`: Use the sun statistics provided by the
#' \url{https://sunrise-sunset.org/} API (requires an internet connection). See
#' \url{https://sunrise-sunset.org/api} to learn more.
#'
#' The `"sunrise-sunset.org"` method tends to give a close, but usually lower,
#' result when compared with the `"suncalc"` method.
#'
#' Please note that when using `method = "sunrise-sunset.org"` you need to show
#' attribution with a link to \url{https://sunrise-sunset.org/}. Also note that
#' summer time adjustments are not included in the returned data when using
#' this method.
#'
#' ## Other statistics
#'
#' The purpose of this function is to return basic statistics about the sun. If
#' you need other related statistics, we recommend checking the following
#' packages.
#'
#' * [`cptec`](https://github.com/rpradosiqueira/cptec): An Interface to the
#' 'CPTEC/INPE' API.
#' * [`nasapower`](https://docs.ropensci.org/nasapower/): NASA POWER API Client.
#' * [`rnoaa`](https://docs.ropensci.org/rnoaa/): 'NOAA' Weather Data from R.
#'
#' @param lat A number indicating the latitude, in decimal degrees,
#' of the desired location.
#' @param lon A number indicating the longitude, in decimal degrees,
#' of the desired location.
#' @param date (optional) a [`Date`][base::as.Date()] value indicating the
#'   moment in time (default: `Sys.Date()`).
#' @param tz (optional) a string indicating the time zone of the results. See
#'   [`timezones`][base::timezones] to learn more (default: `"UTC"`).
#' @param method (optional) a string indicating which API to use. Valid values
#'   are: `"suncalc"` and `"sunrise-sunset.org"`. See the Details section to
#'   learn more (default: `"suncalc"`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' * `date`: A [`Date`][base::as.Date()] object with the same value of the
#' `date` parameter.
#' * `lat`: A number with the same value of the `lat` parameter.
#' * `lon`: A number with the same value of the `lon` parameter.
#' * `tz`: A string with the same value of the `tz` parameter.
#' * `sunrise_start`: An [`hms`][hms::hms()] value indicating the moment when
#' the top edge of the sun appears on the horizon.
#' * `sunrise_end`: An [`hms`][hms::hms()] value indicating the moment when
#' bottom edge of the sun touches the horizon.
#' * `golden_hour_end`: An [`hms`][hms::hms()] value indicating the moment when
#' the morning golden hour (soft light, best time for photography) ends.
#' * `solar_noon`: An [`hms`][hms::hms()] value indicating the moment when sun
#' is in the highest position.
#' * `golden_hour_start`: An [`hms`][hms::hms()] value indicating the moment
#' when the evening golden hour (soft light, best time for photography) starts.
#' * `sunset_start`: An [`hms`][hms::hms()] value indicating the moment when the
#' bottom edge of the sun touches the horizon.
#' * `sunset_end`: An [`hms`][hms::hms()] value indicating the moment when the
#' sun disappears below the horizon. This is also the moment when the evening
#' civil twilight starts.
#' * `dusk`: An [`hms`][hms::hms()] value indicating the moment when the dusk
#' starts. This is also the moment when the evening nautical twilight starts.
#' * `nautical_dusk`: An [`hms`][hms::hms()] value indicating the moment when
#' nautical dusk starts. This is also the moment when the evening astronomical
#' twilight starts.
#' * `night_start`: An [`hms`][hms::hms()] value indicating the moment when
#' the night starts (dark enough for astronomical observations).
#' * `nadir`: An [`hms`][hms::hms()] value indicating the moment the darkest
#' moment of the night, i.e., when the sun is in the lowest position.
#' * `night_end`: An [`hms`][hms::hms()] value indicating the moment when the
#' night ends. This is also the moment when the morning astronomical twilight
#' starts.
#' * `nautical_dawn`:  An [`hms`][hms::hms()] value indicating the moment when
#' nautical dawn starts. This is also the moment when the morning nautical
#' twilight starts.
#' * `dawn`: An [`hms`][hms::hms()] value indicating the moment when the dawn
#' starts. This is also the moment when the morning nautical twilight ends and
#' the morning civil twilight starts.
#'
#' @family API functions
#' @export
#'
#' @examples
#' lat <- -23.5489
#' lon <- -46.6388
#' date <- Sys.Date()
#' tz = "America/Sao_Paulo"
#'
#' if (requireNamespace("suncalc", quietly = TRUE)) {
#'     get_sun_stats(lat = lat, lon, date, tz, method = "suncalc")
#' }
#'
#' if (requireNamespace("curl", quietly = TRUE) &&
#'     requireNamespace("jsonlite", quietly = TRUE)) {
#'     if (curl::has_internet()) {
#'         get_sun_stats(
#'             lat = lat, lon, date, tz, method = "sunrise-sunset.org"
#'             )
#'     }
#' }
get_sun_stats <- function(lat, lon, date = Sys.Date(), tz = "UTC",
                     method = "suncalc") {
    method_choices <- c("suncalc", "sunrise-sunset.org")

    checkmate::assert_number(lat, lower = -90, upper = 90)
    checkmate::assert_number(lon, lower = -180, upper = 180)
    checkmate::assert_date(date, len = 1, any.missing = FALSE)
    checkmate::assert_choice(tz, OlsonNames())
    checkmate::assert_choice(method, method_choices)

    if (method == "suncalc") {
        get_sun_stats_suncalc(lat = lat, lon = lon, date = date, tz = tz)
    } else if (method == "sunrise-sunset.org") {
        get_sun_stats_sunrise_sunset(
            lat = lat, lon = lon, date = date, tz = tz
        )
    }
}

get_sun_stats_suncalc <- function(lat, lon, date = Sys.Date(), tz = "UTC") {
    checkmate::assert_number(lat, lower = -90, upper = 90)
    checkmate::assert_number(lon, lower = -180, upper = 180)
    checkmate::assert_date(date, len = 1, any.missing = FALSE)
    checkmate::assert_choice(tz, OlsonNames())

    require_pkg("suncalc")

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter, object_name_linter.
    sunrise <- sunriseEnd <- goldenHourEnd <- solarNoon <- NULL
    solarNoon <- goldenHour <- sunsetStart <- sunset <- NULL
    nauticalDusk <- nightEnd <- nauticalDawn <- night <- NULL
    where <- sunrise_start <- sunrise_end <- golden_hour_end <- NULL
    solar_noon <- golden_hour_start <- sunset_start <- sunset_end <- NULL
    dusk <- nautical_dusk <- night_start <- nadir <- NULL
    night_end <- nautical_dawn <- dawn <- NULL
    # nolint end

    suncalc::getSunlightTimes(date = date, lat = lat, lon = lon, tz = tz) %>%
        dplyr::mutate(tz = tz) %>%
        dplyr::rename(
            sunrise_start = sunrise, sunrise_end = sunriseEnd,
            golden_hour_end = goldenHourEnd, solar_noon = solarNoon,
            golden_hour_start = goldenHour,
            sunset_start = sunsetStart, sunset_end = sunset,
            nautical_dusk = nauticalDusk, night_end = nightEnd,
            nautical_dawn = nauticalDawn, night_start = night
        ) %>%
        dplyr::mutate(dplyr::across(
            where(lubridate::is.POSIXt), hms::as_hms
        )) %>%
        dplyr::select(
            date, lat, lon, tz, sunrise_start, sunrise_end,
            golden_hour_end, solar_noon, golden_hour_start,
            sunset_start, sunset_end, dusk, nautical_dusk, night_start, nadir,
            night_end, nautical_dawn, dawn
        ) %>%
        as.list()
}

get_sun_stats_sunrise_sunset <- function(lat, lon, date = Sys.Date(),
                                         tz = "UTC") {
    checkmate::assert_number(lat, lower = -90, upper = 90)
    checkmate::assert_number(lon, lower = -180, upper = 180)
    checkmate::assert_date(date, len = 1, any.missing = FALSE)
    checkmate::assert_choice(tz, OlsonNames())
    assert_internet()

    require_pkg("curl", "jsonlite")

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter.
    sunrise <- sunset <- civil_twilight_end <- nautical_twilight_end <- NULL
    astronomical_twilight_end <- astronomical_twilight_begin <- NULL
    nautical_twilight_begin <- civil_twilight_begin <- NULL
    sunrise_start <- sunrise_end <- golden_hour_end <- solar_noon <- NULL
    golden_hour_start <- sunset_start <- sunset_end <- dusk <- NULL
    nautical_dusk <- night_start <- nadir <- night_end <- nautical_dawn <- NULL
    dawn <- NULL
    # nolint end

    get <- read_json(paste0(
        "https://api.sunrise-sunset.org/json?",
        "lat=", lat, "&lng=", lon, "&date=", date
    ))

    if (!get$status == "OK") {
        cli::cli_abort(paste0(
            "{.strong {cli::col_blue('sunrise-sunset.org')}} ",
            "returned a {.strong {cli::col_red('NOT OK')}} status. ",
            "This may be caused by wrong parameters or by API ",
            "shutdown."
        ))
    } else {
        get %>%
            magrittr::extract2("results") %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(dplyr::across(
                !dplyr::matches("^day_length$"),
                ~ lubridate::parse_date_time(.x, "IMS p") %>%
                    lubridate::with_tz(tzone = tz) %>%
                    hms::as_hms()
            )) %>%
            dplyr::mutate(
                date = date, lat = lat, lon = lon, tz = tz,
                sunrise_end = hms::as_hms(NA),
                golden_hour_end = hms::as_hms(NA),
                golden_hour_start = hms::as_hms(NA),
                sunset_start = hms::as_hms(NA),
                nadir = hms::as_hms(NA),

            ) %>%
            dplyr::rename(
                sunrise_start = sunrise,
                sunset_end = sunset,
                dusk = civil_twilight_end,
                nautical_dusk = nautical_twilight_end,
                night_start = astronomical_twilight_end,
                night_end = astronomical_twilight_begin,
                nautical_dawn = nautical_twilight_begin,
                dawn = civil_twilight_begin
            ) %>%
            dplyr::select(
                date, lat, lon, tz, sunrise_start, sunrise_end,
                golden_hour_end, solar_noon, golden_hour_start,
                sunset_start, sunset_end, dusk, nautical_dusk, night_start,
                nadir, night_end, nautical_dawn, dawn
            ) %>%
            as.list()
    }
}
