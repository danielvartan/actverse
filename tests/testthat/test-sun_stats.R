test_that("sun_stats() | general test", {
    object <- sun_stats(
        lat = 40.730610, lon = -73.935242, date = Sys.Date(),
        tz = "America/New_York", method = "suncalc"
    ) %>%
        magrittr::extract2("lat")

    expect_equal(object, 40.730610)

    mock_list <- list(
        results = list(
            sunrise = "9:40:45 AM",
            sunset = "12:03:29 AM",
            solar_noon = "4:52:07 PM",
            day_length = "14:22:44",
            civil_twilight_begin = "9:11:33 AM",
            civil_twilight_end = "12:32:41 AM",
            nautical_twilight_begin = "8:33:35 AM",
            nautical_twilight_end = "1:10:38 AM",
            astronomical_twilight_begin = "7:51:38 AM",
            astronomical_twilight_end = "1:52:36 AM"
        ),
        status = "OK"
    )

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) TRUE,
            read_json = function(...) mock_list,
            {magrittr::extract2(sun_stats(
                lat = 40.730610, lon = -73.935242, date = Sys.Date(),
                tz = "America/New_York", method = "sunrise-sunset.org"
            ),
            "sunrise_start"
            )}
        )
    }

    expect_equal(mock(), hms::parse_hms("04:44:43"))
})

test_that("sun_stats() | error test", {
    # checkmate::assert_number(lat, lower = -90, upper = 90)
    expect_error(sun_stats(
        lat = "", lon = 1, date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lat' failed"
    )
    expect_error(sun_stats(
        lat = -91, lon = 1, date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lat' failed"
    )

    expect_error(sun_stats(
        lat = 91, lon = 1, date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lat' failed"
    )

    # checkmate::assert_number(lon, lower = -180, upper = 180)
    expect_error(sun_stats(
        lat = 1, lon = "", date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lon' failed"
    )

    expect_error(sun_stats(
        lat = 1, lon = -181, date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lon' failed",
    )

    expect_error(sun_stats(
        lat = 1, lon = 181, date = Sys.Date(), tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'lon' failed",
    )

    # checkmate::assert_date(date, len = 1, any.missing = FALSE)
    expect_error(sun_stats(
        lat = 1, lon = 1, date = "", tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'date' failed",
    )

    expect_error(sun_stats(
        lat = 1, lon = 1, date = rep(Sys.Date(), 2), tz = "UTC",
        method = "suncalc"
    ),
    "Assertion on 'date' failed",
    )

    expect_error(sun_stats(
        lat = 1, lon = 1, date = c(Sys.Date(), NA), tz = "UTC",
        method = "suncalc"
    ),
    "Assertion on 'date' failed",
    )

    # checkmate::assert_choice(tz, OlsonNames())
    expect_error(sun_stats(
        lat = 1, lon = 1, date = Sys.Date(), tz = "", method = "suncalc"
    ),
    "Assertion on 'tz' failed"
    )

    # checkmate::assert_choice(method, method_choices)
    expect_error(sun_stats(
        lat = 1, lon = 1, date = Sys.Date(), tz = "UTC", method = ""
    ),
    "Assertion on 'method' failed",
    )
})

test_that("sun_stats_suncalc() | general test", {
    object <- sun_stats(
        lat = -23.5489, lon = -46.6388, date = Sys.Date(),
        tz = "America/Sao_Paulo", method = "suncalc"
    ) %>%
        magrittr::extract2("lat")

    expect_equal(object, -23.5489)
})

test_that("sun_stats_sunrise_sunset() | general test", {
    mock_list <- list(
        results = list(
            sunrise = "9:30:23 AM",
            sunset = "8:35:28 PM",
            solar_noon = "3:02:56 PM",
            day_length = "11:05:05",
            civil_twilight_begin = "9:07:53 AM",
            civil_twilight_end = "8:57:59 PM",
            nautical_twilight_begin = "8:40:40 AM",
            nautical_twilight_end = "9:25:11 PM",
            astronomical_twilight_begin = "8:13:45 AM",
            astronomical_twilight_end = "9:52:06 PM"
        ),
        status = "OK"
    )

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) TRUE,
            read_json = function(...) mock_list,
            {magrittr::extract2(sun_stats(
                lat = -23.5489, lon = -46.6388, date = Sys.Date(),
                tz = "America/Sao_Paulo", method = "sunrise-sunset.org"
            ),
            "sunrise_start"
            )}
        )
    }

    expect_equal(mock(), hms::parse_hms("06:23:55"))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) TRUE,
            read_json = function(...) list(status == "NOT OK"),
            {sun_stats(
                lat = -23.5489, lon = -46.6388, date = Sys.Date(),
                tz = "America/Sao_Paulo", method = "sunrise-sunset.org"
            )}
        )
    }

    expect_error(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) FALSE,
            {sun_stats(
                lat = -23.5489, lon = -46.6388, date = Sys.Date(),
                tz = "America/Sao_Paulo", method = "sunrise-sunset.org"
            )}
        )
    }

    expect_error(mock())
})
