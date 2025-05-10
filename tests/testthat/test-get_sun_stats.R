test_that("get_sun_stats() | general test", {
  object <- get_sun_stats(
    latitude = 40.730610, longitude = -73.935242, date = Sys.Date(),
    tz = "America/New_York", method = "suncalc"
  ) |>
    magrittr::extract2("latitude")

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

  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    read_json = function(...) mock_list
  )

  get_sun_stats(
    latitude = 40.730610,
    longitude = -73.935242,
    date = Sys.Date(),
    tz = "America/New_York",
    method = "sunrise-sunset.org"
  ) |>
    magrittr::extract2("sunrise_start") |>
    expect_equal(hms::parse_hms("04:44:43"))
})

test_that("get_sun_stats() | error test", {
  # checkmate::assert_number(latitude, lower = -90, upper = 90)
  expect_error(
    get_sun_stats(
      latitude = "",
      longitude = 1,
      date = Sys.Date(),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'latitude' failed"
  )

  expect_error(
    get_sun_stats(
      latitude = -91,
      longitude = 1,
      date = Sys.Date(),
      tz = "UTC", method = "suncalc"
    ),
    "Assertion on 'latitude' failed"
  )

  expect_error(
    get_sun_stats(
      latitude = 91,
      longitude = 1,
      date = Sys.Date(),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'latitude' failed"
  )

  # checkmate::assert_number(longitude, lower = -180, upper = 180)
  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = "",
      date = Sys.Date(),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'longitude' failed"
  )

  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = -181,
      date = Sys.Date(),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'longitude' failed",
  )

  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 181,
      date = Sys.Date(),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'longitude' failed",
  )

  # checkmate::assert_date(date, len = 1, any.missing = FALSE)
  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 1,
      date = "",
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'date' failed",
  )

  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 1,
      date = rep(Sys.Date(), 2),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'date' failed",
  )

  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 1,
      date = c(Sys.Date(), NA),
      tz = "UTC",
      method = "suncalc"
    ),
    "Assertion on 'date' failed",
  )

  # checkmate::assert_choice(tz, OlsonNames())
  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 1,
      date = Sys.Date(),
      tz = "",
      method = "suncalc"
    ),
    "Assertion on 'tz' failed"
  )

  # checkmate::assert_choice(method, method_choices)
  expect_error(
    get_sun_stats(
      latitude = 1,
      longitude = 1,
      date = Sys.Date(),
      tz = "UTC",
      method = ""
    ),
    "Assertion on 'method' failed",
  )
})

test_that("get_sun_stats_suncalc() | general test", {
  object <- get_sun_stats_suncalc(
    latitude = -23.5489,
    longitude = -46.6388,
    date = Sys.Date(),
    tz = "America/Sao_Paulo"
  ) |>
    magrittr::extract2("latitude")

  expect_equal(object, -23.5489)
})

test_that("get_sun_stats_sunrise_sunset() | general test", {
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

  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    read_json = function(...) mock_list
  )

  get_sun_stats_sunrise_sunset(
    latitude = -23.5489,
    longitude = -46.6388,
    date = Sys.Date(),
    tz = "America/Sao_Paulo"
  ) |>
    magrittr::extract2("sunrise_start") |>
    expect_equal(hms::parse_hms("06:23:55"))

  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    read_json = function(...) list(status = "NOT OK")
  )

  get_sun_stats_sunrise_sunset(
    latitude = -23.5489,
    longitude = -46.6388,
    date = Sys.Date(),
    tz = "America/Sao_Paulo"
  ) |>
    expect_error()
})
