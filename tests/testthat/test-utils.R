test_that("head_() | general test", {
  expect_equal(
    head_(list(a = 1:10, b = 1:10), n = 5),
    list(a = 1:10, b = 1:10)[seq_len(2)]
  )
  expect_equal(
    head_(data.frame(a = 1:10, b = 1:10), n = 15),
    data.frame(a = 1:10, b = 1:10)[seq_len(10), ]
  )
  expect_equal(head_(1:10, n = 15), c(1:10)[seq_len(10)])
})

test_that("head_() | error test", {
  # checkmate::assert_int(n, lower = 1)
  expect_error(head_(1:10, n = 0), "Assertion on 'n' failed")
})

test_that("period_() | general test", {
  expect_equal(period_(1, unit = "seconds"), lubridate::seconds())
  expect_equal(period_(1, unit = "microseconds"), lubridate::microseconds())
  expect_equal(period_(1, unit = "milliseconds"), lubridate::milliseconds())
  expect_equal(period_(1, unit = "quarters"), lubridate::period(3, "months"))
})

test_that("period_() | error test", {
  # checkmate::assert_number(num)
  expect_error(period_(""), "Assertion on 'num' failed")

  # checkmate::assert_choice(unit, unit_choices)
  expect_error(period_(1, unit = ""), "Assertion on 'unit' failed")
})

test_that("string_to_period() | general test", {
  expect_equal(string_to_period("microsecond"), lubridate::dmicroseconds())
  expect_equal(string_to_period("millisecond"), lubridate::dmilliseconds())
  expect_equal(string_to_period("second"), lubridate::duration("seconds"))
  expect_equal(string_to_period("minute"), lubridate::duration("minute"))
  expect_equal(string_to_period("hour"), lubridate::duration("hour"))
  expect_equal(string_to_period("week"), lubridate::duration("week"))
  expect_equal(string_to_period("day"), lubridate::duration("day"))

  expect_equal(
    string_to_period("month", irregularity = "min"),
    lubridate::ddays(28)
  )
  expect_equal(
    string_to_period("month", irregularity = "mean"),
    lubridate::dmonths()
  )
  expect_equal(
    string_to_period("month", irregularity = "max"),
    lubridate::ddays(31)
  )

  expect_equal(
    string_to_period("quarter", irregularity = "min"),
    lubridate::ddays(28) + (lubridate::ddays(30) * 2)
  )
  expect_equal(
    string_to_period("quarter", irregularity = "mean"),
    lubridate::dmonths(3)
  )
  expect_equal(
    string_to_period("quarter", irregularity = "max"),
    lubridate::ddays(31) * 3
  )

  expect_equal(
    string_to_period("year", irregularity = "min"),
    lubridate::ddays(365)
  )
  expect_equal(
    string_to_period("year", irregularity = "mean"),
    lubridate::dyears()
  )
  expect_equal(
    string_to_period("year", irregularity = "max"),
    lubridate::ddays(366)
  )
})

test_that("string_to_period() | Error test", {
  # checkmate::assert_choice(string, string_choices)
  string_to_period(
    string = "",
    irregularity = "min"
  ) |>
    expect_error("Assertion on 'string' failed")

  # checkmate::assert_choice(irregularity, irregularity_choices)
  string_to_period(
    string = "seconds",
    irregularity = ""
  ) |>
    expect_error("Assertion on 'irregularity' failed")
})

test_that("period_to_string() | General test", {
  expect_equal(period_to_string(lubridate::dmicroseconds()), "microseconds")
  expect_equal(period_to_string(lubridate::dmilliseconds()), "milliseconds")
  expect_equal(period_to_string(lubridate::dseconds()), "seconds")
  expect_equal(period_to_string(lubridate::dminutes()), "minutes")
  expect_equal(period_to_string(lubridate::dhours()), "hours")
  expect_equal(period_to_string(lubridate::ddays()), "days")
  expect_equal(period_to_string(lubridate::dweeks()), "weeks")
  expect_equal(period_to_string(15), as.character(NA))
})

test_that("period_to_string() | General test", {
  # checkmate::assert_number(period)
  period_to_string(period = "") |>
    expect_error("Assertion on 'period' failed")
})
