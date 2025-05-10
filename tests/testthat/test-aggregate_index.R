test_that("aggregate_index() | General test", {
  data <-
    dplyr::tibble(
      index = lubridate::as_datetime(1:10000),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  # unit = "second"

  int_second <-
    dplyr::tibble(
      index = lubridate::as_datetime(1:2),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "second") |>
    tsibble::interval() |>
    expect_equal(int_second)

  # unit = "minute"

  int_minute <-
    dplyr::tibble(
      index = c(
        as.POSIXct("2020-01-01 00:00:00"),
        as.POSIXct("2020-01-01 00:01:00")
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "minute") |>
    tsibble::interval() |>
    expect_equal(int_minute)

  # unit = "hour"

  int_hour <-
    dplyr::tibble(
      index = c(
        as.POSIXct("2020-01-01 00:00:00"),
        as.POSIXct("2020-01-01 01:00:00")
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "hour") |>
    tsibble::interval() |>
    expect_equal(int_hour)

  # unit = "day"

  int_day <-
    dplyr::tibble(
      index = c(as.Date("2020-01-01"), as.Date("2020-01-02")),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  acttrust |>
    aggregate_index(unit = "day") |>
    tsibble::interval() |>
    expect_equal(int_day)

  # unit = "week"

  data <-
    dplyr::tibble(
      index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  int_week <-
    dplyr::tibble(
      index = c(
        tsibble::yearweek(as.Date("2020-01-01")),
        tsibble::yearweek(as.Date("2020-01-08"))
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "week") |>
    tsibble::interval() |>
    expect_equal(int_week)

  # unit = "month"

  int_month <-
    dplyr::tibble(
      index = c(
        tsibble::yearmonth(as.Date("2020-01-01")),
        tsibble::yearmonth(as.Date("2020-02-01"))
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "month") |>
    tsibble::interval() |>
    expect_equal(int_month)

  # unit = "quarter"

  int_quarter <-
    dplyr::tibble(
      index = c(
        tsibble::yearquarter(as.Date("2020-01-01")),
        tsibble::yearquarter(as.Date("2020-05-01"))
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "quarter") |>
    tsibble::interval() |>
    expect_equal(int_quarter)

  # unit = "year"

  int_year <-
    dplyr::tibble(
      index = c(2020, 2021, 2022),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index) |>
    tsibble::interval()

  data |>
    aggregate_index(unit = "year") |>
    tsibble::interval() |>
    expect_equal(int_year)
})

test_that("aggregate_index() | Error test", {
  data <-
    dplyr::tibble(
      index = seq(
        from = as.Date("2020-01-01"),
        to = as.Date("2023-01-01"),
        by = "day"
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(data, min_rows = 2, min_cols = 2)
  aggregate_index(
    data = 1,
    unit = "years",
    fun = mean,
    week_start = 1
  ) |>
    expect_error("Assertion on 'data' failed")

  # checkmate::assert_choice(unit, unit_choices)
  aggregate_index(
    data = data,
    unit = 1,
    fun = mean,
    week_start = 1
  ) |>
    expect_error("Assertion on 'unit' failed")

  # checkmate::assert_function(fun, null.ok = TRUE)
  aggregate_index(
    data = data,
    unit = "years",
    fun = 1,
    week_start = 1
  ) |>
    expect_error("Assertion on 'fun' failed")

  # checkmate::assert_choice(week_start, c(1, 7))
  aggregate_index(
    data = data,
    unit = "years",
    fun = mean,
    week_start = 2
  ) |>
    expect_error("Assertion on 'week_start' failed")

  # assert_epoch_compatibility(data, unit)
  aggregate_index(
    data = data,
    unit = "minutes",
    fun = mean,
    week_start = 1
  ) |>
    expect_error("Assertion on 'data' failed")

  # assert_index_class(data)
  data <-
    dplyr::tibble(
      index = c(2020, 2021, 2022),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  aggregate_index(
    data = data,
    unit = "minutes",
    fun = mean,
    week_start = 1
  ) |>
    expect_error("Assertion on 'data' failed")
})

test_that("aggregate_index_default_fun() | General test", {
  # if (is.numeric(x) && !all(nchar(x) == 1, na.rm = TRUE))
  aggregate_index_default_fun(1:100) |> expect_equal(mean(1:100))

  # else
  aggregate_index_default_fun(1:9) |> expect_equal(1)
  aggregate_index_default_fun(as.numeric(NA)) |> expect_equal(as.numeric(NA))
  aggregate_index_default_fun(c("a", "a", "b")) |> expect_equal("a")
  aggregate_index_default_fun(c("a", "a", "a")) |> expect_equal("a")
  aggregate_index_default_fun(c("a", "b", "c")) |> expect_equal("a")
  aggregate_index_default_fun(c(NA, NA)) |> expect_equal(NA)
})

test_that("aggregate_index_default_fun() | Error test", {
  # checkmate::assert_atomic_vector(x)
  aggregate_index_default_fun(list(a = 1)) |>
    expect_error("Assertion on 'x' failed")
})
