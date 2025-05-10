test_that("*_leq() | general test", {
  test_leq(1, 1) |> expect_true()
  test_leq(1, 2) |> expect_true()
  test_leq(2, 1) |> expect_false()

  check_leq(2, 1) |>
    checkmate::expect_string(pattern = "'2' must be less or equal to '1'")

  check_leq(2, 1, name_x = "x", name_y = "y") |>
    checkmate::expect_string(pattern = "'x' must be less or equal to 'y'")

  check_leq(1, 1) |> expect_true()
  check_leq(1, 2) |> expect_true()

  assert_leq(2, 1) |> expect_error()
  assert_leq(1, 1) |> expect_equal(1)
})

test_that("*_leq() | error test", {
  # checkmate::assert_number(x)
  expect_error(test_leq(NA, 1))
  expect_error(check_leq(NA, 1))

  # checkmate::assert_number(y)
  expect_error(test_leq(1, NA))
  expect_error(check_leq(1, NA))
})

test_that("*_tsibble() | general test", {
  data <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2020-01-02"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  expect_true(test_tsibble(data))
  expect_true(test_tsibble(NULL, null_ok = TRUE))
  expect_false(test_tsibble(1))
  expect_false(test_tsibble(data, min_rows = 3))
  expect_false(test_tsibble(data, min_cols = 3))
  expect_false(test_tsibble(NULL, null_ok = FALSE))

  expect_true(check_tsibble(data))
  expect_true(check_tsibble(NULL, null_ok = TRUE))
  checkmate::expect_string(check_tsibble(NULL),
    pattern = "'NULL' cannot be 'NULL'"
  )
  checkmate::expect_string(
    check_tsibble(1),
    pattern = "Must be of type 'tbl_ts' \\(tsibble\\), not 'numeric'"
  )
  checkmate::expect_string(
    check_tsibble(data, min_rows = 3),
    pattern = "Must have at least 3 rows, but has 2 rows"
  )
  checkmate::expect_string(
    check_tsibble(data, min_cols = 3),
    pattern = "Must have at least 3 cols, but has 2 cols"
  )

  expect_equal(assert_tsibble(data), data)
  expect_error(assert_tsibble(1), "Assertion on '1' failed")
})

test_that("*_tsibble() | error test", {
  data <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  # checkmate::assert_int(min_rows, lower = 1, null.ok = TRUE)
  expect_error(
    test_tsibble(
      x = data,
      min_rows = "",
      min_cols = 1,
      null_ok = TRUE
    ),
    "Assertion on 'min_rows' failed"
  )
  expect_error(
    check_tsibble(
      x = data, min_rows = "", min_cols = 1,
      null_ok = TRUE
    ),
    "Assertion on 'min_rows' failed"
  )

  # checkmate::assert_int(min_cols, lower = 1, null.ok = TRUE)
  expect_error(
    test_tsibble(
      x = data, min_rows = 1, min_cols = "",
      null_ok = TRUE
    ),
    "Assertion on 'min_cols' failed"
  )
  expect_error(
    check_tsibble(data,
      min_rows = 1, min_cols = "",
      null_ok = TRUE
    ),
    "Assertion on 'min_cols' failed"
  )

  # checkmate::assert_flag(null.ok)
  expect_error(
    test_tsibble(
      x = data, min_rows = 1, min_cols = 1,
      null_ok = ""
    ),
    "Assertion on 'null_ok' failed"
  )
  expect_error(
    check_tsibble(
      x = data, min_rows = 1, min_cols = 1,
      null_ok = ""
    ),
    "Assertion on 'null_ok' failed"
  )
})

test_that("*_index_class() | general test", {
  data <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  expect_true(test_index_class(data))
  expect_false(test_index_class(data, classes = "numeric"))

  expect_true(check_index_class(data))
  checkmate::expect_string(
    check_index_class(data, classes = "numeric"),
    pattern = "Must have an index of class 'numeric', not 'Date'"
  )

  expect_equal(assert_index_class(data), data)
  expect_error(
    assert_index_class(data, classes = "numeric"),
    "Assertion on 'data' failed"
  )
})

test_that("*_index_class() | error test", {
  data <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(x)
  expect_error(
    test_index_class(x = 1, classes = "Date"),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_index_class(x = 1, classes = "Date"),
    "Assertion on 'x' failed"
  )

  # checkmate::assert_character(classes)
  expect_error(
    test_index_class(x = data, classes = 1),
    "Assertion on 'classes' failed"
  )
  expect_error(
    check_index_class(x = data, classes = 1),
    "Assertion on 'classes' failed"
  )
})

test_that("*_regularity() | general test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <- dplyr::tibble(
    index = c(
      as.POSIXct(seq(60, 5400, by = 60), origin = lubridate::origin),
      as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
      as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
      as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
      as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
    ),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  expect_true(test_regularity(data_1))
  expect_false(test_regularity(data_2, strict = TRUE))
  expect_false(test_regularity(data_2, threshold = 0.9))

  expect_true(check_regularity(data_1))
  checkmate::expect_string(
    check_regularity(data_2, strict = TRUE),
    pattern = paste0(
      "'data_2' must be strictly regular. ",
      "See '\\?find_epoch' to learn more"
    )
  )
  checkmate::expect_string(
    check_regularity(data_2, threshold = 0.9),
    pattern = paste0(
      "'data_2' must have a regularity equal or greater ",
      "than 90%. See '\\?find_epoch' to learn more"
    )
  )

  expect_equal(assert_regularity(data_1), data_1)
  expect_error(assert_regularity(data_2), "Assertion on 'data_2' failed")

  expect_message(warn_regularity(data_2, strict = TRUE))
  expect_message(warn_regularity(data_2, threshold = 0.9))
})

test_that("*_regularity() | error test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(x, min_rows = 2, min_cols = 2)
  expect_error(
    test_regularity(x = 1, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_regularity(x = 1, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )
  expect_error(
    warn_regularity(x = 1, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )

  # assert_index_class(x)
  expect_error(
    test_regularity(x = data_2, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_regularity(x = data_2, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )
  expect_error(
    warn_regularity(x = data_2, threshold = 1, strict = FALSE),
    "Assertion on 'x' failed"
  )

  # checkmate::assert_number(threshold, lower = 0.001, upper = 1)
  expect_error(
    test_regularity(x = data_1, threshold = -1, strict = FALSE),
    "Assertion on 'threshold' failed"
  )
  expect_error(
    check_regularity(x = data_1, threshold = -1, strict = FALSE),
    "Assertion on 'threshold' failed"
  )
  expect_error(
    warn_regularity(x = data_1, threshold = -1, strict = FALSE),
    "Assertion on 'threshold' failed"
  )

  # checkmate::assert_flag(strict)
  expect_error(
    test_regularity(x = data_1, threshold = 1, strict = 1),
    "Assertion on 'strict' failed"
  )
  expect_error(
    check_regularity(x = data_1, threshold = 1, strict = 1),
    "Assertion on 'strict' failed"
  )
  expect_error(
    warn_regularity(x = data_1, threshold = 1, strict = 1),
    "Assertion on 'strict' failed"
  )
})

test_that("*_clear_epoch() | general test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <- dplyr::tibble(
    index = c(
      as.POSIXct(seq(60, 5400, by = 60), origin = lubridate::origin),
      as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
      as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
      as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
      as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
    ),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  expect_true(test_clear_epoch(data_1))
  expect_false(test_clear_epoch(data_2, threshold = 0.9))

  expect_true(check_clear_epoch(data_1))
  checkmate::expect_string(
    check_clear_epoch(data_2, threshold = 0.9),
    pattern = paste0(
      "'data_2' does not present a clear epoch/",
      "periodicity. See '\\?find_epoch' to learn more"
    )
  )

  expect_equal(assert_clear_epoch(data_1), data_1)
  expect_error(assert_clear_epoch(data_2), "Assertion on 'data_2' failed")
})

test_that("*_clear_epoch() | error test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(x, min_rows = 2, min_cols = 2)
  expect_error(
    test_clear_epoch(x = 1, threshold = 1),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_clear_epoch(x = 1, threshold = 1),
    "Assertion on 'x' failed"
  )

  # assert_index_class(x)
  expect_error(
    test_clear_epoch(x = data_2, threshold = 1),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_clear_epoch(x = data_2, threshold = 1),
    "Assertion on 'x' failed"
  )

  # checkmate::assert_number(threshold, lower = 0.001, upper = 1)
  expect_error(
    test_clear_epoch(x = data_1, threshold = -1),
    "Assertion on 'threshold' failed"
  )
  expect_error(
    check_clear_epoch(x = data_1, threshold = -1),
    "Assertion on 'threshold' failed"
  )
})

test_that("*_epoch_compatibility() | general test", {
  data <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  expect_true(test_epoch_compatibility(data, unit = "years"))
  expect_false(test_epoch_compatibility(data, unit = "hours"))

  expect_true(check_epoch_compatibility(data, unit = "years"))

  check_epoch_compatibility(data, unit = "hours") |>
    checkmate::expect_string()

  expect_equal(assert_epoch_compatibility(data, unit = "years"), data)
  expect_error(
    assert_epoch_compatibility(data, unit = "hours"),
    "Assertion on 'data' failed"
  )
})

test_that("*_epoch_compatibility() | error test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) |>
    tsibble::tsibble(index = index)

  data_3 <- dplyr::tibble(
    index = c(
      as.POSIXct(seq(60, 100, by = 60), origin = lubridate::origin),
      as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
      as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
      as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
      as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
    ),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(x, min_rows = 2, min_cols = 2)
  expect_error(
    test_epoch_compatibility(x = 1, unit = "years"),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_epoch_compatibility(x = 1, unit = "years"),
    "Assertion on 'x' failed"
  )

  # assert_index_class(x)
  expect_error(
    test_epoch_compatibility(x = data_2, unit = "years"),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_epoch_compatibility(x = data_2, unit = "years"),
    "Assertion on 'x' failed"
  )

  # assert_clear_epoch(x, 0.7)
  expect_error(
    test_epoch_compatibility(x = data_3, unit = "years"),
    "Assertion on 'x' failed"
  )
  expect_error(
    check_epoch_compatibility(x = data_3, unit = "years"),
    "Assertion on 'x' failed"
  )

  # checkmate::assert_choice(unit, unit_choices)
  expect_error(
    test_epoch_compatibility(x = data_1, unit = ""),
    "Assertion on 'unit' failed"
  )
  expect_error(
    check_epoch_compatibility(x = data_1, unit = ""),
    "Assertion on 'unit' failed"
  )
})
