test_that("find_epoch() | General test", {
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
    x = seq_along(timestamp)
  ) |>
    tsibble::tsibble(index = index)

  expect_equal(find_epoch(data_1)$best_match, 86400)
  expect_equal(find_epoch(data_2, 1)$best_match, as.numeric(NA))
})

test_that("find_epoch() | Error test", {
  data_1 <- dplyr::tibble(
    index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
    x = seq_along(index)
  ) |>
    tsibble::tsibble(index = index)

  data_2 <-
    dplyr::tibble(
      index = 1:10,
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(data, min_rows = 2, min_cols = 2)
  find_epoch(x = 1, threshold = 1) |>
    expect_error("Assertion on 'x' failed")

  # assert_index_class(data, c("Date", "POSIXt"))
  find_epoch(x = data_2, threshold = 1) |>
    expect_error("Assertion on 'x' failed")

  # checkmate::assert_number(threshold, lower = 0.001, upper = 1)
  find_epoch(x = data_1, threshold = 2) |>
    expect_error("Assertion on 'threshold' failed")
})
