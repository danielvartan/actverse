test_that("get_raw_data() | general test", {
  get_raw_data() |>
    checkmate::expect_character(any.missing = FALSE, all.missing = FALSE)

  get_raw_data() |>
    magrittr::extract(1) |>
    get_raw_data() |>
    checkmate::expect_string(na.ok = FALSE)
})

test_that("get_raw_data() | error test", {
  # checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)
  get_raw_data(file = 1) |>
    expect_error("Assertion on 'file' failed")

  get_raw_data(file = as.character(NA)) |>
    expect_error("Assertion on 'file' failed")
})
