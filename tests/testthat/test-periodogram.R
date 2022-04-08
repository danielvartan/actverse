test_that("periodogram() | random data", {
    set.seed(1)

    dplyr::tibble(
        index = sample(seq(as.POSIXct('2015-01-01'), as.POSIXct('2015-01-08'),
                           by = "min"), 1000),
        x = runif(n = 1000, min = 0, max = 1000)) %>%
        tsibble::tsibble(index = index) %>%
        periodogram("x") %>%
        expect_error()

    dplyr::tibble(
        index = sample(seq(as.POSIXct('2015-01-01'), as.POSIXct('2015-01-08'),
                           by = "hours"), 100),
        x = runif(n = 100, min = 0, max = 1000)) %>%
        tsibble::tsibble(index = index) %>%
        periodogram("x") %>%
        expect_error()
})

test_that("periodogram() | minutes to hours", {
    # Given a time series in minutes, calculate and compare the
    # minute-by-minute periodogram and the hour-by-hour periodogram.

    set.seed(1)

    dplyr::tibble(
        index = sample(seq(as.POSIXct('2015-01-01'), as.POSIXct('2015-01-08'),
                           by = "min"), 1000),
        x = runif(n = 1000, min = 0, max = 1000)) %>%
        tsibble::tsibble(index = index) %>%
        periodogram("x", p_max = 1000) %>%
        expect_error()
})

test_that("periodogram() | error test", {
    data <- dplyr::tibble(
        index = seq(as.POSIXct('2015-01-01'), as.POSIXct('2015-01-08'),
                    by = "min"),
        x = runif(n = 10081, min = 0, max = 1000)) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(periodogram(data = 1, col = "x", p_unit = "minutes", p_min = 1,
                             p_max = 4600, p_step = 1),
                 "Assertion on 'data' failed")

    # checkmate::assert_choice(p_unit, p_unit_choices)
    expect_error(periodogram(data = data, col = "x", p_unit = "a", p_min = 1,
                             p_max = 4600, p_step = 1),
                 "Assertion on 'p_unit' failed")

    # checkmate::assert_int(p_min, lower = 1)
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = "a", p_max = 4600, p_step = 1),
                 "Assertion on 'p_min' failed")

    # checkmate::assert_int(p_max, lower = 1)
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = 1,  p_max = "a", p_step = 1),
                 "Assertion on 'p_max' failed")

    # checkmate::assert_int(p_step, lower = 1)
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = 1, p_max = 4600, p_step = "a"),
                 "Assertion on 'p_step' failed")
})

test_that("compute_periodogram() | general test", {

})

test_that("compute_periodogram() | error test", {

})

test_that("find_periodogram_peaks() | general test", {

})

test_that("find_periodogram_peaks() | error test", {

})

test_that("clean_periodogram_peaks() | general test", {

})

test_that("clean_periodogram_peaks() | error test", {

})

test_that("plot_periodogram() | general test", {

})

test_that("plot_periodogram() | error test", {

})
