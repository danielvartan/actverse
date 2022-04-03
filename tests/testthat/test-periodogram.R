test_that("periodogram() | Random data", {
    set.seed(1)

    tsibble::tsibble(timestamp = sample(seq(as.POSIXct('2015-01-01'),
                                            as.POSIXct('2015-01-08'),
                                            by = "min"), 1000),
                     x = runif(n = 1000, min = 0, max = 1000)) %>%
        periodogram("x") %>%
        expect_error()

    tsibble::tsibble(timestamp = sample(seq(as.POSIXct('2015-01-01'),
                                            as.POSIXct('2015-01-08'),
                                            by = "hours"), 100),
                     x = runif(n = 100, min = 0, max = 1000)) %>%
        periodogram("x") %>%
        expect_error()
})

test_that("periodogram() | Minutes to hours", {
    # Given a time series in minutes, calculate and compare the
    # minute-by-minute periodogram and the hour-by-hour periodogram.

    set.seed(1)

    tsibble::tsibble(timestamp = sample(seq(as.POSIXct('2015-01-01'),
                                            as.POSIXct('2015-01-08'),
                                            by = "min"), 1000),
                     x = runif(n = 1000, min = 0, max = 1000)) %>%
        periodogram("x", p_max = 1000) %>%
        expect_error()
})

test_that("periodogram() | Error test", {
    data <- tsibble::tsibble(timestamp = seq(as.POSIXct('2015-01-01'),
                                             as.POSIXct('2015-01-08'),
                                             by = "min"),
                             x = runif(n = 10081, min = 0, max = 1000))

    expect_error(periodogram(data = 1, col = "x", p_unit = "minutes", p_min = 1,
                             p_max = 4600, p_step = 1),
                 "Assertion on 'data' failed")
    expect_error(periodogram(data = data, col = "x", p_unit = "a", p_min = 1,
                             p_max = 4600, p_step = 1),
                 "Assertion on 'p_unit' failed")
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = "a", p_max = 4600, p_step = 1),
                 "Assertion on 'p_min' failed")
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = 1,  p_max = "a", p_step = 1),
                 "Assertion on 'p_max' failed")
    expect_error(periodogram(data = data, col = "x", p_unit = "minutes",
                             p_min = 1, p_max = 4600, p_step = "a"),
                 "Assertion on 'p_step' failed")
})

