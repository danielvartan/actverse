test_that("periodogram() | Random data", {
    # Basic function handling for data generated uniformly and not equidistant
    # in minutes and hours.
    set.seed(1)

    # By minutes

    index <- sample(seq(as.POSIXct('2015-01-01'),
                        as.POSIXct('2015-01-08'),
                        by = "min"), 1000)
    data <- runif(n = 1000, min = 0, max = 1000) %>%
        xts::as.xts(order.by = index)

    expect_error(periodogram(data))

    # By hours

    index <- sample(seq(as.POSIXct('2015-01-01'),
                        as.POSIXct('2015-01-08'),
                        by = "hours"),
                    100)
    data <- runif(n = 100, min = 0, max = 1000) %>%
        xts::as.xts(order.by = index)

    expect_error(periodogram(data))
})

test_that("periodogram() | Minutes to hours", {
    # Given a time series in minutes, calculate and compare the
    # minute-by-minute periodogram and the hour-by-hour periodogram.

    set.seed(1)

    index <- sample(seq(as.POSIXct('2015-01-01'),
                        as.POSIXct('2015-01-08'),
                        by = "min"),
                    1000)
    data <- runif(n = 1000, min = 0, max = 1000) %>%
        xts::as.xts(order.by = index)

    expect_error(periodogram(data, p_max = 1000))
})

test_that("periodogram() | Error test", {
    index <- seq(as.POSIXct("2020-01-01"),
                 as.POSIXct("2020-01-02 05:59:59"),
                 by="min")
    data <- rep(seq(1,60), times = 30) %>%
        xts::as.xts(order.by = index)

    expect_error(periodogram(data = 1, breaks = "minutes", p_min = 1,
                             p_max = 4600, step = 1),
                 "Assertion on 'data' failed")
    expect_error(periodogram(data = data, breaks = "a", p_min = 1,
                             p_max = 4600, step = 1),
                 "Assertion on 'breaks' failed")
    expect_error(periodogram(data = data, breaks = "minutes", p_min = "a",
                             p_max = 4600, step = 1),
                 "Assertion on 'p_min' failed")
    expect_error(periodogram(data = data, breaks = "minutes", p_min = 1,
                             p_max = "a", step = 1),
                 "Assertion on 'p_max' failed")
    expect_error(periodogram(data = data, breaks = "minutes", p_min = 1,
                             p_max = 4600, step = "a"),
                 "Assertion on 'step' failed")
})

