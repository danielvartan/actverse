test_that("periodogram() | Random data", {
    # Basic function handling for data generated uniformly and not equidistant
    # in minutes and hours.
    set.seed(1)

    # By minutes

    timestamp <- sample(seq(as.POSIXct('2015-01-01'),
                            as.POSIXct('2015-01-08'),
                            by = "min"), 1000)
    data <- runif(n = 1000, min = 0, max = 1000) %>%
        xts::as.xts(order.by = timestamp)

    periodogram_min <- expect_error(periodogram(data))
    periodogram_min <- expect_message(periodogram(data, p_max = 1000))
    periodogram_min <- periodogram(data, p_max = 1000)

    expect_length(periodogram_min, 7)
    expect_length(periodogram_min$normalized_qp, 1000)
    expect_gte(periodogram_min$peak$normalized_qp, 0.9)
    expect_gte(periodogram_min$peak$p, 500)
    expect_equal(length(periodogram_min$ap),
                 length(periodogram_min$normalized_qp))

    # By hours

    timestamp <- sample(seq(as.POSIXct('2015-01-01'),
                            as.POSIXct('2015-01-08'),
                            by = "hours"),
                        100)
    data <- runif(n = 100, min = 0, max = 1000) %>%
        xts::as.xts(order.by = timestamp)

    periodogram_hour <- expect_error(periodogram(data))
    periodogram_hour <- expect_message(periodogram(data, p_max = 100))
    periodogram_hour <- periodogram(data, p_max = 100)

    expect_length(periodogram_hour, 7)
    expect_length(periodogram_hour$normalized_qp, 100)
    expect_gte(periodogram_hour$peak$normalized_qp, 0.9)
    expect_gte(periodogram_hour$peak$p, 50)
    expect_equal(length(periodogram_hour$ap),
                 length(periodogram_hour$normalized_qp))

    expect_gte(periodogram_min$peak$normalized_qp,
               periodogram_hour$peak$normalized_qp)
})

test_that("periodogram() | Minutes to hours", {
    # Given a time series in minutes, calculate and compare the
    # minute-by-minute periodogram and the hour-by-hour periodogram.

    set.seed(1)

    timestamp <- sample(seq(as.POSIXct('2015-01-01'),
                            as.POSIXct('2015-01-08'),
                            by = "min"),
                        1000)
    data <- runif(n = 1000, min = 0, max = 1000) %>%
        xts::as.xts(order.by = timestamp)

    periodogram_min <- expect_message(periodogram(data, p_max = 1000))
    periodogram_min <- periodogram(data, p_max = 1000)

    periodogram_hour <- expect_error(periodogram(data, breaks = "hours",
                                                 p_max = 1000))
    periodogram_hour <- expect_message(periodogram(data, breaks = "hours",
                                                   p_max = 72))
    periodogram_hour <- periodogram(data, breaks = "hours", p_max = 72)

    expect_gte(periodogram_min$peak$normalized_qp,
               periodogram_hour$peak$normalized_qp)
})

test_that("periodogram() | Sequential data", {
    # Time series with 1500 minute by minute (1 day and 6 hours) data, with
    # activity repetition every 60 minutes. the peak period should be divisible
    # by 60 and the normalized Qp should tend to 1.

    set.seed(1)

    timestamp <- seq(as.POSIXct("2020-01-01"),
                     as.POSIXct("2020-01-02 05:59:59"),
                     by="min")
    data <- rep(seq(1,60), times = 30) %>%
        xts::as.xts(order.by = timestamp)

    periodogram <- periodogram(data, breaks = "minutes", p_max = 1500)

    expect_true(periodogram$peak$p %% 60 == 0)
    expect_gte(periodogram$peak$normalized_qp, 0.99)
})

test_that("periodogram() | Error test", {
    timestamp <- seq(as.POSIXct("2020-01-01"),
                     as.POSIXct("2020-01-02 05:59:59"),
                     by="min")
    data <- rep(seq(1,60), times = 30) %>%
        xts::as.xts(order.by = timestamp)

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

