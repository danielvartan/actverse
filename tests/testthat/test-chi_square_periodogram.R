#chi_square_periodogram() tests

test_that("chi_square_periodogram() |Random data", {
    # Basic function handling for data generated uniformly and not equidistant
    # in minutes and hours
    set.seed(1)

    #by minutes
    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-08')
    timestamp <- sample(seq(first_date, last_date, by = "min"), 1000)
    x <- runif(n = 1000, min = 0, max = 1000)
    act_min <- xts::as.xts(x, order.by = timestamp)

    xsp_min <- expect_error(chi_square_periodogram(act_min))
    xsp_min <- expect_warning(chi_square_periodogram(act_min, p_max = 1000))

    expect_length(xsp_min, 7)
    expect_length(xsp_min$NormalizedQp, 1000)
    expect_gte(xsp_min$Peak$normalized_qp, 0.9)
    expect_gte(xsp_min$Peak$p, 500)
    expect_equal(length(xsp_min$Ap), length(xsp_min$NormalizedQp))

    #by hours
    timestamp <- sample(seq(first_date, last_date, by = "hours"), 100)
    x <- runif(n = 100, min = 0, max = 1000)
    act_hour <- xts::as.xts(x, order.by = timestamp)

    xsp_hour <- expect_error(chi_square_periodogram(act_hour))
    xsp_hour <- expect_warning(chi_square_periodogram(act_hour, p_max = 100))

    expect_length(xsp_hour, 7)
    expect_length(xsp_hour$NormalizedQp, 100)
    expect_gte(xsp_hour$Peak$normalized_qp, 0.9)
    expect_gte(xsp_hour$Peak$p, 50)
    expect_equal(length(xsp_hour$Ap), length(xsp_hour$NormalizedQp))

    expect_gte(xsp_min$Peak$normalized_qp, xsp_hour$Peak$normalized_qp)
})

test_that("chi_square_periodogram() | Minutes to hours", {
    # Given a time series in minutes, calculate and compare the minute-by-minute
    # periodogram and the hour-by-hour periodogram
    set.seed(1)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-08')
    timestamp <- sample(seq(first_date, last_date, by = "min"), 1000)
    x <- runif(n = 1000, min = 0, max = 1000)
    act_min <- xts::as.xts(x, order.by = timestamp)
    xsp_min <- expect_warning(chi_square_periodogram(act_min, p_max = 1000))

    xsp <- expect_error(chi_square_periodogram(act_min,
                                               breaks="hours",
                                               p_max=1000))
    xsp <- expect_warning(chi_square_periodogram(act_min,
                                                 breaks="hours",
                                                 p_max=72))

    expect_gte(xsp_min$Peak$normalized_qp, xsp$Peak$normalized_qp)
})

test_that("chi_square_periodogram() | Sequential data", {
    # Time series with 1500 minute by minute (1 day and 6 hours) data, with
    # activity repetition every 60 minutes. the peak period should be divisible
    # by 60 and the normalized Qp should tend to 1
    set.seed(1)

    x <- rep(seq(1,60), times = 30)
    timestamp <- seq(as.POSIXct("2020-01-01"),
                     as.POSIXct("2020-01-02 05:59:59"),
                     by="min")

    act <- xts::as.xts(x, order.by=timestamp)
    xsp <- chi_square_periodogram(act, breaks = "minutes", p_max = 1500)

    expect_true(xsp$Peak$p %% 60 == 0)
    expect_gte(xsp$Peak$normalized_qp, 0.99)
})

