#npcra_iv() tests
test_that('npcra_iv() | Random data', {
    # Random data
    # It is very likely that there are time intervals with no activity data
    # (since there are only 1000 data for 29 days -> 696 hours), the function
    # should still return the numerical value of IV60
    # Remember that although the IV values tend to remain between 0
    # and 2, depending on the distribution of the data this value can exceed 2
    # (something also likely in a random distribution)
    x <- runif(1000, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-30')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 1000) %>%
        sort()

    iv <- npcra_iv(x, timestamp)
    expect_true(dplyr::between(iv, left = 0, right = 3))
})

test_that('npcra_iv() | Senoidal data', {
    # Senoidal data
    # Data distributed according to a sinusoid must converge the
    # result to 0.
    # Note that this is a test of the algorithm theory, since by the
    # distribution used there are negative values of activity
    t <- seq(0, 4 * pi, length.out = 100)
    x <- 3*sin(2*t)+runif(100)*2

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-11')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 100) %>%
        sort()
    iv <- npcra_iv(x, timestamp)
    expect_true(dplyr::between(iv, left = 0, right = 1))
})

test_that('npcra_iv() | Insufficient data', {
    # Pass a time interval greater than the time interval between
    #the first and the last date data
    x <- runif(100, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-01 18:00:00')

    timestamp <- seq(first_date, last_date, by = "min") %>%
        sample(size = 100) %>%
        sort()

    expect_error(npcra_iv(x, timestamp, minutes_interval = 1440))
})

test_that('npcra_iv() | many records', {
    # Run for 1 million observation with 1-minute time interval
    x <- runif(10^6, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-30')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 10^6) %>%
        sort()

    iv <- npcra_iv(x, timestamp, minutes_interval = 1)
    expect_true(dplyr::between(iv, left = 0, right = 3))
})

test_that('npcra_iv() | One day', {
    # Random data for a single day
    # IV can be calculated with data for just one day, as it
    # depends on the variation between averages per minute regardless
    # of the day of collection
    x <- runif(100, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-01 23:59:59')

    timestamp <- seq(first_date, last_date, by = "min") %>%
        sample(size = 100) %>%
        sort()

    iv <- npcra_iv(x, timestamp)
    expect_true(dplyr::between(iv, left = 0, right = 3))
})

#npcra_ivm() tests
test_that('npcra_ivm() | Random data', {
    # Random data for 29 days
    x <- runif(1000, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-30')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 1000) %>%
        sort()

    ivm <- npcra_ivm(x, timestamp)
    expect_true(dplyr::between(ivm, left = 0, right = 3))
})

test_that('npcra_ivm() | Senoidal data', {
    # Senoidal data
    # Data distributed according to a sinusoid must converge the
    # result to 0.
    # Note that this is a test of the algorithm theory, since by the
    # distribution used there are negative values of activity
    t <- seq(0, 4 * pi, length.out = 100)
    x <- 3*sin(2*t)+runif(100)*2

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-11')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 100) %>%
        sort()
    ivm <- npcra_ivm(x, timestamp)
    expect_true(dplyr::between(ivm, left = 0, right = 1))
})

test_that('npcra_ivm() | Insufficient data', {
    # Pass a time interval greater than the time interval between
    #the first and the last date data
    x <- runif(100, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-01 18:00:00')

    timestamp <- seq(first_date, last_date, by = "min") %>%
        sample(size = 100) %>%
        sort()

    expect_error(npcra_ivm(x, timestamp, minute_limit = 1440))
})

test_that('npcra_ivm() | summarize = FALSE', {
    # Returns a tibble with the IVmean for the first 10 minutes on the first
    # line and each IV on the following lines (IV1, IV2, ..., IV10)
    minute_limit = 10
    x <- runif(1000, min = 0, max = 10000)

    first_date <- as.POSIXct('2015-01-01')
    last_date <- as.POSIXct('2015-01-30')

    timestamp <- seq(first_date, last_date, by = "sec") %>%
        sample(size = 1000) %>%
        sort()

    ivm <- npcra_ivm(x, timestamp, minute_limit, summarize = FALSE)
    expect_length(ivm, 2)
    expect_true(dplyr::between(ivm$iv[1], left = 0, right = 3))
    expect_true(nrow(ivm) == minute_limit + 1)
})
