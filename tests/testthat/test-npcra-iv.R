test_that('npcra_iv()', {

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
