 test_that("npcra_m10_whole_period()", {
     #Random data
     set.seed(1)
     first_date <- as.POSIXct('2015-01-01')
     last_date <- as.POSIXct('2015-01-15')
     shuffled_timestamp <- sample(seq(first_date, last_date, by = "min"), 10000)
     timestamp <- sort(shuffled_timestamp)
     x <- runif(10000, 0, 10000)
     m10_data <- npcra_m10_whole_period(x, timestamp)
     expect_equal(m10_data$m10, 5370.67)
     expect_equal(m10_data$start_date, as.POSIXct("2015-01-07 01:52:00 -02"))

     #Large input (1 million entries) ~2min to run
     # shuffled_timestamp <- sample(seq(first_date, last_date, by = "sec"), 10**6)
     # timestamp <- sort(shuffled_timestamp)
     # x <- runif(10**6, 0, 10000)
     # m10_data <- npcra_m10_whole_period(x, timestamp)
     # expect_equal(length(m10_data), 2)
     # expect_is(m10_data$m10, 'numeric')
     # expect_is(m10_data$start_date, 'POSIXct')

     #Data that does not complete at least one 10-hour period
     last_date <- as.POSIXct('2015-01-01 09:59:59')
     shuffled_timestamp <- sample(seq(first_date, last_date, by = "min"), 100)
     timestamp <- sort(shuffled_timestamp)
     x <- runif(100, 0, 10000)
     expect_error(npcra_m10_whole_period(x, timestamp))

     #Two equal m10 values for different dates (should return the first)
     last_date <- as.POSIXct('2015-01-02 12:00:00')
     timestamp <- seq(first_date, last_date, by = "hour")
     x <- replicate(length(timestamp), 900)
     x[1:11] <-replicate(11, 1000)
     m10_data <- npcra_m10_whole_period(x, timestamp)
     expect_equal(m10_data$m10, 1000)
     expect_equal(m10_data$start_date, timestamp[1])

     #All the data of the same day
     last_date <- as.POSIXct('2015-01-01 23:59:59')
     timestamp <- seq(first_date, last_date, by = "min")
     x <- runif(1440, 0, 10000)
     m10_data <- npcra_m10_whole_period(x, timestamp)
     expect_is(m10_data$m10, 'numeric')
     expect_is(m10_data$start_date, 'POSIXct')
})

