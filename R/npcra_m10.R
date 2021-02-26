#' Non-Parametric Function M10 (Most Active 10 Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' M10 is calculated by identifying the 10-hour window with the highest activity
#'  in the given period.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIX vector that contains the date and time of each
#' observation.
#' @param method An integer that represents one of the three common methods for
#' calculating and analyzing M10:
#'
#' 1 = whole period,
#'
#' 2 = average day,
#'
#' 3 = each day.
#'
#' If you prefer you can also directly call the functions
#' \code{npcra_m10_whole_period()}, \code{npcra_m10_average_day()}
#' or \code{npcra_m10_each_day()} and see the differences in the calculation
#' method.
#'
#' @return A tibble with the value of M10 in the first column and the start
#'   date of the 10 most active window in the period in the second column
#'
#' @details
#' The function calculates the averages of a quantitative variable
#' (typically the representative of the activity in the data set) in an interval
#' that comprises each observation until the end of a 10-hour window, moving on
#' to the next observation with the same approach. Note that the accuracy of the
#' M10 is related to the validity of the given data range.
#'
#' The amount and date of the result will vary according to the method chosen.
#' Although the first approaches in actigraphy used the M10 for the entire
#' period (Witting et al., 1990), other approaches were studied later and
#' analyzes of these studies (Goncalves et al., 2015) show the importance of
#' checking the results for each day and for the mean profile.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#' nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#' p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' @family NPCRA functions
#'
#' @examples
#' #Using the test_log data from the package (whole period)
#' npcra_m10(test_log$pim, test_log$timestamp) #expects a tibble 1X2
#'
#'#Running for 1000 random observations (mean profile)
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_m10(x, timestamp, method = 2) #expects a tibble 1X2
#'
#' #Ordering dates and activity data to run (each day)
#' data <- dplyr::as_tibble(x)
#' data <- dplyr::mutate(data, timestamp = shuffled_timestamp)
#' data <- dplyr::arrange(data, timestamp)
#' npcra_m10(data$value, data$timestamp, method = 3) #expects a tibble 10X2
#'
#' @export
npcra_m10 <- function(x, timestamp, method = 1) {
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)
    checkmate::assert_int(method)

    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1
        (Whole period),2 (mean profile) or 3 (each day), but received ",
             method, " (class ", class(method), ")")
    }

    out <- dplyr::tibble()

    if (method == 1) {
        out <- npcra_m10_whole_period(x, timestamp)
    }
    if (method == 2) {
        out <- npcra_m10_mean_profile(x, timestamp)
    }
    if (method == 3) {
        out <- npcra_m10_each_day(x, timestamp)
    }

    out
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for the full period
#' @family NPCRA functions
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates and finds the most active window of 10 hours in all records.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIXct/POSIXlt vector that contains the date and time of
#' each observation.
#' @return A tibble 1 X 2 with the value of M10 in the first column and the
#' start date of the 10 most active window in the period in the second position.
#'
#'@details
#'The M10 is the highest average activity over a 10-hour period among several
#'different periods in a time series. The value of M10 is influenced by daily
#'naps (Witting et al, 1990) and the validity of the data received for the
#'calculation. Higher M10 results may be related to a better quality of life
#'(Goncalves et al, 2015), however, it is important to analyze the result for
#' each day (\code{npcra_m10_each_day()}) and for the average 24-hour profile
#' (\code{npcra_m10_average_day()})
#'
#'The function receives two vectors, one containing the registered activity
#'(numerical values) and another vector containing the date of each occurrence
#'(POSIXct / POSIXlt), so it is possible to map the activity and dates by the
#'indexes of the vectors.
#'
#'Since the vectors are organized according to the date of observation in
#'ascending order (see Note), the function will identify the date of the last
#'full 10-hour period in the record.
#'
#'With this information the verification of all valid activity windows starts,
#'that is, from the first date, the average
#'of the activity vector is calculated until the difference between the date of
#'the current observation and the date of the window is greater that 10 hours,
#'when this happens, the average activity will be recorded in that 10 hour
#'period and move on to the next observation or, if you arrived on the last date
#' with a full 10 hour period, finish the check.
#'
#' Finally, the highest average activity in 10 hours will be captured and, using
#' the index of the vector of averages, the date of the beginning of the period
#' of 10 hours with the highest activity will be retrieved.
#'
#'@note
#'As normally expected, the date array must be ordered in ascending order.
#'This allows the verification of the longest periods to follow a method of
#'asymptotic complexity O(2n) = O(n), where n is the amount of data
#'(activity / date), so for 10,000 data, a little less than 20,000 interactions
#'will be performed, reducing the time to obtain the result for seconds.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#' nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#' p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_m10_whole_period(test_log$pim,
#'                          test_log$timestamp)
#'
#' #Running for 10000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-15')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 10000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(10000, 0, 10000)
#' npcra_m10_whole_period(x, timestamp)
#'
#' @export
npcra_m10_whole_period <- function(x, timestamp) {
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)

    if (length(timestamp) != length(x)) {
        stop("'x' and 'timestamp' must have the same length")
    }

    if (dplyr::last(timestamp) - dplyr::first(timestamp) < lubridate::hours(10)) {
        stop("Data does not complete at least one 10-hour period")
    }

    end_window <- timestamp + lubridate::hours(10)
    index_last_valid_register <- which.min(end_window < dplyr::last(timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    m10 <- 0
    start_date <- lubridate::origin

    for (index in seq_len(index_last_valid_register - 1)) {
        window_sum <- 0

        while (timestamp[window_index] < end_window[index]) {
            window_sum <- window_sum + x[window_index]
            window_index <- window_index + 1
        }

        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- x[index]

        if (sum_in_10_hours / (window_index-index) > m10) {
            m10 <- sum_in_10_hours / (window_index-index)
            start_date <- timestamp[index]
        }
    }

    out <- dplyr::tibble(m10, start_date)
    out
}
#' Non-Parametric Function M10 (Most Active 10 Hours) for the Mean Profile
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the most active window of 10 hours following the mean profile,
#' that is, disregarding the difference in days.
#'
#' For this method the
#' composition of all activities is considered as a single day:
#' the average day.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIXct/POSIXlt vector that contains the date and time of
#' each observation.
#' @return A tibble of two columns, with the value of M10 in the first column
#' and the start time of the 10 most active period for the mean profile in
#' the second position (to maintain the nomenclature standard,
#' this column is called start_date).
#'
#'@details
#'The M10 is the highest average activity over a 10-hour period.
#' The first uses of the m10 consisted of calculating the 10 most active hours
#' based on the hourly means (WITTING et al., 1990). As it does not have an
#' explicit definition, it is expected that the values would be calculated
#' under the complete period (see \code{npcra_m10_whole_period()}) received
#' and that is why that is why many authors perform the analysis
#' by two other approaches (GONCALVES et al., 2015): calculation for the 24h
#' mean profile and for each day(see \code{npcra_m10_each_day()})
#'
#'The mean profile is able to reduce the differences in activity between the
#'days by caring only for the time that an activity value was recorded, being
#'interesting to identify the most active average period
#'
#'The function receives two vectors, one containing the registered activity
#'(numerical values) and another vector containing the date of each occurrence
#'(POSIXct / POSIXlt), so it is possible to map the activity and dates by the
#'indexes of the vectors.
#'
#'m10 for the average profile starts by establishing the same date for all
#'observations, since it will only be necessary to check by time, and then
#'reordering them by the time (therefore, it is not necessary that the
#'observations were passed as a parameter in an orderly manner for the
#'optimal algorithm).
#'
#'Then begins the calculation of the averages of the 10-hour periods
#'and the search for the largest one. It is important to note that records
#'whose start time is later than 2 pm include in their 10 hour period in
#'addition to the later hours, include records prior to 2 pm while within
#'the 10 hour window. For example, a record that starts at 14:30:00 will
#'end the 10 hour period at 00:30
#'
#'At the end of the calculation, the highest average and its respective time
#'is captured
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#' nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#' p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_m10_mean_profile(test_log$pim, test_log$timestamp)
#'
#' #Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-06')
#' shuffled_timestamp <- sample(seq(first_date,
#'                                  last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_m10_mean_profile(x, timestamp)
#'
#' @export
npcra_m10_mean_profile <- function(x, timestamp) {
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)


    if (dplyr::last(timestamp) - dplyr::first(timestamp) < lubridate::hours(10)) {
        stop("Data does not complete at least one 10-hour period")
    }

    if (length(timestamp) != length(x)) {
        stop("'x' and 'timestamp' must have the same length")
    }

    m10 <- 0
    start_date <- lubridate::origin

    lubridate::date(timestamp) <- lubridate::origin
    data <- dplyr::tibble(x, timestamp) %>%
        dplyr::arrange(timestamp)

    index<-1
    window_index <- 1
    size_window <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    last_valid_index <- length(x)
    first_14h_index <- last_valid_index

    if(lubridate::hour(timestamp[last_valid_index]) >= 14) {
        first_14h_index <- which.max(lubridate::hour(data$timestamp) >= 14)
    }

    while (index <= last_valid_index) {
        window_sum <- 0

        if (index < first_14h_index) {
            while (abs(difftime(data$timestamp[window_index],
                                data$timestamp[index], units = "hour")) < 10) {

                window_sum <- window_sum + data$x[window_index]
                window_index <- window_index + 1
                size_window <- size_window + 1

                if (window_index == last_valid_index) {
                    window_index <-1
                    break
                }
            }
        }

        else {
            while (abs(difftime(data$timestamp[window_index],
                                data$timestamp[index], units = "hour")) > 14) {
                window_sum <- window_sum + data$x[window_index]
                window_index <- window_index + 1
                size_window <- size_window + 1
            }
        }

        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum

        if (sum_in_10_hours / (size_window-index) > m10) {
            m10 <- sum_in_10_hours / (size_window-index)
            start_date <- data$timestamp[index]
        }

        value_to_remove <- data$x[index]
        index <- index + 1
    }

        out <- dplyr::tibble(m10, start_date) %>%
            dplyr::mutate(start_date =
                              strftime(start_date, format="%H:%M:%S"))
    out
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for each day
#' @family NPCRA functions
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates and finds the most active period of 10 hours for each different
#' day.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIXct/POSIXlt vector that contains the date and time of
#' each observation.
#' @return A tibble of two columns, with the values of M10 in the first column
#' and the start dates of the 10 most active period for each day in the second
#' position (so, each row is a different day).
#'
#'@details
#'The M10 is the highest average activity over a 10-hour period.
#' The first uses of the m10 consisted of calculating the 10 most active hours
#' based on the hourly means (WITTING et al., 1990). As it does not have an
#' explicit definition, it is expected that the values would be calculated
#' under the complete period (see \code{npcra_m10_whole_period()}) received
#' and that is why that is why many authors perform the analysis
#' by two other approaches (GONCALVES et al., 2015): calculation for each
#' day and for the 24h mean profile (see \code{npcra_m10_mean_profile()})
#'
#'m10 for each day calculates the most active 10 hour period for each
#'different date (day, month and year). Thus, each date of the data set
#'delivered must have a value of m10 as long as there is a complete period
#'of 10 hours for that date. The results can quantify the intensity of daily
#'activity (the higher the result, the more intense) and variations by date.
#'
#'The function receives two vectors, one containing the registered activity
#'(numerical values) and another vector containing the date of each occurrence
#'(POSIXct / POSIXlt), so it is possible to map the activity and dates by the
#'indexes of the vectors.
#'
#'Since the vectors are organized according to the date of observation in
#'ascending order (see Note), the function will identify the date of the last
#'full 10-hour period in the record.
#'
#'With this information the verification of all valid activity windows for each
#'date starts.
#'
#'First all different dates are identified and what activity data belongs to
#'them. In sequence, the averages of the 10-hour windows of the first day
#'are calculated, that is, the averages of all windows that end at 23:59:59
#'or earlier. This process is repeated for the next day and ends when the
#'last valid date arrives.
#'
#'At the end of the calculation, the highest average for its respective day
#'is captured
#'
#'@note
#'As normally expected, the date array must be ordered in ascending order.
#'This allows the verification of the longest periods to follow a method of
#'asymptotic complexity O(2n) = O(n), where n is the amount of data
#'(activity / date), so for 10,000 data, a little less than 20,000 interactions
#'will be performed, reducing the time to obtain the result for seconds.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#' nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#' p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_m10_each_day(test_log$pim, test_log$timestamp)
#'
#' #Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                                  last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_m10_each_day(x, timestamp) #returns a tibble 10X2
#'
#' @export
npcra_m10_each_day <- function(x, timestamp){
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)

    if (dplyr::last(timestamp) - dplyr::first(timestamp) < lubridate::hours(10)) {
        stop("Data does not complete at least one 10-hour period")
    }

    if (length(timestamp) != length(x)) {
        stop("'x' and 'timestamp' must have the same length")
    }

    dates <- lubridate::date(timestamp)
    EndWindow <- timestamp + lubridate::hours(10)

    index_first_days <- match(unique(dates), dates)
    index_last_valid_register <- which.min(EndWindow < dplyr::last(timestamp))
    n_dates <- dplyr::n_distinct(dates)
    m10 <- c()
    m10_index <- c()

    for (index_day in seq(n_dates)){
        m10[index_day] <- 0
        value_to_remove <- 0
        sum_in_10_hours <- 0
        index <- index_first_days[index_day]
        window_index <- index

        while (index < index_last_valid_register &
               dates[window_index] == dates[index]) {

            end_window <- EndWindow[index]
            window_sum <- 0

            while (timestamp[window_index] <= end_window) {
                window_sum <- window_sum + x[window_index]
                window_index <- window_index + 1
            }

            sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum

            if (sum_in_10_hours / (window_index-index) > m10[index_day]) {
                m10[index_day] <- sum_in_10_hours / (window_index-index)
                m10_index[index_day] <- index
            }

            value_to_remove <- x[index]
            index <- index + 1
        }
    }

    start_date <- timestamp[m10_index]
    out <- dplyr::tibble(m10, start_date)
    out
}
