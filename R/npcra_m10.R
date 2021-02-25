#' Non-Parametric Function M10 (Most Active 10 Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' M10 is calculated by identifying the 10-hour window with the highest activity
#'  in the given period.
#'
#' The function calculates the averages of a quantitative variable
#' (typically the representative of the activity in the data set) in an interval
#' that comprises each observation until the end of a 10-hour window, moving on
#' to the next observation with the same approach. Note that the accuracy of the
#' M10 is related to the validity of the given data range.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIX vector that contains the date and time of each
#' observation.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
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
#' or \code{npcra_m10_each_day()}.
#'
#' @return A tibble with the value of M10 in the first position and the start
#'   date of the 10 most active window in the period in the second position.
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
#' \dontrun{
#' m10(test_log, "pim")
#' m10(test_log, method=3)
#' }
#'
#' @export
npcra_m10 <- function(x, timestamp, method=1) {
    m10 <- data.frame()
    if (method == 1) {
        m10 <- npcra_m10_whole_period(x, timestamp)
    }
    if (method == 2) {
       m10 <- npcra_m10_average_day(x, timestamp)
    }
    if (method == 3) {
        m10 <- npcra_m10_each_day(x, timestamp)
    }
    m10
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
#' #Ordering dates and activity data to run
#' data <- dplyr::as_tibble(x)
#' data <- dplyr::mutate(data, timestamp = shuffled_timestamp)
#' data <- dplyr::arrange(data, timestamp)
#' npcra_m10_whole_period(data$value, data$timestamp)
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

    endWindow <- timestamp + lubridate::hours(10)
    index_last_valid_register <- which.min(endWindow < dplyr::last(timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    mean_10_hours <- replicate(index_last_valid_register, 0)

    for (index in seq_len(index_last_valid_register - 1)) {
        window_sum <- 0

        while (timestamp[window_index] < endWindow[index]) {
            window_sum <- window_sum + x[window_index]
            window_index <- window_index + 1
        }

        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- x[index]
        mean_10_hours[index] <- sum_in_10_hours / (window_index-index)
    }

    m10 <- max(mean_10_hours)
    start_date <- timestamp[which.max(mean_10_hours == m10)]
    out <- dplyr::tibble(m10, start_date)

    out
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for the Mean Profile
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the most active window of 10 hours considering the average day of
#'  the observations, that is, the window of the next 10 hours disregarding the
#'   difference in days. The composition of all activities is considered
#'   as a single day: the average day.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 10 most active hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   time (character format) of the 10 most active window in the period in the
#'   second position.
#'
#' @family NPCRA functions
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#'nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#'p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' @examples
#' \dontrun{
#' npcra_m10_average_day(test_log)}
#' @export
npcra_m10_mean_profile <- function(x, timestamp) {
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)

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
    first_14h_index <- which.max(lubridate::hour(data$timestamp) >= 14)
    last_valid_index <- length(x)

    while (index < first_14h_index) {
        window_sum <- 0

        while (abs(difftime(data$timestamp[window_index],
                            data$timestamp[index], units = "hour")) < 10) {

            window_sum <- window_sum + data$x[window_index]
            window_index <- window_index + 1
            size_window <- size_window + 1

            if (window_index == last_valid_index) {
                break
            }
        }

        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum

        if (sum_in_10_hours / (window_index-index) > m10) {
            m10 <- sum_in_10_hours / (window_index-index)
            start_date <- data$timestamp[index]
        }

        value_to_remove <- data$x[index]
        index <- index + 1
    }

    window_index <-1

    while (index <= last_valid_index) {
        window_sum <- 0

        while (abs(difftime(data$timestamp[window_index],
                            data$timestamp[index], units = "hour")) > 14) {
            window_sum <- window_sum + data$x[window_index]
            window_index <- window_index + 1
            size_window <- size_window + 1
        }

        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum

        if (sum_in_10_hours / (window_index-index) > m10) {
            m10 <- sum_in_10_hours / (window_index-index)
            start_date <- data$timestamp[index]
        }

        value_to_remove <- data$x[index]
        index <- index + 1
    }

        out <- dplyr::tibble(m10, start_date)
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
#' and that is why that is why many authors perform the analysis analysis
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
#' #Running for 10000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-31')
#' shuffled_timestamp <- sample(seq(first_date,
#'                                  last_date, by = "min"), 10000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(10000, 0, 10000)
#' npcra_m10_each_day(x, timestamp) #returns a tibble 31X2
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
    hour_end_window <- lubridate::hour(EndWindow)

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
