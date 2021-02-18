#' Non-Parametric Function L5 (Least Active 5  Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' L5 is calculated by identifying the 5-hour window with the least activity in
#' the given period.
#'
#' The function calculates the averages of a quantitative variable
#' (typically the representative of the activity in the data set) in an interval
#' that comprises each observation until the end of a 10-hour window, moving on
#' to the next observation with the same approach. Note that the accuracy of the
#' L5 is related to the validity of the given data range.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the least active 5 hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#' @param method An integer that represents one of the three common methods for
#' calculating and analyzing L5:
#'
#' 1 = whole period,
#'
#' 2 = average day,
#'
#' 3 = each day.
#'
#' If you prefer you can also directly call the functions npcra_l5_whole_period,
#' npcra_l5_average_day or npcra_l5_each_day.
#'
#' @return A Dataframe with the value of L5 in the first position and the start
#'   date of the 5-hour window with less activity in the period in the second position.
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
#' npcra_l5(test_log, "pim")
#' npcra_l5(test_log, method=3)
#' }
#' @export
npcra_l5 <- function(data, col_activity = "pim", timestamp="timestamp", method=1) {
    time_begin <- Sys.time()
    npcra_test_args(data, col_activity, timestamp, method)

    l5 <- data.frame()
    if (method==1) {
        l5 <- npcra_l5_whole_period(data, col_activity, timestamp)
    }
    if (method==2) {
        l5 <- npcra_l5_average_day(data, col_activity, timestamp)
    }
    if (method==3) {
        l5 <- npcra_l5_each_day(data, col_activity, timestamp)
    }

    duration <- round(Sys.time()-time_begin,digits = 2)
    message("L5 was calculated in ", duration, " seconds")

    l5
}

#' Non-Parametric Function L5 (Least Active 5  Hours) for the full period
#' @family NPCRA functions
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates and finds the least active period of 5 hours in all records.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIXct/POSIXlt vector that contains the date and time of
#' each observation.
#' @return A tibble 1 X 2 with the value of L5 in the first column and the
#' start date of the 5 least active window in the period in the second position.
#'
#'@details
#'The L5 is the lowest average activity over a 5-hour period among several
#'different periods in a time series. The value of l5 has the ability to
#'quantify movement during sleep and possible awakenings (Witting et al, 1990),
#'also depending on the validity of the data the validity of the data received
#'for the calculation. Lower  L5 results may be related to a less fragmented
#'rhythm (Goncalves et al, 2015), however, it is important to analyze the result
#' for each day (\code{npcra_l5_each_day()}) and for the average 24-hour profile
#' (\code{npcra_l5_average_day()})
#'
#'The function receives two vectors, one containing the registered activity
#'(numerical values) and another vector containing the date of each occurrence
#'(POSIXct / POSIXlt), so it is possible to map the activity and dates by the
#'indexes of the vectors.
#'
#'Since the vectors are organized according to the date of observation in
#'ascending order (see Note), the function will identify the date of the last
#'full 5-hour period in the record.
#'
#'With this information the verification of all valid activity windows starts,
#'that is, from the first date, the average
#'of the activity vector is calculated until the difference between the date of
#'the current observation and the date of the window is greater that 5 hours,
#'when this happens, the average activity will be recorded in that 5-hour
#'period and move on to the next observation or, if you arrived on the last date
#' with a full 5 hour period, finish the check.
#'
#' Finally, the highest average activity in 5 hours will be captured and, using
#' the index of the vector of averages, the date of the beginning of the period
#' of 5 hours with the highest activity will be retrieved.
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
#' npcra_l5_whole_period(test_log$pim, test_log$timestamp)
#'
#' #Running for 10000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-15')
#' shuffled_timestamp <- sample(seq(first_date, last_date, by = "min"), 10000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(10000, 0, 10000)
#' npcra_l5_whole_period(x, timestamp)
#'
#' #Ordering dates and activity data to run
#' data <- dplyr::as_tibble(x)
#' data <- dplyr::mutate(data, timestamp = shuffled_timestamp)
#' data <- dplyr::arrange(data, timestamp)
#' npcra_l5_whole_period(data$value, data$timestamp)
#'
#' @export
npcra_l5_whole_period <- function(x, timestamp) {
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)

    if (dplyr::last(timestamp) - dplyr::first(timestamp) < lubridate::hours(5)) {
        stop("Data does not complete at least one 5-hour period")
    }

    endWindow <- timestamp + lubridate::hours(5)
    index_last_valid_register <- which.min(endWindow < dplyr::last(timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_5_hours <- 0
    mean_5_hours <- replicate(index_last_valid_register, NA)

    for (index in seq_len(index_last_valid_register - 1)) {
        window_sum <- 0

        while (timestamp[window_index] < endWindow[index]) {
            window_sum <- window_sum + x[window_index]
            window_index <- window_index + 1
        }

        sum_in_5_hours <- sum_in_5_hours - value_to_remove + window_sum
        value_to_remove <- x[index]
        mean_5_hours[index] <- sum_in_5_hours / (window_index-index)
    }

    l5_value <- min(mean_5_hours, na.rm = TRUE)

    start_date <- timestamp[which.max(mean_5_hours == l5_value)]

    l5 <- dplyr::tibble(l5_value, start_date) %>%
        dplyr::rename(l5 = 'l5_value')

    l5
}

#' Non-Parametric Function L5 (Least Active 5 Hours) for the avarage day
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the least active window of 5 hours considering the average day of
#'  the observations, that is, the window of the next 5 hours disregarding the
#'   difference in days. The composition of all activities is considered
#'   as a single day: the average day.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the least active 5-hour period.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return A tibble with the value of L5 in the first position and the start
#'   time (character format) of the 5-hour least active window in the period in the
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
#'
#' @examples
#' \dontrun{
#' l5_average_day(test_log)}
#' @export
l5_average_day <- function(x, timestamp) {
    time <- data.table::as.ITime(timestamp)
    timestamp_same_origin <- timestamp
    x_mean_5_hours <- x
    date(timestamp_same_origin) <- lubridate::origin

    index<-1
    window_index <- 1
    size_window <- 1
    value_to_remove <- 0
    sum_in_5_hours <- 0
    first_19h_index <- which.max(lubridate::hour(timestamp_same_origin) == 19)
    last_valid_index <- dplyr::n_distinct(valid_data)
    index_in_window = abs(difftime(timestamp_same_origin[window_index], timestamp_same_origin[index], units = "hour")) < 10


    while (index < first_19h_index) {
        window_sum <- 0
        while (abs(difftime(valid_data$timestamp_same_origin[window_index], valid_data$timestamp_same_origin[index], units = "hour")) < 5) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
            size_window <- size_window+1
            if (window_index == last_valid_index) break

        }
        sum_in_5_hours <- sum_in_5_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$l5[index] <- sum_in_5_hours / (size_window-index)

        index <- index+1
    }

    window_index <-1

    while (index <= last_valid_index) {
        window_sum <- 0
        while (abs(difftime(valid_data$timestamp_same_origin[window_index], valid_data$timestamp_same_origin[index], units = "hour")) > 14) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
            size_window <- size_window+1
        }
        sum_in_5_hours <- sum_in_5_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$l5[index] <- sum_in_5_hours / (size_window-index)

        index <- index+1
    }

    l5_data <- valid_data %>%
        dplyr::select(l5, timestamp_same_origin) %>%
        dplyr::filter(l5 > 0) %>%
        dplyr::filter(l5 == min(l5)) %>%
        dplyr::rename("start_date" = timestamp_same_origin) %>%
        dplyr::mutate(start_date = strftime(start_date, format="%H:%M:%S"))

    l5_data
}

#' Non-Parametric Function L5 (Least Active 5  Hours) for each day
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the least active period of 5 hours for each different day.
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return A Dataframe with the value of L5 in the first position and the start
#'   date of the 5-hour least active window by day in the second position.
#'
#' @family NPCRA functions
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
#' \dontrun{
#' npcra_l5_each_day(test_log)}
#' @export
l5_each_day <- function(x, timestamp){
    valid_data <- data %>%
        dplyr::select(timestamp, col_activity) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_activity) %>%
        dplyr::mutate(day = ymd(strftime(timestamp, format="%y%m%d")),
               endWindow = timestamp + lubridate::hours(5),
               hourEndWindow = as.numeric(strftime(endWindow, format="%H")),
               mean_5_hours = 0)

    unique_days <- unique(valid_data$day)
    index_first_days <- match(unique_days,valid_data$day)
    index_last_valid_register <- which.min(valid_data$endWindow < dplyr::last(valid_data$timestamp))
    quant_days <- length(unique_days)

    for (index_day in seq(quant_days)){
        index = index_first_days[index_day]
        value_to_remove <- 0
        sum_in_5_hours <- 0
        window_index <- index
        current_day <- lubridate::day(unique_days[index_day])
        while (index < index_last_valid_register & valid_data$hourEndWindow[index] != 0) {
            end_window <- valid_data$endWindow[index]
            window_sum <- 0
            while (valid_data$timestamp[window_index] <= end_window) {
                window_sum <- window_sum + valid_data$x[window_index]
                window_index <- window_index+1
            }
            sum_in_5_hours <- sum_in_5_hours - value_to_remove + window_sum
            valid_data$mean_5_hours[index] <- sum_in_5_hours/(window_index-index)
            value_to_remove <- valid_data$x[index]
            index<-index+1
        }
    }

    valid_data <- valid_data %>%
        dplyr::filter(mean_5_hours>0)

    l5 <- tapply(valid_data$mean_5_hours, valid_data$day, min)
    l5_first_date <- valid_data$timestamp[match(l5,valid_data$mean_5_hours)]
    l5_each_day <- cbind.data.frame(l5, l5_first_date)
    l5_each_day <- l5_each_day %>%
        rename("start_date" = l5_first_date) %>%
        as_tibble()

    l5_each_day
}
