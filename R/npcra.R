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
#' npcra_m10_whole_period, npcra_m10_average_day or npcra_m10_each_day.
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
#'(Goncalves et al, 2015), however, it is important to analyze the result for each day
#' (\code{npcra_m10_each_day()}) and for the average 24-hour profile
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
#' npcra_m10_whole_period(test_log$pim, test_log$timestamp)
#'
#' #Running for 10000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-15')
#' shuffled_timestamp <- sample(seq(first_date, last_date, by = "min"), 10000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(10000, 0, 10000)
#' npcra_m10_whole_period(x, timestamp)
#'
#' #Ordering dates and activity data to run
#' data <- dplyr::as_tibble(x)
#' data <- dplyr::mutate(data, timestamp = shuffled_timestamp)
#' data <- dplyr::arrange(data, timestamp)#'
#' npcra_m10_whole_period(data$value, data$timestamp)
#'
#'
#' @export
npcra_m10_whole_period <- function(x, timestamp) {
    endWindow <- timestamp + lubridate::hours(10)
    index_last_valid_register <- which.min(endWindow < dplyr::last(timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    mean_10_hours <- replicate(length(x), 0)

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

    m10_value <- max(mean_10_hours)
    timestamp_m10 <- timestamp[which(mean_10_hours == m10_value)]

    m10 <- dplyr::tibble(m10_value, timestamp_m10) %>%
        dplyr::rename(m10 = 'm10_value', start_date = 'timestamp_m10')

    m10
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for the avarage day
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
npcra_m10_average_day <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        dplyr::select(timestamp, col_activity) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_activity) %>%
        dplyr::mutate(time = data.table::as.ITime(timestamp)) %>%
        dplyr::mutate(timestamp_same_origin = timestamp) %>%
        dplyr::mutate(m10 = 0) %>%
        dplyr::arrange(time)
    date(valid_data$timestamp_same_origin) <- lubridate::origin

    index<-1
    window_index <- 1
    size_window <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    first_14h_index <- which.max(hour(valid_data$timestamp_same_origin) == 14)
    last_valid_index <- dplyr::n_distinct(valid_data)
    index_in_window = abs(difftime(valid_data$timestamp_same_origin[window_index], valid_data$timestamp_same_origin[index], units = "hour")) < 10


    while (index < first_14h_index) {
        window_sum <- 0
        while (abs(difftime(valid_data$timestamp_same_origin[window_index], valid_data$timestamp_same_origin[index], units = "hour")) < 10) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
            size_window <- size_window+1
            if (window_index == last_valid_index) break

        }
        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$m10[index] <- sum_in_10_hours / (size_window-index)

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
        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$m10[index] <- sum_in_10_hours / (size_window-index)

        index <- index+1
    }


    max_m10 <- max(valid_data$m10)
    m10_data <- valid_data %>%
        dplyr::select(m10, timestamp_same_origin) %>%
        dplyr::filter(m10 == max_m10) %>%
        dplyr::rename("start_date" = timestamp_same_origin) %>%
        dplyr::mutate(start_date = strftime(start_date, format="%H:%M:%S"))

    m10_data
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for each day
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the most active window of 10 hours for each different day.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 10 most active hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return A Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window by day in the second position.
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
#'
#' @examples
#' \dontrun{
#' npcra_m10_each_day(test_log)}
#' @export
npcra_m10_each_day <- function(data, col_activity = "pim", timestamp="timestamp"){
    valid_data <- data %>%
        dplyr::select(timestamp, col_activity) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_activity) %>%
        dplyr::mutate(day = ymd(strftime(timestamp, format="%y%m%d")),
               endWindow = timestamp+lubridate::hours(10),
               hourEndWindow = as.numeric(strftime(endWindow, format="%H")),
               mean_10_hours = 0,)

    unique_days <- unique(valid_data$day)
    index_first_days <- match(unique_days,valid_data$day)
    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    quant_days <- length(unique_days)

    for (index_day in seq(quant_days)){
        index = index_first_days[index_day]
        value_to_remove <- 0
        sum_in_10_hours <- 0
        window_index <- index
        current_day <- lubridate::day(unique_days[index_day])
        while (index < index_last_valid_register & valid_data$hourEndWindow[index] != 0) {
            end_window <- valid_data$endWindow[index]
            window_sum <- 0
            while (valid_data$timestamp[window_index] <= end_window) {
                window_sum <- window_sum + valid_data$x[window_index]
                window_index <- window_index+1
            }
            sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
            valid_data$mean_10_hours[index] <- sum_in_10_hours/(window_index-index)
            value_to_remove <- valid_data$x[index]
            index<-index+1
        }
    }

    m10 <- tapply(valid_data$mean_10_hours, valid_data$day, max)
    m10_first_date <- valid_data$timestamp[match(m10,valid_data$mean_10_hours)]
    m10_each_day <- cbind.data.frame(m10,  m10_first_date)
    m10_each_day <- m10_each_day %>%
        rename("start_date"=m10_first_date) %>%
        as_tibble()

    m10_each_day
}

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

#' Non-Parametric Function L5 (Least Active 5  Hours) for the full period.
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates and finds the least active window of 5 hours in all records.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 5 least active hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return a Dataframe with the value of L5 in the first position and the start
#'   date of the 5 least active window in the period in the second position.
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
#'
#' @examples
#' \dontrun{
#' npcra_l5_whole_period(test_log)}
#' @export
npcra_l5_whole_period <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        dplyr::select(timestamp, col_activity) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_activity) %>%
        dplyr::mutate(endWindow = timestamp + lubridate::hours(5)) %>%
        dplyr::mutate(l5 = 0)

    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_5_hours <- 0

    for (index in seq_len(index_last_valid_register-1)) {
        window_sum <- 0
        while (valid_data$timestamp[window_index] < valid_data$endWindow[index]) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
        }
        sum_in_5_hours <- sum_in_5_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$l5[index] <- sum_in_5_hours / (window_index-index)
    }

    l5_data <- valid_data %>%
        dplyr::select(l5, timestamp) %>%
        dplyr::filter(l5 > 0) %>%
        dplyr::filter(l5 == min(l5)) %>%
        dplyr::rename("start_date" = timestamp)

    l5_data
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
#' Non-Parametric Function RA (Relative Amplitude)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the amplitude of the rest-activity rhythm. the result is based on
#' the difference between the 10 most active hours (M10) and the 5 least active hours (L5).
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param method An integer that represents one of the three common methods for
#' calculating and analyzing RA:
#'
#' 1 = whole period,
#'
#' 2 = average day,
#'
#' 3 = each day.
#'
#' @return For methods 1 and 2 returns the numerical value that represents the
#' relative amplitude. For method equals 3 returns a tibble with the values of
#' RA in the first column and the date in the second column.
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
#'ra()}
#' @export
ra <- function(x, timestamp, method=1){
    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1,2 or 3,
             but received ", method, " (class ", class(method), ")")
    }

    m10 <- m10(x, timestamp, method)
    l5 <- l5(x, timestamp, method)
    ra <- 0

    if(method==1 | method==2){
        m10 <- m10$m10
        l5 <- l5$l5
        ra <- (m10-l5)/(m10+l5)
        ra <- ra[1,1]
    }
    else{
        m10_values <- m10[,1]
        dates <- m10[,2]
        l5_values <- l5[,1]

        ra_values <- (m10_values-l5_values)/(m10_values+l5_values)
        ra <- dplyr::as_tibble(c(ra_values, dates))

        colnames(ra) <- c("ra", "date")
        ra$date = lubridate::date(ra$date)
    }
    ra
}

#' Non-Parametric Function IS (Interdaily Stability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Interdaily Stability identifies the synchronization with the 24h day-night cycle.
#' This fragmentation can have different results for the same data according
#' to the chosen time interval, that is, a minute by minute calculation brings a
#'  more sensitive result than a check in hours, where small changes tend to have
#'  less influence on interdaily stability
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param minutes_interval integer value representing the duration in minutes
#' of the time interval for grouping the data. By default, 60 minutes are
#' considered, this means that the activity will be averaged at hourly intervals.
#'  The minimum value is 1 minute, where all points will be considered
#'  (so there will be no average in intervals).
#'
#' @return A numeric value.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno S. B. et al. Nonparametric methods in actigraphy: an update.
#'  Sleep Science, v. 7, n. 3, p. 158-164, 2014. doi: 10.1016/j.slsci.2014.09.013.
#'
#' @family NPCRA functions
#'
#' @examples
#' \dontrun{
#' is()}
#' @export
is <- function(x, timestamp, minutes_interval=60){
    if(minutes_interval == 1 | minutes_interval == 0) {
        average_x_per_interval <- x
    }
    else{
        time_interval <- paste(minutes_interval, "min")
        interval <- cut(timestamp, time_interval)
        average_x_per_interval <- tapply(x, interval, mean)
    }

    hourly_means <- tapply(x, lubridate::hour(timestamp) , mean)
    is <- stats::var(hourly_means)/stats::var(average_x_per_interval)
    is
}

#' Non-Parametric Function ISm (Interdaily Stability mean)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Interdaily Stability identifies the synchronization with the 24h day-night cycle.
#' This method calculates the average of ISs up to a minute limit. By default,
#' the limit is 60 minutes, so the 60 ISs will be calculated separately and the
#'  results will be averaged to be returned.
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param minute_limit integer value that corresponds to the last minute interval
#' to group the data. The default is 60, so 60 values of IS will be calculated to
#' take the average, with the first every minute and the last every 60 minutes.
#'
#' @return A numeric value.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno S. B. et al. Nonparametric methods in actigraphy: an update.
#'  Sleep Science, v. 7, n. 3, p. 158-164, 2014. doi: 10.1016/j.slsci.2014.09.013.
#'
#' @family NPCRA functions
#'
#'
#' @examples
#' \dontrun{
#' ism()}
#' @export
ism <- function(x, timestamp, minute_limit=60){
    sum_is <- 0
    for (current_minute in seq_len(minute_limit)) {
        sum_is <- sum_is + is(x, timestamp, current_minute)
    }

    is_mean <- sum_is/minute_limit
    is_mean
}

#' Non-Parametric Function IV (Intradaily Variability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intradaily Variability identifies the fragmentation of the rest-activity rhythms.
#' This fragmentation can have different results for the same data according
#' to the chosen time interval, that is, a minute by minute calculation brings a
#'  more sensitive result than a check in hours, where small changes tend to have
#'  less influence on intraday variability.
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param minutes_interval integer value representing the duration in minutes
#' of the time interval for grouping the data. By default, 60 minutes are
#' considered, this means that the activity will be averaged at hourly intervals.
#'  The minimum value is 1 minute, where all points will be considered
#'  (so there will be no average in intervals).
#'
#' @return A numeric value.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno S. B. et al. Nonparametric methods in actigraphy: an update.
#'  Sleep Science, v. 7, n. 3, p. 158-164, 2014. doi: 10.1016/j.slsci.2014.09.013.
#'
#' @family NPCRA functions
#'
#' @examples
#' \dontrun{
#' iv()}
#' @export
iv <- function(x, timestamp, minutes_interval=60){
    if(minutes_interval == 1) {
        average_x_per_interval <- x
    }
    else{
        time_interval <- paste(minutes_interval, "min")
        interval <- cut(timestamp, time_interval)
        average_x_per_interval <- tapply(x, interval, mean)
    }
    n <- length(average_x_per_interval)

    square_diff <- diff(average_x_per_interval)^2
    numerator <- n*sum(square_diff)

    denonimator <- stats::var(average_x_per_interval)
    denonimator <- denonimator*(n-1)^2

    iv <- numerator/denonimator
    iv
}

#' Non-Parametric Function IVm (Intradaily Variability mean)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intradaily Variability identifies the fragmentation of the rest-activity rhythms.
#' This method calculates the average of IVs up to a minute limit. By default,
#' the limit is 60 minutes, so the 60 IVs will be calculated separately and the
#'  results will be averaged to be returned.
#'
#' @param x Numeric vector with the activity data that will be used in the calculation.
#' @param timestamp POSIX vector that contains the date and time of each observation.
#' @param minute_limit integer value that corresponds to the last minute interval
#' to group the data. The default is 60, so 60 values of IV will be calculated to
#' take the average, with the first every minute and the last every 60 minutes.
#'
#' @return A numeric value.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#' GONCALVES, Bruno S. B. et al. Nonparametric methods in actigraphy: an update.
#'  Sleep Science, v. 7, n. 3, p. 158-164, 2014. doi: 10.1016/j.slsci.2014.09.013.
#'
#' @family NPCRA functions
#'
#' @examples
#' \dontrun{
#' ivm(test_log, "body_temperature")}
#' @export
ivm<- function(x, timestamp, minute_limit=60){
    sum_iv <- 0
    for (current_minute in seq_len(minute_limit)) {
        sum_iv <- sum_iv + iv(x, timestamp, current_minute)
    }

    iv_mean <- sum_iv/minute_limit
    iv_mean
}

#' Testing the arguments of nonparametric functions
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Performs the verification of the types of expected values as arguments to the
#' npcra functions.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the period of activity.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
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
#' @return If no test stops, TRUE will be returned.
#' @family NPCRA functions
#'
#' @examples
#' \dontrun{
#' npcra_test_args(test_log, "pim", "timestamp", 1)}
#' @export
npcra_test_args <- function(data, col_activity, timestamp, method) {
    data_col_names <- colnames(data)

    if (!is.data.frame(data)) {
        stop("Parameter 'data' expects a dataframe but received a ", class(data))
    }
    if (!is.element(col_activity, data_col_names)) {
        stop("Parameter 'col_activity' = ", col_activity, " is not a column of the given dataframe")
    }
    if (!is.element(timestamp, data_col_names)) {
        stop("Parameter 'timestamp' = ", timestamp, " is not a column of the given dataframe")
    }
    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1,2 or 3, but received ", method, " (class ", class(method), ")")
    }
    if(any(is.na(data[,col_activity]))){
        stop("Column 'col_activity' = ", col_activity, " has NA values")
    }
    if(any(is.na(data[,timestamp]))){
        stop("Column 'timestamp' = ", timestamp, " has NA values")
    }

    col_activity_class <- lapply(data[, col_activity], class)
    col_timestamp_class <- unlist(col_activity_class)
    if (!is.element("numeric", col_activity_class)) {
        stop("col_activity must be a column that has only 'numeric' values. col_name is composed of: ", col_activity_class)
    }

    col_timestamp_class <- lapply(data[, timestamp], class)
    col_timestamp_class <- unlist(col_timestamp_class)
    if (!is.element("POSIXct", col_timestamp_class) || !is.element("POSIXt", col_timestamp_class)) {
        stop("timestamp must be a column that has 'POSIXct' or 'POSIXt' values. timestamp is composed of: ", col_timestamp_class)
    }
    TRUE
}


ifc <- function(data, col_activity = "pim", timestamp="timestamp") {
    ism <- ism(data, col_activity, timestamp)
    message("ISm = ", ism)
    ivm <- ivm(data, col_activity, timestamp)
    message("IVm = ", ivm)
    ra <- ra(data, col_activity, timestamp, method=1)
    message("RA = ", ra)

    ifc<- (ism+ivm+ra)/3
    message("IFC = ", ifc)
    return(ifc)

}
