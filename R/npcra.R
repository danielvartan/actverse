#' Non-Parametric Function M10 (Most Active 10 Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' M10 is calculated by identifying the 10-hour window with the highest activity in
#' the given period.
#'
#' The function calculates the averages of a quantitative variable
#' (typically the representative of the activity in the data set) in an interval
#' that comprises each observation until the end of a 10-hour window, moving on
#' to the next observation with the same approach. Note that the accuracy of the
#' M10 is related to the validity of the given data range.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 10 most active hours.
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
#' If you prefer you can also directly call the functions npcra_m10_whole_period,
#' npcra_m10_average_day or npcra_m10_each_day.
#'
#' @return A Dataframe with the value of M10 in the first position and the start
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
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10(test_log, "pim")
#' npcra_m10(test_log, method=3)
#' }
npcra_m10 <- function(data, col_activity = "pim", timestamp="timestamp", method=1) {
    time_begin <- Sys.time()
    npcra_test_args(data, col_activity, timestamp, method)

    m10 <- data.frame()
    if (method==1) {
        m10 <- npcra_m10_whole_period(data, col_activity, timestamp)
    }
    if (method==2) {
       m10 <- npcra_m10_average_day(data, col_activity, timestamp)
    }
    if (method==3) {
        m10 <- npcra_m10_each_day(data, col_activity, timestamp)
    }

    duration <- round(Sys.time()-time_begin,digits = 2)
    message("M10 was calculated in ", duration, " seconds")

    m10
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for the full period.
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates and finds the most active window of 10 hours in all records.
#'
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 10 most active hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window in the period in the second position.
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
#' @importFrom lubridate hours
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10_whole_period(test_log)}

npcra_m10_whole_period <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(endWindow = timestamp+lubridate::hours(10)) %>%
        mutate(m10 = 0)

    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0

    for (index in seq_len(index_last_valid_register-1)) {
        window_sum <- 0
        while (valid_data$timestamp[window_index] < valid_data$endWindow[index]) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
        }
        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$m10[index] <- sum_in_10_hours / (window_index-index)
    }

    max_m10 <- max(valid_data$m10)
    m10_data <- valid_data %>%
        select(m10, timestamp) %>%
        filter(m10 == max_m10) %>%
        rename("start_date"=timestamp)

    m10_data
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
#' @importFrom lubridate hour
#' @importFrom lubridate date<-
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr n_distinct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10_average_day(test_log)}
npcra_m10_average_day <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(time = data.table::as.ITime(timestamp)) %>%
        mutate(timestamp_same_origin = timestamp) %>%
        mutate(m10 = 0) %>%
        arrange(time)
    date(valid_data$timestamp_same_origin) <- lubridate::origin

    index<-1
    window_index <- 1
    size_window <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    first_14h_index <- which.max(hour(valid_data$timestamp_same_origin) == 14)
    last_valid_index <- n_distinct(valid_data)
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
        select(m10, timestamp_same_origin) %>%
        filter(m10 == max_m10) %>%
        rename("start_date"=timestamp_same_origin) %>%
        mutate(start_date = strftime(start_date, format="%H:%M:%S"))

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
#' @importFrom lubridate hours
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10_each_day(test_log)}
npcra_m10_each_day <- function(data, col_activity = "pim", timestamp="timestamp"){
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(day = ymd(strftime(timestamp, format="%y%m%d")),
               endWindow = timestamp+hours(10),
               hourEndWindow = as.numeric(strftime(endWindow,format="%H")),
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
        current_day <- day(unique_days[index_day])
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
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_l5(test_log, "pim")
#' npcra_l5(test_log, method=3)
#' }
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
#' @importFrom lubridate hours
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_l5_whole_period(test_log)}

npcra_l5_whole_period <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(endWindow = timestamp+hours(5)) %>%
        mutate(l5 = 0)

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
        select(l5, timestamp) %>%
        filter(l5 > 0) %>%
        filter(l5 == min(l5)) %>%
        rename("start_date"=timestamp)

    l5_data
}

#' Non-Parametric Function L5 (Least Active 5  Hours) for the avarage day
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
#' @return a Dataframe with the value of L5 in the first position and the start
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
#' @importFrom lubridate hour
#' @importFrom lubridate date<-
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr n_distinct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_l50_average_day(test_log)}
npcra_l5_average_day <- function(data, col_activity = "pim", timestamp="timestamp") {
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(time = data.table::as.ITime(timestamp)) %>%
        mutate(timestamp_same_origin = timestamp) %>%
        mutate(l5 = 0) %>%
        arrange(time)
    date(valid_data$timestamp_same_origin) <- lubridate::origin

    index<-1
    window_index <- 1
    size_window <- 1
    value_to_remove <- 0
    sum_in_5_hours <- 0
    first_19h_index <- which.max(hour(valid_data$timestamp_same_origin) == 19)
    last_valid_index <- n_distinct(valid_data)
    index_in_window = abs(difftime(valid_data$timestamp_same_origin[window_index], valid_data$timestamp_same_origin[index], units = "hour")) < 10


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
        select(l5, timestamp_same_origin) %>%
        filter(l5 > 0) %>%
        filter(l5 == min(l5)) %>%
        rename("start_date"=timestamp_same_origin) %>%
        mutate(start_date = strftime(start_date, format="%H:%M:%S"))

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
#' @param data Dataframe that contains the date column and the column that
#'   will be used to identify the 5 least active hours.
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
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
#' @importFrom lubridate hours
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_l5_each_day(test_log)}
npcra_l5_each_day <- function(data, col_activity = "pim", timestamp="timestamp"){
    valid_data <- data %>%
        select(timestamp, col_activity) %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        mutate(day = ymd(strftime(timestamp, format="%y%m%d")),
               endWindow = timestamp+hours(5),
               hourEndWindow = as.numeric(strftime(endWindow,format="%H")),
               mean_5_hours = 0)

    unique_days <- unique(valid_data$day)
    index_first_days <- match(unique_days,valid_data$day)
    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    quant_days <- length(unique_days)

    for (index_day in seq(quant_days)){
        index = index_first_days[index_day]
        value_to_remove <- 0
        sum_in_5_hours <- 0
        window_index <- index
        current_day <- day(unique_days[index_day])
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

    valid_data <- valid_data %>% filter(mean_5_hours>0)

    l5 <- tapply(valid_data$mean_5_hours, valid_data$day, min)
    l5_first_date <- valid_data$timestamp[match(l5,valid_data$mean_5_hours)]
    l5_each_day <- cbind.data.frame(l5,  l5_first_date)
    l5_each_day <- l5_each_day %>%
        rename("start_date"=l5_first_date) %>%
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
#' @param data Dataframe that contains the date column and the column with the
#' activity values
#' @param col_activity String with the name of the column that will be used in the
#'   calculation. The observations in this column must be in numeric format.
#' @param timestamp String with the name of the column that contains the date
#'  and time of each observation (POSIX format).
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
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_ra(test_log, "pim", "timestamp", 3)}
npcra_ra <- function(data, col_activity = "pim", timestamp="timestamp", method=1){
    time_begin <- Sys.time()
    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1,2 or 3, but received ", method, " (class ", class(method), ")")
    }

    m10 <- npcra_m10(data, col_activity, timestamp, method)
    l5 <- npcra_l5(data, col_activity, timestamp, method)
    ra <- 0

    if(method==1 | method==2){
        m10 <- m10[1]
        l5 <- l5[1]
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

    duration <- round(Sys.time()-time_begin,digits = 2)
    message("RA was calculated in ", duration, " seconds")
    ra
}

#' Non-Parametric Function IS (Interdaily Stability )
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "Alterations in IS may indicate a  loose coupling between the rest-activity
#' rhythm and its supposedly stable "Zeitgebers," as IS decreases with higher
#' day-to-day variation (i.e. ,  a  looser coupling) of the activity patterns" -
#' WITTING, W. (1990)
#'
#' @param data Dataframe containing the column of numeric values that will be
#'   used as X in the calculation.
#' @param col_x String with the name of the column that will be used in the
#'   calculation.
#' @param col_date String with the name of the column with date values.
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
#' @importFrom lubridate day
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_is(test_log, "pim", "timestamp")}
npcra_is <- function(data, col_x = "pim", col_date = "timestamp"){
    #Attention: Wrong answer yet
    x_values <- as.vector(rbind(data[,col_x]))
    x_values <- unlist(x_values)

    n <- length(x_values)

    day_of_x <- day(data$timestamp)
    p <- as.integer(mean(table(day(data$timestamp))))

    avg_x <- mean(x_values)
    denominator <- sum((x_values - avg_x)^2)
    denominator <- n*denominator

    dados <- data.frame(cbind(x_values), day_of_x)

    #media do grupo
    dados$sqr_diff_from_day <- with(dados,x_values-ave(x_values,day_of_x))

    numerator <- p*sum(dados$sqr_diff_from_day^2)

    is <- numerator/denominator
    print(n)
    print(p)
    print(numerator)
    print(denominator)
    is
}

#' Non-Parametric Function IV (Intradaily  Variability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "IV was included to detect fragmentation of the rest-activity rhythms. High
#' IV may be an indicative of daytime napping and/or nighttime arousals" -
#' WITTING, W. (1990)
#'
#' @param data Dataframe containing the column of numeric values that will be
#'   used as X in the calculation.
#' @param col_x String with the name of the column that will be used in the
#'   calculation.
#'
#' @return A numeric value.
#' @family NPCRA functions
#'
#' @importFrom stats var
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr n_distinct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_iv(test_log, "body_temperature")}
npcra_iv <- function(data, col_activity = "pim", timestamp="timestamp", minutes_interval=60){
    time_interval <- paste(minutes_interval, "min")
    data <- data %>%
        rename("timestamp" = timestamp, "x" = col_activity) %>%
        group_by(interval=cut(timestamp, time_interval)) %>%
        select(x, interval)

    means_activity <- tapply(data$x, data$interval, mean)

    n <- n_distinct(means_activity)

    square_diff <- diff(means_activity)
    square_diff <- square_diff^2
    sum_diff <- sum(square_diff)
    numerator <- n*sum_diff

    denonimator <- var(means_activity)
    denonimator <- denonimator*(n-1)^2

    iv <- numerator/denonimator
    iv
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
