#' Non-Parametric Function IV (Intradaily Variability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intradaily Variability identifies the fragmentation of the rest-activity
#' rhythms.
#'
#' This fragmentation can have different results for the same data according
#' to the chosen time interval, that is, a minute by minute calculation brings
#' a more sensitive result than a check in hours, where small changes tend
#' to have less influence on intraday variability.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIX vector that contains the date and time of each
#' observation.
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
#' #Using the test_log data from the package
#' npcra_iv(test_log$pim, test_log$timestamp)
#' # [1] 0.7290303
#'
#' #Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 1000)
#' x <- runif(1000, 0, 10000)
#' npcra_iv(x, timestamp, minutes_interval = 120) #expects a numeric value
#' @export
npcra_iv <- function(x, timestamp, minutes_interval = 60){
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)
    checkmate::assert_int(minutes_interval)

    if (minutes_interval < 0 | minutes_interval > 1440) {
        stop('The interval should be between 0 and 1440 minutes')
    }

    if (dplyr::last(timestamp) - dplyr::first(timestamp) <
        lubridate::minutes(minutes_interval)) {
        stop("The requested interval is longer
             than the received data time interval")
    }

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

    out <- numerator / denonimator
    out
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
#' #Using the test_log data from the package
#' npcra_ivm(test_log$pim, test_log$timestamp)
#' # [1] 0.7977032
#'
#' #'#Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_ivm(x, timestamp, minute_limit = 120) #expects a numeric value
#' @export
npcra_ivm<- function(x, timestamp, minute_limit = 60){
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)
    checkmate::assert_int(minute_limit)

    if (minute_limit <= 0 | minute_limit > 1440) {
        stop('The interval should be between 1 and 1440 minutes')
    }

    if (dplyr::last(timestamp) - dplyr::first(timestamp) <
        lubridate::minutes(minute_limit)) {
        stop("The requested interval is longer
             than the received data time interval")
    }

    sum_iv <- 0
    for (current_minute in seq_len(minute_limit)) {
        sum_iv <- sum_iv + npcra_iv(x, timestamp, current_minute)
    }

    out <- sum_iv / minute_limit
    out
}
