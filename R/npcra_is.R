#' Non-Parametric Function IS (Interdaily Stability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Interdaily Stability identifies the synchronization with the 24h day-night
#' cycle.
#' This synchronization can have different results for the same data according
#' to the chosen time interval, that is, a minute by minute calculation brings a
#'  more sensitive result than a check in hours, where small changes tend to
#'  have less influence on interdaily stability
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
#' @details
#' Intraday variability is a number between 0 and 1 that was based on
#' the normalization of data for the 24-hour value of the chi-square
#' periodogram (Witting, 1990).
#'
#' The IS considers the average profile and the time interval,
#' normally 60 minutes. The result can be obtained by dividing the
#' variance of activity data for the average profile by the variance
#' of activity data for the time interval (by default, 60 minutes)
#'
#' Usually the activity data used in the calculation are hourly averages of
#' the activity, thus avoiding activity fluctuations in the same period of
#' time. The calculation for IS by hourly averages is also called IS60
#' (Goncalves et al., 2014), this being the standard minute interval for
#' the method (60 minutes). It is still possible to vary this interval of
#' minutes, which can generate totally different results for the IS that
#' can be analyzed to identify some pattern of the data.
#'
#'Higher values of IS represent a rest-activity rhythm synchronization
#'with the light dark cycle (Goncalves et al., 2015), this is because the
#'variation between the mean profile and the hourly variation is
#'low, representing synchronization and constancy.
#'
#' @references
#' WITTING, W. et al. Alterations in the circadian rest-activity rhythm in aging
#'and Alzheimer's disease. Biological Psychiatry, v. 27, n. 6, p. 563-572,
#'Mar. 1990. doi: 10.1016/0006-3223(90)90523-5.
#'
#'#' GONCALVES, Bruno da Silva Brandao et al. A fresh look at the use of
#' nonparametric analysis in actimetry. Sleep Medicine Reviews, v. 20,
#' p. 84-91, Apr. 2015. doi: 10.1016/j.smrv.2014.06.002.
#'
#' GONCALVES, Bruno S. B. et al. Nonparametric methods in actigraphy: an update.
#'  Sleep Science, v. 7, n. 3, p. 158-164, 2014. doi: 10.1016/j.slsci.2014.09.013.
#'
#' @family NPCRA functions
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_is(test_log$pim, test_log$timestamp)
#' #[1] 0.4723699
#'
#'#'#Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_is(x, timestamp, minutes_interval = 120) #expects a numeric value
#' @export
npcra_is <- function(x, timestamp, minutes_interval = 60){
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

    if(minutes_interval == 1 | minutes_interval == 0) {
        average_x_per_interval <- x
    }

    else{
        time_interval <- paste(minutes_interval, "min")
        interval <- cut(timestamp, time_interval)
        average_x_per_interval <- tapply(x, interval, mean)
    }

    hourly_means <- tapply(x, lubridate::hour(timestamp) , mean)

    out <- stats::var(hourly_means) / stats::var(average_x_per_interval)
    out
}

#' Non-Parametric Function ISm (Interdaily Stability mean)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Interdaily Stability identifies the synchronization with the 24h day-night
#' cycle.
#'
#' This method calculates the average of IS's up to a minute limit. By default,
#' the limit is 60 minutes, so the 60 IS's will be calculated separately and the
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
#'@details
#' Interdaily Stability (see \code{npcra_is()}) is a number between 0 and 1
#' calculated by dividing the square mean of the first derivative of the
#' data by the population variance (Witting, 1990).
#'
#'From the IS stipulated by Witting, other estimates based on the IS were
#'derived, one being the mean IS (ISm). This method simply consists of
#' averaging IVs at different time intervals (Goncalves, 2014).
#' The function of this package considers a minute limit to calculate the
#' average of IS's.
#' As an example, the default is 60 minutes, so IVs will be calculated
#' with time intervals from 1 to 60 minutes and then average all these values.
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
#' npcra_ism(test_log$pim, test_log$timestamp)
#' # [1] 0.3461636
#'
#' #'#Running for 1000 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_ism(x, timestamp, minute_limit = 120) #expects a numeric value
#' @export
npcra_ism <- function(x, timestamp, minute_limit=60){
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

    sum_is <- 0
    for (current_minute in seq_len(minute_limit)) {
        sum_is <- sum_is + npcra_is(x, timestamp, current_minute)
    }

    out <- sum_is/minute_limit
    out
}
