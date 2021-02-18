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

