#' Non-Parametric Function IV (Intradaily Variability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the Intradaily Variability, which identifies the
#' fragmentation of the rest-activity rhythms.
#'
#' This fragmentation can have different results for the same data according
#' to the chosen time interval, that is, a minute by minute calculation brings
#' a more sensitive result than a check in hours, where small changes tend
#' to have less influence on intradaily variability.
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
#' @details
#' Intradaily variability is a number that is usually between 0 and
#' 2 calculated by dividing the square mean of the first derivative
#' of the data by the population variance (Witting et al., 1990).
#'
#' Although real records typically have an IV value less than 2,
#' it is possible that this number exceeds 2. IVs greater than 2
#' are quite common in simulations of random values, since there is
#' no way to define a standard for the data and, therefore, will have
#' a high variability.
#'
#' If X represents the activity or other numeric variable passed as parameter
#' \code{x}, the IV follows the formula below:
#'
#' \deqn{IV = SQM_FD / VAR(X)}
#'
#'Where SQM_FD is the square mean of the first derivative of the data,
#'calculated as below:
#'
#'\deqn{SQM_FD = \sum_{i=2}^{N} (X_i - X_{i-1})^2 / (N-1)}
#'
#'And the population variance VAR(X) is:
#'
#'\deqn{VAR(X) = \sum_{i=1}^{N} (\bar{X} - X_i)^2}
#'
#'Where:
#'
#'\eqn{X_i} is each data point or, more likely, each activity average
#'for the time interval passed by the parameter \code{minutes_interval};
#'
#'N is the amount of data or the number of intervals that can fit in
#'the data set;
#'
#'\eqn{\bar{X}} is the average of all data or the mean of all average
#'activities by time interval.
#'
#' Usually the activity data used in the calculation are hourly averages of
#' the activity, thus avoiding activity fluctuations in the same period of
#' time. The calculation for IV by hourly averages is also called IV60
#' (Goncalves et al., 2014), this being the standard minute interval for
#' the method (60 minutes). It is still possible to vary this interval of
#' minutes, which can generate totally different results for the IV that
#' can be analyzed to identify some pattern of the data.
#'
#' Higher values of IV represent a greater fragmentation of the rest - activity
#'rhythm, this is because the calculation has a certain sensitivity to
#'immediate changes between time intervals, such as naps during the day and
#'nighttime awakenings.
#'
#' @references
#' Witting, W., Kwa, I. H., Eikelenboom, P., Mirmiran, M., & Swaab, D. F.
#' (1990). Alterations in the circadian rest-activity rhythm in aging and
#' Alzheimer's disease. Biological Psychiatry, 27(6), 563-572.
#' \doi{https://doi.org/10.1016/0006-3223(90)90523-5}
#'
#' Goncalves, B. S. B., Cavalcanti, P. R. A., Tavares, G. R., Campos,
#' T. F., & Araujo, J. F. (2014). Nonparametric methods in actigraphy: An
#' update. Sleep Science, 7(3), 158-164.
#' \doi{https://doi.org/10.1016/j.slsci.2014.09.013}
#'
#' @family NPCRA functions
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_iv(test_log$pim, test_log$timestamp)
#' # [1] 0.7290303
#'
#' #Running for 100 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "sec"), 100)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(100, 0, 10000)
#' npcra_iv(x, timestamp, minutes_interval = 120) #expects a numeric  value
#' @export
npcra_iv <- function(x, timestamp, minutes_interval = 60) {
  checkmate::assert_numeric(x)
  checkmate::assert_posixct(timestamp)
  checkmate::assert_int(minutes_interval)

  if(length(x) != length(timestamp)) {
    stop("'x' and 'timestamp' must have the same length",
         "\nLength of x = ", length(x),
         "\nLength of timestamp = ", length(timestamp))
  }

  if (minutes_interval <= 0 | minutes_interval > 1440) {
    stop("The interval should be between 1 and 1440 minutes",
         "\nminutes_interval = ", minutes_interval)
  }

  first_timestamp <- dplyr::first(timestamp)
  last_timestamp <- dplyr::last(timestamp)
  real_interval <- difftime(last_timestamp, first_timestamp, units = "min")

  if (real_interval < lubridate::minutes(minutes_interval)) {
    stop("The requested interval is longer than the ",
         "received data time interval",
         "\nFirst timestamp: ", dplyr::first(timestamp),
         "\nLast timestamp: ", dplyr::last(timestamp),
         "\nReal interval (minutes) = ", real_interval,
         "\nminutes_interval = ", minutes_interval)
  }


  if(minutes_interval == 1) {
    periodic_means <- x
  }

  else {
    time_interval <- paste(minutes_interval, "min")
    interval <- cut(timestamp, time_interval)
    periodic_means <- tapply(x, interval, mean)
  }

  periodic_means <- periodic_means[!is.na(periodic_means)]
  n <- length(periodic_means)
  square_diff <- diff(periodic_means)^2

  numerator <- n * sum(square_diff)

  denonimator <- stats::var(periodic_means)
  denonimator <- denonimator * (n - 1)^2
  out <- numerator / denonimator
  out
}

#' Non-Parametric Function IVm (Intradaily Variability mean)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Intradaily Variability identifies the fragmentation of the
#' rest-activity rhythms.
#'
#' This method calculates the average of IV's up to a minute limit. By default,
#' the limit is 60 minutes, so the 60 IV's will be calculated separately
#' and the results will be averaged to be returned.
#'
#' @param x Numeric vector with the activity data that will be used in
#' the calculation.
#' @param timestamp POSIX vector that contains the date and time of
#' each observation.
#' @param minute_limit integer value that corresponds to the last
#' minute interval to group the data. The default is 60, so 60 values
#' of IV will be calculated to take the average, with the first
#' every minute and the last every 60 minutes.
#' @param show_messages if set to true it will return the IV for
#' every minute on the console as a message.
#' @param summarize if set to true, only the IVm value will be
#' output from the function, if set to false it will also return the
#' IV values for each minute in a tibble
#'
#' @return The numeric value of IVm if summarize is set as TRUE,
#' otherwise will be returned a tibble with the IVs until the minute limit with
#' their names (IVm, IV1, IV2, ...) in the first column and the values of IV
#' in the second column. The first column is called
#' iv_minute and the second as iv, with the IVm in the first row.
#'
#' @details
#' Intradaily variability (see \code{npcra_iv()}) is a number that is
#' usually between 0 and 2 calculated by dividing the square mean of the
#' first derivative of the data by the population variance
#' (Witting et al., 1990).
#'
#' Although real records typically have an IV value less than 2,
#' it is possible that this number exceeds 2. IVs greater than 2
#' are quite common in simulations of random values, since there is
#' no way to define a standard for the data and, therefore, will have
#' a high variability.
#'
#'From the IV stipulated by Witting et al, other estimates based on the IV were
#'derived, one being the mean IV (IVm). This method simply consists of
#' averaging IVs at different time intervals (Goncalves et al., 2014).
#' The function of this package considers a minute limit to calculate the
#' average of IVs.
#' As an example, the default is 60 minutes, so IVs will be calculated
#' with time intervals from 1 to 60 minutes and then average all these values.
#'
#' @references
#' Witting, W., Kwa, I. H., Eikelenboom, P., Mirmiran, M., & Swaab, D. F.
#' (1990). Alterations in the circadian rest-activity rhythm in aging and
#' Alzheimer's disease. Biological Psychiatry, 27(6), 563-572.
#' \doi{https://doi.org/10.1016/0006-3223(90)90523-5}
#'
#' Goncalves, B. S. B., Cavalcanti, P. R. A., Tavares, G. R., Campos,
#' T. F., & Araujo, J. F. (2014). Nonparametric methods in actigraphy: An
#' update. Sleep Science, 7(3), 158-164.
#' \doi{https://doi.org/10.1016/j.slsci.2014.09.013}
#'
#' @family NPCRA functions
#'
#' @examples
#' #Using the test_log data from the package
#' npcra_ivm(test_log$pim, test_log$timestamp)
#' # [1] 0.7977032
#'
#' #Running for 100 random observations
#' first_date <- as.POSIXct('2015-01-01')
#' last_date <- as.POSIXct('2015-01-11')
#' shuffled_timestamp <- sample(seq(first_date,
#'                       last_date, by = "sec"), 100)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(n = 100, min = 0, max = 10000)
#' npcra_ivm(x,
#'           timestamp,
#'           minute_limit = 120,
#'           summarize = FALSE) #Expects a tibble 121 X 2
#' @export
npcra_ivm<- function(x,
                     timestamp,
                     minute_limit = 60,
                     show_messages = TRUE,
                     summarize = TRUE) {

  checkmate::assert_numeric(x)
  checkmate::assert_posixct(timestamp)
  checkmate::assert_numeric(minute_limit)
  checkmate::assert_logical(summarize)
  checkmate::assert_logical(show_messages)

  if(length(x) != length(timestamp)) {
    stop("'x' and 'timestamp' must have the same length",
         "\nLength of x = ", length(x),
         "\nLength of timestamp = ", length(timestamp))
  }

  if (minute_limit <= 0 | minute_limit > 1440) {
    stop("The interval should be between 1 and 1440 minutes",
         "\nminute_limit = ", minute_limit)
  }

  first_timestamp <- dplyr::first(timestamp)
  last_timestamp <- dplyr::last(timestamp)
  real_interval <- difftime(last_timestamp, first_timestamp, units = "min")

  if (real_interval < lubridate::minutes(minute_limit)) {
    stop("The requested interval is longer than the ",
         "received data time interval",
         "\nFirst timestamp: ", dplyr::first(timestamp),
         "\nLast timestamp: ", dplyr::last(timestamp),
         "\nReal interval (minutes) = ", real_interval,
         "\nminute_limit = ", minute_limit)
  }

  iv <- c()
  iv_minute <- c()

  for (current_minute in seq_len(minute_limit)) {
    current_iv <- npcra_iv(x, timestamp, current_minute)

    iv <- c(iv, current_iv)
    iv_minute <- c(iv_minute, paste("IV", current_minute, sep = ""))

    if (show_messages) {
      message("IV", current_minute, ": ", current_iv)
    }
  }

  ivm <- sum(iv) / minute_limit
  message("IVm:", ivm)

  if (summarize) {
    out <- ivm
  }

  else {
    iv <- c(ivm, iv)
    iv_minute <- c("IVm", iv_minute)

    out <- dplyr::tibble(iv_minute, iv)
  }

  out
}
