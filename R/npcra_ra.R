#' Non-Parametric Function RA (Relative Rhythm Amplitude)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the amplitude of the rest-activity rhythm. the result is based on
#' the difference between the 10 most active hours (M10) and the 5
#' least active hours (L5).
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
#' #Using the test_log data from the package (whole period)
#' npcra_ra(test_log$pim, test_log$timestamp) #expects a tibble 1X1
#'
#'#Running for 1000 random observations (mean profile)
#'first_date <- as.POSIXct('2015-01-01')
#'last_date <- as.POSIXct('2015-01-06')
#' shuffled_timestamp <- sample(seq(first_date,
#'                                  last_date, by = "min"), 1000)
#' timestamp <- sort(shuffled_timestamp)
#' x <- runif(1000, 0, 10000)
#' npcra_ra(x, timestamp, method = 2) #expects a tibble 1X1
#'
#'
#' #Ordering dates and activity data to run (each day)
#' data <- dplyr::as_tibble(x)
#' data <- dplyr::mutate(data, timestamp = shuffled_timestamp)
#' data <- dplyr::arrange(data, timestamp)
#' npcra_ra(data$value, data$timestamp, method = 3) #expects a tibble 5X2
#'
#' @export

npcra_ra <- function(x, timestamp, method = 1){
    checkmate::assert_numeric(x)
    checkmate::assert_posixct(timestamp)
    checkmate::assert_int(method)

    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1,2 or 3,
             but received ", method, " (class ", class(method), ")")
    }

    m10 <- npcra_m10(x, timestamp, method)
    l5 <- npcra_l5(x, timestamp, method)
    out <- dplyr::tibble()

    if(method==1 | method==2){
        m10 <- m10$m10
        l5 <- l5$l5
        out <- (m10-l5) / (m10+l5)
    }

    else{
        m10_values <- m10[,1]
        dates <- m10[,2]
        l5_values <- l5[,1]

        ra <- (m10_values-l5_values) / (m10_values+l5_values)
        out <- dplyr::as_tibble(c(ra, dates))

        colnames(out) <- c("ra", "date")
        out$date = lubridate::date(out$date)
    }

    out
}
