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
