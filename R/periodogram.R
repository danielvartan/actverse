#' Chi Square Periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Calculates the measures Ap, Qp and Qp-normalized of the chi-square
#' periodogram.
#'
#' The Chi Square Periodogram is a technique to identify periodic patterns in a
#' time series, being widely used in chronobiology to identify the sleep-wake
#' cycle and its reconciliation with the 24-hour cycle. This periodogram was
#' proposed by Sokolove and Bushell (1978) as an adaptation of Enright's
#' periodogram (1965), adding the peak significance test to the method.
#'
#' @param x Numeric vector with the activity data that will be used in the
#' calculation.
#' @param timestamp POSIX vector that contains the date and time of each
#' observation.
#' @param breaks A string to represent at which interval the timestamp data
#' will be separated to build the periodogram (sec, min or hour)
#' @param p_min Minimum period p to calculate the test and add to the
#' periodogram
#' @param p_max Maximum period p to calculate the test and add to the
#' periodogram
#' @param step Range of values that will be skipped between calculating one
#' test and the next
#'
#' @return A Periodogram object with the values and visualizations of Ap, Qp
#' and normalized Qp, as well the peak and peak period.
#'
#' @details
#'
#' Sokolove and Bushell's periodogram assumes that the delivered data is
#' time equine - there is data for the entire time unit delivered in the breaks
#' parameter. If this does not occur, the function will disregard these missing
#' values in the middle of the time series in calculating the periodogram. If
#' there is more than one data for the breaks unit (for example, when the data
#' was captured per minute and the user wants to calculate the periodogram per
#' hour), the average of all values in this interval is considered as the
#' activity point of the periodogram.
#'
#' With the formatted data, we calculate the measure Ap for each test period p,
#' between the minimum determined by p_min and the maximum p_max, at a step
#' determined by step parameter. The Ap measure of the Enright periodogram is
#' the standard deviation of the column means of a Buys-Ballot table. This
#' table has p columns and m rows, where m is a number that maximizes the
#' amount of values that a time series of N values can have represented in a
#' p columns table (thus, m is N/p rounded down). m can be seen as a cycle, and
#' the greater the similarity between the cycles and the difference between the
#' columns, more intense the standard deviation Ap.
#'
#' Plotting the Ap values allows you to identify the behavior of the standard
#' deviations of the averages of the columns of the tables created for
#' different P's, and the higher the standard deviation, the more it will
#' tend to a peak. As these values are susceptible to high and instantaneous
#' fluctuations, Sokolove and Bushell proposed adding a peak significance test,
#' reducing the intensity of peaks by weighting the total variance of the data
#' and the period in which it appears. In this process, peak moments will be
#' highlighted among its neighbors, and smaller changes will lose prominence,
#' tending to a constant variation from one period to another.
#'
#' The significance test of the peaks given by Ap leads to a chi-square
#' distribution for each p, with the test for a given p being consistent with
#' a distribution of P-1 degrees of freedom. The formula for calculating the
#' test is:
#'
#' \deqn{Qp = P*m*Ap^2/Var(X)}
#'
#' Where:
#'
#' P is the period of this test
#'
#' m is the total rows of the Buys-Ballot table
#'
#' Ap is the standard deviation of the averages from the Buys=Ballot table of
#' this test
#'
#' Var(X) is the variance of the activity data
#'
#' It is also common to use a standardized version of the test, which consists
#' of performing the calculation:
#'
#' \deqn{Qp = Ap^2/Var(X)}
#'
#' Which results in a number between 0 and 1.
#'
#' @references
#'
#' Enright, J. T. (1965). The search for rhythmicity in biological time-series.
#' Journal of Theoretical Biology, 8(3), 426–468.
#' \doi{https://doi.org/10.1016/0022-5193(65)90021-4}
#'
#' Sokolove, P. G., & Bushell, W. N. (1978). The chi square periodogram: Its
#' utility for analysis of circadian rhythms. Journal of Theoretical Biology,
#' 72(1), 131–160. \doi{https://doi.org/10.1016/0022-5193(78)90022-x}
#'
#' @examples
#' x <- rep(seq(1,4), times = 6)
#' timestamp <- seq(as.POSIXct("2020-01-01"),
#'                  as.POSIXct("2020-01-01 23:00:00"),
#'                  by = "hour")
#'
#' xsp <- chi_square_periodogram(x,
#'                               timestamp,
#'                               breaks = "hour",
#'                               p_max = 12)
#' xsp$PeriodPeak
#' # [1] 4
#' xsp$Peak
#' # [1] 0.96
#' length(xsp$Qp)
#' # [1] 12
#'
#' xsp2 <- chi_square_periodogram(x,
#'                               timestamp,
#'                               breaks = "2 hours",
#'                               p_max = 6)
#' xsp2$Peak
#' # [1] 0.92
#' xsp2$PeriodPeak
#' # [1] 2
#' length(xsp2$Qp)
#' # [1] 6
#'
#' @export
chi_square_periodogram <- function(x,
                                   timestamp,
                                   breaks = 'min',
                                   p_min = 1,
                                   p_max = 4320,
                                   step = 1) {
    interval <- cut(timestamp, breaks)
    x <- tapply(x, interval, mean)
    x <- x[!is.na(x)]

    n <- length(x)

    p <- p_min
    aps <- c()
    qps <- c()
    qps_normalized <- c()
    peak <- 0
    p_peak <- 0
    var_x <- stats::var(x)
    p_seq <- seq(p_min, p_max, by = step)

    pb <- utils::txtProgressBar(p, p_max, style = 3)
    for (p in p_seq) {
        utils::setTxtProgressBar(pb, p)

        m <- floor(n / p)

        buys_ballot <- matrix(x[1:(m*p)],
                              nrow = m,
                              ncol = p,
                              byrow = TRUE)
        yph <- colMeans(buys_ballot)
        mean_yph <- mean(yph)

        ap <- sqrt((1/p)*(sum((yph - mean_yph)**2)))
        qp <- ((ap**2)*p)/(var_x/m)
        qp_normalized <- (ap**2)/var_x

        if (qp_normalized > peak) {
            peak <- qp_normalized
            p_peak <- p
        }

        qps <- c(qps, qp)
        qps_normalized <- c(qps_normalized, qp_normalized)
        aps <- c(aps, ap)
    }
    close(pb)

    plot_ap <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, aps)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Ap')

    plot_qp <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, qps)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Qp')

    plot_qp_normalized <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, qps_normalized)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Qp Normalized')

    periodogram <- list(QpNormalized = qps_normalized,
                        Qp = qps,
                        Ap = aps,
                        QpPlot = plot_qp,
                        QpNormalizedPlot = plot_qp_normalized,
                        ApPlot = plot_ap,
                        Peak = round(peak, digits = 2),
                        PeriodPeak = p_peak)
    class(periodogram) <- "Periodogram"

    periodogram
}
