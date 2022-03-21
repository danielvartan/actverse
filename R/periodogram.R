#' Compute the chi square periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `periodogram()` calculates the measures Ap, Qp and Qp-normalized of the
#' chi-square periodogram for an actigraphy dataset.
#'
#' The Chi Square Periodogram is a technique to identify periodic patterns in a
#' time series, being widely used in chronobiology to identify the sleep-wake
#' cycle and its reconciliation with the 24-hour cycle. This periodogram was
#' proposed by Sokolove and Bushell (1978) as an adaptation of Enright's
#' periodogram (1965), adding the peak significance test to the method.
#'
#' @details
#'
#' Sokolove and Bushell's periodogram assumes that the delivered data is time
#' equidistant - there is data for the entire time unit delivered in the breaks
#' parameter. If this does not occur, the function will disregard these missing
#' values in the middle of the time series while calculating the periodogram. If
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
#' \deqn{Qp = P * m * Ap^2 / Var(X)}
#'
#' Where:
#'
#' \eqn{P} = the period of this test.
#' \eqn{m} = the total rows of the Buys-Ballot table.
#' \eqn{Ap} = the standard deviation of the averages from the Buys=Ballot table
#' of this test.
#' \eqn{Var(X)}= the variance of the activity data.
#'
#' It is also common to use a standardized version of the test, which consists
#' of performing the calculation:
#'
#' \deqn{Qp = Ap^2 / Var(X)}
#'
#' Which results in a number between 0 and 1.
#'
#' @param data An [`xts`][xts::xts()] object with a numeric vector. If the
#'   [`xts`][xts::xts()]` have more than 1 vector, the function will use only
#'   the first one.
#' @param breaks A string representing at which interval the timestamp data will
#'   be separated to build the periodogram (seconds, minutes or hours).
#'   (default: "minutes").
#' @param p_min An integer number representing the minimum period p to calculate
#'   the test and add to the periodogram (same unit as breaks). (default: `1`).
#' @param p_max An integer number representing the maximum period p to calculate
#'   the test and add to the periodogram (same unit as `breaks`). (default:
#'   `4600`).
#' @param step An integer number representing the range of values that will be
#'   skipped between calculating one test and the next. (default: `1`).
#'
#' @return A [`list`][list()] object with the values and plots of Ap,
#'   Qp and normalized Qp, as well as the peak and peak period.
#'
#' @family analysis functions
#' @export
#'
#' @references
#'
#' Enright, J. T. (1965). The search for rhythmicity in biological time-series.
#' _Journal of Theoretical Biology_, _8_(3), 426–468.
#' \doi{10.1016/0022-5193(65)90021-4}.
#'
#' Sokolove, P. G., & Bushell, W. N. (1978). The chi square periodogram: its
#' utility for analysis of circadian rhythms. _Journal of Theoretical Biology_,
#' _72_(1), 131–160. \doi{10.1016/0022-5193(78)90022-x}.
#'
#'
#' @examples
#' data <- xts::as.xts(x = rep(seq(1, 4), times = 6),
#'                     order.by = seq(as.POSIXct("2020-01-01"),
#'                                    as.POSIXct("2020-01-01 23:00:00"),
#'                                    by = "hours"))
#' periodogram <- periodogram(data, breaks = "hours", p_max = 12)
#'
#' periodogram$peak$p
#' #> [1] 4 # Expected
#' periodogram$peak$normalized_qp
#' #> [1] 0.9583333 # Expected
#' length(periodogram$qp)
#' #> [1] 12 # Expected
#'
#' ## Using interactive plots
#'
#' if (interactive() &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'     plotly::ggplotly(periodogram$qp_plot)
#' }
periodogram <- function(data,
                        breaks = "minutes",
                        p_min = 1,
                        p_max = 4600,
                        step = 1) {
    checkmate::assert_class(data, "xts")
    checkmate::assert_choice(breaks, c("seconds", "minutes", "hours"))
    checkmate::assert_int(p_min)
    checkmate::assert_int(p_max)
    checkmate::assert_int(step)

    for (i in c("p_min", "p_max")) {
        if (get(i) > length(data[, 1])) {
            cli::cli_abort(paste0(
                "{.strong {cli::col_blue(i)}} is greater than the amount ",
                "of time series data delimited by breaks."
            ))
        }
    }

    if (!(dplyr::n_distinct(diff(zoo::index(data))) == 1)) {
        cli::cli_alert_warning(paste0(
            "The time series index (timestamp) is not equidistant. ",
            "The output may diverge."
        ))
    }

    end_breaks <- xts::endpoints(data, breaks)
    data <- xts::period.apply(data, end_breaks, mean)
    data <- as.numeric(data[, 1])
    n <- length(data)

    ap <- c()
    qp <- c()
    normalized_qp <- c()
    peak <- dplyr::tibble(p = -1, normalized_qp = -1)
    p_seq <- seq(p_min, p_max, by = step)

    cli::cli_progress_bar(total = p_max, clear = FALSE)

    for (p in p_seq) {
        m <- floor(n / p)

        buys_ballot <- matrix(data[1:(m * p)],
                              nrow = m,
                              ncol = p,
                              byrow = TRUE)

        yph <- colMeans(buys_ballot)
        mean_yph <- mean(yph)
        var_x <- stats::var(data[1:(m * p)])

        ap_current <- sqrt((1 / p) * (sum((yph - mean_yph) ^ 2)))
        qp_current <- ((ap_current^2) * p) / (var_x / m)
        normalized_qp_current <- (ap_current ^ 2) / var_x

        qp <- append(qp, qp_current)
        normalized_qp <- append(normalized_qp, normalized_qp_current)
        ap <- append(ap, ap_current)

        if (normalized_qp_current > peak$normalized_qp) {
            peak$normalized_qp <- normalized_qp_current
            peak$p <- p
        }

        cli::cli_progress_update()
    }

    plot_ap <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, ap)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Ap')

    plot_qp <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, qp)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Qp')

    plot_normalized_qp <- ggplot2::ggplot() +
        ggplot2::geom_line(ggplot2::aes(p_seq, normalized_qp)) +
        ggplot2::xlab('Period') +
        ggplot2::ylab('Normalized Qp')

    out <- list(normalized_qp = normalized_qp,
                qp = qp,
                ap = ap,
                peak = peak,
                qp_plot = plot_qp,
                normalized_qp_plot = plot_normalized_qp,
                ap_plot = plot_ap)

    out
}
