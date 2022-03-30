#' Compute Sokolove & Bushell's \eqn{chi^{2}}{chi square} periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `periodogram()` returns the measures Ap, Qp and of the
#' \eqn{chi^{2}}{chi square} periodogram for an actigraphy dataset.
#'
#' The \eqn{chi^{2}}{chi square} periodogram is a technique to identify periodic
#' patterns in a time series, being widely used in chronobiology to identify the
#' sleep-wake cycle and its reconciliation with circadian periodicity. This
#' periodogram was proposed by Sokolove and Bushell (1978) as an adaptation of
#' Enright's periodogram (1965), adding the peak significance test to the
#' method.
#'
#' @details
#'
#' Sokolove and Bushell's periodogram assumes that the data is time equidistant
#' - there is data for the entire time unit delivered in the breaks parameter.
#' If this does not occur, the function will disregard these missing values in
#' the middle of the time series while calculating the periodogram. If there is
#' more than one data for the breaks unit (for example, when the data was
#' captured per minute and the user wants to calculate the periodogram per
#' hour), the average of all values in this interval is considered as the
#' data point of the periodogram.
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
#' The significance test of the peaks given by \eqn{A_{p}}{Ap} leads to a
#' \eqn{chi^{2}}{chi square} distribution for each \eqn{p}, with the test for a
#' given \eqn{p} being consistent with a distribution of \eqn{P - 1} degrees of
#' freedom. The formula for calculating the test is:
#'
#' \deqn{Q_{p} = P * m * Ap^{2} / Var(X)}{Qp = P * m * Ap^2 / Var(X)}
#'
#' Where:
#'
#' * \eqn{P} = the period of this test.
#' * \eqn{m} = the total rows of the Buys-Ballot table.
#' * \eqn{Ap} = the standard deviation of the averages from the Buys-Ballot
#' table of this test.
#' * \eqn{Var(X)}= the variance of the data.
#'
#' @param data An [`xts`][xts::xts()] object with a numeric vector. If the
#'   [`xts`][xts::xts()]` have more than 1 vector, the function will use only
#'   the first one.
#' @param breaks A string indicating at which interval the index must be
#'   aggregated. (valid values: `“microseconds”`, `“milliseconds”`, `“seconds”`,
#'   `“minutes”`, `“hours”`, `“days”`, `“weeks”`, `“months”`, `“quarters”`, and
#'   `“years”`) (default: `"minutes"`).
#' @param p_min An integer number indicating the minimum period p to calculate
#'   the test and add to the periodogram (same unit as `breaks`). (default:
#'   `1`).
#' @param p_max An integer number indicating the maximum period p to calculate
#'   the test and add to the periodogram (same unit as `breaks`). (default:
#'   `4600`).
#' @param p_step An integer number indicating the range of values that will be
#'   skipped between calculating one test and the next. (default: `1`).
#' @param alpha A number, from 0 to 1, indicating the significant level required
#'   for the peaks (default: `0.05`).
#' @param print A [`logical`][logical()] value indicating if the function
#'   must print the Qp plot (default: `TRUE`).
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
#' @examples
#' data <- xts::as.xts(x = rep(seq(1, 60), times = 30),
#'                     order.by = seq(as.POSIXct("2020-01-01"),
#'                                    as.POSIXct("2020-01-02 05:59:59"),
#'                                    by = "min"))
#' periodogram <- periodogram(data, breaks = "minutes", p_min = 1, p_max = 350,
#'                            p_step = 1, alpha = 0.05, print = FALSE)
#'
#' head(periodogram$periods)
#' periodogram$unit
#' periodogram$alpha
#' head(periodogram$a_p)
#' head(periodogram$q_p)
#' head(periodogram$q_p_alpha)
#' head(periodogram$q_p_pvalue)
#' periodogram$q_p_peaks
#' periodogram$q_p_plot
#'
#' ## Using interactive plots
#'
#' if (interactive() &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'     plotly::ggplotly(periodogram$q_p_plot)
#' }
periodogram <- function(data, breaks = "minutes", p_min = 1000, p_max = 2500,
                        p_step = 1, alpha = 0.05, print = TRUE) {
    break_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                       "hours", "days", "weeks", "months", "quarters", "years")

    checkmate::assert_class(data, "xts")
    checkmate::assert_multi_class(zoo::index(data), "POSIXt")
    checkmate::assert_choice(breaks, break_choices)
    checkmate::assert_int(p_min, lower = 1)
    checkmate::assert_int(p_max, lower = 1)
    checkmate::assert_int(p_step, lower = 1)
    checkmate::assert_number(alpha)
    checkmate::assert_flag(print)
    checkmate::assert_true(p_min <= p_max) # Create custom assertion
    checkmate::assert_true((p_min + p_step) <= p_max) # Create custom assertion

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- NULL

    epoch <- find_epoch(data, 0.9)

    if (is.na(epoch$best_match)) {
        cli::cli_abort(paste0(
            "Your {.strong {cli::col_red('data')}} must have at least ",
            "90% of regularity to run this function. ",
            "See {backtick_('?find_epoch()')} to check your data regularity."
        ))
    } else {
        if (string_to_period(breaks) < epoch$best_match) {
            cli::cli_abort(paste0(
                "The periodicity present in {.strong {cli::col_blue('data')}} ",
                "(epoch = {epoch$best_match} seconds) ",
                "don't allow to break it in {breaks}. ",
                "Use a more appropriate value in ",
                "{.strong {cli::col_red('breaks')}} when running this ",
                "function."
            ))
        }
    }

    if (!zoo::is.regular(data, strict = TRUE) &&
        epoch$prevalence$proportion[1] < 0.99) {
        cli::cli_alert_warning(paste0(
            "The time series is not strictly regular ",
            "(see {backtick_('?zoo::is.regular')}). ",
            "The output may diverge."
        ))

        prevalence <- paste0(
            epoch$prevalence$epoch, " (",
            round(epoch$prevalence$proportion * 100, 2) , "% of prevalence)"
            )

        cli::cli_alert_warning(paste0(
            "Found {.strong ",
            "{cli::col_red(dplyr::n_distinct(diff(zoo::index(data))))}} ",
            "unique time differences between the time series indexes: ",
            "{head(prevalence, 10)} seconds ",
            "(showing up to a total of 10 values)."
        ))
    }

    if (!ncol(data) == 1) {
        cli::cli_alert_warning(paste0(
            "{.strong {cli::col_blue('data')}} ",
            "has more than 1 column. ",
            "Only the first column ",
            "({.strong {cli::col_red(names(data)[1])}}) ",
            "will be used."
        ))
    }

    if (any(is.na(as.numeric(data[, 1])))) {
        cli::cli_alert_warning(paste0(
            "The {.strong {cli::col_red(names(data)[1])}} ",
            "column from {.strong {cli::col_blue('data')}} ",
            "has missing values. The output may diverge. ",
            "Try to interpolate the missing values before using this function ",
            "(see {backtick_('?zoo::na.approx')})."
        ))
    }

    data <- data %>%
        xts::endpoints(breaks) %>%
        xts::period.apply(data, ., mean, na.rm = TRUE) %>%
        `[`(j = 1)

    for (i in c("p_min", "p_max")) {
        if (get(i) > length(data)) {
            cli::cli_abort(paste0(
                "{.strong {cli::col_blue(i)}} is greater than the amount ",
                "of time series data delimited by the breaks."
            ))
        }
    }

    data <- as.numeric(data)
    p_seq <- seq(p_min, p_max, by = p_step)
    envir <- environment()
    cli::cli_progress_bar(total = length(p_seq), clear = FALSE, .envir = envir)

    out <- p_seq %>%
        purrr::map(compute_periodogram, data = data, alpha = alpha,
                   envir = envir) %>%
        purrr::pmap(c) %>%
        append(., list(
            unit = breaks,
            alpha = alpha,
            periods = p_seq,
            q_p_peaks = find_periodogram_peaks(p_seq, .$q_p, .$q_p_alpha,
                                               .$q_p_pvalue),
            q_p_plot = plot_periodogram(p_seq, .$q_p, .$q_p_alpha,
                                        alpha, paste0("Period (", breaks, ")"))
        )) %>%
        magrittr::extract(c("unit", "alpha", "periods", "a_p", "q_p",
                            "q_p_alpha", "q_p_pvalue", "q_p_peaks", "q_p_plot"))

    if (isTRUE(print)) print(out$q_p_plot)

    invisible(out)
}

compute_periodogram <- function(p, data, alpha = 0.05, envir = NULL) {
    checkmate::assert_int(p)
    checkmate::assert_numeric(data, min.len = 1)
    checkmate::assert_number(alpha, lower = 0)
    checkmate::assert_environment(envir, null.ok = TRUE)

    n <- length(data)
    m <- floor(n / p)

    buys_ballot <- matrix(data[1:(m * p)],
                          nrow = m,
                          ncol = p,
                          byrow = TRUE)

    y_ph <- colMeans(buys_ballot)
    y_ph_mean <- mean(y_ph, na.rm = TRUE)
    x_var <- stats::var(data[1:(m * p)], na.rm = TRUE)

    a_p <- sqrt((sum((y_ph - y_ph_mean)^2)) / p)
    q_p <- (p * (a_p^2)) / (x_var / m)
    q_p_alpha <- stats::qchisq(p = alpha, df = p - 1, lower.tail = FALSE)
    q_p_pvalue <- stats::pchisq(q = q_p, df = p - 1, lower.tail = FALSE)

    if (!is.null(envir)) cli::cli_progress_update(.envir = envir)

    list(a_p = a_p, q_p = q_p, q_p_alpha = q_p_alpha, q_p_pvalue = q_p_pvalue)
}

find_periodogram_peaks <- function(p_seq, q_p, q_p_alpha, q_p_pvalue = NULL,
                                   max_diff = 1) {
    checkmate::assert_numeric(p_seq)
    checkmate::assert_numeric(q_p)
    checkmate::assert_numeric(q_p_alpha)
    checkmate::assert_numeric(q_p_pvalue, null.ok = TRUE)
    assert_identical(p_seq, q_p, q_p_alpha, type = "length")
    checkmate::assert_int(max_diff, lower = 1)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- q_p_rel <- NULL

    if (length(which(q_p > q_p_alpha)) == 0) {
        out <- dplyr::tibble(period = numeric(), q_p = numeric(),
                             q_p_alpha = numeric(), q_p_rel = numeric(),
                             q_p_pvalue = numeric())

        return(out)
    }

    x <- which(q_p >= q_p_alpha)
    groups <- list(x[1])
    level <- 1

    for (i in seq(2, length(x))) {
         if (abs(diff(c(x[i - 1], x[i]))) <= max_diff) {
            groups[[level]] <- append(groups[[level]], x[i])
        } else {
            level <- level + 1
            if (length(groups) < level) {
                groups[[level]] <- x[i]
            } else {
                groups[[level]] <- append(groups[[level]], x[i])
            }
        }
    }

    peak_index <- purrr::map(groups, function(y) y[q_p[y] == max(q_p[y])]) %>%
        unlist()

    dplyr::tibble(period = p_seq[peak_index],
                  q_p = q_p[peak_index],
                  q_p_alpha = q_p_alpha[peak_index],
                  q_p_rel = q_p - q_p_alpha,
                  q_p_pvalue = q_p_pvalue[peak_index]) %>%
        dplyr::arrange(dplyr::desc(q_p_rel)) %>%
        dplyr::filter(!is.na(period))
}

clean_periodogram_peaks <- function(peaks) {
    checkmate::assert_tibble(peaks, min.rows = 1)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- q_p_rel <- bump <- NULL

    out <- peaks %>%
        dplyr::filter(q_p_rel >= 0.1 * max(q_p_rel)) %>%
        dplyr::arrange(period) %>%
        dplyr::mutate(
            bump = dplyr::case_when(
                dplyr::lead(period) - period <= 0.001 * mean(period) &
                    dplyr::lead(q_p_rel) >= q_p_rel ~ TRUE,
                period - dplyr::lag(period) <= 0.001 * mean(period) &
                    dplyr::lag(q_p_rel) >= q_p_rel ~ TRUE,
                TRUE ~ FALSE
            )) %>%
        dplyr::filter(!bump == TRUE)

    if (nrow(out) == 0) {
        peaks$period
    } else {
        out$period
    }
}

plot_periodogram <- function(p_seq, q_p, q_p_alpha, alpha_level,
                             xlab = "Period", print = FALSE) {
    checkmate::assert_numeric(p_seq)
    checkmate::assert_numeric(q_p)
    checkmate::assert_numeric(q_p_alpha)
    checkmate::assert_number(alpha_level, lower = 0)
    checkmate::assert_string(xlab)
    checkmate::assert_flag(print)

    q_p_alpha_legend <- paste0("Critical value ", "(alpha: ",
                               alpha_level, ")")

    out <- ggplot2::ggplot(mapping = ggplot2::aes(x = p_seq)) +
        ggplot2::geom_line(ggplot2::aes(y = q_p, colour = "Qp")) +
        ggplot2::geom_line(ggplot2::aes(y = q_p_alpha,
                                        colour = q_p_alpha_legend),
                           linetype = "dashed") +
        ggplot2::scale_colour_manual("", breaks = c("Qp", q_p_alpha_legend),
                                     values = c("black", "red")) +
        ggplot2::labs(x = xlab) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = "top")

    peaks <- find_periodogram_peaks(p_seq, q_p, q_p_alpha)

    if (!nrow(peaks) == 0) {
        out <- out + ggplot2::scale_x_continuous(
            sec.axis = ggplot2::sec_axis(
                ~.x, breaks = clean_periodogram_peaks(peaks)))
    }

    if (isTRUE(print)) {
        print(out)
    } else {
        invisible(out)
    }
}
