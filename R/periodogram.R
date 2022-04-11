#' Compute Sokolove & Bushell's \eqn{\chi^{2}}{chi square} periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `periodogram()` returns the measures Ap, Qp and of the
#' \eqn{\chi^{2}}{chi square} periodogram for an actigraphy dataset.
#'
#' The \eqn{\chi^{2}}{chi square} periodogram is a technique to identify
#' periodic patterns in a time series, being widely used in chronobiology to
#' identify the sleep-wake cycle and its reconciliation with circadian
#' periodicity. This periodogram was proposed by Sokolove and Bushell (1978) as
#' an adaptation of Enright's periodogram (1965), adding the peak significance
#' test to the method.
#'
#' @details
#'
#' Sokolove and Bushell's periodogram assumes that the data is time equidistant,
#' i.e., there is data for the entire time unit delivered in the `p_unit`
#' parameter. If this does not occur, the function will disregard these missing
#' values in the middle of the time series while calculating the periodogram. If
#' there's more than one data for the `p_unit` (e.g., when the data was captured
#' per minute and the user wants to calculate the periodogram per hour), the
#' average of all values in this interval is considered as the data point of the
#' periodogram.
#'
#' With the formatted data, we calculate the measure \eqn{A_{p}}{Ap} for each
#' test period \eqn{p}, between the minimum determined by `p_min` and the
#' maximum `p_max`, at a step determined by `p_step` parameter. The
#' \eqn{A_{p}}{Ap} measure of the Enright periodogram is the standard deviation
#' of the column means of a Buys-Ballot table. This table has \eqn{p} columns
#' and m rows, where \eqn{m} is a number that maximizes the amount of values
#' that a time series of \eqn{N} values can have represented in a \eqn{p}
#' columns table (thus, \eqn{m} is \eqn{N/p} rounded down). \eqn{m} can be seen
#' as a cycle, and the greater the similarity between the cycles and the
#' difference between the columns, more intense the standard deviation
#' \eqn{A_{p}}{Ap}.
#'
#' Plotting the \eqn{A_{p}}{Ap} values allows you to identify the behavior of
#' the standard deviations of the averages of the columns of the tables created
#' for different \eqn{P}'s, and the higher the standard deviation, the more it
#' will tend to a peak. As these values are susceptible to high and
#' instantaneous fluctuations, Sokolove and Bushell proposed adding a peak
#' significance test, reducing the intensity of peaks by weighting the total
#' variance of the data and the period in which it appears. In this process,
#' peak moments will be highlighted among its neighbors, and smaller changes
#' will lose prominence, tending to a constant variation from one period to
#' another.
#'
#' The significance test of the peaks given by \eqn{A_{p}}{Ap} leads to a
#' \eqn{\chi^{2}}{chi square} distribution for each \eqn{p}, with the test for a
#' given \eqn{p} being consistent with a distribution of \eqn{P - 1} degrees of
#' freedom. The formula for calculating the test is:
#'
#' __\deqn{Q_{p} = \frac{P \times m \times Ap^{2}}{Var(X)}}{Qp = P * m *
#' Ap^2 / Var(X)}__
#'
#' Where:
#'
#' * \eqn{P} = period of this test.
#' * \eqn{m} = total rows of the Buys-Ballot table.
#' * \eqn{Ap} = standard deviation of the averages from the Buys-Ballot table of
#' this test.
#' * \eqn{Var(X)}= variance of the data (\eqn{X}).
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param col An string indicating which column of `data` to use.
#' @param p_unit (optional) a string indicating at which time unit the index
#'   must be aggregated (valid values: `“seconds”`, `“minutes”`, `“hours”`,
#'   `“days”`, `“weeks”`, `“months”`, `“quarters”`, and `“years”`) (default:
#'   `"minutes"`).
#' @param p_min (optional) an integer number indicating the minimum period
#'   \eqn{p} to calculate the test and add to the periodogram (same unit as
#'   `p_unit`).
#'   (default: `1000`).
#' @param p_max (optional) an integer number indicating the maximum period
#'   \eqn{p} to calculate the test and add to the periodogram (same unit as
#'   `p_unit`). (default: `2500`).
#' @param p_step (optional) an integer number indicating the range of values
#'   that will be skipped between calculating one test and the next. (default:
#'   `1`).
#' @param alpha (optional) a number, from `0` to `1`, indicating the significant
#'   level required for the peaks (default: `0.05`).
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the Qp plot (default: `TRUE`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' * `p_unit`: a string indicating the time unit in which the data was
#' aggregated.
#' * `p_seq`: a [`numeric`][numeric()] object with the the sequence of `p`
#' periods.
#' * `alpha`: a string indicating the significant level used.
#' * `a_p`: a [`numeric`][numeric()] object with the root mean square amplitude
#' (\eqn{A_{p}}{Ap}).
#' * `q_p`: a [`numeric`][numeric()] object with the \eqn{\chi^{2}}{chi square}
#' value of each `p` period (\eqn{Q_{p}}{Qp}).
#' * `q_p_critical`: a [`numeric`][numeric()] object with the
#' \eqn{\chi^{2}}{chi square} critical values for each `q_p`, based on the
#' `alpha` parameter.
#' * `q_p_pvalue`: a [`numeric`][numeric()] object the p-value for each `q_p`,
#' based on the `alpha` parameter.
#' * `q_p_peaks`: a [`tibble`][tibble::tibble()] object listing each peak of `p`
#' period above the critical value along with its `q_p`, `q_p_critical`,
#' relative `q_p` (`q_p_rel  = q_p_critical - q_p`), and `q_p_value`.
#' * `q_p_plot`: a [`ggplot`][ggplot2::ggplot()] object with a line chart
#' showing `q_p` (y1) and `q_p_critical` (y2) by `p_seq` (x).
#'
#' @template references_c
#' @family period analysis functions
#' @export
#'
#' @examples
#' data <- dplyr::tibble(
#'     index = seq(as.POSIXct("2020-01-01"),
#'                 as.POSIXct("2020-01-02 05:59:59"),
#'                 by = "min"),
#'     x = rep(seq(1, 60), times = 30))
#' data <- tsibble::tsibble(data, index = index)
#'
#' per <- periodogram(data, "x", p_unit = "minutes", p_min = 1,
#'                    p_max = 350, p_step = 1, alpha = 0.05,
#'                    print = FALSE)
#'
#' per$p_unit
#' head(per$p_seq)
#' per$alpha
#' head(per$a_p)
#' head(per$q_p)
#' head(per$q_p_critical)
#' head(per$q_p_pvalue)
#' per$q_p_peaks
#' per$q_p_plot
#'
#' ## Using interactive plots
#'
#' if (interactive() &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'     plotly::ggplotly(per$q_p_plot)
#' }
periodogram <- function(data, col, p_unit = "minutes", p_min = 1000,
                        p_max = 2500, p_step = 1, alpha = 0.05, print = TRUE) {
    p_unit_choices <- c("second", "minute", "hour", "day", "week", "month",
                      "quarter", "year")
    p_unit_choices <- append(p_unit_choices, paste0(p_unit_choices, "s"))

    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data)
    assert_clear_epoch(data, 0.9)
    checkmate::assert_choice(col, names(data))
    checkmate::assert_numeric(data[[col]])
    checkmate::assert_choice(p_unit, p_unit_choices)
    assert_epoch_compatibility(data, p_unit)
    checkmate::assert_int(p_min, lower = 1)
    checkmate::assert_int(p_max, lower = 1)
    checkmate::assert_int(p_step, lower = 1)
    assert_leq(p_min + p_step, p_max)
    checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    checkmate::assert_flag(print)

    warn_regularity(data, 0.99)
    warn_any_missing(data[[col]])

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- NULL

    data <- data %>%
        dplyr::select(dplyr::all_of(c(tsibble::index2_var(.), col))) %>%
        aggregate_index(p_unit) %>%
        magrittr::extract2(col)

    for (i in c("p_min", "p_max")) {
        if (get(i) > length(data)) {
            cli::cli_abort(paste0(
                "{.strong {cli::col_red(i)}} is greater than the amount ",
                "of the time series data when aggregate by ",
                "{.strong cli::col_blue(p_unit)}."
            ))
        }
    }

    p_seq <- seq(p_min, p_max, by = p_step)
    envir <- environment()
    cli::cli_progress_bar(total = length(p_seq), clear = FALSE, .envir = envir)

    out <- p_seq %>%
        purrr::map(compute_periodogram, data = data, alpha = alpha,
                   envir = envir) %>%
        purrr::pmap(c) %>%
        append(list(
            p_unit = p_unit,
            p_seq = p_seq,
            alpha = alpha,
            q_p_peaks = find_periodogram_peaks(p_seq, .$q_p, .$q_p_critical,
                                               .$q_p_pvalue),
            q_p_plot = plot_periodogram(p_seq, .$q_p, .$q_p_critical,
                                        alpha, paste0("Period (", p_unit, ")"))
        )) %>%
        magrittr::extract(c("p_unit", "p_seq", "alpha", "a_p", "q_p",
                            "q_p_critical", "q_p_pvalue", "q_p_peaks",
                            "q_p_plot"))

    if (isTRUE(print)) shush(print(out$q_p_plot))

    invisible(out)
}

compute_periodogram <- function(p, data, alpha = 0.05, envir = NULL) {
    checkmate::assert_int(p, lower = 1)
    checkmate::assert_numeric(data, min.len = 1)
    checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    checkmate::assert_environment(envir, null.ok = TRUE)

    n <- length(data)
    m <- floor(n / p)

    buys_ballot <- matrix(data[1:(m * p)],
                          nrow = m,
                          ncol = p,
                          byrow = TRUE)

    y_ph <- colMeans(buys_ballot, na.rm = TRUE)
    y_ph_mean <- mean(y_ph, na.rm = TRUE)
    x_var <- stats::var(data[1:(m * p)], na.rm = TRUE)

    a_p <- sqrt((sum((y_ph - y_ph_mean)^2)) / p)
    q_p <- (p * (a_p^2)) / (x_var / m)
    q_p_critical <- stats::qchisq(p = alpha, df = p - 1, lower.tail = FALSE)
    q_p_pvalue <- stats::pchisq(q = q_p, df = p - 1, lower.tail = FALSE)

    if (!is.null(envir)) cli::cli_progress_update(.envir = envir)

    list(a_p = a_p, q_p = q_p, q_p_critical = q_p_critical,
         q_p_pvalue = q_p_pvalue)
}

find_periodogram_peaks <- function(p_seq, q_p, q_p_critical, q_p_pvalue = NULL,
                                   max_diff = 1) {
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_numeric(q_p, min.len = 1)
    checkmate::assert_numeric(q_p_critical, min.len = 1)
    checkmate::assert_numeric(q_p_pvalue, min.len = 1, null.ok = TRUE)
    assert_identical(p_seq, q_p, q_p_critical, type = "length")
    checkmate::assert_int(max_diff, lower = 1)

    if (!is.null(q_p_pvalue)) {
        assert_identical(p_seq, q_p, q_p_critical, q_p_pvalue, type = "length")
    }

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- q_p_rel <- NULL

    if (length(which(q_p > q_p_critical)) == 0) {
        out <- dplyr::tibble(period = numeric(), q_p = numeric(),
                             q_p_critical = numeric(), q_p_rel = numeric(),
                             q_p_pvalue = numeric())

        return(out)
    }

    x <- which(q_p >= q_p_critical & !q_p == 0)
    groups <- list(x[1])
    level <- 1

    if (!length(x) == 1) {
        for (i in seq(2, length(x))) {
            if (abs(diff(c(x[i - 1], x[i]))) <= max_diff) {
                groups[[level]] <- append(groups[[level]], x[i])
            } else {
                level <- level + 1
                groups[[level]] <- x[i]
            }
        }
    }

    peak_index <- purrr::map(groups, function(y) y[q_p[y] == max(q_p[y])]) %>%
        unlist()

    dplyr::tibble(period = p_seq[peak_index],
                  q_p = q_p[peak_index],
                  q_p_critical = q_p_critical[peak_index],
                  q_p_rel = q_p - q_p_critical,
                  q_p_pvalue = q_p_pvalue[peak_index]) %>%
        dplyr::arrange(dplyr::desc(q_p_rel)) %>%
        dplyr::filter(!is.na(period))
}

clean_periodogram_peaks <- function(peaks, prop_q_p_rel = 0.1,
                                    prop_bump = 0.001) {
    checkmate::assert_tibble(peaks)
    checkmate::assert_number(prop_q_p_rel, lower = 0.001, upper = 0.999)
    checkmate::assert_number(prop_bump, lower = 0.001, upper = 0.999)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- q_p_rel <- bump <- NULL

    if (nrow(peaks) == 0) return(peaks)

    out <- peaks %>%
        dplyr::filter(q_p_rel >= prop_q_p_rel * max(q_p_rel),
                      q_p_rel >= 100) %>%
        dplyr::arrange(period)

    while(any(dplyr::lead(out$period) - out$period <=
              prop_bump * mean(out$period), na.rm = TRUE) &&
          any(dplyr::lag(out$period) - out$period <=
              prop_bump * mean(out$period), na.rm = TRUE)) {
        out <- out %>% dplyr::mutate(
            bump = dplyr::case_when(
                dplyr::lead(period) - period <= prop_bump * mean(period) &
                    dplyr::lead(q_p_rel) >= q_p_rel ~ TRUE,
                period - dplyr::lag(period) <= prop_bump * mean(period) &
                    dplyr::lag(q_p_rel) >= q_p_rel ~ TRUE,
                TRUE ~ FALSE
            )) %>%
            dplyr::filter(!bump == TRUE)
    }

    out$period
}

plot_periodogram <- function(p_seq, q_p, q_p_critical, alpha_level,
                             xlab = "Period", print = FALSE) {
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_numeric(q_p, min.len = 1)
    checkmate::assert_numeric(q_p_critical, min.len = 1)
    checkmate::assert_number(alpha_level, lower = 0.001, upper = 0.999)
    checkmate::assert_string(xlab)
    checkmate::assert_flag(print)

    q_p_critical_legend <- paste0("Critical value ", "(alpha: ",
                               alpha_level, ")")

    out <- ggplot2::ggplot(mapping = ggplot2::aes(x = p_seq)) +
        ggplot2::geom_line(ggplot2::aes(y = q_p, colour = "Qp")) +
        ggplot2::geom_line(ggplot2::aes(y = q_p_critical,
                                        colour = q_p_critical_legend),
                           linetype = "dashed") +
        ggplot2::scale_colour_manual("", breaks = c("Qp", q_p_critical_legend),
                                     values = c("black", "red")) +
        ggplot2::labs(x = xlab) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       legend.position = "top")

    peaks <- find_periodogram_peaks(p_seq, q_p, q_p_critical)

    if (!length(clean_periodogram_peaks(peaks)) == 0) {
        out <- out + ggplot2::scale_x_continuous(
            sec.axis = ggplot2::sec_axis(
                ~.x, breaks = clean_periodogram_peaks(peaks)))
    }

    if (isTRUE(print)) shush(print(out))

    invisible(out)
}
