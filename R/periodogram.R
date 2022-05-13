#' Compute Sokolove & Bushell's \eqn{\chi^{2}}{chi square} periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `periodogram()` returns the Sokolove & Bushell's \eqn{\chi^{2}}{chi square}
#' periodogram plot and measures for a [`tsibble`][tsibble::tsibble()] object.
#'
#' The \eqn{\chi^{2}}{chi square} periodogram is a technique to identify
#' periodic patterns in a time series, being used in chronobiology to identify
#' the presence/absence of circadian periodicities in rest-activity data. It was
#' proposed by Sokolove and Bushell (1978) as an adaptation of Enright's
#' periodogram (1965), adding the peak significance test to the procedure.
#'
#' @details
#'
#' ## Irregular intervals
#'
#' Sokolove and Bushell's periodogram assumes that the time series is regular,
#' i.e., that it has regular intervals/epochs. `periodogram()` will still work
#' with slightly irregular time series, but only if the latter have at least a
#' 90% prevalence of the same periodicity. Warning messages will be issued
#' telling the user if any data irregularities were found.
#'
#' ## Data aggregation
#'
#' If there's more than one data for each `p_unit` (e.g., when the data was
#' recorded per minute and the user wants to compute the periodogram per hour),
#' `periodogram()` will aggregate the values by averaging them or by assigning
#' the most frequent value (mode) (for non-numeric or single integer variables).
#'
#' ## Missing values
#'
#' `periodogram()` will work even with missing values. As is the case for
#' any analysis with missing data, the results may diverge. You may want to
#' interpolate these data points.
#'
#' There are few articles that deals with interpolation in actigraphy. Tonon et
#' al. (2022) recommends not using interpolation (i.e., maintain `NA` values)
#' whenever is possible. The same authors also recommends using the [weekly mean
#' method][actverse::na_weekly_mean()] of interpolation when the parameters
#' cannot be computed in the presence of `NA` values.
#'
#' @section Guidelines:
#'
#' Enright (1965) and Sokolove & Bushell (1978) guidelines for the
#' \eqn{\chi^{2}}{chi square} periodogram computation are as follows.
#'
#' ## Notes
#'
#' * If you are visualizing this documentation in plain text, you may have some
#' trouble understanding the equations. You can see this documentation on the
#' package [website](https://gipso.github.io/actverse/reference/).
#'
#' ## \eqn{A_{p}}{Ap}: The statistic adopted to express "importance"
#'
#' `periodogram()` compute the \eqn{A_{p}}{Ap} stat for each test period
#' \eqn{p}, between the minimum determined by `p_min` argument and the maximum
#' `p_max` argument, at a step determined by `p_step` argument (e.g., when
#' `p_min == 1`, `p_max == 5`, and `p_step == 1`, the test periods will be
#' `1`, `2`, `3`, `4` and `5`).
#'
#' The \eqn{A_{p}}{Ap} measure of Enright's periodogram is the standard
#' deviation of column means of a Buys-Ballot table, or, as Enright puts it,
#' "the root mean square __amplitude__". This Buys-Ballot table has \eqn{P}
#' columns and \eqn{m} rows, where \eqn{m} is a number that maximizes the amount
#' of values that a time series of \eqn{N} values can have represented in a
#' \eqn{P} columns table (thus, \eqn{m} is \eqn{N / p} rounded down). \eqn{m}
#' can be seen as a cycle, and the greater the similarity between the cycles and
#' the difference between the columns, more intense will be the standard
#' deviation (\eqn{A_{p}}{Ap}).
#'
#' * Buys-Ballot table for an integral period \eqn{p}:
#'
#' ```
#'                                P (count)
#'           |------------------------------------------------|     -
#'                                                                  |
#' Row 1          X_1          X_2          ...          X_P        |
#' Row 2         X_P+1        X_P+2         ...          X_2P       | m (count)
#' ...            ...          ...          ...          ...        |
#' Row m      X_P(m-1)+1   X_P(m-1)+2       ...          X_Pm       |
#'           --------------------------------------------------     -
#' Totals        U_P,1        U_P,2         ...          U_P,P
#' Averages      Y_P,1        Y_P,2         ...          Y_P,P
#' ````
#'
#' As the table above shows, \eqn{P} is the number of columns in the array
#' (matrix/table), while \eqn{m} is the number of rows. When \eqn{p} (test
#' period) is an integer, \eqn{p = P}. That way, \eqn{A_{p}}{Ap} can be computed
#' as:
#'
#' __\deqn{A_{p} = \sqrt{\frac{\sum^{P}_{h = 1} (Y_{p, h} -
#' \overline{Y}_{p})^{2}}{P}}}{Ap = ((sum^{P}_{h = 1} (Y_p,h - MeanY_p)^{2}) /
#' P)^{1/2}}__
#'
#' In which:
#'
#' __\deqn{\overline{Y}_{p} = \frac{\sum^{P}_{h = 1} Y_{p, h}}{P}}{MeanY_p =
#' (sum^{P}_{h = 1} Y_p,h) / P}__
#'
#' ## \eqn{Q_{p}}{Qp}: Sokolove & Bushell's peak significance test
#'
#' Plotting the \eqn{A_{p}}{Ap} values allows you to identify the magnitude of
#' the standard deviations for different \eqn{p}s. The higher the standard
#' deviation the more it will tend to a peak. As these values are susceptible to
#' high and instantaneous fluctuations, Sokolove & Bushell proposed adding a
#' peak significance test, reducing the intensity of peaks by weighting the
#' total variance of the data and the period in which it appears. In this
#' process, peak moments will be highlighted among its neighbors, and smaller
#' changes will lose prominence, tending to a constant variation from one period
#' to another.
#'
#' The significance test of the peaks given by \eqn{A_{p}}{Ap} leads to a
#' \eqn{\chi^{2}}{chi square} distribution for each \eqn{p}, with the test for a
#' given \eqn{p} being consistent with a distribution of \eqn{P - 1} degrees of
#' freedom.
#'
#' The formula for calculating the test is:
#'
#' __\deqn{Q_{p} = \frac{P \times Ap^{2}}{\sigma^{2}_{\overline{X}}}}{Qp =
#' P * m * Ap^2 / Var(X_line)}__
#'
#' In which:
#'
#' __\deqn{\sigma^{2}_{\overline{X}} = \frac{\sigma^{2}_{X}}{m}}{
#' Var(X_line) = Var(X) / m}__
#'
#' Where:
#'
#' * \eqn{P} = Number of columns of the test Buys-Ballot table.
#' * \eqn{m} = Number of rows of the test Buys-Ballot table.
#' * \eqn{A_{p}}{Ap} = Standard deviation of the column averages from the test
#' Buys-Ballot table.
#' * \eqn{\sigma^{2}_{\overline{X}}}{Var(X_line)}= Variance of the test data
#' (\eqn{X}).
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param col A string indicating which column of `data` to use.
#' @param p_unit (optional) a string indicating at which time unit the index
#'   must be aggregated. By aggregating the index, this will change the time
#'   series interval and, consequently, its `p` periods. Valid values are:
#'   `“seconds”`, `“minutes”`, `“hours”`, `“days”`, `“weeks”`, `“months”`,
#'   `“quarters”`, and `“years”`) (default: `"minutes"`).
#' @param p_min (optional) an integer number indicating the minimum period
#'   (\eqn{p}), with the same unit as `p_unit`, to compute the test (e.g., if
#'   `p_unit = "minutes"`, `p_min = 1` means a period of 1 minute) (default:
#'   `1000`).
#' @param p_max (optional) an integer number indicating the maximum period
#'  (\eqn{p}), with the same unit as `p_unit`, to compute the test (default:
#'   `2500`).
#' @param p_step (optional) an integer number indicating the range of values
#'   that must be skipped between computing one test and the next (e.g., when
#'   `p_min == 1`, `p_max == 7`, and `p_step == 2`, the test periods will be
#'   `1`, `3`, `5`, and `7`) (default: `1`).
#' @param alpha (optional) a number, from `0` to `1`, indicating the significant
#'   level (\eqn{\alpha}{alpha}) required for the peak significance test
#'   (default: `0.05`).
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the \eqn{Q_{p}}{Qp} plot (default: `TRUE`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' * `p_unit`: a string indicating the time unit in which the data was
#' aggregated.
#' * `p_seq`: a [`numeric`][numeric()] object with the the sequence of the
#' tested periods.
#' * `alpha`: a number indicating the significant level used.
#' * `a_p`: a [`numeric`][numeric()] object with the root mean square amplitude
#' (\eqn{A_{p}}{Ap}) for each period.
#' * `a_p_plot`: a [`ggplot`][ggplot2::ggplot()] object with a line chart
#' showing `a_p` (y) by `p_seq` (x).
#' * `q_p`: a [`numeric`][numeric()] object with the peak significant test
#' (\eqn{Q_{p}}{Qp}) for each period .
#' * `q_p_critical`: a [`numeric`][numeric()] object with the
#' \eqn{\chi^{2}}{chi square} critical values for each `q_p`, based on the
#' `alpha` parameter.
#' * `q_p_pvalue`: a [`numeric`][numeric()] object with the p-value for each
#' `q_p`, based on the `alpha` parameter.
#' * `q_p_peaks`: a [`tibble`][tibble::tibble()] object listing each period that
#' peaked above the critical value along with its `q_p`, `q_p_critical`,
#' relative `q_p` (`q_p_rel = q_p_critical - q_p`), and `q_p_pvalue`.
#' * `q_p_plot`: a [`ggplot`][ggplot2::ggplot()] object with a line chart
#' showing `q_p` (y1) and `q_p_critical` (y2) by `p_seq` (x).
#'
#' @template references_c
#' @family period analysis functions
#' @export
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("curl", quietly = TRUE) &&
#'     requireNamespace("jsonlite", quietly = TRUE) &&
#'     requireNamespace("tools", quietly = TRUE)) {
#'         if (curl::has_internet()) {
#'             file <- get_from_zenodo(
#'                 doi = "10.5281/zenodo.4898822", path = tempdir(),
#'                 file = "processed.txt"
#'             )
#'
#'             data <- read_acttrust(file, tz = "America/Sao_Paulo")
#'             per <- periodogram(data, "pim")
#'         }
#' }
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

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
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
            a_p_plot = plot_periodogram_a_p(
                p_seq, .$a_p, paste0("Period (", p_unit, ")")
                ),
            q_p_peaks = find_periodogram_peaks(
                p_seq, .$q_p, .$q_p_critical, .$q_p_pvalue
                ),
            q_p_plot = plot_periodogram_q_p(
                p_seq, .$q_p, .$q_p_critical, alpha,
                paste0("Period (", p_unit, ")")
                )
        )) %>%
        magrittr::extract(c(
            "p_unit", "p_seq", "alpha", "a_p", "a_p_plot", "q_p",
            "q_p_critical", "q_p_pvalue", "q_p_peaks", "q_p_plot"
            ))

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

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)

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

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)

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

plot_periodogram_a_p <- function(p_seq, a_p, xlab = "Period", print = FALSE) {
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_numeric(a_p, min.len = 1)
    checkmate::assert_string(xlab)
    checkmate::assert_flag(print)

    out <- ggplot2::ggplot(mapping = ggplot2::aes(x = p_seq, y = a_p)) +
        ggplot2::geom_line() +
        ggplot2::labs(x = xlab, y = "Ap")

    if (isTRUE(print)) shush(print(out))

    invisible(out)
}

plot_periodogram_q_p <- function(p_seq, q_p, q_p_critical, alpha,
                                 xlab = "Period", print = FALSE) {
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_numeric(q_p, min.len = 1)
    checkmate::assert_numeric(q_p_critical, min.len = 1)
    checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    checkmate::assert_string(xlab)
    checkmate::assert_flag(print)

    q_p_critical_legend <- paste0("Critical value ", "(alpha: ", alpha, ")")

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
