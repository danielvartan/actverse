#' Replace `NA` by interpolation
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `actverse` provides several functions to interpolate your time series data in
#' an easy manner. These functions can be found with the prefix `na_` + method.
#' See the Methods section to learn more about each one.
#'
#' `na_plot()` creates a plot showing the shape of the interpolation. It helps
#' to visualize and find the best interpolation technique for your data.
#'
#' @details
#'
#' There are few articles that deals about interpolation in actigraphy. Tono et
#' al. (2022) recommends not using interpolation (i.e., maintain `NA` values)
#' whenever is possible. The same authors also recommends using the weekly mean
#' method of interpolation when the parameters cannot be computed in the
#' presence of `NA`.
#'
#' ## `fill_na_tips` argument
#'
#' Some interpolation methods can result in outputs with remaining `NA` values.
#' That's the case, for example, with the linear interpolation (`na_approx()`)
#' method.
#'
#' * Example:
#'
#' ```{r}
#' x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
#' index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")
#'
#' na_approx(x, index, fill_na_tips = FALSE)
#' ````
#'
#' \cr
#' By using `fill_na_gaps == TRUE` (default), the function will fill those gaps
#' with the closest non-missing data point.
#' \cr
#'
#' * Example:
#'
#' ```{r}
#' na_approx(x, index, fill_na_tips = TRUE)
#' ````
#'
#' @section Methods:
#'
#' ## `na_approx()`: Linear interpolation
#'
#' As the name suggests, this method creates a "bridge" between the gaps found
#' in `x`. Learn more about it in [zoo::na.approx()] and [stats::approx()].
#'
#' * Visual example:
#' \cr
#' ```{r na-approx-example, echo = FALSE}
#' x <- na_example_data()$x
#' index <- na_example_data()$index
#'
#' na_plot(x, index, na_approx(x, index))
#' ````
#'
#' ## `na_locf()`: Last observation carried forward
#' \cr
#' This method replaces `NA` values with the preceding observation of the `NA`
#' block.
#'
#' * Visual example:
#' \cr
#' ```{r na-locf-example, echo = FALSE}
#' na_plot(x, index, na_locf(x))
#' ````
#'
#' ## `na_overall_mean()`: Overall mean
#'
#' This method replaces `NA` values with the overall mean of `x`.
#'
#' * Visual example:
#' \cr
#' ```{r na-overall-mean-example, echo = FALSE}
#' na_plot(x, index, na_overall_mean(x))
#' ````
#' ## `na_overall_median()`: Overall median
#'
#' This method replaces `NA` values with the overall median of `x`.
#'
#' * Visual example:
#' \cr
#' ```{r na-overall-median-example, echo = FALSE}
#' na_plot(x, index, na_overall_median(x))
#' ````
#'
#' ## `na_overall_mode()`: Overall mode
#'
#' This method replaces `NA` values with the most frequent value (mode) of
#' `x`.
#'
#' If no mode can be found, the function will return `x` without any
#' interpolation. The function will show a warning message to inform the user if
#' that happen.
#'
#' * Visual example:
#' \cr
#' ```{r na-overall-mode-example, echo = FALSE}
#' na_plot(x, index, na_overall_mode(x))
#' ````
#'
#' ## `na_spline()`: Cubic spline interpolation
#'
#' This method uses low-degree polynomials in each of the intervals, and chooses
#' the polynomial pieces such that they fit smoothly together. It can produce
#' extreme values when dealing with large gaps.
#'
#' Learn more about the spline method in the [spline interpolation Wikipedia
#' page](https://en.wikipedia.org/wiki/Spline_interpolation). See also:
#' [stats::spline()] and [zoo::na.spline()].
#'
#' * Visual example:
#' \cr
#' ```{r na-spline-example, echo = FALSE}
#' na_plot(x, index, na_spline(x, index))
#' ````
#'
#' ## `na_weekly_mean()`: Weekly mean
#'
#' This method replaces `NA` values with the weekly mean of `x`. For datasets
#' with only one week in the `index` the result will be the same as
#' `na_overall_mean()`.
#'
#' * Visual example:
#' \cr
#' ```{r na-weekly-mean-example, echo = FALSE}
#' na_plot(x, index, na_weekly_mean(x, index, week_start = 1))
#' ````
#'
#' ## `na_zero()`: Replace `NA` with `0`s
#'
#' This method replaces `NA` values with `0`s.
#'
#' * Visual example:
#' \cr
#' ```{r na-zero-example, echo = FALSE}
#' na_plot(x, index, na_zero(x))
#'
#' @param x A [`numeric`][numeric()] object.
#' @param index An R object, of the same length of `x`, with the index of the
#'   time series.
#' @param fill_na_tips (optional) a [`logical`][logical()] value indicating if
#'   the function must fill remaining `NA` values with the closest non-missing
#'   data point. Learn more about it in the Details section (default: `TRUE`).
#' @param week_start (optional) an integer number indicating the day on which
#'   the week starts (`1` for Monday and `7` for `Sunday`) (default: `1`).
#' @param intp (optional) a [`numeric`][numeric()] object, of the same length
#'   of `x`, with the output of the `NA` interpolation of `x`.
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the plot (default: `TRUE`).
#'
#' @return
#'
#' * `na_*`: a [`numeric`][numeric()] object of the same length of `x`.
#' * `na_plot()`: a [`ggplot`][ggplot2::ggplot()] object with a point and line
#' chart showing the original versus the interpolated data.
#'
#' @template references_d
#' @family interpolation functions
#' @export
#'
#' @examples
#' x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
#' index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")
#'
#' x
#' #> [1] NA  1  5 10 NA  5 10  1 NA 10  1  5 NA NA # Expected
#' na_plot(x, index)
#'
#' ## 'na_approx()': Linear interpolation
#'
#' na_approx(x, index, fill_na_tips = FALSE)
#' #> [1]   NA  1.0  5.0 10.0  7.5  5.0 10.0  1.0  5.5 10.0
#' #> [11] 1.0  5.0  NA   NA # Expected
#'
#' na_approx(x, index, fill_na_tips = TRUE)
#' #> [1]   1.0  1.0  5.0 10.0  7.5  5.0 10.0  1.0  5.5 10.0
#' #> [11] 1.0  5.0  5.0  5.0 # Expected
#' na_plot(x, index, na_approx(x, index, fill_na_tips = TRUE))
#'
#' ## 'na_locf()': Last observation carried forward
#'
#' na_locf(x, fill_na_tips = FALSE)
#' #> [1] NA  1  5 10 10  5 10  1  1 10  1  5  5  5 # Expected
#' na_plot(x, index, na_locf(x, fill_na_tips = FALSE))
#'
#' na_locf(x, fill_na_tips = TRUE)
#' #> [1]  1  1  5 10 10  5 10  1  1 10  1  5  5  5 # Expected
#' na_plot(x, index, na_locf(x, fill_na_tips = TRUE))
#'
#' ## 'na_overall_mean()': Overall mean
#'
#' na_overall_mean(x)
#' #> [1]  5.333333  1.000000  5.000000 10.000000  5.333333  5.000000 10.000000
#' #> [8] 1.000000 5.333333 10.000000  1.000000  5.000000  5.333333
#' #> [14] 5.333333 # Expected
#' mean(x, na.rm = TRUE)
#' #> [1] 5.333333 # Expected
#' na_plot(x, index, na_overall_mean(x))
#'
#' ## 'na_overall_median()': Overall median
#'
#' na_overall_median(x)
#' #> [1]  5  1  5 10  5  5 10  1  5 10  1  5  5  5 # Expected
#' stats::median(x, na.rm = TRUE)
#' #> [1] 5 # Expected
#' na_plot(x, index, na_overall_median(x))
#'
#' ## 'na_overall_mode()': Overall mode
#'
#' na_overall_mode(x)
#' #> ! No mode was found. x was not interpolated.
#' #> [1] NA  1  5 10 NA  5 10  1 NA 10  1  5 NA NA # Expected
#'
#' x2 <- append(x, 1)
#' index2 <- append(index, as.Date("2020-01-15"))
#'
#' na_overall_mode(x2)
#' #> [1]  1  1  5 10  1  5 10  1  1 10  1  5  1  1  1 # Expected
#' na_plot(x2, index2, na_overall_mode(x2))
#'
#' ## 'na_spline()': Cubic spline interpolation
#'
#' na_spline(x, index)
#' #> [1]  4.567728   1.000000   5.000000  10.000000   6.589146   5.000000
#' #> [7]  10.000000  1.000000   5.037198  10.000000   1.000000   5.000000
#' #> [13] 42.905390 131.216171
#' na_plot(x, index, na_spline(x, index))
#'
#' ## 'na_weekly_mean()': Weekly mean
#'
#' na_weekly_mean(x, index, fill_na_tips = FALSE)
#' #> [1]  5.333333  1.000000  5.000000 10.000000  5.333333  5.000000 10.000000
#' #> [8] 1.000000 5.333333 10.000000  1.000000  5.000000  NA  NA # Expected
#' na_plot(x, index, na_weekly_mean(x, index, fill_na_tips = FALSE))
#'
#' na_weekly_mean(x, index, fill_na_tips = TRUE)
#' #> [1]  5.333333  1.000000  5.000000 10.000000  5.333333  5.000000 10.000000
#' #> [8] 1.000000 5.333333 10.000000  1.000000  5.000000  5.000000
#' #> [14] 5.000000 # Expected
#' na_plot(x, index, na_weekly_mean(x, index, fill_na_tips = TRUE))
#'
#' ## 'na_zero()': Replace 'NA' with '0's
#'
#' na_zero(x)
#' #> [1]  0  1  5 10  0  5 10  1  0 10  1  5  0  0 # Expected
#' na_plot(x, index, na_zero(x))
na_approx <- function(x, index, fill_na_tips = TRUE) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    assert_identical(x, index, type = "length")
    checkmate::assert_flag(fill_na_tips)
    require_pkg("zoo")

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- NULL

    # TO DO: Remove zoo dependency.

    zoo::zoo(x = x, order.by = index) %>%
        zoo::na.approx() %>%
        as.numeric() %>%
        na_tip_correction(x, ., fill_na_tips)
}

#' @rdname na_approx
#' @export
na_locf <- function(x, fill_na_tips = TRUE) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    checkmate::assert_flag(fill_na_tips)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- NULL

    not_na <- !is.na(x)

    c(NA, x[not_na])[cumsum(not_na) + 1] %>%
        na_tip_correction(x, ., fill_na_tips)
}

#' @rdname na_approx
#' @export
na_overall_mean <- function(x) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)

    x[is.na(x)] <- mean(x, na.rm = TRUE)

    x
}

#' @rdname na_approx
#' @export
na_overall_median <- function(x) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)

    x[is.na(x)] <- stats::median(x, na.rm = TRUE)

    x
}

#' @rdname na_approx
#' @export
na_overall_mode <- function(x) {
    y <- x[which(!is.na(x))]
    unique <- unique(y)
    mode_value <- unique[which.max(tabulate(match(y, unique)))]

    if (!length(which(tabulate(match(y, unique)) ==
                     max(tabulate(match(y, unique))))) == 1) { #?
        cli::cli_alert_warning(paste0(
            "No mode was found. ",
            "{.strong {cli::col_red('x')}} was not interpolated."
        ))

        x
    } else {
        x[which(is.na(x))] <- mode_value

        x
    }
}

#' @rdname na_approx
#' @export
na_spline <- function(x, index) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    assert_identical(x, index, type = "length")
    require_pkg("zoo")

    # TO DO: Remove zoo dependency.

    zoo::zoo(x = x, order.by = index) %>%
        zoo::na.spline() %>%
        as.numeric()
}

#' @rdname na_approx
#' @export
na_weekly_mean <- function(x, index, fill_na_tips = TRUE, week_start = 1) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    assert_identical(x, index, type = "length")
    checkmate::assert_choice(week_start, c(1, 7))

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- NULL

    data <- dplyr::tibble(index = index, x = x) %>%
        tsibble::tsibble(index = index) %>%
        tsibble::index_by(index_week = tsibble::yearweek(
            index, week_start = week_start)) %>%
        dplyr::summarise(dplyr::across(
            dplyr::everything(), ~ mean(.x, na.rm = TRUE)))

    assign_weekly_mean <- function(x, index, data, week_start) {
        if (is.na(x)) {
            index <- tsibble::yearweek(index, week_start = week_start)
            week_i <- which(index == data$index_week)

            data$x[week_i]
        } else {
            x
        }
    }

    purrr::map2(x, index, assign_weekly_mean, data = data,
                week_start = week_start) %>%
        purrr::flatten_dbl() %>%
        dplyr::if_else(is.nan(.), as.numeric(NA), .) %>%
        na_tip_correction(x, ., fill_na_tips)
}

#' @rdname na_approx
#' @export
na_zero <- function(x) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)

    x[is.na(x)] <- 0

    x
}

#' @rdname na_approx
#' @export
na_plot <- function(x, index, intp = NULL, print = TRUE) {
    method_choices <- c("approx", "locf", "overall_mean", "overall_mode",
                        "spline", "weekly_mean", "zero")

    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    checkmate::assert_numeric(intp, min.len = 1, all.missing = FALSE,
                              null.ok = TRUE)
    assert_identical(x, index, type = "length")
    if (!is.null(intp)) assert_identical(x, index, intp, type = "length")
    checkmate::assert_flag(print)

    if (!is.null(intp)) {
        intp_points <- dplyr::if_else(!is.na(x), as.numeric(NA), intp)

        p <- ggplot2::ggplot(mapping = ggplot2::aes(x = index)) +
            ggplot2::geom_line(ggplot2::aes(y = intp),
                               colour = "gray") +
            ggplot2::geom_point(ggplot2::aes(
                y = x, colour = "Original data"), size = 3) +
            ggplot2::geom_point(ggplot2::aes(
                y = intp_points, colour = "Interpolated data"),
                size = 3) +
            ggplot2::scale_colour_manual(
                "", breaks = c("Original data", "Interpolated data"),
                values = c("black", "red")) +
            ggplot2::labs(x = "Index") +
            ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                           legend.position = "top")
    } else {
        p <- ggplot2::ggplot(mapping = ggplot2::aes(x = index)) +
            ggplot2::geom_point(ggplot2::aes(
                y = x, colour = "Original points"), size = 3) +
            ggplot2::scale_colour_manual(
                "", breaks = c("Original points"),
                values = c("black")) +
            ggplot2::labs(x = "index") +
            ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                           legend.position = "top")
    }

    if (isTRUE(print)) shush(print(p))

    invisible(p)
}

na_tip_correction <- function(x, intp, fill_na_tips = TRUE) {
    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    checkmate::assert_numeric(intp, min.len = 1, all.missing = FALSE)
    checkmate::assert_flag(fill_na_tips)

    # x <- c(NA, NA, NA, 1:10, NA, NA, NA)
    # index <- seq(as.Date("2020-01-01"), as.Date("2020-01-16"), by = "day")

    equal_length <- length(x) == length(intp)
    x_has_left_na_tip <- is.na(x[1])
    intp_has_left_na_tip <- is.na(intp[1])
    x_has_right_na_tip <- is.na(x[length(x)])
    intp_has_right_na_tip <- is.na(intp[length(intp)])

    x_first_non_na_index <- dplyr::first(which(!is.na(x)))
    x_last_non_na_index <- dplyr::last(which(!is.na(x)))

    if (isTRUE(fill_na_tips)) {
        x_first_sub <- x[x_first_non_na_index]
        x_last_sub <- x[x_last_non_na_index]
    } else {
        x_first_sub <- as.numeric(NA)
        x_last_sub <- as.numeric(NA)
    }

    if (equal_length) {
        if (x_has_left_na_tip && intp_has_left_na_tip) {
            intp[seq_len(x_first_non_na_index - 1)] <-  x_first_sub
        }

        if (x_has_right_na_tip && intp_has_right_na_tip) {
            intp[seq(from = x_last_non_na_index + 1,
                     to = length(x))] <- x_last_sub
        }
    } else {
        if (x_has_left_na_tip && !intp_has_left_na_tip) {
            intp <- append(rep(x_first_sub,
                               x_first_non_na_index - 1), intp)
        }

        if (x_has_right_na_tip && !intp_has_right_na_tip) {
            intp <- append(intp, rep(x_last_sub,
                                     length(x) - x_last_non_na_index))
        }
    }

    intp
}

na_example_data <- function() {
    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- timestamp <- pim <- x <- index <- NULL
    acttrust <- acttrust

    acttrust %>%
        tsibble::as_tibble() %>%
        dplyr::slice_head(n = 500) %>%
        dplyr::mutate(
            pim = dplyr::if_else(
                timestamp >= mean(timestamp) - lubridate::dhours(1.1) &
                    timestamp <= mean(timestamp) + lubridate::dhours(1.15),
                as.numeric(NA), pim
            )) %>%
        dplyr::rename(x = pim, index = timestamp) %>%
        dplyr::select(index, x) %>%
        as.list()
}
