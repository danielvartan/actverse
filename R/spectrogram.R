#' Compute a spectrogram based on Sokolove & Bushell's periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `spectrogram()` compute a series of Sokolove & Bushell's \eqn{chi^{2}}{chi
#' square} periodograms with the purpose of visualize differences in
#' periodicities in a given interval of a time series.
#'
#' @param int A string indicating the interval unit (default: `"days"`).
#' @param n_int An integer number indicating the amount of intervals.
#' @param epoch_step An integer number indicating the amount of epochs to
#'   advance at the end of each interval.
#' @param alpha A number, from 0 to 1, indicating the significant level required
#'   for the peaks (default: `0.9`). The spectrogram plot only shows the
#'   significant peaks. A high `alpha` will produce a more detailed image.
#' @param print A [`logical`][logical()] value indicating if the function
#'   must print the spectrogram plot (default: `TRUE`).
#'
#' @return a [`list`][list()] object with the following elements:
#'
#' * `periodograms`: A [`list`][list()] object with the periodograms data.
#' * `spectogram`: a [`ggplot`][ggplot2::ggplot()] object with a heat map plot
#' of the spectrogram.
#'
#' @inheritParams periodogram
#' @family analysis functions
#' @export
#'
#' @examples
#' data <- xts::as.xts(x = rep(seq(1, 60), times = 30),
#'                     order.by = seq(as.POSIXct("2020-01-01"),
#'                                    as.POSIXct("2020-01-02 05:59:59"),
#'                                    by = "min"))
#' spectrogram <- spectrogram(data, breaks = "minutes", p_min = 1, p_max = 120,
#'                            p_step = 1, int = "hours", n_int = 2,
#'                            epoch_step = 59, alpha = 0.5, print = FALSE)
#'
#' head(names(spectrogram$periodograms))
#' spectrogram$periodograms$int_1$unit
#' spectrogram$periodograms$int_1$alpha
#' spectrogram$periodograms$int_1$interval
#' head(spectrogram$periodograms$int_1$periods)
#' head(spectrogram$periodograms$int_1$a_p)
#' head(spectrogram$periodograms$int_1$q_p)
#' head(spectrogram$periodograms$int_1$q_p_alpha)
#' head(spectrogram$periodograms$int_1$q_p_rel)
#' head(spectrogram$periodograms$int_1$q_p_pvalue)
#' head(spectrogram$periodograms$int_1$q_p_rel)
#' spectrogram$periodograms$int_1$q_p_peaks
#'
#' spectrogram$spectrogram
#'
#' ## Using interactive plots
#'
#' if (interactive() &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'     plotly::ggplotly(spectrogram$spectrogram)
#' }
spectrogram <- function(data, breaks = "minutes", p_min = 1000, p_max = 2500,
                        p_step = 1, int = "days", n_int = 7,
                        epoch_step = 720, alpha = 0.8, print = TRUE) {
    break_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                       "hours", "days", "weeks", "months", "quarters", "years")

    checkmate::assert_class(data, "xts")
    checkmate::assert_multi_class(zoo::index(data), "POSIXt")
    checkmate::assert_choice(breaks, break_choices)
    checkmate::assert_int(p_min, lower = 1)
    checkmate::assert_int(p_max, lower = 1)
    checkmate::assert_int(p_step, lower = 1)
    checkmate::assert_choice(int, break_choices)
    checkmate::assert_int(n_int, lower = 1)
    checkmate::assert_int(epoch_step, lower = 1)
    checkmate::assert_number(alpha, lower = 0.01, upper = 0.99)
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

    if (which(break_choices == int) <= which(break_choices == breaks)) {
        cli::cli_abort(paste0(
            "The {.strong {cli::col_blue('int')}} value must be greater ",
            "than the {.strong {cli::col_red('breaks')}} value."
        ))
    }

    data_int <- interval(zoo::index(data)[1],
                         zoo::index(data)[length(zoo::index(data))])

    if (as.numeric(data_int) < string_to_period(int)) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_blue('data')}} has a length of ",
            "{lubridate::duration(as.numeric(data_int))}. ",
            "The {.strong {cli::col_red('int')}} value must be greater ",
            "than that."
        ))
    }

    if (data_int <= lubridate::as.interval(period_(n_int, int),
                                           zoo::index(data)[1])) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_blue('data')}} has a length of ",
            "{data_int / period_(1, int)} {int}. ",
            "The {.strong {cli::col_red('n_int')}} value must be below ",
            "that."
        ))
    }

    int_max_n_epoch <- string_to_period(int) / epoch$best_match

    if (epoch_step >= int_max_n_epoch) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_red('epoch_step')}} cannot be ",
            "equal or greater than the total amount of epochs in ",
            "{.strong {cli::col_blue('int')}} ({int_max_n_epoch})."
        ))
    }

    per_main <- shush(periodogram(data, breaks, p_min, p_max, p_step,
                                  print = FALSE))

    data <- data %>%
        xts::endpoints(breaks) %>%
        xts::period.apply(data, ., mean, na.rm = TRUE) %>%
        magrittr::extract(j = 1)

    for (i in c("p_min", "p_max")) {
        if (get(i) > length(data)) {
            cli::cli_abort(paste0(
                "{.strong {cli::col_blue(i)}} is greater than the amount ",
                "of time series data delimited by the breaks."
            ))
        }
    }

    ints <- find_spectrogram_intervals(data, int, n_int, epoch_step)
    peaks <- per_main$q_p_peaks
    p_seq <- seq(p_min, p_max, by = p_step)
    envir <- environment()
    cli::cli_progress_bar(total = length(ints), clear = FALSE, .envir = envir)

    per_ints <- ints %>%
        purrr::map(compute_interval_periodogram, data = data, p_seq = p_seq,
               breaks = breaks, alpha = alpha, envir = envir) %>%
        magrittr::set_names(paste0( "int_", seq_along(ints)))

    out <- list(periodograms = per_ints,
                spectrogram = plot_spectrogram(
                    p_seq, per_ints, peaks, paste0("Period (", breaks, ")")))

    if (isTRUE(print)) print(out$spectrogram)

    invisible(out)
}

find_spectrogram_intervals <- function(data, int = "days", n_int = 7,
                                       epoch_step = 720) {
    break_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                       "hours", "days", "weeks", "months", "quarters", "years")

    checkmate::assert_class(data, "xts")
    checkmate::assert_multi_class(zoo::index(data), "POSIXt")
    checkmate::assert_choice(int, break_choices)
    checkmate::assert_int(n_int, lower = 1)
    checkmate::assert_int(epoch_step, lower = 1)

    epoch <- find_epoch(data, 0.9)$best_match
    data_int <- interval(zoo::index(data)[1],
                         zoo::index(data)[length(zoo::index(data))])

    step <- lubridate::dseconds(epoch * epoch_step)
    out <- lubridate::as.interval(period_(n_int, int),
                                  lubridate::int_start(data_int))
    check <- lubridate::as.interval(
        period_(n_int, int),
        lubridate::int_start(dplyr::last(out)) + step) %>%
        lubridate::int_end()

    while(check < lubridate::int_end(data_int)) {
        out <- append(out, lubridate::as.interval(
            period_(n_int, int),
            lubridate::int_start(dplyr::last(out)) + step))

        check <- lubridate::as.interval(
            period_(n_int, int),
            lubridate::int_start(dplyr::last(out)) + step) %>%
            lubridate::int_end()
    }

    gsub("--", "/", as.character(out))
}

compute_interval_periodogram <- function(data, int_i, p_seq, breaks,
                                         alpha = 0.9, envir = NULL) {
    checkmate::assert_class(data, "xts")
    checkmate::assert_multi_class(zoo::index(data), "POSIXt")
    checkmate::assert_character(int_i, min.len = 1)
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_string(breaks)
    checkmate::assert_number(alpha, lower = 0.01, upper = 0.99)
    checkmate::assert_environment(envir, null.ok = TRUE)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- q_p <- q_p_alpha <- q_p_rel <- NULL

    data <- data[int_i]
    data <- as.numeric(data)

    int_i <- lubridate::interval(
        strsplit(int_i, "/")[[1]][1], strsplit(int_i, "/")[[1]][2])

    out <- p_seq %>%
        purrr::map(compute_periodogram, data = data, alpha = alpha) %>%
        purrr::pmap(c) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(q_p_rel = q_p - q_p_alpha,
                      q_p_rel = dplyr::if_else(q_p_rel < 0, 0, q_p_rel)) %>%
        as.list() %>%
        append(., list(
            unit = breaks,
            alpha = alpha,
            interval = int_i,
            periods = p_seq,
            q_p_peaks = find_periodogram_peaks(p_seq, .$q_p, .$q_p_alpha,
                                               .$q_p_pvalue)
        )) %>%
        magrittr::extract(c("unit", "alpha", "interval", "periods", "a_p",
                            "q_p", "q_p_alpha", "q_p_rel", "q_p_pvalue",
                            "q_p_peaks"))

    if (!is.null(envir)) cli::cli_progress_update(.envir = envir)

    out
}

plot_spectrogram <- function(p_seq, per_ints, peaks, xlab = "Period",
                             print = FALSE) {
    checkmate::assert_numeric(p_seq)
    checkmate::assert_list(per_ints)
    checkmate::assert_tibble(peaks)
    checkmate::assert_string(xlab)
    checkmate::assert_flag(print)

    q_p_rel <- per_ints %>%
        purrr::map(purrr::pluck("q_p_rel")) %>%
        purrr::flatten_dbl()

    breaks <- c(1, grep("0$", seq_along(per_ints)))

    out <- ggplot2::ggplot(mapping = ggplot2::aes(
        x = rep(p_seq, length(per_ints)),
        y = rep(seq_along(per_ints), each = length(p_seq)),
        fill = q_p_rel)) +
        ggplot2::geom_raster() +
        ggplot2::scale_y_reverse(breaks = breaks) +
        viridis::scale_fill_viridis("Qp", option = "B") +
        ggplot2::labs(x = xlab) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

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
