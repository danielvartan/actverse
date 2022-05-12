#' Compute a spectrogram based on Sokolove & Bushell's periodogram
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `spectrogram()` computes a series of Sokolove & Bushell's (1978)
#' \eqn{\chi^{2}}{chi square} periodograms with the purpose of visualize
#' differences in periodicities in a given interval of a
#' [`tsibble`][tsibble::tsibble()] object.
#'
#' See [?periodogram][periodogram()] to learn more about the periodogram
#' computation.
#'
#' @param int_unit (optional) a string indicating the interval unit. Valid
#'   values are: `“seconds”`, `“minutes”`, `“hours”`, `“days”`, `“weeks”`,
#'   `“months”`, `“quarters”`, and `“years”`) (default: `"days"`).
#' @param int_n (optional) an integer number indicating the size of the
#'   intervals, with the same unit as `int_unit` (default: `7`).
#' @param int_step (optional) an integer number indicating the amount of epochs
#'   to advance at the end of each interval (default: `720`).
#' @param alpha (optional) a number, from `0` to `1`, indicating the significant
#'   level required for the peaks. The spectrogram plot only shows the
#'   significant peaks (default: `0.05`).
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the spectrogram plot (default: `TRUE`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' * `periodograms`: a [`list`][list()] object with the periodogram data for
#' each interval. See [`?periodogram()`][periodogram()] to learn more about the
#' list elements.
#' * `spectrogram`: a [`ggplot`][ggplot2::ggplot()] object with a heat map chart
#' showing one periodogram per line (`q_p`)(y) by the period sequence (`p_seq`)
#' (x).
#'
#' @inheritParams periodogram
#' @template references_e
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
#'             spec <- spectrogram(data, "pim")
#'         }
#' }
#' }
spectrogram <- function(data, col, p_unit = "minutes", p_min = 1000,
                        p_max = 2500, p_step = 1, int_unit = "days", int_n = 7,
                        int_step = 720, alpha = 0.05, print = TRUE) {
    p_unit_choices <- c("second", "minute", "hour", "day", "week", "month",
                        "quarter", "year")
    p_unit_choices <- append(p_unit_choices, paste0(p_unit_choices, "s"))

    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("Date", "POSIXt"))
    assert_clear_epoch(data, 0.9)
    checkmate::assert_choice(col, names(data))
    checkmate::assert_numeric(data[[col]], min.len = 2)
    checkmate::assert_choice(p_unit, p_unit_choices)
    assert_epoch_compatibility(data, p_unit)
    checkmate::assert_int(p_min, lower = 1)
    checkmate::assert_int(p_max, lower = 1)
    checkmate::assert_int(p_step, lower = 1)
    assert_leq(p_min + p_step, p_max)
    checkmate::assert_choice(int_unit, p_unit_choices)
    checkmate::assert_int(int_n, lower = 1)
    checkmate::assert_int(int_step, lower = 1)
    checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    checkmate::assert_flag(print)

    warn_regularity(data, 0.99)
    warn_any_missing(data[[col]])

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- NULL

    if (which(p_unit_choices == int_unit) <= which(p_unit_choices == p_unit)) {
        cli::cli_abort(paste0(
            "The {.strong {cli::col_blue('int_unit')}} value must be greater ",
            "than the {.strong {cli::col_red('p_unit')}} value."
        ))
    }

    data_int <- interval(dplyr::first(data[[tsibble::index_var(data)]]),
                         dplyr::last(data[[tsibble::index_var(data)]]))

    if (as.numeric(data_int) < as.numeric(string_to_period(int_unit))) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_blue('data')}} has a length of ",
            "{lubridate::duration(as.numeric(data_int))}. ",
            "The {.strong {cli::col_red('int_unit')}} value must be lower ",
            "than that."
        ))
    }

    if (data_int <= lubridate::as.interval(
        period_(int_n, int_unit),
        dplyr::first(data[[tsibble::index_var(data)]])
        )) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_blue('data')}} has a length of ",
            "{data_int / period_(1, int_unit)} {int_unit}. ",
            "The {.strong {cli::col_red('int_n')}} value must be below ",
            "that."
        ))
    }

    per_main <- shush(periodogram(data, col, p_unit, p_min, p_max, p_step,
                                  alpha = alpha, print = FALSE))

    data <- data %>%
        dplyr::select(tsibble::index2_var(.), col) %>%
        aggregate_index(p_unit)

    epoch <- find_epoch(data, 0.9)
    int_max_n_epoch <- as.numeric(string_to_period(int_unit)) / epoch$best_match

    if (int_step >= int_max_n_epoch) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_red('int_step')}} cannot be ",
            "greater or equal to the total amount of epochs available in ",
            "{.strong {cli::col_blue('int_unit')}} after data aggregation ",
            "({int_max_n_epoch})."
        ))
    }

    ints <- find_spectrogram_intervals(data, int_unit, int_n, int_step)
    peaks <- per_main$q_p_peaks
    p_seq <- seq(p_min, p_max, by = p_step)
    envir <- environment()
    cli::cli_progress_bar(total = length(ints), clear = FALSE, .envir = envir)

    per_ints <- ints %>%
        purrr::map(compute_interval_periodogram, data = data, col = col,
                   p_unit = p_unit, p_seq = p_seq, alpha = alpha,
                   envir = envir) %>%
        magrittr::set_names(paste0("int_", seq_along(ints)))

    out <- list(periodograms = per_ints,
                spectrogram = plot_spectrogram(
                    p_seq, per_ints, peaks, paste0("Period (", p_unit, ")")))

    if (isTRUE(print)) print(out$spectrogram)

    invisible(out)
}

find_spectrogram_intervals <- function(data, int_unit = "days", int_n = 7,
                                       int_step = 720) {
    int_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                       "hours", "days", "weeks", "months", "quarters", "years")

    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("Date", "POSIXt"))
    checkmate::assert_choice(int_unit, int_choices)
    checkmate::assert_int(int_n, lower = 1)
    checkmate::assert_int(int_step, lower = 1)

    epoch <- find_epoch(data, 0.9)$best_match
    data_int <- interval(dplyr::first(data[[tsibble::index_var(data)]]),
                         dplyr::last(data[[tsibble::index_var(data)]]))

    step <- lubridate::dseconds(epoch * int_step)
    out <- lubridate::as.interval(period_(int_n, int_unit),
                                  lubridate::int_start(data_int))
    check <- lubridate::as.interval(
        period_(int_n, int_unit),
        lubridate::int_start(dplyr::last(out)) + step) %>%
        lubridate::int_end()

    while (check < lubridate::int_end(data_int)) {
        out <- append(out, lubridate::as.interval(
            period_(int_n, int_unit),
            lubridate::int_start(dplyr::last(out)) + step))

        check <- lubridate::as.interval(
            period_(int_n, int_unit),
            lubridate::int_start(dplyr::last(out)) + step) %>%
            lubridate::int_end()
    }

    out
}

compute_interval_periodogram <- function(data, col, int_i, p_unit, p_seq,
                                         alpha = 0.9, envir = NULL) {
    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("Date", "POSIXt"))
    checkmate::assert_choice(col, names(data))
    checkmate::assert_numeric(data[[col]])
    assert_interval(int_i, any.missing = FALSE)
    checkmate::assert_string(p_unit)
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    checkmate::assert_environment(envir, null.ok = TRUE)

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- q_p <- q_p_critical <- q_p_rel <- NULL

    data <- data %>%
        tsibble::filter_index(as.character(lubridate::int_start(int_i)) ~
                                  as.character(lubridate::int_end(int_i))) %>%
        magrittr::extract2(col)

    out <- p_seq %>%
        purrr::map(compute_periodogram, data = data, alpha = alpha) %>%
        purrr::pmap(c) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(q_p_rel = q_p - q_p_critical,
                      q_p_rel = dplyr::if_else(q_p_rel < 0, 0, q_p_rel)) %>%
        as.list() %>%
        append(list(
            p_unit = p_unit,
            p_seq = p_seq,
            int = int_i,
            alpha = alpha,
            q_p_peaks = find_periodogram_peaks(p_seq, .$q_p, .$q_p_critical,
                                               .$q_p_pvalue)
        )) %>%
        magrittr::extract(c("p_unit", "p_seq", "int", "alpha", "a_p",
                            "q_p", "q_p_critical", "q_p_rel", "q_p_pvalue",
                            "q_p_peaks"))

    if (!is.null(envir)) cli::cli_progress_update(.envir = envir)

    out
}

plot_spectrogram <- function(p_seq, per_ints, peaks, xlab = "Period",
                             print = FALSE) {
    checkmate::assert_numeric(p_seq, min.len = 1)
    checkmate::assert_list(per_ints, min.len = 1)
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

    if (isTRUE(print)) shush(print(out))

    invisible(out)
}
