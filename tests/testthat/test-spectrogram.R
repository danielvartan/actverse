test_that("spectrogram() | general test", {
    data <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-30 23:59:00"),
                    by = "min"),
        x = rep(seq(1, 3600), times = 12)
    ) %>%
        tsibble::tsibble(index = index)

    object <- spectrogram(
        data = data, col = "x", p_unit = "hours", p_min = 1, p_max = 100,
        p_step = 1, int = "days", int_n = 7, int_step = 5, alpha = 0.05,
        print = TRUE) %>%
        shush()

    checkmate::expect_list(object, len = 2)
    checkmate::expect_list(object$periodograms, len = 111)
    expect_s3_class(object$spectrogram, "ggplot")
})

test_that("spectrogram() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-10 00:00:00"),
                    by = "min"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:100, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    data_3 <- dplyr::tibble(
        index = c(lubridate::as_datetime(seq(0, 60 * 100, by = 60)),
                  lubridate::as_datetime(seq(6030, 6030 + (30 * 100),
                                             by = 30))),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_4 <- dplyr::tibble(
        index = seq(as.POSIXct("2015-01-01"), as.POSIXct("2015-01-08"),
                    by = "min"),
        x = ""
    ) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(spectrogram(
        data = 1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'data' failed")
    expect_error(spectrogram(
        data = data_1[1, ], col = "x", p_unit = "minutes", p_min = 1,
        p_max = 4600, int = "days", int_n = 7, int_step = 720, p_step = 1,
        alpha = 0.05, print = FALSE),
        "Assertion on 'data' failed")
    expect_error(spectrogram(
        data = data_1[, 1], col = "x", p_unit = "minutes", p_min = 1,
        p_max = 4600, int = "days", int_n = 7, int_step = 720, p_step = 1,
        alpha = 0.05, print = FALSE),
        "Assertion on 'data' failed")

    # assert_index_class(data, c("Date", "POSIXt"))
    expect_error(spectrogram(
        data = data_2, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'data' failed")

    # assert_clear_epoch(data, 0.9)
    expect_error(spectrogram(
        data = data_3, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'data' failed")

    # checkmate::assert_choice(col, names(data))
    expect_error(spectrogram(
        data = data_1, col = "a", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'col' failed")

    # checkmate::assert_numeric(data[[col]])
    expect_error(spectrogram(
        data = data_4, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'data\\[\\[col\\]\\]' failed")

    # checkmate::assert_choice(p_unit, p_unit_choices)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "a", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_unit' failed")

    # assert_epoch_compatibility(data, p_unit)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "seconds", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'data' failed")

    # checkmate::assert_int(p_min, lower = 1)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = "a",
        p_max = 4600, int = "days", int_n = 7, int_step = 720, p_step = 1,
        alpha = 0.05, print = FALSE),
        "Assertion on 'p_min' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = -1,
        p_max = 4600, int = "days", int_n = 7, int_step = 720, p_step = 1,
        alpha = 0.05, print = FALSE),
        "Assertion on 'p_min' failed")

    # checkmate::assert_int(p_max, lower = 1)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1,  p_max = "a",
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_max' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1,  p_max = -1,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_max' failed")

    # checkmate::assert_int(p_step, lower = 1)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = "a", int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_step' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = -1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_step' failed")

    # assert_leq(p_min + p_step, p_max)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 2,
        p_step = 2, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'p_min \\+ p_step' failed")

    # checkmate::assert_choice(int, p_unit_choices)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "hours", p_min = 1, p_max = 4600,
        p_step = 1, int = "", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'int' failed")

    # checkmate::assert_int(int_n, lower = 1)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = "", int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'int_n' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = -1, int_step = 720, alpha = 0.05,
        print = FALSE),
        "Assertion on 'int_n' failed")

    # checkmate::assert_int(int_step, lower = 1)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = "", alpha = 0.05,
        print = FALSE),
        "Assertion on 'int_step' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = -1, alpha = 0.05,
        print = FALSE),
        "Assertion on 'int_step' failed")

    # checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = -1,
        print = FALSE),
        "Assertion on 'alpha' failed")
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 2,
        print = FALSE),
        "Assertion on 'alpha' failed")

    # checkmate::assert_flag(print)
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 7, int_step = 720, alpha = 0.05,
        print = 1),
        "Assertion on 'print' failed")

    # if (which(p_unit_choices == int) <= which(p_unit_choices == p_unit)) {
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "seconds", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE))

    # if (as.numeric(data_int) < as.numeric(string_to_period(int))) {
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "years", int_n = 7, int_step = 720, alpha = 0.05,
        print = FALSE))

    # if (data_int <= lubridate::as.interval(
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, int = "days", int_n = 1000, int_step = 720, alpha = 0.05,
        print = FALSE))

    # if (int_step >= int_max_n_epoch) {
    expect_error(spectrogram(
        data = data_1, col = "x", p_unit = "hours", p_min = 1, p_max = 100,
        p_step = 1, int = "days", int_n = 1, int_step = 1000000, alpha = 0.05,
        print = FALSE))
})

test_that("spectrogram() | warning/message test", {
    data_1 <- dplyr::tibble(
        index = c(lubridate::as_datetime(seq(0, 60 * 1000, by = 60)),
                  lubridate::as_datetime(seq(60030, 60030 + (30 * 100),
                                             by = 30))),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(
        index = lubridate::as_datetime(seq(0, 60 * 1000, by = 60)),
        x = c(1:500, rep(as.numeric(NA), length(index) - 500))
    ) %>%
        tsibble::tsibble(index = index)

    # warn_regularity(data, 0.99)
    expect_message(spectrogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 100,
        p_step = 1, int = "hours", int_n = 1, int_step = 59, alpha = 0.05,
        print = FALSE))

    # warn_any_missing(data[[col]])
    expect_message(spectrogram(
        data = data_2, col = "x", p_unit = "minutes", p_min = 1, p_max = 100,
        p_step = 1, int = "hours", int_n = 1, int_step = 59, alpha = 0.05,
        print = FALSE))
})

test_that("find_spectrogram_intervals() | general test", {
    data <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-30 23:59:00"),
                    by = "min"),
        x = rep(seq(1, 3600), times = 12)
    ) %>%
        tsibble::tsibble(index = index)

    object <- find_spectrogram_intervals(
        data = data, int = "days", int_n = 7, int_step = 720) %>%
        shush()

    expect_equal(dplyr::last(object),
                 lubridate::interval("2020-01-23 12:00:00",
                                     "2020-01-30 12:00:00"))
})

test_that("find_spectrogram_intervals() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-10 00:00:00"),
                    by = "min"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:100, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    data_3 <- dplyr::tibble(
        index = seq(as.POSIXct("2015-01-01"), as.POSIXct("2015-01-08"),
                    by = "min"),
        x = ""
    ) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(find_spectrogram_intervals(
        data = 1, int = "days", int_n = 7, int_step = 720),
        "Assertion on 'data' failed")
    expect_error(find_spectrogram_intervals(
        data = data_1[1, ], int = "days", int_n = 7, int_step = 720),
        "Assertion on 'data' failed")
    expect_error(find_spectrogram_intervals(
        data = data_1[, 1], int = "days", int_n = 7, int_step = 720),
        "Assertion on 'data' failed")

    # assert_index_class(data, c("Date", "POSIXt"))
    expect_error(find_spectrogram_intervals(
        data = data_2, int = "days", int_n = 7, int_step = 720),
        "Assertion on 'data' failed")

    # checkmate::assert_choice(int, int_choices)
    expect_error(find_spectrogram_intervals(
        data = data_1, int = "", int_n = 7, int_step = 720),
        "Assertion on 'int' failed")

    # checkmate::assert_int(int_n, lower = 1)
    expect_error(find_spectrogram_intervals(
        data = data_1, int = "days", int_n = "", int_step = 720),
        "Assertion on 'int_n' failed")
    expect_error(find_spectrogram_intervals(
        data = data_1, int = "days", int_n = -1, int_step = 720),
        "Assertion on 'int_n' failed")

    # checkmate::assert_int(int_step, lower = 1)
    expect_error(find_spectrogram_intervals(
        data = data_1, int = "days", int_n = 7, int_step = ""),
        "Assertion on 'int_step' failed")
    expect_error(find_spectrogram_intervals(
        data = data_1, int = "days", int_n = 7, int_step = -1),
        "Assertion on 'int_step' failed")
})

test_that("compute_interval_periodogram() | general test", {
    data <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-30 23:59:00"),
                    by = "min"),
        x = rep(seq(1, 3600), times = 12)
    ) %>%
        tsibble::tsibble(index = index)

    envir <- environment()
    cli::cli_progress_bar(total = 1, clear = FALSE, .envir = envir)

    object <- compute_interval_periodogram(
        data = data, col = "x",
        int_i = lubridate::interval("2020-01-01 00:00:00",
                                    "2020-01-03 00:00:00"),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = envir)

    checkmate::expect_list(object, len = 10)
    checkmate::expect_string(object$p_unit)
    checkmate::expect_numeric(object$p_seq, len = 1501)
    expect_equal(object$int, lubridate::interval("2020-01-01 00:00:00",
                                                 "2020-01-03 00:00:00"))
    checkmate::expect_number(object$alpha)
    checkmate::expect_numeric(object$a_p, len = 1501)
    checkmate::expect_numeric(object$q_p, len = 1501)
    checkmate::expect_numeric(object$q_p_critical, len = 1501)
    checkmate::expect_numeric(object$q_p_rel, len = 1501)
    checkmate::expect_numeric(object$q_p_pvalue, len = 1501)
    checkmate::expect_tibble(object$q_p_peaks)
})

test_that("compute_interval_periodogram() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-10 00:00:00"),
                    by = "min"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:100, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    data_3 <- dplyr::tibble(
        index = seq(as.POSIXct("2015-01-01"), as.POSIXct("2015-01-08"),
                    by = "min"),
        x = ""
    ) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(compute_interval_periodogram(
        data = 1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'data' failed")
    expect_error(compute_interval_periodogram(
        data = data_1[1, ], col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'data' failed")
    expect_error(compute_interval_periodogram(
        data = data_1[, 1], col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'data' failed")

    # assert_index_class(data, c("Date", "POSIXt"))
    expect_error(compute_interval_periodogram(
        data = data_2, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'data' failed")

    # checkmate::assert_choice(col, names(data))
    expect_error(compute_interval_periodogram(
        data = data_1, col = "a",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'col' failed")

    # checkmate::assert_numeric(data[[col]])
    expect_error(compute_interval_periodogram(
        data = data_3, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'data\\[\\[col\\]\\]' failed")

    # assert_interval(int_i, any.missing = FALSE)
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = 1,
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'int_i' failed")
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(NA),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'int_i' failed")

    # checkmate::assert_string(p_unit)
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = 1, p_seq = seq(1000, 2500), alpha = 0.05,
        envir = environment()),
        "Assertion on 'p_unit' failed")

    # checkmate::assert_numeric(p_seq, min.len = 1)
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = "a", alpha = 0.05, envir = environment()),
        "Assertion on 'p_seq' failed")
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = numeric(), alpha = 0.05,
        envir = environment()),
        "Assertion on 'p_seq' failed")

    # checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = -1,
        envir = environment()),
        "Assertion on 'alpha' failed")
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 2,
        envir = environment()),
        "Assertion on 'alpha' failed")

    # checkmate::assert_environment(envir, null.ok = TRUE)
    expect_error(compute_interval_periodogram(
        data = data_1, col = "x",
        int_i = lubridate::as.interval(1, as.Date("2020-01-01")),
        p_unit = "minutes", p_seq = seq(1000, 2500), alpha = 0.05,
        envir = 1),
        "Assertion on 'envir' failed")
})

test_that("plot_spectrogram() | general test", {
    data <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-30 23:59:00"),
                    by = "min"),
        x = rep(seq(1, 3600), times = 12)
    ) %>%
        tsibble::tsibble(index = index)

    per <- periodogram(data = data, col = "x", p_min = 1, p_max = 100,
                       print = FALSE) %>%
        shush()

    spec <- spectrogram(
        data = data, col = "x", p_unit = "hours", p_min = 1, p_max = 100,
        p_step = 1, int = "days", int_n = 7, int_step = 5, alpha = 0.05,
        print = FALSE) %>%
        shush()

    expect_s3_class(plot_spectrogram(
        p_seq = per$p_seq, per_ints = spec$periodograms, peaks = per$q_p_peaks,
        xlab = "Period", print = TRUE),
        "ggplot")
})

test_that("plot_spectrogram() | error test", {
    # checkmate::assert_numeric(p_seq, min.len = 1)
    expect_error(plot_spectrogram(
        p_seq = "a", per_ints = list(a = 1), peaks = dplyr::tibble(),
        xlab = "", print = TRUE),
        "Assertion on 'p_seq' failed")
    expect_error(plot_spectrogram(
        p_seq = numeric(), per_ints = list(a = 1), peaks = dplyr::tibble(),
        xlab = "", print = TRUE),
        "Assertion on 'p_seq' failed")

    # checkmate::assert_list(per_ints, min.len = 1)
    expect_error(plot_spectrogram(
        p_seq = 1, per_ints = 1, peaks = dplyr::tibble(),
        xlab = "", print = TRUE),
        "Assertion on 'per_ints' failed")
    expect_error(plot_spectrogram(
        p_seq = 1, per_ints = list(), peaks = dplyr::tibble(),
        xlab = "", print = TRUE),
        "Assertion on 'per_ints' failed")

    # checkmate::assert_tibble(peaks)
    expect_error(plot_spectrogram(
        p_seq = 1, per_ints = list(a = 1), peaks = 1,
        xlab = "", print = TRUE),
        "Assertion on 'peaks' failed")

    # checkmate::assert_string(xlab)
    expect_error(plot_spectrogram(
        p_seq = 1, per_ints = list(a = 1), peaks = dplyr::tibble(),
        xlab = 1, print = TRUE),
        "Assertion on 'xlab' failed")

    # checkmate::assert_flag(print)
    expect_error(plot_spectrogram(
        p_seq = 1, per_ints = list(a = 1), peaks = dplyr::tibble(),
        xlab = "", print = 1),
        "Assertion on 'print' failed")
})
