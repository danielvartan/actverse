test_that("periodogram() | general test", {
    set.seed(1)

    data <- dplyr::tibble(
        index = seq(lubridate::as_datetime("2020-01-01 00:00:00"),
                    lubridate::as_datetime("2020-01-10 00:00:00"),
                    by = "min"),
        x = runif(n = length(index), min = 0, max = 1000)
    ) %>%
        tsibble::tsibble(index = index)

    object <- periodogram(data, "x", p_unit = "minutes", p_min = 1000,
                          p_max = 2500, p_step = 1, alpha = 0.05, print = TRUE)

    checkmate::expect_list(object, len = 10)
    checkmate::expect_string(object$p_unit)
    checkmate::expect_numeric(object$p_seq, len = 1501)
    checkmate::expect_number(object$alpha)
    checkmate::expect_numeric(object$a_p, len = 1501)
    expect_s3_class(object$a_p_plot, "ggplot")
    checkmate::expect_numeric(object$q_p, len = 1501)
    checkmate::expect_numeric(object$q_p_critical, len = 1501)
    checkmate::expect_numeric(object$q_p_pvalue, len = 1501)
    checkmate::expect_tibble(object$q_p_peaks)
    expect_s3_class(object$q_p_plot, "ggplot")
})

test_that("periodogram() | error test", {
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
    expect_error(periodogram(
        data = 1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")
    expect_error(periodogram(
        data = data_1[1, ], col = "x", p_unit = "minutes", p_min = 1,
        p_max = 4600, p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")
    expect_error(periodogram(
        data = data_1[, 1], col = "x", p_unit = "minutes", p_min = 1,
        p_max = 4600, p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")

    # assert_index_class(data)
    expect_error(periodogram(
        data = data_2, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")

    # assert_clear_epoch(data, 0.9)
    expect_error(periodogram(
        data = data_3, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")

    # checkmate::assert_choice(col, names(data))
    expect_error(periodogram(
        data = data_1, col = "a", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'col' failed")

    # checkmate::assert_numeric(data[[col]])
    expect_error(periodogram(
        data = data_4, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data\\[\\[col\\]\\]' failed")

    # checkmate::assert_choice(p_unit, p_unit_choices)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "a", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_unit' failed")

    # assert_epoch_compatibility(data, p_unit)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "seconds", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'data' failed")

    # checkmate::assert_int(p_min, lower = 1)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = "a",
        p_max = 4600, p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_min' failed")
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = -1,
        p_max = 4600, p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_min' failed")

    # checkmate::assert_int(p_max, lower = 1)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1,  p_max = "a",
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_max' failed")
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1,  p_max = -1,
        p_step = 1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_max' failed")

    # checkmate::assert_int(p_step, lower = 1)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = "a", alpha = 0.05, print = FALSE),
                 "Assertion on 'p_step' failed")
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = -1, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_step' failed")

    # assert_leq(p_min + p_step, p_max)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 2,
        p_step = 2, alpha = 0.05, print = FALSE),
                 "Assertion on 'p_min \\+ p_step' failed")

    # checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = -1, print = FALSE),
                 "Assertion on 'alpha' failed")
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 2, print = FALSE),
                 "Assertion on 'alpha' failed")

    # checkmate::assert_flag(print)
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 4600,
        p_step = 1, alpha = 0.05, print = 1),
                 "Assertion on 'print' failed")

    # for (i in c("p_min", "p_max")) {
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 50000,
        p_max = 50002, p_step = 1, alpha = 0.05, print = FALSE))
    expect_error(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1,
        p_max = 50000, p_step = 1, alpha = 0.05, print = FALSE))
})

test_that("periodogram() | warning/message test", {
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
    expect_message(periodogram(
        data = data_1, col = "x", p_unit = "minutes", p_min = 1, p_max = 100,
        p_step = 1, alpha = 0.05, print = FALSE))

    # warn_any_missing(data[[col]])
    expect_message(periodogram(
        data = data_2, col = "x", p_unit = "minutes", p_min = 1, p_max = 100,
        p_step = 1, alpha = 0.05, print = FALSE))
})

test_that("compute_periodogram() | general test", {
    envir <- environment()
    cli::cli_progress_bar(total = 1000, clear = FALSE, .envir = envir)
    set.seed(1)

    object <- compute_periodogram(
        p = 100, data = runif(n = 1000, min = 0, max = 1000), alpha = 0.05,
        envir = envir)

    checkmate::expect_list(object, len = 4)
    expect_equal(object$a_p, 101.073554)
    expect_equal(object$q_p, 122.835424)
    expect_equal(object$q_p_critical, 123.22522)
    expect_equal(object$q_p_pvalue, 0.052522739)
})

test_that("compute_periodogram() | error test", {
    # checkmate::assert_int(p, lower = 1)
    expect_error(compute_periodogram(
        p = "", data = 1:100, alpha = 0.05, envir = NULL),
                 "Assertion on 'p' failed")
    expect_error(compute_periodogram(
        p = 0, data = 1:100, alpha = 0.05, envir = NULL),
                 "Assertion on 'p' failed")

    # checkmate::assert_numeric(data, min.len = 1)
    expect_error(compute_periodogram(
        p = 1, data = "a", alpha = 0.05, envir = NULL),
                 "Assertion on 'data' failed")
    expect_error(compute_periodogram(
        p = 1, data = numeric(), alpha = 0.05, envir = NULL),
                 "Assertion on 'data' failed")

    # checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    expect_error(compute_periodogram(
        p = 1, data = 1:100, alpha = "", envir = NULL),
                 "Assertion on 'alpha' failed")
    expect_error(compute_periodogram(
        p = 1, data = 1:100, alpha = 0, envir = NULL),
                 "Assertion on 'alpha' failed")
    expect_error(compute_periodogram(
        p = 1, data = 1:100, alpha = 1, envir = NULL),
                 "Assertion on 'alpha' failed")

    # checkmate::assert_environment(envir, null.ok = TRUE)
    expect_error(compute_periodogram(
        p = 1, data = 1:100, alpha = 0.05, envir = 1),
                 "Assertion on 'envir' failed")
})

test_that("find_periodogram_peaks() | general test", {
    expected_1 <- dplyr::tibble(period = numeric(), q_p = numeric(),
                              q_p_critical = numeric(), q_p_rel = numeric(),
                              q_p_pvalue = numeric())

    expect_equal(find_periodogram_peaks(
        p_seq = 1:100, q_p = 201:300, q_p_critical = 301:400,
        q_p_pvalue = 1:100, max_diff = 1),
        expected_1)

    expected_2 <- dplyr::tibble(
        period = 100,
        q_p = 400,
        q_p_critical = 300,
        q_p_rel = 100,
        q_p_pvalue = 100
    )

    expect_equal(find_periodogram_peaks(
        p_seq = 1:100, q_p = 301:400, q_p_critical = 201:300,
        q_p_pvalue = 1:100, max_diff = 1),
        expected_2)
})

test_that("find_periodogram_peaks() | error test", {
    # checkmate::assert_numeric(p_seq, min.len = 1)
    expect_error(find_periodogram_peaks(
        p_seq = "", q_p = 1, q_p_critical = 1, q_p_pvalue = NULL, max_diff = 1),
                 "Assertion on 'p_seq' failed")
    expect_error(find_periodogram_peaks(
        p_seq = numeric(), q_p = 1, q_p_critical = 1, q_p_pvalue = NULL,
        max_diff = 1),
                 "Assertion on 'p_seq' failed")

    # checkmate::assert_numeric(q_p, min.len = 1)
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = "", q_p_critical = 1, q_p_pvalue = NULL, max_diff = 1),
                 "Assertion on 'q_p' failed")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = numeric(), q_p_critical = 1, q_p_pvalue = NULL,
        max_diff = 1),
        "Assertion on 'q_p' failed")

    # checkmate::assert_numeric(q_p_critical, min.len = 1)
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = "", q_p_pvalue = NULL, max_diff = 1),
        "Assertion on 'q_p_critical' failed")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = numeric(), q_p_pvalue = NULL,
        max_diff = 1),
        "Assertion on 'q_p_critical' failed")

    # checkmate::assert_numeric(q_p_pvalue, min.len = 1, null.ok = TRUE)
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1, q_p_pvalue = "", max_diff = 1),
        "Assertion on 'q_p_pvalue' failed")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1, q_p_pvalue = numeric(),
        max_diff = 1),
        "Assertion on 'q_p_pvalue' failed")

    # assert_identical(p_seq, q_p, q_p_critical, type = "length")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1:2, q_p_pvalue = NULL,
        max_diff = 1),
        "Assertion failed: 'p_seq', 'q_p', and 'q_p_critical' must have")

    # checkmate::assert_int(max_diff, lower = 1)
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1, q_p_pvalue = NULL, max_diff = ""),
        "Assertion on 'max_diff' failed")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1, q_p_pvalue = NULL, max_diff = -1),
        "Assertion on 'max_diff' failed")

    # assert_identical(p_seq, q_p, q_p_critical, q_p_pvalue, type = "length")
    expect_error(find_periodogram_peaks(
        p_seq = 1, q_p = 1, q_p_critical = 1, q_p_pvalue = 1:2,
        max_diff = 1),
        "Assertion failed: 'p_seq', 'q_p', 'q_p_critical', and 'q_p_pvalue'")
})

test_that("clean_periodogram_peaks() | general test", {
    expect_equal(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = 0.1, prop_bump = 0.001),
        dplyr::tibble())

    peaks <- dplyr::tibble(
        period = c(1, 2, 3, 100,101),
        q_p = c(100, 200, 300, 400, 500),
        q_p_critical = c(90, 190, 290, 200, 100),
        q_p_rel = q_p - q_p_critical,
        q_p_pvalue = 1:5
    )

    expect_equal(clean_periodogram_peaks(
        peaks = peaks, prop_q_p_rel = 0.1, prop_bump = 0.1),
        101)
})

test_that("clean_periodogram_peaks() | error test", {
    # checkmate::assert_tibble(peaks)
    expect_error(clean_periodogram_peaks(
        peaks = 1, prop_q_p_rel = 0, prop_bump = 0.1),
        "Assertion on 'peaks' failed")

    # checkmate::assert_number(prop_q_p_rel, lower = 0.001, upper = 0.999)
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = "", prop_bump = 0.1),
        "Assertion on 'prop_q_p_rel' failed")
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = -1, prop_bump = 0.1),
        "Assertion on 'prop_q_p_rel' failed")
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = 2, prop_bump = 0.1),
        "Assertion on 'prop_q_p_rel' failed")

    # checkmate::assert_number(prop_bump, lower = 0.001, upper = 0.999)
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = 0.1, prop_bump = ""),
        "Assertion on 'prop_bump' failed")
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = 0.1, prop_bump = -1),
        "Assertion on 'prop_bump' failed")
    expect_error(clean_periodogram_peaks(
        peaks = dplyr::tibble(), prop_q_p_rel = 0.1, prop_bump = 2),
        "Assertion on 'prop_bump' failed")
})

test_that("plot_periodogram_a_p() | general test", {
    data <- dplyr::tibble(
        index = seq(as.POSIXct("2020-01-01"),
                    as.POSIXct("2020-01-02 05:59:59"),
                    by = "min"),
        x = rep(seq(1, 60), times = 30)) %>%
        tsibble::tsibble(index = index)

    per <- periodogram(data = data, col = "x", p_min = 1, p_max = 350,
                       print = FALSE)

    expect_s3_class(plot_periodogram_a_p(
        p_seq = per$p_seq, a_p = per$a_p, xlab = "Period", print = TRUE),
        "ggplot")
})

test_that("plot_periodogram_a_p() | error test", {
    # checkmate::assert_numeric(p_seq, min.len = 1)
    expect_error(plot_periodogram_a_p(
        p_seq = "", a_p = 1, xlab = "Period", print = FALSE),
        "Assertion on 'p_seq' failed")
    expect_error(plot_periodogram_a_p(
        p_seq = numeric(), a_p = 1, xlab = "Period", print = FALSE),
        "Assertion on 'p_seq' failed")

    # checkmate::assert_numeric(a_p, min.len = 1)
    expect_error(plot_periodogram_a_p(
        p_seq = 1, a_p = "", xlab = "Period", print = FALSE),
        "Assertion on 'a_p' failed")
    expect_error(plot_periodogram_a_p(
        p_seq = 1, a_p = numeric(), xlab = "Period", print = FALSE),
        "Assertion on 'a_p' failed")

    # checkmate::assert_string(xlab)
    expect_error(plot_periodogram_a_p(
        p_seq = 1, a_p = 1, xlab = 1, print = FALSE),
        "Assertion on 'xlab' failed")

    # checkmate::assert_flag(print)
    expect_error(plot_periodogram_a_p(
        p_seq = 1, a_p = 1, xlab = "Period", print = 1),
        "Assertion on 'print' failed")
})

test_that("plot_periodogram_q_p() | general test", {
    data <- dplyr::tibble(
        index = seq(as.POSIXct("2020-01-01"),
                    as.POSIXct("2020-01-02 05:59:59"),
                    by = "min"),
        x = rep(seq(1, 60), times = 30)) %>%
        tsibble::tsibble(index = index)

    per <- periodogram(data = data, col = "x", p_min = 1, p_max = 350,
                       print = FALSE)

    expect_s3_class(plot_periodogram_q_p(
        p_seq = per$p_seq, q_p = per$q_p, q_p_critical = per$q_p_critical,
        alpha = 0.05, xlab = "Period", print = TRUE),
        "ggplot")
})

test_that("plot_periodogram_q_p() | error test", {
    # checkmate::assert_numeric(p_seq, min.len = 1)
    expect_error(plot_periodogram_q_p(
        p_seq = "", q_p = 1, q_p_critical = 1, alpha = 0.1, xlab = "Period",
        print = FALSE),
        "Assertion on 'p_seq' failed")
    expect_error(plot_periodogram_q_p(
        p_seq = numeric(), q_p = 1, q_p_critical = 1, alpha = 0.1,
        xlab = "Period", print = FALSE),
        "Assertion on 'p_seq' failed")

    # checkmate::assert_numeric(q_p, min.len = 1)
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = "", q_p_critical = 1, alpha = 0.1, xlab = "Period",
        print = FALSE),
        "Assertion on 'q_p' failed")
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = numeric(), q_p_critical = 1, alpha = 0.1,
        xlab = "Period", print = FALSE),
        "Assertion on 'q_p' failed")

    # checkmate::assert_numeric(q_p_critical, min.len = 1)
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = "", alpha = 0.1, xlab = "Period",
        print = FALSE),
        "Assertion on 'q_p_critical' failed")
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = numeric(), alpha = 0.1,
        xlab = "Period", print = FALSE),
        "Assertion on 'q_p_critical' failed")

    # checkmate::assert_number(alpha, lower = 0.001, upper = 0.999)
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = 1, alpha = "", xlab = "Period",
        print = FALSE),
        "Assertion on 'alpha' failed")
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = 1, alpha = -1, xlab = "Period",
        print = FALSE),
        "Assertion on 'alpha' failed")
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = 1, alpha = 2, xlab = "Period",
        print = FALSE),
        "Assertion on 'alpha' failed")

    # checkmate::assert_string(xlab)
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = 1, alpha = 0.1, xlab = 1,
        print = FALSE),
        "Assertion on 'xlab' failed")

    # checkmate::assert_flag(print)
    expect_error(plot_periodogram_q_p(
        p_seq = 1, q_p = 1, q_p_critical = 1, alpha = 0.1, xlab = "Period",
        print = 1),
        "Assertion on 'print' failed")
})
