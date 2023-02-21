test_that("na_approx() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    expect_equal(na_approx(x, index, fill_na_tips = FALSE),
                 c(NA, 1, 5, 10, 7.5, 5, 10, 1, 5.5, 10, 1, 5, NA, NA))
    expect_equal(na_approx(x, index, fill_na_tips = TRUE),
                 c(1, 1, 5, 10, 7.5, 5, 10, 1, 5.5, 10, 1, 5, 5, 5))
})

test_that("na_approx() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_approx(x = "", index = index, fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_approx(x = as.numeric(NA), index = index,
                           fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_approx(x = c(as.numeric(NA), NA, NA), index = index,
                           fill_na_tips = TRUE),
                 "Assertion on 'x' failed")

    # assert_identical(x, index, type = "length")
    expect_error(na_approx(x = 1:2, index = index, fill_na_tips = TRUE),
                 "Assertion failed: 'x' and 'index' must have identical")

    # checkmate::assert_flag(fill_na_tips)
    expect_error(na_approx(x = x, index = index, fill_na_tips = 1),
                 "Assertion on 'fill_na_tips' failed")
})

test_that("na_locf() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    expect_equal(na_locf(x, fill_na_tips = FALSE),
                 c(NA, 1, 5, 10, 10, 5, 10, 1, 1, 10, 1, 5, 5, 5))
    expect_equal(na_locf(x, fill_na_tips = TRUE),
                 c(1, 1, 5, 10, 10, 5, 10, 1, 1, 10, 1, 5, 5, 5))
})

test_that("na_locf() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_locf(x = "", fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_locf(x = as.numeric(NA), fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_locf(x = c(as.numeric(NA), NA, NA), fill_na_tips = TRUE),
                 "Assertion on 'x' failed")

    # checkmate::assert_flag(fill_na_tips)
    expect_error(na_locf(x = x, fill_na_tips = 1),
                 "Assertion on 'fill_na_tips' failed")
})

test_that("na_overall_mean() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    expect_equal(
        na_overall_mean(x),
        c(5 + (1 / 3), 1, 5, 10, 5 + (1 / 3), 5, 10, 1, 5 + (1 / 3), 10, 1,
          5, 5 + (1 / 3), 5 + (1 / 3))
        )
})

test_that("na_overall_mean() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_overall_mean(x = ""),
                 "Assertion on 'x' failed")
    expect_error(na_overall_mean(x = as.numeric(NA)),
                 "Assertion on 'x' failed")
    expect_error(na_overall_mean(x = c(as.numeric(NA), NA, NA)),
                 "Assertion on 'x' failed")
})

test_that("na_overall_median() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    expect_equal(na_overall_median(x),
                 c(5, 1, 5, 10, 5, 5, 10, 1, 5, 10, 1, 5, 5, 5))
})

test_that("na_overall_median() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_overall_median(x = ""),
                 "Assertion on 'x' failed")
    expect_error(na_overall_median(x = as.numeric(NA)),
                 "Assertion on 'x' failed")
    expect_error(na_overall_median(x = c(as.numeric(NA), NA, NA)),
                 "Assertion on 'x' failed")
})

test_that("na_overall_mode() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    expect_message(na_overall_mode(x))
    expect_equal(suppressMessages((na_overall_mode(x))), x)

    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA, 5)

    expect_equal(na_overall_mode(x),
                 c(5, 1, 5, 10, 5, 5, 10, 1, 5, 10, 1, 5, 5, 5, 5))
})

test_that("na_spline() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    expect_equal(na_spline(x, index),
                 c(4.567728, 1, 5, 10, 6.589146, 5, 10, 1, 5.037198, 10, 1,
                   5, 42.905390, 131.216171))
})

test_that("na_spline() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_spline(x = "", index = index), "Assertion on 'x' failed")
    expect_error(na_spline(x = as.numeric(NA), index = index),
                 "Assertion on 'x' failed")
    expect_error(na_spline(x = c(as.numeric(NA), NA, NA), index = index),
                 "Assertion on 'x' failed")

    # assert_identical(x, index, type = "length")
    expect_error(na_spline(x = 1:2, index = index),
                 "Assertion failed: 'x' and 'index' must have identical")
})

test_that("na_weekly_mean() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    expect_equal(
        na_weekly_mean(x, index, fill_na_tips = TRUE, week_start = 1),
        c(5 + (1 / 3), 1, 5, 10, 5 + (1 / 3), 5, 10, 1, 5 + (1 / 3), 10, 1, 5,
          5, 5)
        )

    expect_equal(
        na_weekly_mean(x, index, fill_na_tips = FALSE, week_start = 1),
        c(5 + (1 / 3), 1, 5, 10, 5 + (1 / 3), 5, 10, 1, 5 + (1 / 3), 10, 1, 5,
          NA, NA)
        )

    expect_equal(
        na_weekly_mean(x, index, fill_na_tips = TRUE, week_start = 7),
        c(5 + (1 / 3), 1, 5, 10, 5.4, 5, 10, 1, 5.4, 10, 1, 5, 5, 5)
        )
})

test_that("na_weekly_mean() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_weekly_mean(x = "", index = index, fill_na_tips = TRUE,
                                week_start = 1),
                 "Assertion on 'x' failed")
    expect_error(na_weekly_mean(x = as.numeric(NA), index = index,
                                fill_na_tips = TRUE, week_start = 1),
                 "Assertion on 'x' failed")
    expect_error(na_weekly_mean(x = c(as.numeric(NA), NA, NA), index = index,
                                fill_na_tips = TRUE, week_start = 1),
                 "Assertion on 'x' failed")

    # assert_identical(x, index, type = "length")
    expect_error(na_weekly_mean(x = 1:2, index = index),
                 "Assertion failed: 'x' and 'index' must have identical")

    # checkmate::assert_flag(fill_na_tips)
    expect_error(na_weekly_mean(x = x, index = index, fill_na_tips = 1,
                                week_start = 1),
                 "Assertion on 'fill_na_tips' failed")

    # checkmate::assert_choice(week_start, c(1, 7))
    expect_error(na_weekly_mean(x = x, index = index, fill_na_tips = TRUE,
                                week_start = 2),
                 "Assertion on 'week_start' failed")
})

test_that("na_zero() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    expect_equal(na_zero(x),
                 c(0, 1, 5, 10, 0, 5, 10, 1, 0, 10, 1, 5, 0, 0))
})

test_that("na_zero() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_zero(x = ""), "Assertion on 'x' failed")
    expect_error(na_zero(x = as.numeric(NA)), "Assertion on 'x' failed")
    expect_error(na_zero(x = c(as.numeric(NA), NA, NA)),
                 "Assertion on 'x' failed")
})

test_that("na_plot() | general test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")
    intp <- na_overall_mean(x)

    expect_s3_class(na_plot(x, index), "ggplot")
    expect_s3_class(na_plot(x, index, intp = intp), "ggplot")
})

test_that("na_plot() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    index <- seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day")
    intp <- na_overall_mean(x)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_plot(x = "", index = index, intp = intp, print = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_plot(x = as.numeric(NA), index = index, intp = intp,
                         print = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_plot(x = c(as.numeric(NA), NA, NA), index = index,
                         intp = intp,  print = TRUE),
                 "Assertion on 'x' failed")

    # checkmate::assert_numeric(intp, min.len = 1, all.missing = FALSE,
    #                           null.ok = TRUE)
    expect_error(na_plot(x = x, index = index, intp = "", print = TRUE),
                 "Assertion on 'intp' failed")
    expect_error(na_plot(x = x, index = index, intp = as.numeric(NA),
                         print = TRUE),
                 "Assertion on 'intp' failed")
    expect_error(na_plot(x = x, index = index, intp = c(as.numeric(NA), NA, NA),
                         print = TRUE),
                 "Assertion on 'intp' failed")

    # assert_identical(x, index, type = "length")
    expect_error(na_plot(x = 1:2, index = index, intp = NULL, print = TRUE),
                 "Assertion failed: 'x' and 'index' must have identical")

    # if (!is.null(intp)) assert_identical(x, index, intp, type = "length")
    expect_error(na_plot(x = x, index = index, intp = 1:2, print = TRUE),
                 "Assertion failed: 'x', 'index'. and 'intp' must have")

    # checkmate::assert_flag(print)
    expect_error(na_plot(x = x, index = index, intp = NULL, print = 1),
                 "Assertion on 'print' failed")
})

test_that("na_tip_correction() | general test", {
    x <- c(NA, 1, 5, 10, 1, 5, 10, 1, 5, 10, 1, 5, 10, NA)

    intp_1 <- c(NA, 1, 5, 10, 1, 5, 10, 1, 5, 10, 1, 5, 10, NA)

    expect_equal(na_tip_correction(x = x, intp = intp_1, fill_na_tips = TRUE),
                 c(1, 1, 5, 10, 1, 5, 10, 1, 5, 10, 1, 5, 10, 10))

    intp_2 <- 1:12

    expect_equal(na_tip_correction(x = x, intp = intp_2, fill_na_tips = FALSE),
                 c(NA, 1:12, NA))
})

test_that("na_tip_correction() | error test", {
    x <- c(NA, 1, 5, 10, NA, 5, 10, 1, NA, 10, 1, 5, NA, NA)
    intp <- na_overall_mean(x)

    # checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    expect_error(na_tip_correction(x = "", intp = intp, fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_tip_correction(x = as.numeric(NA), intp = intp,
                                   fill_na_tips = TRUE),
                 "Assertion on 'x' failed")
    expect_error(na_tip_correction(x = c(as.numeric(NA), NA, NA), intp = intp,
                         fill_na_tips = TRUE),
                 "Assertion on 'x' failed")

    # checkmate::assert_numeric(intp, min.len = 1, all.missing = FALSE,
    #                           null.ok = TRUE)
    expect_error(na_tip_correction(x = x, intp = "", fill_na_tips = TRUE),
                 "Assertion on 'intp' failed")
    expect_error(na_tip_correction(x = x, intp = as.numeric(NA),
                                   fill_na_tips = TRUE),
                 "Assertion on 'intp' failed")
    expect_error(na_tip_correction(x = x, intp = c(as.numeric(NA), NA, NA),
                         fill_na_tips = TRUE),
                 "Assertion on 'intp' failed")

    # checkmate::assert_flag(fill_na_tips)
    expect_error(na_tip_correction(x = x, intp = intp, fill_na_tips = 1),
                 "Assertion on 'fill_na_tips' failed")
})

test_that("na_example_data() | general test", {
    expect_true(is.list(na_example_data()))
    expect_equal(names(na_example_data()), c("index", "x"))
})
