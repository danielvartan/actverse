test_that("*_any_missing() | general test", {
    expect_true(test_any_missing(NA))
    expect_false(test_any_missing(1))

    checkmate::expect_string(check_any_missing(NA),
                             pattern = "'NA' cannot have missing values")
    checkmate::expect_string(check_any_missing(NA, name = "x"),
                             pattern = "'x' cannot have missing values")
    expect_true(check_any_missing(1))

    expect_error(assert_any_missing(NA))
    expect_equal(assert_any_missing(1), 1)

    expect_message(warn_any_missing(NA))
    expect_true(warn_any_missing(1))
})

test_that("*_leq() | general test", {
    expect_true(test_leq(1, 1))
    expect_true(test_leq(1, 2))
    expect_false(test_leq(2, 1))

    checkmate::expect_string(check_leq(2, 1),
                             pattern = "'2' must be less or equal to '1'")
    checkmate::expect_string(check_leq(2, 1, name_x = "x", name_y = "y"),
                             pattern = "'x' must be less or equal to 'y'")
    expect_true(check_leq(1, 1))
    expect_true(check_leq(1, 2))

    expect_error(assert_leq(2, 1))
    expect_equal(assert_leq(1, 1), 1)
})

test_that("*_leq() | error test", {
    # checkmate::assert_number(x)
    expect_error(test_leq(NA, 1))
    expect_error(check_leq(NA, 1))

    # checkmate::assert_number(y)
    expect_error(test_leq(1, NA))
    expect_error(check_leq(1, NA))
})

test_that("assert_internet() | general test", {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) TRUE,
            {
                assert_internet()
            }
        )
    }

    expect_true(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            has_internet = function(...) FALSE,
            {
                assert_internet()
            }
        )
    }

    expect_error(mock())
})

test_that("assert_identical() | general test", {
    expect_error(assert_identical(1))
    expect_error(assert_identical(1, c(1, 1), type = "value"))
    expect_true(assert_identical(1, 1, type = "value"))

    expect_error(assert_identical(1, c(2, 2), type = "length"))
    expect_true(assert_identical(1, 2, type = "length"))

    expect_error(assert_identical(1, "a", type = "class"))
    expect_true(assert_identical(1, 3, type = "class"))

    expect_true(assert_identical(NULL, NULL, null.ok = TRUE))
    expect_error(assert_identical(1, NA, any.missing = FALSE))
    expect_error(assert_identical(NULL, NULL, null.ok = FALSE))
})

test_that("*_hms() | general test", {
    expect_true(test_hms(x = hms::hms(1)))
    expect_true(test_hms(x = c(hms::hms(1), NA), any.missing = TRUE))
    expect_true(test_hms(x = NULL, null.ok = TRUE))
    expect_false(test_hms(x = "a"))
    expect_false(test_hms(x = 1))
    expect_false(test_hms(x = lubridate::hours()))
    expect_false(test_hms(x = lubridate::dhours()))
    expect_false(test_hms(x = datasets::iris))

    expect_false(test_hms(
        x = c(lubridate::dhours(1), NA), any.missing = FALSE)
    )

    expect_false(test_hms(x = NULL, null.ok = FALSE))

    expect_false(test_hms(
        x = hms::hms(1), lower = hms::hms(2))
    )

    expect_false(test_hms(
        x = hms::hms(1), upper = hms::hms(0))
    )

    checkmate::expect_string(check_hms(
        x = c(1, NA), any.missing = FALSE
    ),
    "'c\\(1, NA\\)' cannot have missing values"
    )

    checkmate::expect_string(check_hms(
        x = NULL, null.ok = FALSE
    ),
    "'NULL' cannot be 'NULL'"
    )

    checkmate::expect_string(check_hms(
        x = hms::hms(1), lower = hms::hms(2)
    ),
    "Element 1 is not <= "
    )

    checkmate::expect_string(check_hms(
        x = hms::hms(1), upper = hms::hms(0)
    ),
    "Element 1 is not >= "
    )

    checkmate::expect_string(check_hms(
        x = c(1, 1)
    ),
    "Must be of type 'hms', not 'numeric'"
    )

    expect_true(check_hms(x = c(hms::hms(1), hms::hms(1))))
    expect_true(check_hms(x = NULL, null.ok = TRUE))

    expect_equal(assert_hms(
        x = c(hms::hms(1), hms::hms(1))
    ),
    c(hms::hms(1), hms::hms(1))
    )

    expect_error(assert_hms(
        x = c(1, 1)
    ),
    "Assertion on 'c\\(1, 1\\)' failed"
    )
})

test_that("*_hms() | error test", {
    # checkmate::assert_flag(any.missing)
    expect_error(test_hms(hms::hms(1), any.missing = 1))
    expect_error(check_hms(hms::hms(1), any.missing = 1))

    # checkmate::assert_flag(null.ok)
    expect_error(test_hms(hms::hms(1), null.ok = 1))
    expect_error(check_hms(hms::hms(1), null.ok = 1))
})

test_that("*_posixt() | general test", {
    expect_true(test_posixt(x = lubridate::as_datetime(1)))
    expect_true(test_posixt(x = as.POSIXlt(lubridate::as_datetime(1))))

    expect_true(test_posixt(
        x = c(lubridate::as_datetime(1), NA), any.missing = TRUE)
    )

    expect_true(test_posixt(x = NULL, null.ok = TRUE))
    expect_false(test_posixt(x = "a"))
    expect_false(test_posixt(x = 1))
    expect_false(test_posixt(x = lubridate::hours()))
    expect_false(test_posixt(x = hms::hms(1)))
    expect_false(test_posixt(x = datasets::iris))

    expect_false(test_posixt(
        x = c(lubridate::as_datetime(1), NA), any.missing = FALSE)
    )

    expect_false(test_posixt(x = NULL, null.ok = FALSE))

    expect_false(test_posixt(
        x = lubridate::as_datetime(1), lower = lubridate::as_datetime(2))
    )

    expect_false(test_posixt(
        x = lubridate::as_datetime(1), upper = lubridate::as_datetime(0))
    )

    checkmate::expect_string(check_posixt(
        x = c(1, NA), any.missing = FALSE
    ),
    "'c\\(1, NA\\)' cannot have missing values"
    )

    checkmate::expect_string(check_posixt(
        x = NULL, null.ok = FALSE
    ),
    "'NULL' cannot be 'NULL'"
    )

    checkmate::expect_string(check_posixt(
        x = lubridate::as_datetime(1), lower = lubridate::as_datetime(2)
    ),
    "Element 1 is not <= "
    )

    checkmate::expect_string(check_posixt(
        x = lubridate::as_datetime(1), upper = lubridate::as_datetime(0)
    ),
    "Element 1 is not >= "
    )

    checkmate::expect_string(check_posixt(
        x = c(1, 1)
    ),
    "Must be of type 'POSIXct' or 'POSIXlt', "
    )

    expect_true(check_posixt(
        x = c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
    )

    expect_true(check_posixt(x = NULL, null.ok = TRUE))

    expect_equal(assert_posixt(
        x = c(lubridate::as_datetime(1), lubridate::as_datetime(1))
    ),
    c(lubridate::as_datetime(1), lubridate::as_datetime(1))
    )

    expect_error(assert_posixt(
        x = c(1, 1)
    ),
    "Assertion on 'c\\(1, 1\\)' failed"
    )
})

test_that("*_posixt() | error test", {
    # checkmate::assert_flag(any.missing)
    expect_error(test_posixt(lubridate::as_datetime(1), any.missing = 1))
    expect_error(check_posixt(lubridate::as_datetime(1), any.missing = 1))

    # checkmate::assert_flag(null.ok)
    expect_error(test_posixt(lubridate::as_datetime(1), null.ok = 1))
    expect_error(check_posixt(lubridate::as_datetime(1), null.ok = 1))
})

test_that("*_interval() | general test", {
    int <- lubridate::interval(
        as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-02 00:00:00"))
    int_lower <- lubridate::interval(
        as.POSIXct("2020-01-01 01:00:00"), as.POSIXct("2020-01-02 00:00:00"))
    int_upper <- lubridate::interval(
        as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-01-02 01:00:00"))

    expect_true(test_interval(int))
    expect_true(test_interval(c(int, lubridate::as.interval(NA)),
                              any.missing = TRUE))
    expect_true(test_interval(NULL, null.ok = TRUE))
    expect_true(test_interval(int, lower = int_lower))
    expect_true(test_interval(int, upper = int_upper))
    expect_false(test_interval("a"))
    expect_false(test_interval(1))
    expect_false(test_interval(lubridate::hours()))
    expect_false(test_interval(hms::hms(1)))
    expect_false(test_interval(datasets::iris))
    expect_false(test_interval(c(int, NA), any.missing = FALSE))
    expect_false(test_interval(NULL, null.ok = FALSE))
    expect_false(test_interval(int, lower = int_upper))
    expect_false(test_interval(int, upper = int_lower))

    checkmate::expect_string(
        check_interval(c(1, NA), any.missing = FALSE),
        pattern = "'c\\(1, NA\\)' cannot have missing values"
        )
    checkmate::expect_string(check_interval(NULL, null.ok = FALSE),
                             pattern = "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_interval(int, lower = int_upper),
                             pattern = "Element 1 is not >= ")
    checkmate::expect_string(check_interval(int, upper = int_lower),
                             pattern = "Element 1 is not <= ")
    checkmate::expect_string(check_interval(c(1, 1)),
                             pattern = "Must be of type 'Interval'")
    expect_true(check_interval(c(int, int_lower)))
    expect_true(check_interval(NULL, null.ok = TRUE))

    expect_equal(assert_interval(c(int, int_lower)), c(int, int_lower))
    expect_error(assert_interval(c(1, 1)), "Assertion on 'c\\(1, 1\\)' failed")
})

test_that("*_interval() | error test", {
    # checkmate::assert_flag(any.missing)
    expect_error(test_interval(lubridate::as_datetime(1), any.missing = 1))
    expect_error(check_interval(lubridate::as_datetime(1), any.missing = 1))

    # checkmate::assert_flag(null.ok)
    expect_error(test_interval(lubridate::as_datetime(1), null.ok = 1))
    expect_error(check_interval(lubridate::as_datetime(1), null.ok = 1))
})

test_that("*_tsibble() | general test", {
    data <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2020-01-02"), by = "day"),
        x = seq_along(index)
        ) %>%
        tsibble::tsibble(index = index)

    expect_true(test_tsibble(data))
    expect_true(test_tsibble(NULL, null.ok = TRUE))
    expect_false(test_tsibble(1))
    expect_false(test_tsibble(data, min.rows = 3))
    expect_false(test_tsibble(data, min.cols = 3))
    expect_false(test_tsibble(NULL, null.ok = FALSE))

    expect_true(check_tsibble(data))
    expect_true(check_tsibble(NULL, null.ok = TRUE))
    checkmate::expect_string(check_tsibble(NULL),
                             pattern = "'NULL' cannot be 'NULL'")
    checkmate::expect_string(
        check_tsibble(1),
        pattern = "Must be of type 'tbl_ts' \\(tsibble\\), not 'numeric'"
        )
    checkmate::expect_string(
        check_tsibble(data, min.rows = 3),
        pattern = "Must have at least 3 rows, but has 2 rows"
    )
    checkmate::expect_string(
        check_tsibble(data, min.cols = 3),
        pattern = "Must have at least 3 cols, but has 2 cols"
    )

    expect_equal(assert_tsibble(data), data)
    expect_error(assert_tsibble(1), "Assertion on '1' failed")
})

test_that("*_tsibble() | error test", {
    data <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
        ) %>%
        tsibble::tsibble(index = index)

    # checkmate::assert_int(min.rows, lower = 1, null.ok = TRUE)
    expect_error(test_tsibble(x = data, min.rows = "", min.cols = 1,
                              null.ok = TRUE),
                 "Assertion on 'min.rows' failed")
    expect_error(check_tsibble(x = data, min.rows = "", min.cols = 1,
                               null.ok = TRUE),
                 "Assertion on 'min.rows' failed")

    # checkmate::assert_int(min.cols, lower = 1, null.ok = TRUE)
    expect_error(test_tsibble(x = data, min.rows = 1, min.cols = "",
                              null.ok = TRUE),
                 "Assertion on 'min.cols' failed")
    expect_error(check_tsibble(data, min.rows = 1, min.cols = "",
                               null.ok = TRUE),
                 "Assertion on 'min.cols' failed")

    # checkmate::assert_flag(null.ok)
    expect_error(test_tsibble(x = data, min.rows = 1, min.cols = 1,
                              null.ok = ""),
                 "Assertion on 'null.ok' failed")
    expect_error(check_tsibble(x = data, min.rows = 1, min.cols = 1,
                               null.ok = ""),
                 "Assertion on 'null.ok' failed")
})

test_that("*_index_class() | general test", {
    data <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
        ) %>%
        tsibble::tsibble(index = index)

    expect_true(test_index_class(data))
    expect_false(test_index_class(data, classes = "numeric"))

    expect_true(check_index_class(data))
    checkmate::expect_string(
        check_index_class(data, classes = "numeric"),
        pattern = "Must have an index of class 'numeric', not 'Date'"
    )

    expect_equal(assert_index_class(data), data)
    expect_error(assert_index_class(data, classes = "numeric"),
                 "Assertion on 'data' failed")
})

test_that("*_index_class() | error test", {
    data <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
        ) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(x)
    expect_error(test_index_class(x = 1, classes = "Date"),
                 "Assertion on 'x' failed")
    expect_error(check_index_class(x = 1, classes = "Date"),
                 "Assertion on 'x' failed")

    # checkmate::assert_character(classes)
    expect_error(test_index_class(x = data, classes = 1),
                 "Assertion on 'classes' failed")
    expect_error(check_index_class(x = data, classes = 1),
                 "Assertion on 'classes' failed")
})

test_that("*_regularity() | general test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(
        index = c(
            as.POSIXct(seq(60, 5400, by = 60), origin = lubridate::origin),
            as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
            as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
            as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
            as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
        ),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    expect_true(test_regularity(data_1))
    expect_false(test_regularity(data_2, strict = TRUE))
    expect_false(test_regularity(data_2, threshold = 0.9))

    expect_true(check_regularity(data_1))
    checkmate::expect_string(
        check_regularity(data_2, strict = TRUE),
        pattern = paste0("'data_2' must be strictly regular. ",
                         "See '\\?find_epoch' to learn more")
    )
    checkmate::expect_string(
        check_regularity(data_2, threshold = 0.9),
        pattern = paste0("'data_2' must have a regularity equal or greater ",
                         "than 90%. See '\\?find_epoch' to learn more")
    )

    expect_equal(assert_regularity(data_1), data_1)
    expect_error(assert_regularity(data_2), "Assertion on 'data_2' failed")

    expect_message(warn_regularity(data_2, strict = TRUE))
    expect_message(warn_regularity(data_2, threshold = 0.9))
})

test_that("*_regularity() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
        ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(x, min.rows = 2, min.cols = 2)
    expect_error(test_regularity(x = 1, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")
    expect_error(check_regularity(x = 1, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")
    expect_error(warn_regularity(x = 1, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")

    # assert_index_class(x)
    expect_error(test_regularity(x = data_2, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")
    expect_error(check_regularity(x = data_2, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")
    expect_error(warn_regularity(x = data_2, threshold = 1, strict = FALSE),
                 "Assertion on 'x' failed")

    # checkmate::assert_number(threshold, lower = 0.001, upper = 1)
    expect_error(test_regularity(x = data_1, threshold = -1, strict = FALSE),
                 "Assertion on 'threshold' failed")
    expect_error(check_regularity(x = data_1, threshold = -1, strict = FALSE),
                 "Assertion on 'threshold' failed")
    expect_error(warn_regularity(x = data_1, threshold = -1, strict = FALSE),
                 "Assertion on 'threshold' failed")

    # checkmate::assert_flag(strict)
    expect_error(test_regularity(x = data_1, threshold = 1, strict = 1),
                 "Assertion on 'strict' failed")
    expect_error(check_regularity(x = data_1, threshold = 1, strict = 1),
                 "Assertion on 'strict' failed")
    expect_error(warn_regularity(x = data_1, threshold = 1, strict = 1),
                 "Assertion on 'strict' failed")

})

test_that("*_clear_epoch() | general test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(
        index = c(
            as.POSIXct(seq(60, 5400, by = 60), origin = lubridate::origin),
            as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
            as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
            as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
            as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
        ),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    expect_true(test_clear_epoch(data_1))
    expect_false(test_clear_epoch(data_2, threshold = 0.9))

    expect_true(check_clear_epoch(data_1))
    checkmate::expect_string(
        check_clear_epoch(data_2, threshold = 0.9),
        pattern = paste0("'data_2' does not present a clear epoch/",
                         "periodicity. See '\\?find_epoch' to learn more")
    )

    expect_equal(assert_clear_epoch(data_1), data_1)
    expect_error(assert_clear_epoch(data_2), "Assertion on 'data_2' failed")
})

test_that("*_clear_epoch() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(x, min.rows = 2, min.cols = 2)
    expect_error(test_clear_epoch(x = 1, threshold = 1),
                 "Assertion on 'x' failed")
    expect_error(check_clear_epoch(x = 1, threshold = 1),
                 "Assertion on 'x' failed")

    # assert_index_class(x)
    expect_error(test_clear_epoch(x = data_2, threshold = 1),
                 "Assertion on 'x' failed")
    expect_error(check_clear_epoch(x = data_2, threshold = 1),
                 "Assertion on 'x' failed")

    # checkmate::assert_number(threshold, lower = 0.001, upper = 1)
    expect_error(test_clear_epoch(x = data_1, threshold = -1),
                 "Assertion on 'threshold' failed")
    expect_error(check_clear_epoch(x = data_1, threshold = -1),
                 "Assertion on 'threshold' failed")
})

test_that("*_epoch_compatibility() | general test", {
    data <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    expect_true(test_epoch_compatibility(data, unit = "years"))
    expect_false(test_epoch_compatibility(data, unit = "hours"))

    expect_true(check_epoch_compatibility(data, unit = "years"))
    checkmate::expect_string(
        check_epoch_compatibility(data, unit = "hours"),
        pattern = paste0("The epoch/periodicity present in 'data'don't allow ",
                         "to aggregate it in hours. See '\\?find_epoch' ",
                         "to learn more")
    )

    expect_equal(assert_epoch_compatibility(data, unit = "years"), data)
    expect_error(assert_epoch_compatibility(data, unit = "hours"),
                 "Assertion on 'data' failed")
})

test_that("*_epoch_compatibility() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    data_3 <- dplyr::tibble(
        index = c(
            as.POSIXct(seq(60, 100, by = 60), origin = lubridate::origin),
            as.POSIXct(seq(5430, 5490, by = 30), origin = lubridate::origin),
            as.POSIXct(seq(5505, 5520, by = 15), origin = lubridate::origin),
            as.POSIXct(seq(5530, 5540, by = 10), origin = lubridate::origin),
            as.POSIXct(seq(5545, 5555, by = 5), origin = lubridate::origin)
        ),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(x, min.rows = 2, min.cols = 2)
    expect_error(test_epoch_compatibility(x = 1, unit = "years"),
                 "Assertion on 'x' failed")
    expect_error(check_epoch_compatibility(x = 1, unit = "years"),
                 "Assertion on 'x' failed")

    # assert_index_class(x)
    expect_error(test_epoch_compatibility(x = data_2, unit = "years"),
                 "Assertion on 'x' failed")
    expect_error(check_epoch_compatibility(x = data_2, unit = "years"),
                 "Assertion on 'x' failed")

    # assert_clear_epoch(x, 0.7)
    expect_error(test_epoch_compatibility(x = data_3, unit = "years"),
                 "Assertion on 'x' failed")
    expect_error(check_epoch_compatibility(x = data_3, unit = "years"),
                 "Assertion on 'x' failed")

    # checkmate::assert_choice(unit, unit_choices)
    expect_error(test_epoch_compatibility(x = data_1, unit = ""),
                 "Assertion on 'unit' failed")
    expect_error(check_epoch_compatibility(x = data_1, unit = ""),
                 "Assertion on 'unit' failed")
})
