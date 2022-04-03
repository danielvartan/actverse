# Sort tests by type or use the alphabetical order.

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

test_that("*_posixt() | general test", {
    expect_true(test_posixt(lubridate::as_datetime(1)))
    expect_true(test_posixt(as.POSIXlt(lubridate::as_datetime(1))))
    expect_true(test_posixt(c(lubridate::as_datetime(1), NA),
                            any.missing = TRUE))
    expect_true(test_posixt(NULL, null.ok = TRUE))
    expect_false(test_posixt("a"))
    expect_false(test_posixt(1))
    expect_false(test_posixt(lubridate::hours()))
    expect_false(test_posixt(hms::hms(1)))
    expect_false(test_posixt(datasets::iris))
    expect_false(test_posixt(c(lubridate::as_datetime(1), NA),
                             any.missing = FALSE))
    expect_false(test_posixt(NULL, null.ok = FALSE))
    expect_false(test_posixt(lubridate::as_datetime(1),
                             lower = lubridate::as_datetime(2)))
    expect_false(test_posixt(lubridate::as_datetime(1),
                             upper = lubridate::as_datetime(0)))

    checkmate::expect_string(check_posixt(c(1, NA), any.missing = FALSE),
                             "'c\\(1, NA\\)' cannot have missing values")
    checkmate::expect_string(check_posixt(NULL, null.ok = FALSE),
                             "'NULL' cannot be 'NULL'")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          lower = lubridate::as_datetime(2)),
                             "Element 1 is not <= ")
    checkmate::expect_string(check_posixt(lubridate::as_datetime(1),
                                          upper = lubridate::as_datetime(0)),
                             "Element 1 is not >= ")
    checkmate::expect_string(check_posixt(c(1, 1)),
                             "Must be of type 'POSIXct' or 'POSIXlt', ")
    expect_true(check_posixt(c(lubridate::as_datetime(1),
                               lubridate::as_datetime(1))))
    expect_true(check_posixt(NULL, null.ok = TRUE))

    expect_equal(assert_posixt(c(lubridate::as_datetime(1),
                                 lubridate::as_datetime(1))),
                 c(lubridate::as_datetime(1), lubridate::as_datetime(1)))
    expect_error(assert_posixt(c(1, 1)),
                 "Assertion on 'c\\(1, 1\\)' failed")
})
