test_that("na_as() | general test", {
    ## Nonexistent method error
    ## The error message may change depending on the user's 'locale' settings.
    expect_error(na_as(list(NA)))
})

test_that("na_as.logical() | general test", {
    expect_equal(na_as(logical()),as.logical(NA))
})

test_that("na_as.character() | general test", {
    expect_equal(na_as(character()),as.character(NA))
})

test_that("na_as.integer() | general test", {
    expect_equal(na_as(integer()),as.integer(NA))
})

test_that("na_as.numeric() | general test", {
    expect_equal(na_as(numeric()),as.numeric(NA))
})

test_that("na_as.Duration() | general test", {
    expect_equal(na_as(lubridate::duration()), lubridate::as.duration(NA))
})

test_that("na_as.Period() | general test", {
    expect_equal(na_as(lubridate::period()), lubridate::as.period(NA))
})

test_that("na_as.difftime() | general test", {
    expect_equal(na_as(as.difftime(1, units = "secs")),
                 as.difftime(as.numeric(NA), units = "secs"))
})

test_that("na_as.hms() | general test", {
    expect_equal(na_as(hms::hms()), hms::as_hms(NA))
})

test_that("na_as.Date() | general test", {
    expect_equal(na_as(as.Date("1970-01-01")), as.Date(NA))
})

test_that("na_as.POSIXct() | general test", {
    expect_equal(na_as(as.POSIXct(1, tz = "UTC", origin = lubridate::origin)),
                 as.POSIXct(NA, tz = "UTC"))

})

test_that("na_as.POSIXlt() | general test", {
    expect_equal(na_as(as.POSIXlt(1, tz = "UTC", origin = lubridate::origin)),
                 as.POSIXlt(NA, tz = "UTC"))
})

test_that("na_as.Interval() | general test", {
    expect_equal(na_as(lubridate::as.interval(lubridate::dhours(1),
                                              lubridate::ymd("2020-01-01"))),
                 lubridate::interval(NA, NA, tz = "UTC"))
})
