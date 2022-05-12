test_that("backtick_() | general test", {
    expect_equal(backtick_(x = "a"), paste0("`", "a", "`"))
    expect_equal(backtick_(x = 1), paste0("`", 1, "`"))
})

test_that("single_quote_() | general test", {
    expect_equal(single_quote_(x = "a"), paste0("'", "a", "'"))
    expect_equal(single_quote_(x = 1), paste0("'", 1, "'"))
})

test_that("double_quote_() | general test", {
    expect_equal(double_quote_(x = "a"), paste0("\"", "a", "\""))
    expect_equal(double_quote_(x = 1), paste0("\"", 1, "\""))
})

test_that("class_collapse() | general test", {
    expect_equal(class_collapse(
        x = "test"
    ),
    single_quote_(paste0(class("test"), collapse = "/"))
    )

    expect_equal(class_collapse(
        x = 1
    ),
    single_quote_(paste0(class(1), collapse = "/"))
    )

    expect_equal(class_collapse(
        x = lubridate::dhours()
    ),
    single_quote_(paste0(class(lubridate::dhours()), collapse = "/"))
    )
})

test_that("flat_posixt_date() | general test", {
    expect_equal(flat_posixt_date(
        posixt = as.POSIXct("2020-01-01 05:55:55", tz = "UTC"),
        base = as.Date("1975-01-01")
    ),
    lubridate::as_datetime("1975-01-01 05:55:55", tz = "UTC")
    )
})

test_that("flat_posixt_date() | error test", {
    # assert_posixt(posixt, null.ok = FALSE)
    expect_error(flat_posixt_date(posixt = "", base = as.Date("1970-01-01")),
                 "Assertion on 'posixt' failed")

    # checkmate::assert_date(base, len = 1, any.missing = FALSE)
    expect_error(
        flat_posixt_date(posixt = Sys.time(), base = ""),
        "Assertion on 'base' failed"
    )

    expect_error(flat_posixt_date(
        posixt = Sys.time(), base = c(Sys.Date(), NA)
    ),
    "Assertion on 'base' failed"
    )
})

test_that("flat_posixt_hour() | general test", {
    expect_equal(flat_posixt_hour(
        posixt = as.POSIXct("2020-01-01 05:55:55", tz = "UTC"),
        base = hms::parse_hms("00:01:00")
    ),
    lubridate::as_datetime("2020-01-01 00:01:00", tz = "UTC")
    )
})

test_that("flat_posixt_hour() | error test", {
    # assert_posixt(posixt, null.ok = FALSE)
    expect_error(flat_posixt_hour(
        posixt = "", base = hms::parse_hms("00:00:00")
    ),
    "Assertion on 'posixt' failed"
    )

    # assert_hms(base, any.missing = FALSE)
    expect_error(
        flat_posixt_hour(posixt = Sys.time(), base = ""),
        "Assertion on 'base' failed"
    )

    expect_error(flat_posixt_hour(
        posixt = Sys.time(), base = c(hms::hms(0), NA)
    ),
    "Assertion on 'base' failed"
    )
})

test_that("find_absolute_path() | general test", {
    expect_equal(
        find_absolute_path(list.files()[1]),
        file.path(getwd(), list.files()[1])
    )
})

test_that("paste_collapse() | general test", {
    expect_equal(paste_collapse(1), 1)
    expect_equal(paste_collapse(1:2), "12")
    expect_equal(paste_collapse(1:2, sep = " "), "1 2")
    expect_equal(paste_collapse(1:2, sep = " ", last = ""), "12")
    expect_equal(paste_collapse(1:2, sep = " ", last = " or "), "1 or 2")
    expect_equal(paste_collapse(1:3, sep = ", ", last = ", or "), "1, 2, or 3")
})

test_that("paste_collapse() | error test", {
    # checkmate::assert_string(sep)
    expect_error(paste_collapse(x = 1:2, sep = 1, last = ""),
                 "Assertion on 'sep' failed")

    # checkmate::assert_string(last)
    expect_error(paste_collapse(x = 1:2, sep = "", last = 1),
                 "Assertion on 'last' failed")
})

test_that("inline_collapse() | general test", {
    expect_equal(inline_collapse(1:2), "'1' and '2'")
    expect_equal(inline_collapse(1:3), "'1', '2', and '3'")
    expect_equal(inline_collapse(1:3, last = "or"), "'1', '2', or '3'")
    expect_equal(inline_collapse(1:3, single_quote = FALSE), "1, 2, and 3")
    expect_equal(inline_collapse(1:3, serial_comma  = FALSE),
                 "'1', '2' and '3'")

})

test_that("inline_collapse() | error test", {
    # checkmate::assert_string(last)
    expect_error(inline_collapse(
        x = 1:3, last = 1, single_quote = TRUE, serial_comma = TRUE),
                 "Assertion on 'last' failed")
    # checkmate::assert_flag(single_quote)
    expect_error(inline_collapse(
        x = 1:3, last = "", single_quote = 1, serial_comma = TRUE),
        "Assertion on 'single_quote' failed")
    # checkmate::assert_flag(serial_comma)
    expect_error(inline_collapse(
        x = 1:3, last = "", single_quote = TRUE, serial_comma = 1),
        "Assertion on 'serial_comma' failed")
})

test_that("head_() | general test", {
    expect_equal(head_(list(a = 1:10, b = 1:10), n = 5),
                 list(a = 1:10, b = 1:10)[seq_len(2)])
    expect_equal(head_(data.frame(a = 1:10, b = 1:10), n = 15),
                 data.frame(a = 1:10, b = 1:10)[seq_len(10), ])
    expect_equal(head_(1:10, n = 15), c(1:10)[seq_len(10)])
})

test_that("head_() | error test", {
    # checkmate::assert_int(n, lower = 1)
    expect_error(head_(1:10, n = 0), "Assertion on 'n' failed")
})

test_that("period_() | general test", {
    expect_equal(period_(1, unit = "seconds"), lubridate::seconds())
    expect_equal(period_(1, unit = "microseconds"), lubridate::microseconds())
    expect_equal(period_(1, unit = "milliseconds"), lubridate::milliseconds())
    expect_equal(period_(1, unit = "quarters"), lubridate::period(3, "months"))
})

test_that("period_() | error test", {
    # checkmate::assert_number(num)
    expect_error(period_(""), "Assertion on 'num' failed")

    # checkmate::assert_choice(unit, unit_choices)
    expect_error(period_(1, unit = ""), "Assertion on 'unit' failed")
})

test_that("string_to_period() | general test", {
    expect_equal(string_to_period("microsecond"), lubridate::dmicroseconds())
    expect_equal(string_to_period("millisecond"), lubridate::dmilliseconds())
    expect_equal(string_to_period("second"), lubridate::duration("seconds"))
    expect_equal(string_to_period("minute"), lubridate::duration("minute"))
    expect_equal(string_to_period("hour"), lubridate::duration("hour"))
    expect_equal(string_to_period("week"), lubridate::duration("week"))
    expect_equal(string_to_period("day"), lubridate::duration("day"))

    expect_equal(string_to_period("month", irregularity = "min"),
                 lubridate::ddays(28))
    expect_equal(string_to_period("month", irregularity = "mean"),
                 lubridate::dmonths())
    expect_equal(string_to_period("month", irregularity = "max"),
                 lubridate::ddays(31))

    expect_equal(string_to_period("quarter", irregularity = "min"),
                 lubridate::ddays(28) + (lubridate::ddays(30) * 2))
    expect_equal(string_to_period("quarter", irregularity = "mean"),
                 lubridate::dmonths(3))
    expect_equal(string_to_period("quarter", irregularity = "max"),
                 lubridate::ddays(31) * 3)

    expect_equal(string_to_period("year", irregularity = "min"),
                 lubridate::ddays(365))
    expect_equal(string_to_period("year", irregularity = "mean"),
                 lubridate::dyears())
    expect_equal(string_to_period("year", irregularity = "max"),
                 lubridate::ddays(366))
})

test_that("string_to_period() | error test", {
    # checkmate::assert_choice(string, string_choices)
    expect_error(string_to_period(""), "Assertion on 'string' failed")

    # checkmate::assert_choice(irregularity, irregularity_choices)
    expect_error(string_to_period("seconds", irregularity = ""),
                 "Assertion on 'irregularity' failed")
})

test_that("period_to_string() | general test", {
    expect_equal(period_to_string(lubridate::dmicroseconds()), "microseconds")
    expect_equal(period_to_string(lubridate::dmilliseconds()), "milliseconds")
    expect_equal(period_to_string(lubridate::dseconds()), "seconds")
    expect_equal(period_to_string(lubridate::dminutes()), "minutes")
    expect_equal(period_to_string(lubridate::dhours()), "hours")
    expect_equal(period_to_string(lubridate::ddays()), "days")
    expect_equal(period_to_string(lubridate::dweeks()), "weeks")
    expect_equal(period_to_string(15), as.character(NA))
})

test_that("period_to_string() | general test", {
    # checkmate::assert_number(period)
    expect_error(period_to_string(""), "Assertion on 'period' failed")
})

test_that("get_names() | general test", {
    expect_equal(get_names(x, y, z), noquote(c("x", "y", "z")))
})

test_that("require_pkg() | general test", {
    expect_null(require_pkg("base"))
    expect_error(require_pkg("test65464564"))
    expect_error(require_pkg("test1654654", "test265464564"))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            {require_pkg("test")}
        )
    }

    expect_null(mock())
})

test_that("require_pkg() | error test", {
    # lapply(out, checkmate::assert_string,
    #        pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")
    expect_error(require_pkg(1), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg(".test"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test."), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tes_t"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tést"), "Assertion on 'X\\[\\[i\\]\\]' failed")

    # (!identical(unique(unlist(out)), unlist(out)))
    expect_error(require_pkg(
        "test", "test"
    ),
    "'...' cannot have duplicated values."
    )
})

test_that("rm_na() | general test", {
    expect_equal(rm_na(c(NA, 1)), 1)
})

test_that("shush() | general test", {
    expect_equal(shush(x = "a", quiet = FALSE), "a")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(x = test(), quiet = TRUE), "test")
    expect_warning(shush(x = test(), quiet = FALSE), "test")
})

