# Sort tests by type or use the alphabetical order.

test_that("require_pkg() | general test", {
    expect_null(require_pkg("base"))
    expect_error(require_pkg("test"))
    expect_error(require_pkg("test1", "test2"))

    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # require_namespace <- actverse:::require_namespace

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            require_pkg("test"))
    }

    # mock()
    expect_null(mock())
})

test_that("require_pkg() | error test", {
    expect_error(require_pkg(1), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg(".test"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test."), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tes_t"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tést"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test", "test"),
                 "'...' cannot have duplicated values.")
})

test_that("shush() | general test", {
    expect_equal(shush("a", quiet = FALSE), "a")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(test(), quiet = TRUE), "test")
    expect_warning(shush(test(), quiet = FALSE), "test")
})
