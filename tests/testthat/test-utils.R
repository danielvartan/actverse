test_that("single_quote_() | general test", {
    expect_equal(single_quote_("a"), paste0("'", "a", "'"))
    expect_equal(single_quote_(1), paste0("'", 1, "'"))
})

test_that("double_quote_() | general test", {
    expect_equal(double_quote_("a"), paste0("\"", "a", "\""))
    expect_equal(double_quote_(1), paste0("\"", 1, "\""))
})

test_that("backtick_() | general test", {
    expect_equal(backtick_("a"), paste0("`", "a", "`"))
    expect_equal(backtick_(1), paste0("`", 1, "`"))
})
