test_that("is_interactive() | general test", {
    expect_equal(is_interactive(), interactive())
})

test_that("require_namespace() | general test", {
    expect_equal(require_namespace(
        "base"
    ),
    requireNamespace("base", quietly = TRUE)
    )
})
