test_that("label_jump() | general test", {
    expect_equal(label_jump( x = 0:5, type = "even"), c(0, 2, 4))
    expect_equal(label_jump( x = 0:5, type = "odd"), c(1, 3, 5))
})

test_that("label_jump() | error test", {
    # checkmate::assert_atomic(x)
    expect_error(label_jump(
        x = list(), type = "even"
    ),
    "Assertion on 'x' failed"
    )

    # checkmate::assert_choice(type, c("even", "odd"))
    expect_error(
        label_jump(x = 1, type = ""),
        "Assertion on 'type' failed"
    )
})
