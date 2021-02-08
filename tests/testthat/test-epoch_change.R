test_that("epoch()", {
    object <- epoch(test_log)
    expect_equal(object, 60)
})
