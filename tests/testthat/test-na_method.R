test_that("na_approx() | general test", {

})

test_that("na_approx() | error test", {

})

test_that("na_locf() | general test", {

})

test_that("na_approx() | error test", {

})

test_that("na_overall_mean() | general test", {

})

test_that("na_overall_mean() | error test", {

})

test_that("na_overall_median() | general test", {

})

test_that("na_overall_median() | error test", {

})

test_that("na_overall_mode() | general test", {

})

test_that("na_overall_mode() | error test", {

})

test_that("na_spline() | general test", {

})

test_that("na_spline() | error test", {

})

test_that("na_weekly_mean() | general test", {

})

test_that("na_weekly_mean() | error test", {

})

test_that("na_zero() | general test", {

})

test_that("na_zero() | error test", {

})

test_that("na_plot() | general test", {

})

test_that("na_plot() | error test", {

})

test_that("na_tip_correction() | general test", {

})

test_that("na_tip_correction() | error test", {

})

test_that("na_example_data() | general test", {
    expect_true(is.list(na_example_data()))
    expect_equal(names(na_example_data()), c("index", "x"))
})
