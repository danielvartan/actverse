test_that("has_internet() | general test", {
    expect_equal(has_internet(), curl::has_internet())
})


test_that("is_interactive() | general test", {
    expect_equal(is_interactive(), interactive())
})

test_that("read_json() | general test", {
    tmp <- tempfile()
    jsonlite::write_json(acttrust, tmp)

    expect_equal(
        read_json(tmp)[[1]][["pim"]],
        acttrust[["pim"]][1]
    )
})

test_that("require_namespace() | general test", {
    expect_equal(
        require_namespace("base"),
        requireNamespace("base", quietly = TRUE)
    )
})
