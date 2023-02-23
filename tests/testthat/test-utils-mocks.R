test_that("cluster_map() | general test", {
    x <- make_cluster(1)
    object <- cluster_map(x, function(x) TRUE, 1)
    stop_cluster(x)

    checkmate::expect_list(object)
})

test_that("make_cluster() | general test", {
    object <- make_cluster(1)
    stop_cluster(object)

    checkmate::expect_class(object, "cluster")
})

test_that("stop_cluster() | general test", {
    x <- make_cluster(1)

    expect_null(stop_cluster(x))
})

test_that("curl_download() | general test", {
    if (curl::has_internet()) {
        checkmate::expect_file(
            curl_download(
                url = "https://api.github.com/users/giperbio",
                destfile = tempfile()
            )
        )
    }
})

test_that("curl_fetch_memory() | general test", {
    if (curl::has_internet()) {
        checkmate::expect_list(
            curl_fetch_memory(
                url = "https://api.github.com/users/giperbio"
                )
            )
    }
})

test_that("has_internet() | general test", {
    expect_equal(has_internet(), curl::has_internet())
})

test_that("is_interactive() | general test", {
    expect_equal(is_interactive(), interactive())
})

test_that("from_json() | general test", {
    tmp <- tempfile()
    jsonlite::write_json(acttrust, tmp)

    expect_equal(
        from_json(readLines(tmp))[["pim"]][1],
        acttrust[["pim"]][1]
    )
})

test_that("read_json() | general test", {
    tmp <- tempfile()
    jsonlite::write_json(acttrust, tmp)

    expect_equal(
        read_json(tmp)[[1]][["pim"]],
        acttrust[["pim"]][1]
    )
})

test_that("raw_to_char() | general test", {
    expect_equal(raw_to_char(charToRaw("a")), "a")
})

test_that("require_namespace() | general test", {
    expect_equal(
        require_namespace("base"),
        requireNamespace("base", quietly = TRUE)
    )
})
