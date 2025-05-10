test_that("cluster_map() | general test", {
  x <- make_cluster(1)
  object <- cluster_map(x, function(x) TRUE, 1)
  stop_cluster(x)

  object |> checkmate::expect_list()
})

test_that("make_cluster() | general test", {
  object <- make_cluster(1)
  stop_cluster(object)

  object |> checkmate::expect_class("cluster")
})

test_that("stop_cluster() | general test", {
  1 |>
    make_cluster() |>
    stop_cluster() |>
    expect_null()
})

test_that("curl_download() | general test", {
  url <- paste0(
    "https://api.stackexchange.com/2.2/answers?",
    "order=desc&sort=activity&site=stackoverflow"
  )

  if (curl::has_internet()) {
    curl_download(url = url, destfile = tempfile()) |>
      checkmate::expect_file()
  }
})

test_that("curl_fetch_memory() | general test", {
  url <- paste0(
    "https://api.stackexchange.com/2.2/answers?",
    "order=desc&sort=activity&site=stackoverflow"
  )

  if (curl::has_internet()) {
    curl_fetch_memory(url = url) |> checkmate::expect_list()
  }
})

test_that("from_json() | general test", {
  tmp <- tempfile()
  jsonlite::write_json(acttrust, tmp)

  tmp |>
    readLines() |>
    from_json() |>
    magrittr::extract2("pim") |>
    magrittr::extract(1) |>
    expect_equal(
      acttrust |>
        magrittr::extract2("pim") |>
        magrittr::extract(1)
    )
})

test_that("read_json() | general test", {
  tmp <- tempfile()
  jsonlite::write_json(acttrust, tmp)

  tmp |>
    read_json() |>
    magrittr::extract2(1) |>
    magrittr::extract2("pim") |>
    expect_equal(
      acttrust |>
        magrittr::extract2("pim") |>
        magrittr::extract(1)
    )
})

test_that("raw_to_char() | general test", {
  charToRaw("a") |>
    raw_to_char() |>
    expect_equal("a")
})
