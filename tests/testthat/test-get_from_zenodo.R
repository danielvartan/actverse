test_that("get_from_zenodo() | General test", {
  mock_data <- list(files = dplyr::tibble(key = "test"))

  temp_path <- tempfile("mock")
  dir.create(temp_path)

  # if (is.null(file)) {
  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    get_metadata_from_zenodo = function(...) mock_data,
    get_data_from_zenodo = function(...) TRUE,
    check_zenodo_file_integrity = function(...) TRUE,
    path_abs = function(...) TRUE
  )

  get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = temp_path,
    file = NULL,
    parallel = FALSE,
    force = TRUE
  ) |>
    expect_equal(fs::path(character()))

  # if (is.null(file)) { ... } else {
  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    get_metadata_from_zenodo = function(...) mock_data,
    get_data_from_zenodo = function(...) TRUE,
    check_zenodo_file_integrity = function(...) TRUE,
    path_abs = function(...) TRUE
  )

  get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = ".",
    file = "test",
    parallel = FALSE,
    force = TRUE
  ) |>
    expect_equal(fs::path(character()))
})

test_that("get_from_zenodo() | Error test", {
  # checkmate::assert_string(doi, pattern = "^10\\.5281\\/zenodo")
  expect_error(
    get_from_zenodo(
      doi = 1, dir = ".", file = NULL, parallel = FALSE
    ),
    "Assertion on 'doi' failed"
  )

  expect_error(
    get_from_zenodo(
      doi = "10.1177/0748730402239679", dir = ".", file = NULL,
      parallel = FALSE
    ),
    "Assertion on 'doi' failed"
  )

  # checkmate::assert_directory_exists(dir)
  expect_error(
    get_from_zenodo(
      doi = "10.5281/zenodo.4898822", dir = 1, file = NULL,
      parallel = FALSE
    ),
    "Assertion on 'dir' failed"
  )

  # checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)
  get_from_zenodo(
    doi = "10.5281/zenodo.4898822",
    dir = ".",
    file = 1,
    parallel = FALSE
  ) |> expect_error("Assertion on 'file' failed")

  expect_error(
    get_from_zenodo(
      doi = "10.5281/zenodo.4898822",
      dir = ".",
      file = c("a", NA),
      parallel = FALSE
    ),
    "Assertion on 'file' failed",
  )

  # checkmate::assert_flag(parallel)
  expect_error(
    get_from_zenodo(
      doi = "10.5281/zenodo.4898822", dir = ".", file = NULL,
      parallel = 1
    ),
    "Assertion on 'parallel' failed"
  )
})

test_that("get_metadata_from_zenodo() | General test", {
  testthat::local_mocked_bindings(
    assert_internet = function(...) TRUE,
    curl_fetch_memory = function(...) list(content = "61"),
    raw_to_char = function(...) TRUE,
    from_json = function(...) TRUE
  )

  get_metadata_from_zenodo(doi = "10.5281/zenodo.4898822") |>
    expect_true() |>
    shush()
})

test_that("get_metadata_from_zenodo() | Error test", {
  # checkmate::assert_string(doi, pattern = "^10\\.5281\\/zenodo")
  expect_error(
    get_metadata_from_zenodo(doi = 1),
    "Assertion on 'doi' failed"
  )

  expect_error(
    get_metadata_from_zenodo(doi = "10.1177/0748730402239679"),
    "Assertion on 'doi' failed"
  )
})

test_that("get_data_from_zenodo() | General test", {
  testthat::local_mocked_bindings(
    curl_download = function(...) TRUE
  )

  get_data_from_zenodo(
    file_url = "http://test.com/test.txt",
    file_dest = "./TeSt.txt",
    parallel = FALSE
  ) |>
    expect_null() |>
    shush()

  testthat::local_mocked_bindings(
    make_cluster = function(...) TRUE,
    cluster_map = function(...) TRUE,
    curl_download = function(...) TRUE,
    stop_cluster = function(...) TRUE
  )

  get_data_from_zenodo(
    file_url = "http://test.com/test.txt",
    file_dest = "./TeSt.txt",
    parallel = FALSE
  ) |>
    expect_null() |>
    shush()
})

test_that("get_data_from_zenodo() | Error test", {
  # checkmate::assert_character(file_url, pattern = url_pattern)
  expect_error(
    get_data_from_zenodo(
      file_url = 1,
      file_dest = "./TeSt.txt",
      parallel = FALSE
    ),
    "Assertion on 'file_url' failed"
  )

  expect_error(
    get_data_from_zenodo(
      file_url = "a", file_dest = "./TeSt.txt", parallel = FALSE
    ),
    "Assertion on 'file_url' failed"
  )

  # checkmate::assert_character(file_dest, any.missing = FALSE)
  expect_error(
    get_data_from_zenodo(
      file_url = "http://test.com/test.txt",
      file_dest = 1, parallel = FALSE
    ),
    "Assertion on 'file_dest' failed"
  )

  expect_error(
    get_data_from_zenodo(
      file_url = "http://test.com/test.txt",
      file_dest = c("a", NA), parallel = FALSE
    ),
    "Assertion on 'file_dest' failed"
  )

  # checkmate::assert_directory(dirname(file_dest))
  expect_error(
    get_data_from_zenodo(
      file_url = "http://test.com/test.txt",
      file_dest = "./TeSTtTtT/test.txt", parallel = FALSE
    ),
    "Assertion on 'dirname\\(file_dest\\)' failed"
  )

  # checkmate::assert_flag(parallel)
  expect_error(
    get_data_from_zenodo(
      file_url = "http://test.com/test.txt",
      file_dest = "./TeSt.txt", parallel = 1
    ),
    "Assertion on 'parallel' failed"
  )
})

test_that("check_zenodo_file_integrity() | General test", {
  mock_file <- tempfile()
  writeLines("test", mock_file)
  # tools::md5sum(mock_file)

  expect_null(shush(check_zenodo_file_integrity(
    file_md5 = "md5:9f06243abcb89c70e0c331c61d871fa7",
    file_dest = mock_file
  )))

  shush(
    expect_message(check_zenodo_file_integrity(
      file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
      file_dest = mock_file
    ))
  )
})

test_that("check_zenodo_file_integrity() | Error test", {
  # checkmate::assert_character(file_md5)
  expect_error(
    check_zenodo_file_integrity(
      file_md5 = 1, file_dest = "./TeSt.txt"
    ),
    "Assertion on 'file_md5' failed"
  )

  # checkmate::assert_character(file_dest, any.missing = FALSE)
  expect_error(
    check_zenodo_file_integrity(
      file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec", file_dest = 1
    ),
    "Assertion on 'file_dest' failed"
  )

  expect_error(
    check_zenodo_file_integrity(
      file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
      file_dest = c("a", NA)
    ),
    "Assertion on 'file_dest' failed"
  )

  # checkmate::assert_directory(dirname(file_dest))
  expect_error(
    check_zenodo_file_integrity(
      file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
      file_dest = "./TeSTtTtT/test.txt"
    ),
    "Assertion on 'dirname\\(file_dest\\)' failed"
  )
})
