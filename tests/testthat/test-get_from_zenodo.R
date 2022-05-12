test_that("get_from_zenodo() | general test", {
    mock_data <- list(
        files = dplyr::tibble(
            key = "test"
        )
    )

    # if (is.null(file)) {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            assert_internet = function(...) TRUE,
            get_metadata_from_zenodo = function(...) mock_data,
            get_data_from_zenodo = function(...) TRUE,
            check_zenodo_file_integrity = function(...) TRUE,
            find_absolute_path = function(...) TRUE,
            {get_from_zenodo(
                doi = "10.5281/zenodo.4898822", path = ".", file = NULL,
                parallel = FALSE
            )}
        )
    }

    expect_true(mock())

    # if (is.null(file)) { ... } else {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            assert_internet = function(...) TRUE,
            get_metadata_from_zenodo = function(...) mock_data,
            get_data_from_zenodo = function(...) TRUE,
            check_zenodo_file_integrity = function(...) TRUE,
            find_absolute_path = function(...) TRUE,
            {get_from_zenodo(
                doi = "10.5281/zenodo.4898822", path = ".", file = "test",
                parallel = FALSE
            )}
        )
    }

    expect_true(mock())
})

test_that("get_from_zenodo() | error test", {
    # checkmate::assert_string(doi, pattern = "^10\\.5281\\/zenodo")
    expect_error(get_from_zenodo(
        doi = 1, path = ".", file = NULL, parallel = FALSE
    ),
    "Assertion on 'doi' failed"
    )

    expect_error(get_from_zenodo(
        doi = "10.1177/0748730402239679", path = ".", file = NULL,
        parallel = FALSE
    ),
    "Assertion on 'doi' failed"
    )

    # checkmate::assert_directory_exists(path)
    expect_error(get_from_zenodo(
        doi = "10.5281/zenodo.4898822", path = 1, file = NULL,
        parallel = FALSE
    ),
    "Assertion on 'path' failed"
    )

    # checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)
    expect_error(get_from_zenodo(
        doi = "10.5281/zenodo.4898822", path = ".", file = 1, parallel = FALSE
    ),
    "Assertion on 'file' failed",
    )

    expect_error(get_from_zenodo(
        doi = "10.5281/zenodo.4898822", path = ".", file = c("a", NA),
        parallel = FALSE
    ),
    "Assertion on 'file' failed",
    )

    # checkmate::assert_flag(parallel)
    expect_error(get_from_zenodo(
        doi = "10.5281/zenodo.4898822", path = ".", file = NULL,
        parallel = 1
    ),
    "Assertion on 'parallel' failed"
    )
})

test_that("get_metadata_from_zenodo() | general test", {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            assert_internet = function(...) TRUE,
            curl_fetch_memory = function(...) list(content = "61"),
            raw_to_char = function(...) TRUE,
            from_json = function(...) TRUE,
            {shush(get_metadata_from_zenodo(doi = "10.5281/zenodo.4898822"))}
        )
    }

    expect_true(mock())
})

test_that("get_metadata_from_zenodo() | error test", {
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

test_that("get_data_from_zenodo() | general test", {
    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            curl_download = function(...) TRUE,
            {shush(get_data_from_zenodo(
                file_url = "http://test.com/test.txt",
                file_dest = "./TeSt.txt", parallel = FALSE
                ))}
        )
    }

    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            make_cluster = function(...) TRUE,
            cluster_map = function(...) TRUE,
            curl_download = function(...) TRUE,
            stop_cluster = function(...) TRUE,
            {shush(get_data_from_zenodo(
                file_url = "http://test.com/test.txt",
                file_dest = "./TeSt.txt", parallel = TRUE
            ))}
        )
    }

    expect_null(mock())
})

test_that("get_data_from_zenodo() | error test", {
    # checkmate::assert_character(file_url, pattern = url_pattern)
    expect_error(get_data_from_zenodo(
        file_url = 1, file_dest = "./TeSt.txt", parallel = FALSE
    ),
    "Assertion on 'file_url' failed"
    )

    expect_error(get_data_from_zenodo(
        file_url = "a",  file_dest = "./TeSt.txt", parallel = FALSE
    ),
    "Assertion on 'file_url' failed"
    )

    # checkmate::assert_character(file_dest, any.missing = FALSE)
    expect_error(get_data_from_zenodo(
        file_url = "http://test.com/test.txt",
        file_dest = 1, parallel = FALSE
    ),
    "Assertion on 'file_dest' failed"
    )

    expect_error(get_data_from_zenodo(
        file_url = "http://test.com/test.txt",
        file_dest = c("a", NA), parallel = FALSE
    ),
    "Assertion on 'file_dest' failed"
    )

    # checkmate::assert_directory(dirname(file_dest))
    expect_error(get_data_from_zenodo(
        file_url = "http://test.com/test.txt",
        file_dest = "./TeSTtTtT/test.txt", parallel = FALSE
    ),
    "Assertion on 'dirname\\(file_dest\\)' failed"
    )

    # checkmate::assert_flag(parallel)
    expect_error(get_data_from_zenodo(
        file_url = "http://test.com/test.txt",
        file_dest = "./TeSt.txt", parallel = 1
    ),
    "Assertion on 'parallel' failed"
    )
})

test_that("check_zenodo_file_integrity() | general test", {
    mock_file <- tempfile()
    writeLines("test", mock_file)

    expect_null(shush(check_zenodo_file_integrity(
        file_md5 = "md5:9f06243abcb89c70e0c331c61d871fa7",
        file_dest = mock_file
    )))

    expect_message(check_zenodo_file_integrity(
        file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
        file_dest = mock_file
    ))
})

test_that("check_zenodo_file_integrity() | error test", {
    # checkmate::assert_character(file_md5)
    expect_error(check_zenodo_file_integrity(
        file_md5 = 1, file_dest = "./TeSt.txt"
    ),
    "Assertion on 'file_md5' failed"
    )

    # checkmate::assert_character(file_dest, any.missing = FALSE)
    expect_error(check_zenodo_file_integrity(
        file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec", file_dest = 1
    ),
    "Assertion on 'file_dest' failed"
    )

    expect_error(check_zenodo_file_integrity(
        file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
        file_dest = c("a", NA)
    ),
    "Assertion on 'file_dest' failed"
    )

    # checkmate::assert_directory(dirname(file_dest))
    expect_error(check_zenodo_file_integrity(
        file_md5 = "md5:d1985b883d7d188b316d864d0c8376ec",
        file_dest = "./TeSTtTtT/test.txt"
    ),
    "Assertion on 'dirname\\(file_dest\\)' failed"
    )
})
