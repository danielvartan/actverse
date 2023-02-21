#' Get data from a Zenodo record
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `get_from_zenodo()` allows you to easy download a Zenodo record or
#' a Zenodo file.
#'
#' This function only works for Zenodo created DOIs (not when the DOI is, for
#' example, derived from Zookeys).
#'
#' @details
#'
#' ## Requirements
#'
#' This function requires an internet connection and the
#' [`curl`](https://github.com/jeroen/curl),
#' [`jsonlite`](https://github.com/jeroen/jsonlite),
#' [`parallel`][parallel::parallel-package], and
#' [`tools`](https://github.com/jeroen/jsonlite) packages to work.
#'
#' If you don't have any or one of the packages mentioned above, you can install
#' them with `install.packages("curl", "jsonlite", "parallel", "tools")`.
#'
#' ## Zenodo API
#'
#' You can find more about the Zenodo API at
#' \url{https://developers.zenodo.org/}.
#'
#' ## License
#'
#' `get_from_zenodo()` code is based on the `download_zenodo()` function found
#' in the [`inborutils`](https://github.com/inbo/inborutils) package of the
#' [Research Institute for Nature and Forest (INBO)](http://www.inbo.be/en).
#' `download_zenodo()` was created by Hans Van Calster (hans.vancalster@inbo.be)
#' and Floris Vanderhaeghe (floris.vanderhaeghe@inbo.be).
#'
#' We give our thanks for the INBO institute and for all developers involved
#' in this piece of software.
#'
#' Please note that this code comes with an [MIT
#' License](https://opensource.org/license/mit/). You can read the latter below.
#'
#' ```
#' Copyright (c) 2016 Instituut voor Natuur en Bosonderzoek (INBO)
#'
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#'
#' The above copyright notice and this permission notice shall be included in
#' all copies or substantial portions of the Software.
#'
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' ```
#'
#' @param doi A string indicating a Zenodo DOI (Digital Object Identifier)
#'   starting with `"10.5281/zenodo."`. See the Examples section to learn more.
#' @param path (optional) a string indicating a directory path where the data
#'   must be downloaded (default: `"."`).
#' @param file (optional) a [`character`][base::character()] object with the
#'   names of the files that must be downloaded. If `NULL`, the function will
#'   download the entire record (default: `NULL`).
#' @param parallel (optional) a [`logical`][base::logical()] value indicating if
#'   the function must run a number of parallel processes, each downloading
#'   another file (requires the [`parallel`][parallel::parallel-package]
#'   package). This is useful when multiple large files are present in the
#'   Zenodo record, which otherwise would be downloaded sequentially. Of course,
#'   the operation is limited by bandwidth and traffic limitations (default:
#'   `FALSE`).
#'
#' @return A [`character`][character()] object with the paths for all the files
#'   downloaded.
#'
#' @family API functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## Downloading a single file from a Zenodo record
#'
#' path <- tempdir()
#' file <- "sleep-diary.txt"
#'
#' get_from_zenodo(
#'     doi = "10.5281/zenodo.4898822", path = path, file = file
#'     )
#'
#' readLines(file.path(path, file))
#'
#' ## Downloading all the files from a Zenodo record
#'
#' get_from_zenodo(
#'     doi = "10.5281/zenodo.4898822", path = tempdir(), file = NULL
#'     )
#'
#' ## Downloading all the files from a Zenodo record using parallel
#' ## computation
#'
#' get_from_zenodo(
#'     doi = "10.5281/zenodo.4898822", path = tempdir(), file = NULL,
#'     parallel = TRUE
#'     )
#' }
get_from_zenodo <- function(doi, path = ".", file = NULL, parallel = FALSE) {
    checkmate::assert_string(doi, pattern = "^10\\.5281\\/zenodo")
    checkmate::assert_directory_exists(path)
    checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)
    checkmate::assert_flag(parallel)
    assert_internet()

    require_pkg("curl", "jsonlite", "tools")

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- key <- NULL

    metadata <- get_metadata_from_zenodo(doi = doi)

    if (is.null(file)) {
        file <- metadata$files$key
    } else {
        checkmate::assert_subset(file, metadata$files$key)
    }

    metadata <- metadata$files %>% dplyr::filter(key %in% file)

    get_data_from_zenodo(
        file_url = metadata$links$self, file_dest = file.path(path, file),
        parallel = parallel
        )

    check_zenodo_file_integrity(
        file_md5 = metadata$checksum, file_dest = file.path(path, file)
        )

    list.files(
        path, full.names = TRUE, recursive = TRUE, include.dirs = FALSE
        ) %>%
        magrittr::extract(
            grepl(paste0(basename(file), "$", collapse = "|"), .)
            ) %>%
        find_absolute_path()
}

get_metadata_from_zenodo <- function(doi) {
    checkmate::assert_string(doi, pattern = "^10\\.5281\\/zenodo")
    assert_internet()

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- NULL

    cli::cli_progress_step("Downloading metadata")

    gsub("10.5281/zenodo.", "", doi, fixed = TRUE) %>%
        paste0("https://zenodo.org/api/records/", .) %>%
        curl_fetch_memory() %>%
        magrittr::extract2("content") %>%
        raw_to_char() %>%
        from_json()
}

get_data_from_zenodo <- function(file_url, file_dest, parallel = FALSE) {
    url_pattern <- paste0(
        "^http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|",
        "(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    )

    checkmate::assert_character(file_url, pattern = url_pattern)
    checkmate::assert_character(file_dest, any.missing = FALSE)
    checkmate::assert_directory(dirname(file_dest))
    checkmate::assert_flag(parallel)
    assert_internet()

    file_length <- length(file_url)

    if (isTRUE(parallel)) {
        require_pkg("parallel")

        nr_nodes <- min(10, length(file_url))

        cli::cli_alert_info(paste0(
            "Initializing parallel download on ",
            "{.strong {cli::col_blue(nr_nodes)}}",
            "{cli::qty(nr_nodes)} R session node{?s}."
        ))

        cli::cli_progress_step(paste0(
            "{cli::qty(file_length)}Starting parallel download{?s}"
        ))

        clus <- make_cluster(nr_nodes)

        cli::cli_alert_info(paste0(
            "This may take a while. ",
            "No progress bar will be available. Be patient."
        ))

        cluster_map(
            clus, curl_download, file_url, file_dest,
            MoreArgs = list(quiet = TRUE)
        )

        stop_cluster(clus)

        cli::cli_progress_step(paste0(
            "{cli::qty(file_length)}Ending parallel download{?s}"
        ))
    } else {
        cli::cli_progress_step("{cli::qty(file_length)}Downloading file{?s}")

        mapply(
            function(url, destfile, progress, quiet) {
                curl_download(url = url, destfile = destfile, quiet = quiet)
            },
            file_url, file_dest,
            cli::cli_progress_along(file_url, clear = FALSE),
            MoreArgs = list(quiet = TRUE)
        )
    }

    invisible(NULL)
}

check_zenodo_file_integrity <- function(file_md5, file_dest) {
    checkmate::assert_character(file_md5)
    checkmate::assert_character(file_dest, any.missing = FALSE)
    checkmate::assert_directory(dirname(file_dest))

    cli::cli_progress_step("Checking file integrity")

    file_name <- basename(file_dest)

    for (i in seq_along(file_md5)) {
        name <- file_name[i]
        dest <- file_dest[i]
        md5 <- unname(tools::md5sum(dest))
        zenodo_md5 <- gsub("^md5:", "", file_md5[i])

        if (!identical(md5, zenodo_md5)) {
            cli::cli_alert_warning(paste0(
                "{.strong {cli::col_red('Incorrect download')}}! ",
                "md5sum {.strong {md5}} for file {.strong {name}} does ",
                "not match the Zenodo archived md5sum {.strong {zenodo_md5}}."
            ))
        }
    }

    invisible(NULL)
}
