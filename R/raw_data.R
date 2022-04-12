#' Get paths to `actverse` raw datasets
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `actverse` comes bundled with raw datasets for testing and learning
#' purposes. `raw_data()` makes it easy to access their paths.
#'
#' @param file (optional) a [`character`][character()] object indicating the raw
#'   data file name(s). If `NULL`, all raw data file names will be returned
#'   (default: `NULL`).
#'
#' @return If `file == NULL`, a [`character`][character()] object with all file
#'   names available. Else, a string with the file name path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## To list all raw data file names available
#'
#' raw_data()
#'
#' ## To get the file path from a specific raw data
#'
#' raw_data("acttrust.txt")
raw_data <- function(file = NULL) {
    checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)

    if (is.null(file)) {
        list.files(system.file("extdata", package = "actverse"))
    } else {
        system.file("extdata", file, package = "actverse", mustWork = TRUE)
    }
}
