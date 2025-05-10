#' Get paths to `actverse` raw datasets
#'
#' @description
#'
#' `actverse` comes bundled with raw datasets for testing and learning
#' purposes. `get_raw_data()` makes it easy to access their paths.
#'
#' @param file (optional) A [`character`][character()] vector specifying the raw
#'   data file name(s) to retrieve. If `NULL`, returns all available raw data
#'   file names (default: `NULL`).
#'
#' @return If `file` is `NULL`, a [`character`][character()] object with all
#'   file names available. Else, a string with the file name path.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' get_raw_data()
#'
#' get_raw_data("acttrust.txt")
get_raw_data <- function(file = NULL) {
  checkmate::assert_character(file, any.missing = FALSE, null.ok = TRUE)

  if (is.null(file)) {
    list.files(system.file("extdata", package = "actverse"))
  } else {
    system.file("extdata", file, package = "actverse", mustWork = TRUE)
  }
}
