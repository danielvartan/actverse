#' Get path to data example
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Actverse comes bundled with a number of sample files in its `inst/extdata`
#' directory. This function make them easy to access.
#'
#' @param path A string with the file name. If `NULL`, the example
#'   files will be listed.
#' @return If path is equal to `NULL`, returns a character vector with all
#'   the example file names. Else, returns the full path where the file is
#'   located.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' data_example()
#' data_example("test_log.txt")}

data_example <- function(path = NULL) {

    if (is.null(path)) {
        dir(system.file("extdata", package = "actstudio"))
    } else {
        system.file("extdata", path, package = "actstudio", mustWork = TRUE)
    }

}
