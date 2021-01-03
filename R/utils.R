#' Get path to data example
#'
#' @description
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

#' Check if a data frame is tidy
#'
#' @description
#'
#' To use this package is best that your data conforms to the proposed
#' data structure of the tidy_data function. You can still use the tools
#' available without tiding your data, but by doing that you may spend more
#' time using the functions, and may be more prone to error.
#'
#' @param data A data frame or tibble.
#' @return A logic value indicating if the data frame is tidy.
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' is_tidy(test_log)}

is_tidy <- function(data) {

    # Check arguments --------------------

    if (!(is.data.frame(data))) {
        stop("data is not a data frame", call. = FALSE)
    }

    # Check if data conforms to the tidy structure --------------------

    model <- model_data()
    output <- TRUE

    if (!(identical(names(data), names(model)))) {
        output <- FALSE
    }

    if (!(identical(unlist(lapply(data, class)),
                    unlist(lapply(model, class))))) {
        output <- FALSE
    }

    # Return output --------------------

    output

}

#' Return a tidy data frame
#'
#' @description
#'
#' This function return a data frame conformed to the proposed data structure
#' of the [tidy_data()] function. It can be used for testing and structuring
#' other routines.
#'
#' @return A tibble.
#' @family Utility functions
#' @export

model_data <- function() {
    test_log
}
