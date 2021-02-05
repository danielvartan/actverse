

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
