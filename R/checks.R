#' @family check functions
#' @noRd
check_any_na <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x))) {
        glue::glue("{glue::backtick(name)} cannot have any missing values")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_any_na <- checkmate::makeAssertionFunction(check_any_na)

#' @family check functions
#' @noRd
check_not_all_na <- function(x, name = deparse(substitute(x))) {

    if (all(is.na(x))) {
        glue::glue("{glue::backtick(name)} cannot have all values as missing")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_not_all_na <- checkmate::makeAssertionFunction(check_not_all_na)

#' @family check functions
#' @noRd
check_length_one <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (!(length(x) == 1)) {
        glue::glue("{glue::backtick(name)} must have length 1, not length ",
                   "{length(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

#' @family check functions
#' @noRd
check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (length(x) < 1) {
        glue::glue("{glue::backtick(name)} must have length greater than zero")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

#' @family check functions
#' @noRd
check_whole_number <- function(x, any.missing = TRUE, null.ok = FALSE,
                             name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot be `NULL`")
    } else  if (!all(is_whole_number(x))) {
        glue::glue("{glue::backtick(name)} must consist of whole numbers")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_whole_number <- checkmate::makeAssertionFunction(check_whole_number)

#' @family check functions
#' @noRd
check_numeric_ <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!is_numeric_(x)) {
        glue::glue("Must be of type 'numeric', not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_numeric_ <- checkmate::makeAssertionFunction(check_numeric_)

#' @family check functions
#' @noRd
check_duration <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!lubridate::is.duration(x)) {
        glue::glue("Must be of type 'Duration', not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_duration <- checkmate::makeAssertionFunction(check_duration)

#' @family check functions
#' @noRd
check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!lubridate::is.POSIXt(x)) {
        glue::glue("Must be of type 'POSIXct' or 'POSIXlt', not ",
                   "{class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

#' @family check functions
#' @noRd
assert_identical <- function(..., type = "value", any.missing = TRUE,
                            null.ok = FALSE) {

    checkmate::assert_list(list(...), min.len = 2)
    checkmate::assert_choice(type, c("value", "length", "class"))
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    names <- get_names(...)
    out <- list(...)

    if (type == "length") {
        error_message <- glue::glue("Assertion failed: ",
                                    "{inline_collapse(names)} ",
                                    "must have identical lengths.")
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- glue::glue("Assertion failed: ",
                                    "{inline_collapse(names)} ",
                                    "must have identical classes.")

        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- glue::glue("Assertion failed: ",
                                    "{inline_collapse(names)} ",
                                    "must be identical.")
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null))) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        rlang::abort(glue::glue("{inline_collapse(names)} cannot have ",
                                "missing values"))
    } else if (any(is.null(unlist(out))) && isFALSE(null.ok)) {
        rlang::abort(glue::glue("{inline_collapse(names)} cannot have ",
                                 "`NULL` values"))
    } else if (isFALSE(check)) {
        rlang::abort(error_message)
    } else {
       invisible(TRUE)
    }

}

#' Used in swap_decimal()
#' @family check functions
#' @noRd
check_custom_1 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!(is.character(x) || is_numeric_(x))) {
        glue::glue("Must inherit from class 'character'/'numeric', ",
                   "but has class {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' Used in swap_decimal()
#' @family check functions
#' @noRd
assert_custom_1 <- checkmate::makeAssertionFunction(check_custom_1)
