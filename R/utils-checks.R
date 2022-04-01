# Sort functions by type or use the alphabetical order.

test_interval <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.interval(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.interval(x)
    }
}

check_interval <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                         null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else if (lubridate::is.interval(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.interval(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!lubridate::is.interval(x)) {
        paste0("Must be of type 'Interval', not ",
               class_collapse(x))
    } else {
        TRUE
    }
}

assert_interval <- checkmate::makeAssertionFunction(check_interval)

assert_identical <- function(..., type = "value", any.missing = TRUE,
                             null.ok = FALSE) {

    if (!checkmate::test_list(list(...), min.len = 2)) {
        cli::cli_abort("'...' must have 2 or more elements.")
    }

    checkmate::assert_choice(type, c("value", "length", "class"))
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    names <- get_names(...)
    out <- list(...)

    if (type == "length") {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must have identical lengths.")
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must have identical classes.")
        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must be identical.")
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null)), na.rm = TRUE) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        cli::cli_abort("{names} cannot have missing values.")
    } else if (any(is.null(unlist(out)), na.rm = TRUE) && isFALSE(null.ok)) {
        cli::cli_abort("{names} cannot have 'NULL' values.")
    } else if (isFALSE(check)) {
        cli::cli_abort(error_message)
    } else {
        invisible(TRUE)
    }
}

test_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                        null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.POSIXt(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.POSIXt(x)
    }
}

check_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                         null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else if (lubridate::is.POSIXt(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.POSIXt(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!lubridate::is.POSIXt(x)) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               class_collapse(x))
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)



test_xts <- function(x, index_class = NULL, vector_class = NULL,
                     min.rows = NULL, min.cols = NULL,
                     null.ok = FALSE) {
    checkmate::assert_character(index_class, null.ok = TRUE)
    checkmate::assert_string(vector_class, null.ok = TRUE)
    checkmate::assert_int(min.rows, lower = 1, null.ok = TRUE)
    checkmate::assert_int(min.cols, lower = 1, null.ok = TRUE)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (!xts::is.xts(x)) {
        FALSE
    } else if (!is.null(index_class) &&
               (!any(index_class %in% xts::tclass(x), na.rm = TRUE))) {
        FALSE
    } else if (!is.null(vector_class) &&
               (!any(vector_class %in% xts::tclass(x), na.rm = TRUE))) {
        FALSE
    }else if ((!is.null(min.rows) && !any(nrow(x) >= min.rows)) ||
               (!is.null(min.cols) && !any(ncol(x) >= min.cols))) {
        FALSE
    } else {
        TRUE
    }
}

check_xts <- function(x, index_class = NULL, vector_class = NULL,
                      min.rows = NULL, min.cols = NULL,
                      null.ok = FALSE,
                      name = deparse(substitute(x))) {
    checkmate::assert_character(index_class, null.ok = TRUE)
    checkmate::assert_string(vector_class, null.ok = TRUE)
    checkmate::assert_int(min.rows, lower = 1, null.ok = TRUE)
    checkmate::assert_int(min.cols, lower = 1, null.ok = TRUE)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if ((is.null(x) && isFALSE(null.ok)) || !xts::is.xts(x)) {
        paste0("Must be of type 'xts', not ", class_collapse(x))
    } else if (!is.null(index_class) &&
               (!any(index_class %in% xts::tclass(x), na.rm = TRUE))) {
        paste0("Must have an index of class ",
               inline_collapse(index_class, "or"),
               ", not ", class_collapse(zoo::index(x)))
    } else if (!is.null(min.rows) && !any(nrow(x) >= min.rows)) {
        paste0("Must have at least ", min.rows, " rows, but has ", nrow(x),
               " rows")
    } else if (!is.null(min.cols) && !any(ncol(x) >= min.cols)) {
        paste0("Must have at least ", min.cols, " cols, but has ", ncol(x),
               " cols")
    } else {
        TRUE
    }
}

assert_xts <- checkmate::makeAssertionFunction(check_xts)
