# Sort by type or by alphabetical order.

test_any_missing <- function(x) any(is.na(x))

check_any_missing <- function(x, name = deparse(substitute(x))) {
    if (any(is.na(x))) {
        paste0(single_quote_(name), " cannot have missing values")
    } else {
        TRUE
    }
}

assert_any_missing  <- checkmate::makeAssertionFunction(check_any_missing)

warn_any_missing <- function(x, name = deparse(substitute(x))) {
    if (any(is.na(x))) {
        cli::cli_alert_warning(paste0(
            "{.strong {cli::col_red(name)}} has missing values. ",
            "The output may diverge."
        ))
    } else {
        TRUE
    }
}

test_leq <- function(x, y) {
    checkmate::assert_number(x)
    checkmate::assert_number(y)

    x <= y
}

check_leq <- function(x, y, name_x = deparse(substitute(x)),
                      name_y = deparse(substitute(y))) {
    checkmate::assert_number(x)
    checkmate::assert_number(y)

    if (!x <= y) {
        paste0(single_quote_(name_x), " must be less or equal to ",
               single_quote_(name_y))
    } else {
        TRUE
    }
}

assert_leq  <- checkmate::makeAssertionFunction(check_leq)

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
                                " must have identical lengths."
                                )
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must have identical classes."
                                )
        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must be identical."
                                )
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null)), na.rm = TRUE) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        cli::cli_abort("{names} cannot have missing values.")
    } else if (any(is.null(unlist(out)), na.rm = TRUE) && isFALSE(null.ok)) {
        cli::cli_abort("{names} cannot be 'NULL'.")
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
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (lubridate::is.POSIXt(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.POSIXt(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!lubridate::is.POSIXt(x)) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               class_collapse(x)
               )
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

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
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (lubridate::is.interval(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.interval(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!lubridate::is.interval(x)) {
        paste0("Must be of type 'Interval', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_interval <- checkmate::makeAssertionFunction(check_interval)

test_tsibble <- function(x, min.rows = NULL, min.cols = NULL, null.ok = FALSE) {
    checkmate::assert_int(min.rows, lower = 1, null.ok = TRUE)
    checkmate::assert_int(min.cols, lower = 1, null.ok = TRUE)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (!tsibble::is_tsibble(x)) {
        FALSE
    } else if ((!is.null(min.rows) && !any(nrow(x) >= min.rows)) ||
               (!is.null(min.cols) && !any(ncol(x) >= min.cols))) {
        FALSE
    } else {
        TRUE
    }
}

check_tsibble <- function(x, min.rows = NULL, min.cols = NULL, null.ok = FALSE,
                          name = deparse(substitute(x))) {
    checkmate::assert_int(min.rows, lower = 1, null.ok = TRUE)
    checkmate::assert_int(min.cols, lower = 1, null.ok = TRUE)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (!tsibble::is_tsibble(x)) {
        paste0("Must be of type 'tbl_ts' (tsibble), not ", class_collapse(x))
    } else if (!is.null(min.rows) && !any(nrow(x) >= min.rows)) {
        paste0("Must have at least ", min.rows, " rows, but has ", nrow(x),
               " rows"
               )
    } else if (!is.null(min.cols) && !any(ncol(x) >= min.cols)) {
        paste0("Must have at least ", min.cols, " cols, but has ", ncol(x),
               " cols"
               )
    } else {
        TRUE
    }
}

assert_tsibble <- checkmate::makeAssertionFunction(check_tsibble)

test_index_class <- function(x, classes = c("Date", "POSIXt", "yearweek",
                                             "yearmonth", "yearquarter")) {
    assert_tsibble(x)
    checkmate::assert_character(classes)

    if (!any(classes %in% class(x[[tsibble::index_var(x)]]), na.rm = TRUE)) {
        FALSE
    } else {
        TRUE
    }
}

check_index_class <- function(x, classes = c("Date", "POSIXt", "yearweek",
                                             "yearmonth", "yearquarter"),
                              name = deparse(substitute(x))) {
    assert_tsibble(x)
    checkmate::assert_character(classes)

    if (!any(classes %in% class(x[[tsibble::index_var(x)]]), na.rm = TRUE)) {
        paste0("Must have an index of class ",
               inline_collapse(classes, "or"),
               ", not ", class_collapse(x[[tsibble::index_var(x)]])
               )
    } else {
        TRUE
    }
}

assert_index_class <- checkmate::makeAssertionFunction(check_index_class)

test_regularity <- function(x, threshold = 0.99, strict = FALSE) {
    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)
    checkmate::assert_flag(strict)

    prevalence <- find_epoch(x, threshold)$prevalence

    if (isTRUE(strict) && !nrow(prevalence) == 1) {
        FALSE
    } else if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
        FALSE
    } else {
        TRUE
    }
}

check_regularity <- function(x, threshold = 0.99, strict = FALSE,
                             name = deparse(substitute(x))) {
    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)
    checkmate::assert_flag(strict)

    prevalence <- find_epoch(x, threshold)$prevalence

    if (isTRUE(strict) && !nrow(prevalence) == 1) {
        paste0(single_quote_(name), " must be strictly regular. ",
               "See '?find_epoch' to learn more"
               )
    } else if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
        paste0(single_quote_(name), " must have a regularity equal or ",
               "greater than ", threshold * 100, "%. ",
               "See '?find_epoch' to learn more"
               )
    } else {
        TRUE
    }
}

assert_regularity <- checkmate::makeAssertionFunction(check_regularity)

warn_regularity <- function(x, threshold = 0.99, strict = FALSE,
                            name = deparse(substitute(x))) {
    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)
    checkmate::assert_flag(strict)

    prevalence <- find_epoch(x, threshold)$prevalence

    if (isTRUE(strict) && !nrow(prevalence) == 1) {
        cli::cli_alert_warning(paste0(
            "{.strong {cli::col_red(name)}} is not strictly regular. ",
            "The output may diverge. ",
            "See '?find_epoch' to learn more."
        ))
    } else if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
        cli::cli_alert_warning(paste0(
            "{.strong {cli::col_red(name)}} does not have a regularity ",
            "equal or greater than {.strong {threshold * 100}%}. ",
            "The output may diverge. ",
            "See '?find_epoch' to learn more."
        ))
    } else {
        TRUE
    }
}

test_clear_epoch <- function(x, threshold = 0.9) {
    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)

    prevalence <- find_epoch(x, threshold)$prevalence

    if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
        FALSE
    } else {
        TRUE
    }
}

check_clear_epoch <- function(x, threshold = 0.9,
                             name = deparse(substitute(x))) {
    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)

    prevalence <- find_epoch(x, threshold)$prevalence

    if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
        paste0(single_quote_(name), " does not present a clear ",
               "epoch/periodicity. See '?find_epoch' to learn more"
        )
    } else {
        TRUE
    }
}

assert_clear_epoch <- checkmate::makeAssertionFunction(check_clear_epoch)

test_epoch_compatibility <- function(x, unit) {
    unit_choices <- c("microsecond", "millisecond", "second", "minute",
                      "hour", "day", "week", "month", "quarter",
                      "year")
    unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    assert_clear_epoch(x, 0.7)
    checkmate::assert_choice(unit, unit_choices)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- proportion <- NULL

    epochs <- find_epoch(x)$prevalence %>%
        dplyr::filter(proportion >= 0.7) %>%
        magrittr::extract2("epoch")

    if (!all(as.numeric(string_to_period(unit)) >= epochs, na.rm = TRUE)) {
        FALSE
    } else {
        TRUE
    }
}

check_epoch_compatibility <- function(x, unit,
                              name = deparse(substitute(x))) {
    unit_choices <- c("microsecond", "millisecond", "second", "minute",
                      "hour", "day", "week", "month", "quarter",
                      "year")
    unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

    assert_tsibble(x, min.rows = 2, min.cols = 2)
    assert_index_class(x)
    assert_clear_epoch(x, 0.7)
    checkmate::assert_choice(unit, unit_choices)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)
    . <- proportion <- NULL

    epochs <- find_epoch(x)$prevalence %>%
        dplyr::filter(proportion >= 0.7) %>%
        magrittr::extract2("epoch")

    if (!grepl("s$", unit)) unit <- paste0(unit, "s")

    if (!all(as.numeric(string_to_period(unit)) >= epochs,
                    na.rm = TRUE)) {
        paste0("The epoch/periodicity present in ", single_quote_(name),
               "don't allow to aggregate it in ", unit, ". ",
               "See '?find_epoch' to learn more"
        )
    } else {
        TRUE
    }
}

assert_epoch_compatibility <- checkmate::makeAssertionFunction(
    check_epoch_compatibility)
