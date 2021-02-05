#' @family utility functions
#' @noRd
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}

#' @family utility functions
#' @noRd
is_numeric_ <- function(x) {

    any(class(x) %in% c("integer", "double", "numeric"))

}

#' @family utility functions
#' @noRd
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {

    checkmate::assert_multi_class(x, c("integer", "numeric"))

    abs(x - round(x)) < tol # Example function from ?integer

}

#' @family utility functions
#' @noRd
inline_collapse <- function(x, single_quote = TRUE, serial_comma = TRUE) {

    checkmate::assert_character(x)
    checkmate::assert_flag(single_quote)
    checkmate::assert_flag(serial_comma)

    if (isTRUE(single_quote)) x <- glue::single_quote(x)

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        glue::glue_collapse(x, sep = ", ", last = " and ")
    } else {
        glue::glue_collapse(x, sep = ", ", last = ", and ")
    }

}

#' @family utility functions
#' @noRd
shush <- function(x, quiet = TRUE){

    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }

}

#' @family utility functions
#' @noRd
close_round <- function(x, digits = 5) {

    pattern_9 <- paste0("\\.", paste(rep(9, digits), collapse = ""))
    pattern_0 <- paste0("\\.", paste(rep(0, digits), collapse = ""))

    dplyr::case_when(
        stringr::str_detect(x, pattern_9) |
            stringr::str_detect(x, pattern_0) ~ round(x),
        TRUE ~ x
    )

}

#' @family utility functions
#' @noRd
swap <- function(x, y) {

    assert_identical(x, y, type = "length")
    assert_identical(x, y, type = "class")

    a <- x
    b <- y

    x <- b
    y <- a

    list(x = x, y = y)

}

#' @family utility functions
#' @noRd
swap_if <- function(x, y, condition = "x > y") {

    choices <- c("x == y", "x < y", "x <= y", "x > y", "x >= y")

    assert_identical(x, y, type = "length")
    assert_identical(x, y, type = "class")
    checkmate::assert_choice(condition, choices)

    condition <- stringr::str_replace(condition, "x", "a")
    condition <- stringr::str_replace(condition, "y", "b")

    a <- x
    b <- y

    x <- dplyr::if_else(eval(parse(text = condition)), b, a)
    y <- dplyr::if_else(eval(parse(text = condition)), a, b)

    list(x = x, y = y)

}

#' @family utility functions
#' @noRd
count_na <- function(x) {

    length(which(is.na(x)))

}

#' @family utility functions
#' @noRd
escape_regex <- function(x) {

    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)

}

#' @family utility functions
#' @noRd
get_names <- function(...) {

    out <- lapply(substitute(list(...))[-1], deparse)
    out <- vapply(out, unlist, character(1))
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out

}

#' @family utility functions
#' @noRd
get_class <- function(x) {

    foo <- function(x) {
        class(x)[1]
    }

    if (is.list(x) || is.data.frame(x)) {
        vapply(x, foo, character(1))
    } else {
        class(x)[1]
    }

}
