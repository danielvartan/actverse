backtick_ <- function(x) paste0("`", x, "`")
single_quote_ <- function(x) paste0("'", x, "'")
double_quote_ <- function(x) paste0("\"", x, "\"")

class_collapse <- function(x) single_quote_(paste0(class(x), collapse = "/"))

paste_collapse <- function(x, sep = "", last = sep) {
    checkmate::assert_string(sep)
    checkmate::assert_string(last)

    if (length(x) == 1) {
        x
    } else {
        paste0(paste(x[-length(x)], collapse = sep), last, x[length(x)])
    }
}

inline_collapse <- function(x, last = "and", single_quote = TRUE,
                            serial_comma = TRUE) {
    checkmate::assert_string(last)
    checkmate::assert_flag(single_quote)
    checkmate::assert_flag(serial_comma)

    if (isTRUE(single_quote)) x <- single_quote_(x)

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        paste_collapse(x, sep = ", ", last = paste0(" ", last, " "))
    } else {
        paste_collapse(x, sep = ", ", last = paste0(", ", last, " "))
    }
}

period_ <- function(num, unit = "seconds") {
    unit_choices <- c("microsecond", "millisecond", "second", "minute",
                       "hour", "day", "week", "month", "quarter",
                       "year")
    unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

    checkmate::assert_number(num)
    checkmate::assert_choice(unit, unit_choices)

    if (grepl("^microsecond*", unit)) {
        lubridate::microseconds(num)
    } else if (grepl("^millisecond*", unit)) {
        lubridate::milliseconds(num)
    } else if (grepl("^quarter*", unit)) {
        lubridate::period(3, "months")
    } else {
        lubridate::period(num, unit)
    }
}

string_to_period <- function(string, irregularity = "min") {
    string_choices <- c("microsecond", "millisecond", "second", "minute",
                        "hour", "day", "week", "month", "quarter",
                        "year")
    string_choices <- append(string_choices, paste0(string_choices, "s"))
    irregularity_choices <- c("min", "mean", "max")

    checkmate::assert_choice(string, string_choices)
    checkmate::assert_choice(irregularity, irregularity_choices)

    if (irregularity == "min") {
        month <- lubridate::ddays(28)
        quarter <- lubridate::ddays(28) + (lubridate::ddays(30) * 2)
        year <- lubridate::ddays(365)
    } else if (irregularity == "mean") {
        month <- lubridate::dmonths()
        quarter <- lubridate::dmonths() * 3
        year <- lubridate::dyears()
    } else if (irregularity == "max") {
        month <- lubridate::dmonths(31)
        quarter <- lubridate::dmonths(31) * 3
        year <- lubridate::ddays(366)
    }

    if (grepl("^microsecond*", string)) {
        lubridate::dmicroseconds()
    } else if (grepl("^millisecond*", string)) {
        lubridate::dmilliseconds()
    } else if (any(grepl("^second*|^minute*|^hour|^week*|^day*", string))) {
        lubridate::duration(string)
    } else if (grepl("^month*", string)) {
        month
    } else if (grepl("^quarter*", string)) {
        quarter
    } else if (grepl("^year*", string)) {
        year
    }
}

period_to_string <- function(period) {
    checkmate::assert_number(period)

    # Workaround for when 'period' is of class 'Duration'
    period <- as.numeric(period)

    out <- as.character(NA)

    for (i in c("microseconds", "milliseconds", "seconds", "minutes",
                "hours", "days", "weeks")) {
        if (period == as.numeric(string_to_period(i))) {
            out <- i
        }
    }

    out
}

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse) %>%
        vapply(unlist, character(1)) %>%
        noquote()

    gsub("\\\"","", out)
}

rm_na <- function(x) x[which(!is.na(x))]

require_pkg <- function(...) {
    out <- list(...)

    lapply(out, checkmate::assert_string,
           pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")

    if (!identical(unique(unlist(out)), unlist(out))) {
        cli::cli_abort("'...' cannot have duplicated values.")
    }

    pkg <- unlist(out)
    namespace <- vapply(pkg, require_namespace, logical(1),
                        quietly = TRUE, USE.NAMES = FALSE)
    pkg <- pkg[!namespace]

    if (length(pkg) == 0) {
        invisible(NULL)
    } else {
        cli::cli_abort(paste0(
            "This function requires the {single_quote_(pkg)} package{?s} ",
            "to run. You can install {?it/them} by running:", "\n\n",
            "install.packages(",
            "{paste(double_quote_(pkg), collapse = ', ')})"
        ))
    }
}

shush <- function(x, quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }
}
