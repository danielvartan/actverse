backtick_ <- function(x) paste0("`", x, "`")
single_quote_ <- function(x) paste0("'", x, "'")
double_quote_ <- function(x) paste0("\"", x, "\"")

class_collapse <- function(x) {
    single_quote_(paste0(class(x), collapse = "/"))
}

period_ <- function(num, units = "seconds") {
    units_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                       "hours", "days", "weeks", "months", "quarters", "years")

    checkmate::assert_number(num)
    checkmate::assert_choice(units, units_choices)

    if (units == "microseconds") {
        lubridate::microseconds(num)
    } else if (units == "milliseconds") {
        lubridate::milliseconds(num)
    } else if (units == "quarters") {
        lubridate::period(3, "months")
    } else {
        lubridate::period(num, units)
    }
}

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse) %>%
        vapply(unlist, character(1)) %>%
        noquote()

    gsub("\\\"","", out)
}

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

string_to_period <- function(string) {
    string_choices <- c("microseconds", "milliseconds", "seconds", "minutes",
                        "hours", "days", "weeks", "months", "quarters", "years")

    checkmate::assert_choice(string, string_choices)

    if (string == "microseconds") {
        lubridate::dmicroseconds() %>% as.numeric()
    } else if (string == "milliseconds") {
        lubridate::dmilliseconds() %>% as.numeric()
    } else if (string %in% c("seconds", "minutes", "hours", "days")) {
        lubridate::duration(string) %>% as.numeric()
    } else if (string == "weeks") {
        (lubridate::ddays() * 7) %>% as.numeric()
    } else if (string == "months") {
        (lubridate::ddays() * 28) %>% as.numeric()
    } else if (string == "quarters") {
        (lubridate::ddays() * 30  * (12 / 4)) %>% as.numeric()
    } else if (string == "years") {
        (lubridate::ddays() * 365) %>% as.numeric()
    }
}
