#' Find the epochs/periodicities of a time series
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `find_epoch()` returns the different epochs/periodicities/sample intervals
#' present in a time series, along with a best match epoch based on a predefined
#' threshold.
#'
#' @details
#'
#' In the rare cases that a time series have periodicities with the same
#' prevalence above the the threshold, the `find_epoch()` list element
#' `best_match` will return just one of the values.
#'
#' @param data A [`zoo`][zoo::zoo()] or an [`xts`][xts::xts()] object with
#'   a [`Date`][as.Date()] or [POSIXt][as.POSIXct()] vector as index.
#' @param threshold A number, from 0 to 1, indicating the minimum proportion
#'   that an epoch must have to be considered valid. `threshold = 1` means that
#'   the regularity of the time series must be strict (i.e., have just 1
#'   periodicity).
#'
#' @return a [`list`][list()] object with the following elements:
#'
#' * `best_match`: A number indicating the epoch/periodicity above the
#' `threshold` with greater prevalence in seconds. If none is find, `best_match`
#' value will be equal as `as.numeric(NA)`.
#' * `prevalence`: a [`tibble`][dplyr::tibble()] listing the unique
#' epoch/periodicity found in `data` along with its proportions.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' index <- c(
#' as.POSIXct(seq(60, 5400, by = 60), origin = as.POSIXct("1970-01-01")),
#' as.POSIXct(seq(5430, 5490, by = 30), origin = as.POSIXct("1970-01-01")),
#' as.POSIXct(seq(5505, 5520, by = 15), origin = as.POSIXct("1970-01-01")),
#' as.POSIXct(seq(5530, 5540, by = 10), origin = as.POSIXct("1970-01-01")),
#' as.POSIXct(seq(5545, 5555, by = 5), origin = as.POSIXct("1970-01-01"))
#' )
#'
#' data <- xts::xts(1:100, order.by = index)
#'
#' find_epoch(data, 0.8)
find_epoch <- function(data, threshold = 0.9) {
    checkmate::assert_multi_class(data, c("ts", "zoo", "xts"))
    checkmate::assert_multi_class(zoo::index(data), c("Date", "POSIXt"))
    checkmate::assert_number(length(data[, 1]), lower = 2)
    checkmate::assert_number(threshold, lower = 0.001, upper = 1)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- proportion <- NULL

    diff <- diff(zoo::index(data))
    if (lubridate::is.difftime(diff)) units(diff) <- "secs"
    diff <- diff %>% as.numeric()

    prevalence <- unique(diff) %>%
        purrr::map(find_epoch_prevalence, diff = diff) %>%
        purrr::reduce(dplyr::bind_rows) %>%
        dplyr::arrange(dplyr::desc(proportion))

    if (prevalence$proportion[1] >= threshold) {
        best_match <- prevalence$epoch[1]
    } else {
        best_match <- as.numeric(NA)
    }

    list(best_match = best_match,
         prevalence = prevalence)
}

find_epoch_prevalence <- function(epoch, diff) {
    checkmate::assert_number(epoch)
    checkmate::assert_numeric(diff)

    dplyr::tibble(epoch = epoch,
                  proportion = length(which(diff == epoch)) / length(diff))
}
