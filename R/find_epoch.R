#' Find distinct epochs in a series
#'
#' @description
#'
#' `find_epoch()` identifies the distinct epochs or periodicities present in an
#' object and returns them along with the most prevalent epoch that meets a
#' specified threshold.
#'
#' @details
#'
#' In rare cases where multiple periodicities have the same highest prevalence
#' above the threshold, `best_match` will return only one of those values.
#'
#' @param x Any [`atomic`][base::atomic()] vector, provided that the function
#'   has a method for handling it.
#' @param threshold (optional) A number, from `0` to `1`, indicating the minimum
#'   proportion that an epoch must have to be considered valid. `threshold = 1`
#'   means that the regularity of the time series must be strict (i.e., have
#'   just 1 periodicity) (default: `0.9`).
#'
#' @return A [`list`][list()] object with the following elements:
#'
#' - `best_match`: A number indicating the epoch/periodicity above the
#' `threshold` with greater prevalence in seconds. If none is find, `best_match`
#' value will be equal as `as.numeric(NA)`.
#' - `prevalence`: a [`tibble`][tibble::tibble()] listing the unique
#' epochs/periodicities found in `data` along with its proportions.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' data <-
#'   dplyr::tibble(
#'     index = c(
#'       as.POSIXct(
#'         seq(60, 5400, by = 60),
#'         origin = lubridate::origin
#'       ),
#'       as.POSIXct(
#'         seq(5430, 5490, by = 30),
#'         origin = lubridate::origin
#'       ),
#'       as.POSIXct(
#'         seq(5505, 5520, by = 15),
#'         origin = lubridate::origin
#'       ),
#'       as.POSIXct(
#'         seq(5530, 5540, by = 10),
#'         origin = lubridate::origin
#'       ),
#'       as.POSIXct(
#'         seq(5545, 5555, by = 5),
#'         origin = lubridate::origin
#'       )
#'     ),
#'     x = seq_along(timestamp)
#'   ) |>
#'   tsibble::tsibble(index = index)
#'
#' find_epoch(data, 0.8)
#'
#' seq(1, 100, by = 5) |> find_epoch(threshold = 0.8)
find_epoch <- function(x, threshold = 0.9) {
  UseMethod("find_epoch")
}

#' @rdname find_epoch
#' @export
find_epoch.numeric <- function(x, threshold = 0.9) {
  checkmate::assert_numeric(x, min.len = 2)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  # R CMD Check variable bindings fix
  # nolint start
  . <- .x <- proportion <- NULL
  # nolint end

  x |>  diff() |> compute_prevalence(threshold = threshold)
}

#' @rdname find_epoch
#' @export
find_epoch.hms <- function(x, threshold = 0.9) {
  prettycheck::assert_hms(x)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  x |> as.numeric() |> find_epoch(threshold = threshold)
}

#' @rdname find_epoch
#' @export
find_epoch.tbl_df <- function(x, threshold = 0.9) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x, c("Date", "POSIXt"))
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  x |>
    magrittr::extract2(tsibble::index_var(x)) |>
    diff() %>%
    `units<-`("secs") |>
    as.numeric() |>
    compute_prevalence(threshold = threshold)
}

compute_prevalence <- function(diff, threshold = 0.9) {
  checkmate::assert_numeric(diff, min.len = 1)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  # R CMD Check variable bindings fix
  # nolint start
  proportion <- NULL
  # nolint end

  prevalence <-
    diff |>
    unique() |>
    purrr::map(
      ~ dplyr::tibble(
        epoch = .x,
        proportion = length(which(diff == .x)) / length(diff)
      )
    ) |>
    purrr::reduce(dplyr::bind_rows) |>
    dplyr::arrange(dplyr::desc(proportion))

  if (prevalence$proportion[1] >= threshold) {
    best_match <- prevalence$epoch[1]
  } else {
    best_match <- as.numeric(NA)
  }

  list(best_match = best_match, prevalence = prevalence)
}
