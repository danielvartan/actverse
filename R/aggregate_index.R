#' Aggregate the index of a `tsibble`
#'
#' @description
#'
#' `aggregate_index()` allows you to aggregate the index of a
#' [`tsibble`][tsibble::tsibble()] object by applying a specific function to
#' its measured variables.
#'
#' @details
#'
#' `aggregate_index()` is designed to simplify the regularization of
#' [`tsibble`][tsibble::tsibble()] objects by aggregating their index to a
#' specified time unit. For more advanced or customized aggregation, consider
#' using the [`index_by()`][tsibble::index_by()] function from the
#' [`tsibble`](https://tsibble.tidyverts.org/) package, which offers greater
#' flexibility and control.
#'
#' ## Default function
#'
#' If the `fun` argument is `NULL`, `aggregate_index()` will use the following
#' function to transform each measured variable:
#'
#' ```
#' function(x) {
#'     if (is.numeric(x) && !all(nchar(x) == 1, na.rm = TRUE)) {
#'         mean(x, na.rm = TRUE)
#'     } else {
#'         y <- x[which(!is.na(x))]
#'         unique <- unique(y)
#'         unique[which.max(tabulate(match(y, unique)))]
#'     }
#' }
#' ```
#'
#' This function computes the mean for numeric variables and assigns the most
#' frequent value (mode) for non-numeric variables. If no mode is found, it
#' returns the first non-missing value of `x`.
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param unit A string indicating at which time unit the index must be
#'   aggregated. Valid values are: `“seconds”`, `“minutes”`, `“hours”`,
#'   `“days”`, `“weeks”`, `“months”`, `“quarters”`, and `“years”`) (default:
#'   `"minutes"`).
#' @param fun (optional) A [`function`][base::is.function()] to be applied to
#'   each measure variable of `data`. If `NULL`, `aggregate_index()` will apply
#'   its default function (see the Details section to learn more)
#'   (default: `NULL`).
#' @param week_start (optional) An integer number indicating the day on which
#'   the week starts (`1` for Monday and `7` for `Sunday`). This is only used
#'   when `unit` is set to `"weeks"` (default: `1`).
#'
#' @return A [`tsibble`][tsibble::tsibble()] object with the same columns as
#'   `data`, but with the index aggregated to the specified time unit.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' acttrust
#'
#' aggregate_index(acttrust, unit = "hour")
#'
#' aggregate_index(acttrust, unit = "day")
#'
#' aggregate_index(acttrust, unit = "week")
#'
#' aggregate_index(acttrust, unit = "month")
#'
#' aggregate_index(acttrust, unit = "quarter")
#'
#' aggregate_index(acttrust, unit = "year")
aggregate_index <- function(data, unit, fun = NULL, week_start = 1) {
  unit_choices <- c(
    "second", "minute", "hour", "day", "week", "month",
    "quarter", "year"
  )
  unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

  assert_tsibble(data, min_rows = 2, min_cols = 2)
  assert_index_class(data, c("Date", "POSIXt"))
  assert_epoch_compatibility(data, unit)
  checkmate::assert_choice(unit, unit_choices)
  checkmate::assert_function(fun, null.ok = TRUE)
  checkmate::assert_choice(week_start, c(1, 7))

  # R CMD Check variable bindings fix
  # nolint start
  . <- .iNdEx_PlAcEhOlDeR <- NULL
  # nolint end

  index_var <- tsibble::index_var(data)
  index <- data[[index_var]]

  # Workaround to avoid problems with `dplyr::select()`.
  data <-
    data |>
    dplyr::rename(.iNdEx_PlAcEhOlDeR = tsibble::index_var(data))

  if (is.null(fun)) fun <- aggregate_index_default_fun

  if (grepl("^day*", unit)) {
    group <- lubridate::floor_date(index, "days") |> as.Date()
  } else if (grepl("^week*", unit)) {
    group <- tsibble::yearweek(index, week_start = week_start)
  } else if (grepl("^month*", unit)) {
    group <- tsibble::yearmonth(index)
  } else if (grepl("^quarter*", unit)) {
    group <- tsibble::yearquarter(index)
  } else if (grepl("^year*", unit)) {
    group <- lubridate::year(index)
  } else {
    group <- lubridate::floor_date(index, unit)
  }

  data |>
    tsibble::index_by(.InDeX_pLaCeHoLdEr2 = group) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), fun)) |>
    dplyr::select(-.iNdEx_PlAcEhOlDeR) |>
    dplyr::rename_with(~ gsub("^.InDeX_pLaCeHoLdEr2$", index_var, .x))
}

aggregate_index_default_fun <- function(x) {
  checkmate::assert_atomic_vector(x)

  if (is.numeric(x) && !all(nchar(x) == 1, na.rm = TRUE)) {
    mean(x, na.rm = TRUE)
  } else {
    # Return value that has highest number of occurrences (mode).
    y <- x[which(!is.na(x))]
    unique <- unique(y)
    unique[which.max(tabulate(match(y, unique)))]
  }
}
