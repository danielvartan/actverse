warn_any_missing <- function(x, name = deparse(substitute(x))) {
  if (any(is.na(x))) {
    cli::cli_alert_warning(
      paste0(
        "{.strong {cli::col_red(name)}} has missing values. ",
        "Results may diverge."
      )
    )
  } else {
    TRUE
  }
}

test_leq <- function(x, y) {
  checkmate::assert_number(x)
  checkmate::assert_number(y)

  x <= y
}

check_leq <- function(
  x,
  y,
  name_x = deparse(substitute(x)),
  name_y = deparse(substitute(y))
) {
  checkmate::assert_number(x)
  checkmate::assert_number(y)

  if (!x <= y) {
    paste0(
      glue::single_quote(name_x),
      " must be less or equal to ",
      glue::single_quote(name_y)
    )
  } else {
    TRUE
  }
}

assert_leq <- checkmate::makeAssertionFunction(check_leq)

test_tsibble <- function(
  x,
  min_rows = NULL,
  min_cols = NULL,
  null_ok = FALSE
) {
  checkmate::assert_int(min_rows, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_cols, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (!tsibble::is_tsibble(x)) {
    FALSE
  } else if (
    (!is.null(min_rows) && !any(nrow(x) >= min_rows)) ||
      (!is.null(min_cols) && !any(ncol(x) >= min_cols))
  ) {
    FALSE
  } else {
    TRUE
  }
}

check_tsibble <- function(
  x,
  min_rows = NULL,
  min_cols = NULL,
  null_ok = FALSE,
  name = deparse(substitute(x))
) {
  checkmate::assert_int(min_rows, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_cols, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(null_ok)

  if (is.null(x) && isTRUE(null_ok)) {
    TRUE
  } else if (is.null(x) && isFALSE(null_ok)) {
    paste0(glue::single_quote(name), " cannot be 'NULL'")
  } else if (!tsibble::is_tsibble(x)) {
    paste0(
      "Must be of type 'tbl_ts' (tsibble), not ",
      class_collapse(x)
    )
  } else if (!is.null(min_rows) && !any(nrow(x) >= min_rows)) {
    paste0(
      "Must have at least ",
      min_rows,
      " rows, but has ",
      nrow(x),
      " rows"
    )
  } else if (!is.null(min_cols) && !any(ncol(x) >= min_cols)) {
    paste0(
      "Must have at least ",
      min_cols,
      " cols, but has ",
      ncol(x),
      " cols"
    )
  } else {
    TRUE
  }
}

assert_tsibble <- checkmate::makeAssertionFunction(check_tsibble)

test_index_class <- function(
  x,
  classes = c("Date", "POSIXt", "yearweek", "yearmonth", "yearquarter")
) {
  assert_tsibble(x)
  checkmate::assert_character(classes)

  if (!any(classes %in% class(x[[tsibble::index_var(x)]]), na.rm = TRUE)) {
    FALSE
  } else {
    TRUE
  }
}

check_index_class <- function(
  x,
  classes = c("Date", "POSIXt", "yearweek", "yearmonth", "yearquarter"),
  name = deparse(substitute(x))
) {
  assert_tsibble(x)
  checkmate::assert_character(classes)

  if (!any(classes %in% class(x[[tsibble::index_var(x)]]), na.rm = TRUE)) {
    paste0(
      "Must have an index of class ",
      inline_collapse(classes, "or"),
      ", not ",
      class_collapse(x[[tsibble::index_var(x)]])
    )
  } else {
    TRUE
  }
}

assert_index_class <- checkmate::makeAssertionFunction(check_index_class)

test_regularity <- function(x, threshold = 0.99, strict = FALSE) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
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

check_regularity <- function(
  x,
  threshold = 0.99,
  strict = FALSE,
  name = deparse(substitute(x))
) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)
  checkmate::assert_flag(strict)

  prevalence <- find_epoch(x, threshold)$prevalence

  if (isTRUE(strict) && !nrow(prevalence) == 1) {
    paste0(
      glue::single_quote(name), " ",
      "must be strictly regular. ",
      "See '?find_epoch' to learn more"
    )
  } else if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
    paste0(
      glue::single_quote(name), " ",
      "must have a regularity equal or ",
      "greater than ", threshold * 100, "%. ",
      "See '?find_epoch' to learn more"
    )
  } else {
    TRUE
  }
}

assert_regularity <- checkmate::makeAssertionFunction(check_regularity)

warn_regularity <- function(
  x,
  threshold = 0.99,
  strict = FALSE,
  name = deparse(substitute(x))
) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)
  checkmate::assert_flag(strict)

  prevalence <- find_epoch(x, threshold)$prevalence

  if (isTRUE(strict) && !nrow(prevalence) == 1) {
    cli::cli_alert_warning(
      paste0(
        "{.strong {cli::col_red(name)}} is not strictly regular. ",
        "The output may diverge. ",
        "See '?find_epoch' to learn more."
      )
    )
  } else if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
    cli::cli_alert_warning(
      paste0(
        "{.strong {cli::col_red(name)}} does not have a regularity ",
        "equal or greater than {.strong {threshold * 100}%}. ",
        "The output may diverge. ",
        "See '?find_epoch' to learn more."
      )
    )
  } else {
    TRUE
  }
}

test_clear_epoch <- function(x, threshold = 0.9) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  prevalence <- find_epoch(x, threshold)$prevalence

  if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
    FALSE
  } else {
    TRUE
  }
}

check_clear_epoch <- function(
  x,
  threshold = 0.9,
  name = deparse(substitute(x))
) {
  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  checkmate::assert_number(threshold, lower = 0.001, upper = 1)

  prevalence <- find_epoch(x, threshold)$prevalence

  if (!any(prevalence$proportion >= threshold, na.rm = TRUE)) {
    paste0(
      glue::single_quote(name), " does not present a clear ",
      "epoch/periodicity. See '?find_epoch' to learn more"
    )
  } else {
    TRUE
  }
}

assert_clear_epoch <- checkmate::makeAssertionFunction(check_clear_epoch)

test_epoch_compatibility <- function(x, unit) {
  unit_choices <- c(
    "microsecond", "millisecond", "second", "minute", "hour", "day", "week",
    "month", "quarter", "year"
  )

  unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  assert_clear_epoch(x, 0.7)
  checkmate::assert_choice(unit, unit_choices)

  # R CMD Check variable bindings fix
  # nolint start
  . <- proportion <- NULL
  # nolint end

  epochs <-
    find_epoch(x)$prevalence |>
    dplyr::filter(proportion >= 0.7) |>
    magrittr::extract2("epoch")

  if (!all(as.numeric(string_to_period(unit)) >= epochs, na.rm = TRUE)) {
    FALSE
  } else {
    TRUE
  }
}

check_epoch_compatibility <- function(
  x,
  unit,
  name = deparse(substitute(x))
) {
  unit_choices <- c(
    "microsecond", "millisecond", "second", "minute",
    "hour", "day", "week", "month", "quarter",
    "year"
  )
  unit_choices <- unit_choices |> append(paste0(unit_choices, "s"))

  assert_tsibble(x, min_rows = 2, min_cols = 2)
  assert_index_class(x)
  assert_clear_epoch(x, 0.7)
  checkmate::assert_choice(unit, unit_choices)

  # R CMD Check variable bindings fix
  # nolint start
  . <- proportion <- NULL
  # nolint end

  epochs <-
    find_epoch(x)$prevalence |>
    dplyr::filter(proportion >= 0.7) |>
    magrittr::extract2("epoch")

  if (!grepl("s$", unit)) unit <- paste0(unit, "s")

  if (!all(as.numeric(string_to_period(unit)) >= epochs, na.rm = TRUE)) {
    paste0(
      "The epoch/periodicity present in ", glue::single_quote(name),
      "do not allow to aggregate it in ", unit, ". ",
      "See '?find_epoch' to learn more"
    )
  } else {
    TRUE
  }
}

assert_epoch_compatibility <-
  checkmate::makeAssertionFunction(check_epoch_compatibility)
