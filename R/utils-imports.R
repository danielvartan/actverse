# Borrowed from `rutils`: github.com/danielvartan/rutils (Adapted)
class_collapse <- function(x) {
  glue::single_quote(paste0(class(x), collapse = "/"))
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
drop_na <- function(x) {
  checkmate::assert_atomic(x)

  x[which(!is.na(x))]
}

# Borrowed from `lubritime`: github.com/danielvartan/lubritime
flat_posixt_date <- function(
    x, #nolint
    base = as.Date("1970-01-01"),
    force_tz = TRUE,
    tz = "UTC"
  ) {
  prettycheck::assert_posixt(x, null_ok = FALSE)
  checkmate::assert_date(base, len = 1, all.missing = FALSE)
  checkmate::assert_flag(force_tz)
  checkmate::assert_choice(tz, OlsonNames())

  out <-
    x |>
    lubridate::`date<-`(base) |>
    c()

  if (isTRUE(force_tz)) {
    out |> lubridate::force_tz(tz)
  } else {
    out
  }
}

# Borrowed from `lubritime`: github.com/danielvartan/lubritime
flat_posixt_hour <- function(
    x, #nolint
    base = hms::parse_hm("00:00"),
    force_tz = TRUE,
    tz = "UTC"
  ) {
  prettycheck::assert_posixt(x)
  prettycheck::assert_hms(base, any_missing = FALSE)
  prettycheck::assert_length(base, len = 1)
  checkmate::assert_flag(force_tz)
  checkmate::assert_choice(tz, OlsonNames())

  out <-
    x |>
    lubridate::date() |>
    paste0(" ", base) |>
    lubridate::as_datetime(tz = lubridate::tz(x))

  if (isTRUE(force_tz)) {
    out |> lubridate::force_tz(tz)
  } else {
    out
  }
}

# Borrowed from `rutils`: github.com/danielvartan/rutils (Adapted)
inline_collapse <- function(
  x,
  last = "and",
  single_quote = TRUE,
  serial_comma = TRUE
) {
  checkmate::assert_string(last)
  checkmate::assert_flag(single_quote)
  checkmate::assert_flag(serial_comma)

  if (isTRUE(single_quote)) x <- glue::single_quote(x)

  if (length(x) <= 2 || isFALSE(serial_comma)) {
    glue::glue_collapse(x, sep = ", ", last = paste0(" ", last, " "))
  } else {
    glue::glue_collapse(x, sep = ", ", last = paste0(", ", last, " "))
  }
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
na_as <- function(x) {
  UseMethod("na_as")
}

#' @export
na_as.logical <- function(x) as.logical(NA)

#' @export
na_as.character <- function(x) as.character(NA)

#' @export
na_as.integer <- function(x) as.integer(NA)

#' @export
na_as.numeric <- function(x) as.numeric(NA)

#' @export
na_as.Duration <- function(x) lubridate::as.duration(NA)

#' @export
na_as.Period <- function(x) lubridate::as.period(NA)

#' @export
na_as.difftime <- function(x) {
  as.difftime(as.numeric(NA), units = attributes(x)$units)
}

#' @export
na_as.hms <- function(x) hms::as_hms(NA)

#' @export
na_as.Date <- function(x) as.Date(NA)

#' @export
na_as.hms <- function(x) hms::as_hms(NA)

#' @export
na_as.POSIXct <- function(x) as.POSIXct(NA, tz = attributes(x)$tzone)

#' @export
na_as.POSIXlt <- function(x) as.POSIXlt(NA, tz = attributes(x)$tzone)

#' @export
na_as.Interval <- function(x) {
  lubridate::interval(NA, NA, tz = attributes(x)$tzone)
}

# Borrowed from `rutils`: github.com/danielvartan/rutils
shush <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    suppressMessages(suppressWarnings(x))
  } else {
    x
  }
}
