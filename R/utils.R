head_ <- function(x, n = 6) {
  checkmate::assert_int(n, lower = 1)

  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) < n) n <- length(x)
    x[seq_len(n)]
  } else if (is.data.frame(x)) {
    if (nrow(x) < n) n <- nrow(x)
    x[seq_len(n), ]
  } else {
    if (length(x) < n) n <- length(x)
    x[seq_len(n)]
  }
}

period_ <- function(num, unit = "seconds") {
  unit_choices <- c(
    "microsecond", "millisecond", "second", "minute", "hour", "day", "week",
    "month", "quarter", "year"
  )

  unit_choices <- unit_choices |> append(paste0(unit_choices, "s"))

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
  string_choices <- c(
    "microsecond", "millisecond", "second", "minute", "hour", "day", "week",
    "month", "quarter", "year"
  )

  string_choices <- string_choices |> append(paste0(string_choices, "s"))
  irregularity_choices <- c("min", "mean", "max")

  checkmate::assert_choice(string, string_choices)
  checkmate::assert_choice(irregularity, irregularity_choices)

  if (irregularity == "min") {
    month <- lubridate::ddays(28)
    quarter <- lubridate::ddays(28) + (lubridate::ddays(30) * 2)
    year <- lubridate::ddays(365)
  } else if (irregularity == "mean") {
    month <- lubridate::dmonths()
    quarter <- lubridate::dmonths(3)
    year <- lubridate::dyears()
  } else if (irregularity == "max") {
    month <- lubridate::ddays(31)
    quarter <- lubridate::ddays(31) * 3
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
  checkmate::assert_number(period, na.ok = TRUE)

  if (is.na(period)) {
    as.character(NA)
  } else {
    # Workaround for when 'period' is of class 'Duration'
    period <- as.numeric(period)
    out <- as.character(NA)

    period_choices <- c(
      "microseconds", "milliseconds", "seconds", "minutes",
      "hours", "days", "weeks"
    )

    for (i in period_choices) {
      if (period == as.numeric(string_to_period(i))) out <- i
    }

    out
  }
}

get_en_locale <- function() {
  current_locale <- Sys.getlocale("LC_TIME")
  withr::defer(Sys.setlocale("LC_TIME", current_locale))

  locale_values <- c(
    "en_US.UTF-8", "en_US.utf8", "en_US", "en-US", "en",
    "English_United States"
  )

  out <- character()

  for (i in locale_values) {
    test <- Sys.setlocale("LC_TIME", locale = i) |> suppressWarnings()

    if (!test == "" && Sys.getlocale("LC_TIME") == i) {
      out <- i

      break
    }
  }

  out
}
