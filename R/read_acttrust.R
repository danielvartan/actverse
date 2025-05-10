#' Read, tidy, and validate an ActTrust file
#'
#' @description
#'
#' `read_acttrust()` allows you to read, tidy, and validate an ActTrust file in
#' a consistent and easy manner. You can see the output data structure in
#' [`?acttrust`][actverse::acttrust].
#'
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://condorinst.com/en/).
#'
#' @details
#'
#' ## `regularize` parameter
#'
#' ActTrust data files may have uneven epochs or intervals due to small drifts
#' in the device's internal clock. These irregularities can affect analyses.
#' Setting `regularize = TRUE` in `read_acttrust()` will detect and correct
#' such issues by regularizing the time intervals.
#'
#' Regularization is performed only if a clear epoch or periodicity is found.
#' During this process, values within each epoch are aggregated: numeric
#' variables are averaged, and categorical or integer variables are assigned
#' their most frequent value (mode).
#'
#' Any gaps in the time series are filled with `NA`, and the corresponding
#' `state` is set to `9`.
#'
#' ## Offwrist data
#'
#' `read_acttrust()` will convert all offwrist data (where `state == 4`) to
#' missing values (`NA`). These data points will remain classified as offwrist
#' in the `state` variable.
#'
#' @param file (optional) A string with the file path for the ActTrust data. If
#'   not assigned, a dialog window will open allowing the user to browse and
#'   select a file (default: [`tk_choose.files()`][tcltk::tk_choose.files()]).
#' @param tz (optional) A string that specifies which time zone to parse the
#'   dates/time with. The string must be a time zone that is recognized by the
#'   user's OS. For more information see [`?timezone`][OlsonNames()]
#'   (default: `"UTC"`).
#' @param regularize (optional) A [`logical`][logical()] value indicating if the
#'   function must correct irregular intervals (**strongly recommended**). See
#'   more about it in the Details section (default: `TRUE`).
#'
#' @return A [tsibble][tsibble::tsibble()] object. The data structure can be
#'   found in [`?acttrust`][actverse::acttrust].
#'
#' @family read/write functions
#' @export
#'
#' @examples
#' get_raw_data("acttrust.txt") |> read_acttrust()
read_acttrust <- function(
  file = tcltk::tk_choose.files(mult = FALSE),
  tz = "UTC",
  regularize = TRUE
) {
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file)
  checkmate::assert_choice(tz, OlsonNames())
  checkmate::assert_flag(regularize)

  read_acttrust_data(file) |>
    tidy_acttrust_data(tz = tz) |>
    validate_acttrust_data(regularize)
}

read_acttrust_data <- function(
  file = tcltk::tk_choose.files(mult = FALSE)
) {
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file)

  cli::cli_progress_step("Reading data")

  lines <- readLines(file, n = 100)

  if (grepl("Condor Instruments Report", lines[1])) {
    for (i in seq(2, length(lines))) {
      if (
        grepl("^\\+\\-\\-\\-", lines[i]) &&
          grepl("\\-\\-\\-\\+$", lines[i])
      ) {
        break
      }
    }

    skip <- i
    n <- i + 1
  } else {
    skip <- 0
    n <- 1
  }

  if (grepl(";", readLines(file, n = n)[n])) {
    delim <- ";"
  } else {
    delim <- "\t"
  }

  file |>
    readr::read_delim(
      delim = delim,
      na = c("", " ", "NA"),
      col_types = readr::cols(.default = "c"),
      skip = skip,
      trim_ws = TRUE
    )
}

tidy_acttrust_data <- function(data, tz = "UTC") {
  checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
  checkmate::assert_choice(tz, OlsonNames())

  # R CMD Check variable bindings fix
  # nolint start
  timestamp <- ms <- NULL
  pim <- pim_n <- tat <- tat_n <- zcm <- zcm_n <- NULL
  orientation <- wrist_temperature <- external_temperature <- NULL
  light <- ambient_light <- red_light <- green_light <- blue_light <- NULL
  ir_light <- uva_light <- uvb_light <- NULL
  event <- state <- NULL

  `DATE/TIME` <- DATE <- TIME <- MS <- NULL
  PIM <- PIMn <- TAT <- TATn <- ZCM <- ZCMn <- NULL
  ORIENTATION <- TEMPERATURE <- `EXT TEMPERATURE` <- NULL
  LIGHT <- `AMB LIGHT` <- `RED LIGHT` <- `GREEN LIGHT` <- NULL
  `BLUE LIGHT` <- `IR LIGHT` <- `UVA LIGHT` <- `UVB LIGHT` <- NULL
  EVENT <- STATE <- NULL
  # nolint end

  cli::cli_progress_step("Tidying data")

  out <-
    data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = trimws
      )
    ) |>
    dplyr::rename(
      ms = MS,
      pim = PIM,
      pim_n = PIMn,
      tat = TAT,
      tat_n = TATn,
      zcm = ZCM,
      zcm_n = ZCMn,
      orientation = ORIENTATION,
      wrist_temperature = TEMPERATURE,
      external_temperature = `EXT TEMPERATURE`,
      light = LIGHT,
      ambient_light = `AMB LIGHT`,
      red_light = `RED LIGHT`,
      green_light = `GREEN LIGHT`,
      blue_light = `BLUE LIGHT`,
      ir_light = `IR LIGHT`,
      uva_light = `UVA LIGHT`,
      uvb_light = `UVB LIGHT`,
      event = EVENT,
      state = STATE
    )

  if (!(names(out)[1] == "DATE")) {
    out <- out |> dplyr::rename(timestamp = `DATE/TIME`)
  } else {
    out <-
      out |>
      dplyr::mutate(timestamp = (paste(DATE, TIME))) |>
      dplyr::select(-DATE, -TIME)
  }

  if (any(grepl(",", out$wrist_temperature)[1:100], na.rm = TRUE)) {
    out <-
      out |>
      dplyr::mutate(
        dplyr::across(
          !dplyr::matches("^timestamp$"),
          ~ gsub(",", "\\.", .x)
        )
      )
  }

  out |>
    dplyr::mutate(
      timestamp =
        lubridate::dmy_hms(timestamp, tz = tz) +
          lubridate::dmilliseconds(as.numeric(ms)), #nolint
      event = as.integer(event),
      state = as.integer(state)
    ) |>
    dplyr::mutate(
      dplyr::across(
        -timestamp,
        as.numeric
      )
    ) |>
    dplyr::select(-ms, -pim_n, -tat_n, -zcm_n) |>
    dplyr::relocate(
      timestamp,
      pim, tat, zcm,
      orientation, wrist_temperature, external_temperature,
      light, ambient_light, red_light, green_light, blue_light, ir_light,
      uva_light, uvb_light,
      event, state
    )
}

validate_acttrust_data <- function(data, regularize = TRUE) {
  checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
  checkmate::assert_flag(regularize)

  # R CMD Check variable bindings fix
  # nolint start
  . <- timestamp <- NULL
  # nolint end

  cli::cli_progress_step("Validating data")

  out <- data

  regular <-
    out |>
    tsibble::tsibble(index = timestamp, regular = FALSE) |>
    test_regularity(strict = TRUE)

  if (isTRUE(regular)) {
    out <- out |> tsibble::tsibble(index = timestamp, regular = TRUE)
  } else {
    out <- out |> tsibble::tsibble(index = timestamp, regular = FALSE)
  }

  if (isFALSE(regular) && isTRUE(regularize)) {
    out <- out |> regularize_acttrust_data()
  }

  out <-
    out |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::matches("^timestamp$|^state$"),
        ~ dplyr::if_else(state == 4, na_as(.x), .x)
      )
    )

  offwrist_ints <- find_offwrist_intervals(out)

  if (!any(is.na(offwrist_ints))) {
    cli::cli_alert_info(paste0(
      "Found {.strong {cli::col_red(length(offwrist_ints))}} ",
      "offwrist{?s} blocks in the time series. ",
      "All values were set as {.strong NA}."
    ))
  }

  out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("^orientation$|^event$"),
        ~ dplyr::if_else(is.na(.x), 0, .x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::matches("^timestamp$|^orientation$"),
        ~ dplyr::if_else(.x < 0, na_as(.x), .x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_temperature"),
        ~ dplyr::if_else(.x >= 100, na_as(.x), .x)
      )
    )
}

regularize_acttrust_data <- function(data) {
  assert_tsibble(data, min_rows = 2, min_cols = 2)
  assert_index_class(data, c("Date", "POSIXt"))

  # R CMD Check variable bindings fix
  # nolint start
  . <- .from <- .to <- state <- NULL
  # nolint end

  epoch <-
    data |>
    find_epoch(0.9) |>
    magrittr::extract2("best_match")

  epoch_unit <- period_to_string(epoch)

  if (is.na(epoch) || is.na(epoch_unit)) {
    cli::cli_alert_warning(paste0(
      "The data was not regularized because no clear epoch was ",
      "found. Your {.strong {cli::col_blue('data')}} must have at ",
      "least {.strong {cli::col_red('90%')}} of regularity. See ",
      "'?find_epoch()' to check data regularity."
    ))

    data
  } else {
    out <-
      data |>
      aggregate_index(epoch_unit) |>
      dplyr::mutate(
        dplyr::across(
          !dplyr::matches("^timestamp$"),
          ~ dplyr::if_else(is.nan(.x), na_as(.x), .x)
        )
      )

    count_gaps <-
      tsibble::count_gaps(out) |>
      dplyr::mutate(paste = paste(.from, .to, sep = "/")) |>
      magrittr::extract2("paste")

    if (!length(count_gaps) == 0) {
      cli::cli_alert_info(paste0(
        "Found {.strong {cli::col_red(length(count_gaps))}} ",
        "gap{?s} in the time series: ",
        "{head_(count_gaps, 5)} (showing up to a total of 5 values)."
      ))
    }

    out |>
      tsibble::fill_gaps() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::matches("^orientation$|^event$"),
          ~ dplyr::if_else(is.na(.x), 0, .x)
        )
      ) |>
      dplyr::mutate(
        state = dplyr::if_else(is.na(state), 9, state)
      )
  }
}

find_offwrist_intervals <- function(data) {
  assert_tsibble(data, min_rows = 2, min_cols = 2)
  assert_index_class(data, c("Date", "POSIXt"))
  checkmate::assert_subset(c("timestamp", "state"), names(data))

  # R CMD Check variable bindings fix
  # nolint start
  . <- timestamp <- state <- offwrist_start <- offwrist_end <- NULL
  # nolint end

  out <-
    data |>
    dplyr::mutate(
      offwrist_start = state == 4 & !dplyr::lag(state) == 4,
      offwrist_end = state == 4 & !dplyr::lead(state) == 4,
      offwrist_start = dplyr::if_else(
        timestamp == dplyr::first(timestamp) & state == 4, TRUE,
        offwrist_start
      ),
      offwrist_end = dplyr::if_else(
        timestamp == dplyr::last(timestamp) & state == 4, TRUE,
        offwrist_end
      )
    ) |>
    dplyr::select(timestamp, offwrist_start, offwrist_end) |>
    dplyr::filter(offwrist_start == TRUE | offwrist_end == TRUE)

  if (nrow(out) == 0) {
    lubridate::as.interval(NA)
  } else {
    purrr::map2(
      out$timestamp[out$offwrist_start == TRUE],
      out$timestamp[out$offwrist_end == TRUE],
      ~ lubridate::interval(.x, .y)
    ) |>
      purrr::reduce(c)
  }
}

read_acttrust_ <- function(
  file = tcltk::tk_choose.files(mult = FALSE),
  tz = "America/Sao_Paulo"
) {
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file)
  checkmate::assert_choice(tz, OlsonNames())

  out <- read_acttrust(
    file = file,
    tz = tz,
    regularize = TRUE
  )

  cli::cli_progress_step("Interpolating data")

  out |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::matches("^timestamp$|^orientation$|^event$|^state$"),
        ~ dplyr::if_else(state == 9, na_locf(.x, fill_na_tips = TRUE), .x)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::matches("^timestamp$|^orientation$|^event$"),
        ~ na_weekly_mean(.x, timestamp, week_start = 1)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::matches("^timestamp$|^orientation$"),
        ~ dplyr::if_else(.x < 0, 0, .x)
      )
    )
}
