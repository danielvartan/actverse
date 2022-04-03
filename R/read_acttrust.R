#' Read, tidy, and validate an ActTrust file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `read_acttrust()` allows you to read, tidy, and validate an ActTrust file in
#' a consistent and easy manner. You can see the output data structure in
#' [`?acttrust`][actverse::acttrust].
#'
#' This function was created just for convenience. If it doesn't work for your
#' file, we recommend using the [readr](https://readr.tidyverse.org/) package to
#' load it to R.
#'
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://www.condorinst.com.br/).
#'
#' @details
#'
#' ## `Regularize` parameter
#'
#' It's common to find some uneven epoch/interval in ActTrust data files. This
#' occurs because the actigraph internal clock can go off by some seconds while
#' recording, and can become an issue while doing some computations. By using
#' `regularize == TRUE`, `read_acttrust()` find and correct those
#' irregularities.
#'
#' It's important to note that this process will only work if a clear
#' epoch/periodicity can be found in the data. Regularization is made by
#' aggregating the values between epochs, averaging values for numeric variables
#' and assigning the most frequent value (mode) for integer or other type of
#' variables.
#'
#' We highly recommend regularizing your data and interpolating possible gaps
#' that can be found. If you not interpolate the gaps, they will be assign
#' as `NA`.
#'
#' ## Offwrist and offwrist interpolation
#'
#' Unless `interpolate_offwrist == TRUE`, `read_acttrust()` will transform any
#' offwrist data into missing data (`NA`).
#'
#' Some analysis can go off with missing data (e.g., Sokolove & Bushell's
#' \eqn{\chi^{2}}{chi square} periodogram, Non-parametric Circadian Rhythm
#' Analysis (NPCRA)). It's always best to interpolate missing values. They will
#' still going to be classified as offwrist in the `state` variable.
#'
#' ## Data wrangling
#'
#' The process of _tiding_ a dataset is understood as transforming it in input
#' data, like described in Loo and Jonge (2018). It's a very similar process of
#' tiding data described in the workflow proposed by Wickham and Grolemund
#' (n.d.).
#'
#' The process of _validating_ a dataset is understood as detecting invalid
#' data, by checking whether data satisfies certain assumptions from domain
#' knowledge, to then,  removing or, if possible, fixing them. This process can
#' be considered as part of the process of transforming data, described in the
#' workflow proposed by Wickham and Grolemund (n.d.).
#'
#' To learn more about the concept of tidy data, see Wickham (2014) and Wickham
#' and Grolemund (n.d.). You can find more about data validation and error
#' location in Loo and Jonge (2018).
#'
#' @param file A string with the file path for the ActTrust data. If not
#'   assigned, a dialog window will open allowing the user to browse and select
#'   a file.
#' @param tz A string that specifies which time zone to parse the dates/time
#'   with. The string must be a time zone that is recognized by the user's OS.
#'   For more information, see [`?timezone`][OlsonNames()] (default: `"UTC"`).
#' @param regularize (optional) a [`logical`][logical()] value indicating if the
#'   function must correct irregular intervals (__highly recommended__). See
#'   more about it in the Details section (default: `TRUE`).
#' @param interpolate_gaps (optional) a [`logical`][logical()] value indicating
#'   if the function must interpolate the gaps found when `regularize == TRUE`
#'   (__highly recommended__) (default: `TRUE`).
#' @param interpolate_gaps_method (optional) a string indicating the method of
#'   interpolation for the gaps. It follows the same valid values as
#'   `interpolate_offwrist_method` (default: `"locf"`).
#' @param interpolate_offwrist (optional) a [`logical`][logical()] value
#'   indicating if the function must interpolate offwrist states (when the
#'   device is removed) (__highly recommended__) (default: `TRUE`).
#' @param interpolate_offwrist_method (optional) a string indicating the method
#'   of interpolation for the offwrist values. Valid values:
#'   [`approx`][zoo::na.approx()], [`locf`][zoo::na.locf()],
#'   [`overall_mean`][zoo::na.aggregate()], [`spline`][zoo::na.spline()]. See
#'   the links to learn more about them (default: `"overall_mean"`).
#'
#' @return A [tsibble][tsibble::tsibble()] object. The data structure can be
#' found in [`?acttrust`][actverse::acttrust].
#'
#' @template references_a
#' @family utility functions
#' @export
#'
#' @examples
#' read_acttrust(raw_data("acttrust.txt"))
read_acttrust <- function(file = file.choose(), tz = "UTC",
                          regularize = TRUE,
                          interpolate_gaps = TRUE,
                          interpolate_gaps_method = "locf",
                          interpolate_offwrist = TRUE,
                          interpolate_offwrist_method = "overall_mean") {
    method_choices <- c("approx", "locf", "overall_mean", "spline")

    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_choice(tz, OlsonNames())
    checkmate::assert_flag(regularize)
    checkmate::assert_flag(interpolate_gaps)
    checkmate::assert_choice(interpolate_gaps_method, method_choices)
    checkmate::assert_flag(interpolate_offwrist)
    checkmate::assert_choice(interpolate_offwrist_method, method_choices)
    require_pkg("readr", "stringr")

    if (any(c(interpolate_offwrist, interpolate_gaps))) {
        require_pkg("zoo")
    }

    read_acttrust_data(file) %>%
        tidy_acttrust_data(tz = tz) %>%
        validate_acttrust_data(regularize, interpolate_gaps,
                               interpolate_gaps_method, interpolate_offwrist,
                               interpolate_offwrist_method)
}

read_acttrust_data <- function(file = file.choose()) {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    require_pkg("readr", "stringr")

    if (stringr::str_detect(readLines(file, n = 1),
                            "Condor Instruments Report")) {
        skip <- 25
        n <- 26
    } else {
        skip <- 0
        n <- 1
    }

    if (stringr::str_detect(readLines(file, n = n)[n], ";")) {
        delim <- ";"
    } else {
        delim <- "\t"
    }

    file %>%
        readr::read_delim(delim = delim,
                          na = c("", " ", "NA"),
                          col_types = readr::cols(.default = "c"),
                          skip = skip,
                          trim_ws = TRUE)
}

tidy_acttrust_data <- function(data,
                               tz = "America/Sao_Paulo") {
    checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    checkmate::assert_choice(tz, OlsonNames())
    require_pkg("stringr", "utils")

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- NULL
    timestamp <- date <- time <- ms <- NULL
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

    out <- data %>%
        dplyr::mutate(dplyr::across(.fns = as.character)) %>%
        dplyr::mutate(dplyr::across(.fns = stringr::str_trim)) %>%
        dplyr::rename(ms = MS,
                      pim = PIM, pim_n = PIMn, tat = TAT, tat_n = TATn,
                      zcm = ZCM, zcm_n = ZCMn,
                      orientation = ORIENTATION,
                      wrist_temperature = TEMPERATURE,
                      external_temperature = `EXT TEMPERATURE`,
                      light = LIGHT, ambient_light = `AMB LIGHT`,
                      red_light = `RED LIGHT`, green_light = `GREEN LIGHT`,
                      blue_light = `BLUE LIGHT`, ir_light = `IR LIGHT`,
                      uva_light = `UVA LIGHT`, uvb_light = `UVB LIGHT`,
                      event = EVENT, state = STATE)

    if (!(names(out)[1] == "DATE")) {
        out <- out %>% dplyr::rename(timestamp = `DATE/TIME`)
    } else {
        out <- out %>%
            dplyr::rename(date = DATE, time = TIME) %>%
            dplyr::mutate(timestamp = (paste(date, time))) %>%
            dplyr::select(-date, -time)
    }

    if (isTRUE(any(utils::head(stringr::str_detect(out$wrist_temperature, ","),
                               100)))) {
        out <- out %>%
            dplyr::mutate(dplyr::across(
                dplyr::vars(-timestamp, -date, -time),
                function(x) stringr::str_replace(x, ",", "\\."))
            )
    }

    out %>%
        dplyr::mutate(timestamp = lubridate::dmy_hms(timestamp, tz = tz) +
                          lubridate::dmilliseconds(as.numeric(ms)),
                      event = as.integer(event),
                      state = as.integer(state)) %>%
        dplyr::mutate(dplyr::across(-timestamp, as.numeric)) %>%
        dplyr::select(-ms, -pim_n, -tat_n, -zcm_n) %>%
        dplyr::relocate(
            timestamp,
            pim, tat, zcm,
            orientation, wrist_temperature, external_temperature,
            light, ambient_light, red_light, green_light, blue_light, ir_light,
            uva_light, uvb_light,
            event, state
        )
}

validate_acttrust_data <- function(data, regularize = TRUE,
                                   interpolate_gaps = TRUE,
                                   interpolate_gaps_method = "locf",
                                   interpolate_offwrist = FALSE,
                                   interpolate_offwrist_method = "overall_mean") {
    method_choices <- c("approx", "locf", "overall_mean", "spline")

    checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    checkmate::assert_flag(regularize)
    checkmate::assert_flag(interpolate_gaps)
    checkmate::assert_choice(interpolate_gaps_method, method_choices)
    checkmate::assert_flag(interpolate_offwrist)
    checkmate::assert_choice(interpolate_offwrist_method, method_choices)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- timestamp <- NULL

    # TO DO: use the 'validate' package.

    out <- data
    regular <- out %>%
        tsibble::tsibble(index = timestamp, regular = FALSE) %>%
        test_regularity(strict = TRUE)

    if (isTRUE(regular)) {
        out <- out %>% tsibble::tsibble(index = timestamp, regular = TRUE)
    } else {
        out <- out %>% tsibble::tsibble(index = timestamp, regular = FALSE)
    }

    if (isFALSE(regular) && isTRUE(regularize)) {
        out <- out %>% regularize_acttrust_data(
            interpolate_gaps = interpolate_gaps,
            interpolate_gaps_method = interpolate_gaps_method)
    }

    out <- out %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^state$"),
            ~ dplyr::if_else(state == 4, na_as(.x), .x)))

    offwrist_ints <- find_offwrist_intervals(out)

    if (!any(is.na(offwrist_ints))) {
        cli::cli_alert_info(paste0(
            "Found {.strong {cli::col_red(length(offwrist_ints))}} ",
            "offwrist{?s} blocks in the time series."
        ))
    }

    if (!any(is.na(offwrist_ints)) && isTRUE(interpolate_offwrist)) {
        out <- out %>%
            dplyr::mutate(dplyr::across(
                !dplyr::matches(
                    "^timestamp$|^orientation$|^event$|^state$"),
                ~ interpolate_na(.x, timestamp,
                                 interpolate_offwrist_method)
            ))
    }

    out %>%
        dplyr::mutate(dplyr::across(
            dplyr::matches("^orientation$|^event$"),
            ~ dplyr::if_else(is.na(.x), 0, .x)
            )) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^orientation$"),
            ~ dplyr::if_else(.x < 0, na_as(.x), .x)
            )) %>%
        dplyr::mutate(dplyr::across(
            dplyr::ends_with("_temperature"),
            ~ dplyr::if_else(.x >= 100, na_as(.x), .x)
            ))
}

find_offwrist_intervals <- function(data) {
    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- timestamp <- state <- offwrist_start <- offwrist_end <- NULL

    out <- data %>%
        dplyr::mutate(
            offwrist_start = state == 4 & !dplyr::lag(state) == 4,
            offwrist_end = state == 4 & !dplyr::lead(state) == 4,
            offwrist_start = dplyr::if_else(
                timestamp == dplyr::first(timestamp) & state == 4, TRUE,
                offwrist_start),
            offwrist_end = dplyr::if_else(
                timestamp == dplyr::last(timestamp) & state == 4, TRUE,
                offwrist_end)
            ) %>%
        dplyr::select(timestamp, offwrist_start, offwrist_end) %>%
        dplyr::filter(offwrist_start == TRUE | offwrist_end == TRUE)

    if (nrow(out) == 0) {
        lubridate::as.interval(NA)
    } else {
        purrr::map2(out$timestamp[out$offwrist_start == TRUE],
                    out$timestamp[out$offwrist_end == TRUE],
                    ~ lubridate::interval(.x, .y)) %>%
            purrr::reduce(c)
    }
}

regularize_acttrust_data <- function(data, interpolate_gaps = TRUE,
                                     interpolate_gaps_method = "locf") {
    method_choices <- c("approx", "locf", "overall_mean", "spline")

    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data)
    checkmate::assert_flag(interpolate_gaps)
    checkmate::assert_choice(interpolate_gaps_method, method_choices)

    # R CMD Check variable bindings fix (see: http://bit.ly/3bliuam)

    . <- .from <- .to <- NULL

    out <- data

    epoch <- out %>%
        find_epoch(0.9) %>%
        magrittr::extract2("best_match")

    epoch_unit <- period_to_string(epoch)

    if (is.na(epoch) || is.na(epoch_unit)) {
        cli::cli_abort(paste0(
            "The data was not regularized because no clear epoch was ",
            "found. Your {.strong {cli::col_blue('data')}} must have at ",
            "least {.strong {cli::col_red('90%')}} of regularity. See ",
            "'?find_epoch()' to check data regularity."
        ))
    } else {
        out <- out %>% aggregate_index(epoch_unit) %>%
            dplyr::mutate(dplyr::across(
                !dplyr::matches("^timestamp$"),
                ~ dplyr::if_else(is.nan(.x), na_as(.x), .x)
            ))

        count_gaps <- tsibble::count_gaps(out) %>%
            dplyr::mutate(paste = paste(.from, .to, sep = "/")) %>%
            magrittr::extract2("paste")

        if (!length(count_gaps) == 0) {
            cli::cli_alert_info(paste0(
                "Found {.strong {cli::col_red(length(count_gaps))}} ",
                "gap{?s} in the time series: ",
                "{head(count_gaps, 5)} (showing up to a total of 5 values)."
            ))
        }

        out <- out %>% tsibble::fill_gaps() %>%
            dplyr::mutate(dplyr::across(
                dplyr::matches("^orientation$|^event$"),
                ~ dplyr::if_else(is.na(.x), 0, .x)))

        if (isTRUE(interpolate_gaps)) {
            out <- out %>%
                dplyr::mutate(dplyr::across(
                    !dplyr::matches("^timestamp$|^orientation$|^event$"),
                    ~ interpolate_na(.x, timestamp, interpolate_gaps_method)
                )) %>%
                dplyr::mutate(dplyr::across(
                    !dplyr::matches("^timestamp$|^orientation$"),
                    ~ dplyr::if_else(.x < 0, 0, .x)
                ))
        }
    }

    out
}

# data %>% ggplot2::ggplot(ggplot2::aes(x = timestamp, y = pim)) +
#     ggplot2::geom_line()
