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
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://condorinst.com/en/).
#'
#' @details
#'
#' ## Requirements
#'
#' `read_acttrust()` requires the [`readr`][readr::readr-package] package. If
#' you don't already have it installed, you can install it with:
#'
#' ```
#' install.packages("readr")
#' ````
#'
#' ## `Regularize` parameter
#'
#' It's common to find some uneven epochs/intervals in ActTrust data files. This
#' occurs because the actigraph internal clock can go off by some seconds while
#' recording and can become an issue while doing some computations. By using
#' `regularize == TRUE`, `read_acttrust()` find and correct those
#' irregularities.
#'
#' It's important to note that this process will only work if a clear
#' epoch/periodicity can be found in the data. Regularization is made by
#' aggregating the values between epochs, averaging values for numeric variables
#' and assigning the most frequent value (mode) for single integer or other type
#' of variables.
#'
#' Any gap found in the time series will be assign as `NA`, with a `state`
#' value of `9`.
#'
#' ## Offwrist data
#'
#' `read_acttrust()` will transform any offwrist data (data with
#' `state == 4`) into missing data (`NA`). They will still going to be
#' classified as offwrist in the `state` variable.
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
#'   For more information see [`?timezone`][OlsonNames()] (default: `"UTC"`).
#' @param regularize (optional) a [`logical`][logical()] value indicating if the
#'   function must correct irregular intervals (__highly recommended__). See
#'   more about it in the Details section (default: `TRUE`).
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
read_acttrust <- function(file = file.choose(), tz = "UTC", regularize = TRUE) {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_choice(tz, OlsonNames())
    checkmate::assert_flag(regularize)
    require_pkg("readr")

    read_acttrust_data(file) %>%
        tidy_acttrust_data(tz = tz) %>%
        validate_acttrust_data(regularize)
}

read_acttrust_data <- function(file = file.choose()) {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    require_pkg("readr")

    cli::cli_progress_step("Reading data")

    lines <- readLines(file, n = 100)

    if (grepl("Condor Instruments Report", lines[1])) {
        for (i in seq(2, length(lines))) {
            if (grepl("^\\+\\-\\-\\-", lines[i]) &&
                grepl("\\-\\-\\-\\+$", lines[i])) {
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

    file %>%
        readr::read_delim(delim = delim,
                          na = c("", " ", "NA"),
                          col_types = readr::cols(.default = "c"),
                          skip = skip,
                          trim_ws = TRUE)
}

tidy_acttrust_data <- function(data, tz = "UTC") {
    checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    checkmate::assert_choice(tz, OlsonNames())

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter, object_name_linter.
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

    out <- data %>%
        dplyr::mutate(dplyr::across(
            .cols = dplyr::everything(),
            .fns = as.character
            )) %>%
        dplyr::mutate(dplyr::across(
            .cols = dplyr::everything(),
            .fns = trimws
            )) %>%
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
            dplyr::mutate(timestamp = (paste(DATE, TIME))) %>%
            dplyr::select(-DATE, - TIME)
    }

    if (isTRUE(any(grepl(",", out$wrist_temperature)[1:100], na.rm = TRUE))) {
        out <- out %>%
            dplyr::mutate(dplyr::across(
                !dplyr::matches("^timestamp$"),
                ~ gsub(",", "\\.", .x)
                ))
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

validate_acttrust_data <- function(data, regularize = TRUE) {
    checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    checkmate::assert_flag(regularize)

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter.
    . <- timestamp <- NULL
    # nolint end

    # TODO: use the 'validate' package.

    cli::cli_progress_step("Validating data")

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
        out <- out %>% regularize_acttrust_data()
    }

    out <- out %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^state$"),
            ~ dplyr::if_else(state == 4, rutils::na_as(.x), .x)))

    offwrist_ints <- find_offwrist_intervals(out)

    if (!any(is.na(offwrist_ints))) {
        cli::cli_alert_info(paste0(
            "Found {.strong {cli::col_red(length(offwrist_ints))}} ",
            "offwrist{?s} blocks in the time series. ",
            "All values were set as {.strong NA}."
        ))
    }

    out %>%
        dplyr::mutate(dplyr::across(
            dplyr::matches("^orientation$|^event$"),
            ~ dplyr::if_else(is.na(.x), 0, .x)
            )) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^orientation$"),
            ~ dplyr::if_else(.x < 0, rutils::na_as(.x), .x)
            )) %>%
        dplyr::mutate(dplyr::across(
            dplyr::ends_with("_temperature"),
            ~ dplyr::if_else(.x >= 100, rutils::na_as(.x), .x)
            ))
}

regularize_acttrust_data <- function(data) {
    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("Date", "POSIXt"))

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter.
    . <- .from <- .to <- state <- NULL
    # nolint end

    epoch <- data %>%
        find_epoch(0.9) %>%
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
        out <- data %>%
            aggregate_index(epoch_unit) %>%
            dplyr::mutate(dplyr::across(
                !dplyr::matches("^timestamp$"),
                ~ dplyr::if_else(is.nan(.x), rutils::na_as(.x), .x)
            ))

        count_gaps <- tsibble::count_gaps(out) %>%
            dplyr::mutate(paste = paste(.from, .to, sep = "/")) %>%
            magrittr::extract2("paste")

        if (!length(count_gaps) == 0) {
            cli::cli_alert_info(paste0(
                "Found {.strong {cli::col_red(length(count_gaps))}} ",
                "gap{?s} in the time series: ",
                "{head_(count_gaps, 5)} (showing up to a total of 5 values)."
            ))
        }

        out %>%
            tsibble::fill_gaps() %>%
            dplyr::mutate(dplyr::across(
                dplyr::matches("^orientation$|^event$"),
                ~ dplyr::if_else(is.na(.x), 0, .x))) %>%
            dplyr::mutate(state = dplyr::if_else(is.na(state), 9, state))
    }
}

find_offwrist_intervals <- function(data) {
    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("Date", "POSIXt"))
    checkmate::assert_subset(c("timestamp", "state"), names(data))

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter.
    . <- timestamp <- state <- offwrist_start <- offwrist_end <- NULL
    # nolint end

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

read_acttrust_giperbio <- function(file = file.choose(),
                                tz = "America/Sao_Paulo") {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_choice(tz, OlsonNames())

    out <- read_acttrust(file = file, tz = tz, regularize = TRUE)

    cli::cli_progress_step("Interpolating data")

    out %>% dplyr::mutate(dplyr::across(
            !dplyr::matches(
                "^timestamp$|^orientation$|^event$|^state$"),
            ~ dplyr::if_else(state == 9, na_locf(.x, fill_na_tips = TRUE), .x)
        )) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^orientation$|^event$"),
            ~ na_weekly_mean(.x, timestamp, week_start = 1)
        )) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^orientation$"),
            ~ dplyr::if_else(.x < 0, 0, .x)
        ))
}
