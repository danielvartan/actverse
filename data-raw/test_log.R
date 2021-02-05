library(tidyverse)
library(lubridate)

# usethis::use_data(test_log, overwrite = TRUE)

#' Load and tidy an actimetry raw dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' A wrapper for [load_data(]) and [tidy_data()] functions.
#'
#' If you need to set specific parameters, please use the functions
#' mentioned above.
#'
#' @param file A string with the csv file address for the raw dataset. If left
#'   with no value, a dialog window will open allowing browsing and file
#'   selection.
#' @param device A string with the name of the device used to generate the data.
#'   See [tidy_data()] documentation to know the values available.
#' @param tz a character string that specifies which time zone to parse the
#'   dates with. The string must be a time zone that is recognized by the
#'   user's OS. For more information, see `?lubridate::ymd_hms`.
#'
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' load_and_tidy(data_example("test_log.txt"))
#' }

load_and_tidy <- function(file = file.choose(),
                          device = "acttrust1",
                          tz = "America/Sao_Paulo") {

    # Load and tidy data --------------------

    output <- load_data(file, device = device) %>%
        tidy_data(device = device, tz = tz)

    # Return output --------------------

    output

}

#' Load an actimetry raw dataset from a CSV file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This is a wrapper function to help simple data loading. It's based on
#' [read_delim()] function found in the `readr` package (tidyverse). You don't need
#' to use this function if you already loaded your file.
#'
#' If this function doesn't work for your file, we recommend using the `readr`
#' package to load it.
#'
#' @param file A string with the csv file address for the raw dataset. If left
#'   with no value, a dialog window will open allowing browsing and file
#'   selection.
#' @param device A string with the name of the device use to generate the file.
#'   i.e. If this parameter is not `NULL`, all the other parameters, except
#'   `file` will be overwritten. At the moment, the only value accepted is
#'   `"acttrust1"`.
#' @param delim A string with the field separator in `file`.
#' @param na A character vector indicating values that must be interpreted
#'   as `NA`.
#' @param skip A 0 or integer value with the number of rows to skip from file.
#' @param skip_empty_rows A logical value indicating if blank rows must be
#'   ignored altogether. i.e. If this option is TRUE then blank rows will
#'   not be represented at all. If it is `FALSE` then they will be represented
#'   by `NA` values in all the columns.
#' @param trim_ws A logical value indicating if leading and trailing
#'   whitespace must be trimmed from each field before parsing it.
#'
#' @return A tibble with all variables as character.
#'
#' @importFrom magrittr %>%
#' @noRd
#'
#' @examples
#' \dontrun{
#' load_data(data_example("test_log.txt"))
#' }

load_data <- function(file = file.choose(),
                      device = "acttrust1",
                      delim = ",",
                      na = c("", " ", "NA"),
                      skip = 0,
                      skip_empty_rows = TRUE,
                      trim_ws = TRUE) {

    # Check arguments -----

    for (i in c("file", "delim", "na")) {
        if (!(is.character(get(i)))) {
            stop(paste(i, "is not class character"), call. = FALSE)
        }
    }

    for (i in c("file", "delim")) {
        if (!(nzchar(get(i)))) {
            stop(paste(i, "must be at least one character"), call. = FALSE)
        }
    }

    if (!(file.exists(file))) {
        stop(paste("file do not exist"), call. = FALSE)
    }

    for (i in c("skip")) {
        if (!(is.numeric(get(i)))) {
            stop(paste(i, "is not 0 or a integer"), call. = FALSE)
        }

        if (!(get(i) %% 1 == 0)) {
            stop(paste(i, "is not 0 or a integer"), call. = FALSE)
        }
    }

    for (i in c("trim_ws", "skip_empty_rows")) {
        if (!(is.logical(get(i)))) {
            stop(paste(i, "is not class logical"), call. = FALSE)
        }
    }

    if (!(is.null(device))) {
        if (!(device %in% c("acttrust1"))) {
            stop("device is not a valid. See documentation.", call. = FALSE)
        }
    }

    # Set values -----

    if (is.null(device)) {
        # do nothing
    } else if (device == "acttrust1") {
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

        na <- c("", " ", "NA")
        skip_empty_rows <- TRUE
        trim_ws <- TRUE
    }

    # Load data and return output -----

    file %>%
        readr::read_delim(delim = delim,
                          na = na,
                          col_types = readr::cols(.default = "c"),
                          skip = skip,
                          trim_ws = trim_ws) %>%
        tibble::as_tibble()

}

#' Tidy an actimetry raw dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function tidy a already loaded raw actimetry dataset. If your data
#' ain't loaded yet, please use the [load_data()] function or load it by
#' yourself.
#'
#' To use this package is best that your data conforms to the proposed
#' data structure of this function. You can still use the tools available
#' without tiding your data, but by doing that you may spend more time using
#' the functions, and may be more prone to error.
#'
#' @param data A data frame or tibble.
#' @param device A string with the name of the device used to generate the data.
#'   i.e. If this parameter is not `NULL`, all the other parameters, except
#'   `tz` will be overwritten. At the moment, the only value accepted is
#'   "acttrust1".
#' @param tz A character string that specifies which time zone to parse the
#'   dates with. The string must be a time zone that is recognized by the
#'   user's OS. For more information, see `?lubridate::ymd_hms`.
#'
#' @return A tibble.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := .data is_true
#' @importFrom utils head
#' @noRd
#'
#' @examples
#' \dontrun{
#' tidy_data(load_data(data_example("test_log.txt")))
#' }

tidy_data <- function(data,
                      device = "acttrust1",
                      tz = "America/Sao_Paulo") {

    # Check arguments -----

    if (!(is.data.frame(data))) {
        stop("data is not a data frame", call. = FALSE)
    }

    if (!(is.null(device))) {
        if (!(device %in% c("acttrust1"))) {
            stop("device is not a valid. See documentation.", call. = FALSE)
        }
    }

    # Transform values -----

    output <- data %>%
        dplyr::mutate_all(as.character) %>%
        dplyr::mutate_all(stringr::str_trim)

    # Adjust variables and values -----

    if (is.null(device)) {
        # do nothing
    } else if (device == "acttrust1") {

        ## Rename and create new variables

        replacement <- list(
            c("ms", "MS"),
            c("x_axis", NA),
            c("y_axis", NA),
            c("z_axis", NA),
            c("event", "EVENT"),
            c("body_temperature", "TEMPERATURE"),
            c("external_temperature", "EXT TEMPERATURE"),
            c("orientation", "ORIENTATION"),
            c("pim", "PIM"),
            c("pim_n", "PIMn"),
            c("tat", "TAT"),
            c("tat_n", "TATn"),
            c("zcm", "ZCM"),
            c("zcm_n", "ZCMn"),
            c("light", "LIGHT"),
            c("ambient_light", "AMB LIGHT"),
            c("red_light", "RED LIGHT"),
            c("green_light", "GREEN LIGHT"),
            c("blue_light", "BLUE LIGHT"),
            c("ir_light", "IR LIGHT"),
            c("uva_light", "UVA LIGHT"),
            c("uvb_light", "UVB LIGHT"),
            c("state", "STATE")
        )

        if (!(names(output)[1] == "DATE")) {
            replacement[[length(replacement) + 1]] <-
                c("timestamp", "DATE/TIME")
        } else {
            replacement[[length(replacement) + 1]] <-
                c("date", "DATE")
            replacement[[length(replacement) + 1]] <-
                c("time", "TIME")
        }

        for (i in seq_along(names(output))) {
            for (j in seq_along(replacement)) {
                if (is.na(replacement[[j]][2])) {
                    output[replacement[[j]][1]] <- as.character(NA)
                } else if (names(output)[i] == replacement[[j]][2]) {
                    names(output)[i] <- replacement[[j]][1]
                }
            }
        }

        ## Adjust decimal marker

        if (is_true(any(head((
            stringr::str_detect(output$body_temperature, ",")))))) {
            output %>% dplyr::mutate_at(
                dplyr::vars(-.data$date, -.data$time),
                function(x) stringr::str_replace(x, ",", "\\."))
        }


        ## Adjust values of the event variable (logical variable)

        for (i in c("event")) {
            output <- output %>%
                dplyr::mutate(!!as.symbol(i) := dplyr::case_when(
                    !!as.symbol(i) == "0" ~ "FALSE",
                    !!as.symbol(i) == "1" ~ "TRUE",
                    TRUE ~ !!as.symbol(i)))
        }

        ## Adjust values of the state variable (factor variable)

        output <- output %>%
            dplyr::mutate(!!as.symbol("state") := dplyr::case_when(
                .data$state == "0" ~ "Awake",
                .data$state == "1" ~ "Sleeping",
                .data$state == "2" ~ "Resting",
                .data$state == "4" ~ "Offwrist",
                .data$state == "6" ~ "Editable 1",
                .data$state == "7" ~ "Editable 2",
                .data$state == "8" ~ "Editable 3",
                TRUE ~ .data$state))

        ## Adjust date and time values

        if (!("timestamp" %in% names(output))) {
            output <- output %>%
                dplyr::mutate(timestamp = lubridate::dmy_hms(paste(
                    .data$date, .data$time), tz = tz))
        } else {
            output <- output %>%
                dplyr::mutate(timestamp = lubridate::dmy_hms(paste(
                    .data$timestamp), tz = tz))
        }
    }

    # Tidy data and return output -----

    output %>%
        dplyr::transmute(
            timestamp = .data$timestamp,
            ms = as.numeric(.data$ms),
            x_axis = as.numeric(.data$x_axis),
            y_axis = as.numeric(.data$x_axis),
            z_axis = as.numeric(.data$x_axis),
            pim = as.numeric(.data$pim),
            pim_n = as.numeric(.data$pim_n),
            tat = as.numeric(.data$tat),
            tat_n = as.numeric(.data$tat_n),
            zcm = as.numeric(.data$zcm),
            zcm_n = as.numeric(.data$zcm_n),
            orientation = as.numeric(.data$orientation),
            body_temperature = as.numeric(.data$body_temperature),
            external_temperature =
                as.numeric(.data$external_temperature),
            light = as.numeric(.data$light),
            ambient_light = as.numeric(.data$ambient_light),
            red_light = as.numeric(.data$red_light),
            green_light = as.numeric(.data$green_light),
            blue_light = as.numeric(.data$blue_light),
            ir_light = as.numeric(.data$ir_light),
            uva_light = as.numeric(.data$uva_light),
            uvb_light = as.numeric(.data$uvb_light),
            event = as.logical(.data$event),
            state = factor(.data$state, ordered = FALSE))

}
