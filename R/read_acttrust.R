#' Read and tidy an ActTrust file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `read_acttrust()` allows you to read and tidy an ActTrust file in a
#' consistent and easy manner.
#'
#' `read_acttrust()` was created just for convenience. If this function doesn't
#' work for your file, we recommend using the
#' [readr](https://readr.tidyverse.org/) package to load it to R.
#'
#' ActTrust is a trademark of
#' [Condor Instruments](https://www.condorinst.com.br/).
#'
#' @param file A string with the file path for the ActTrust dataset. If not
#'   assigned, a dialog window will open allowing the user to browse and select
#'   a file.
#' @param tz A string that specifies which time zone to parse the dates/time
#'   with. The string must be a time zone that is recognized by the user's OS.
#'   For more information, see [`?timezone`][?OlsonNames()].
#'   (default: `"America/Sao_Paulo"`).
#'
#' @return An [xts][xts::xts()] object.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' read_acttrust()
#' }
read_acttrust <- function(file = file.choose(),
                          tz = "America/Sao_Paulo") {
    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_choice(tz, OlsonNames())
    require_pkg("readr", "stringr")

    read_acttrust_data(file) %>% tidy_acttrust_data(tz = tz)
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
                          trim_ws = TRUE) %>%
        dplyr::as_tibble()
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
        dplyr::mutate(timestamp = lubridate::dmy_hms(timestamp, tz = tz)) %>%
        dplyr::mutate(dplyr::across(-timestamp, as.numeric)) %>%
        dplyr::relocate(
            timestamp, ms,
            pim, pim_n, tat, tat_n, zcm, zcm_n,
            orientation, wrist_temperature, external_temperature,
            light, ambient_light, red_light, green_light, blue_light, ir_light,
            uva_light, uvb_light,
            event, state
        ) %>%
        xts::xts(x = .[, -1], order.by = .$timestamp, tzone = tz)
}
