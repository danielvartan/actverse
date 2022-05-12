#' Adapt and write a `tsibble` to a readable ActTrust file
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `write_acttrust()` allows you to adapt and write a
#' [`tsibble`][tsibble::tsibble()] object to a readable ActTrust file.
#'
#' Note that your data must conform to a predefined structure. See
#' [`?acttrust`][actverse::acttrust] to learn more.
#'
#' ActTrust is a trademark of
#' [Condor Instruments Ltda](https://www.condorinst.com.br/).
#'
#' @details
#'
#' ## Requirements
#'
#' `write_acttrust()` requires the [`readr`][readr::readr-package] package. If
#' you don't already have it installed, you can install it with:
#'
#' ```
#' install.packages("readr")
#' ````
#'
#'  ## `NA` values
#'
#' `write_acttrust()` will transform any `NA` value to `0`. This is because
#' the ActTrust software only deals with numeric values.
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object with a
#'   [`POSIXt`][as.POSIXct()] index. The ActTrust software only deals with this
#'   type of index.
#' @param file A string with a file path to write to.
#' @param delim (optional) a string indicating the delimiter that must be used
#'   to separate values. Valid delimiters are: `";"` and `"\t"` (default:
#'   `";"`).
#' @param header (optional) a string indicating the path to a file (usually
#'   the raw data file) with the ActTrust header. This is not mandatory,
#'   the ActTrust software can read files without it (default: `NULL`).
#'
#' @return An invisible `NULL`. This function don't aim to return values.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' acttrust
#'
#' file <- tempfile()
#' header <- raw_data("acttrust.txt")
#'
#' write_acttrust(acttrust, file, header = header)
#'
#' readLines(file, n = 30)
write_acttrust <- function(data, file, delim = ";", header = NULL) {
    assert_tsibble(data, min.rows = 2, min.cols = 2)
    assert_index_class(data, c("POSIXt"))
    assert_clear_epoch(data, 0.9)
    checkmate::assert_string(file)
    checkmate::assert_choice(delim, c(";", "\t"))
    checkmate::assert_string(header, null.ok = TRUE)
    require_pkg("readr")

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    acttrust <- acttrust
    . <- NULL
    timestamp <- date <- time <- ms <- NULL
    pim <- pim_n <- tat <- tat_n <- zcm <- zcm_n <- NULL
    orientation <- wrist_temperature <- external_temperature <- NULL
    light <- ambient_light <- red_light <- green_light <- blue_light <- NULL
    ir_light <- uva_light <- uvb_light <- NULL
    event <- state <- NULL

    epoch <- find_epoch(data)$best_match
    cols <- paste0("^timestamp$|^", tsibble::index2_var(data), "$") %>%
        grep(names(acttrust), value = TRUE, invert = TRUE)

    if (!any(cols %in% names(data))) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_red('data')}} must have at least one column ",
            "with the following names: {cols}. ",
            "See the function documentation  to learn more."
        ))
    }

    if (!identical(names(data), names(acttrust))) {
        for (i in names(acttrust)[!(names(acttrust) %in% names(data))]) {
            data <- data %>%
                dplyr::mutate(.PlAcEHoLDeR = as.numeric(NA)) %>%
                dplyr::rename_with(~ gsub("^.PlAcEHoLDeR$", i, .x))
        }
    }

    cli::cli_progress_step("Adapting data")

    out <- data %>%
        tsibble::as_tibble() %>%
        dplyr::rename(timestamp = tsibble::index2_var(data)) %>%
        dplyr::mutate(
            dplyr::across(-timestamp, as.numeric),
            dplyr::across(-timestamp,
                          ~ dplyr::if_else(is.na(.x), 0, .x))
            ) %>%
        dplyr::mutate(
            timestamp = as.character(timestamp, "%d/%m/%Y %H:%M:%S"),
            ms = 0,
            pim_n = pim / epoch,
            tat_n = tat / epoch,
            zcm_n = zcm / epoch,
            event = dplyr::if_else(event %in% 0:1,
                                   event, 0),
            state = dplyr::if_else(state %in% c(0, 1, 2, 4, 6, 7, 8), state, 0)
            ) %>%
        dplyr::transmute(
            `DATE/TIME` = timestamp, MS = ms,
            EVENT = event,
            TEMPERATURE = wrist_temperature,
            `EXT TEMPERATURE` = external_temperature,
            ORIENTATION = orientation,
            PIM = pim, PIMn = pim_n, TAT = tat, TATn = tat_n, ZCM = zcm,
            ZCMn = zcm_n,
            LIGHT = light, `AMB LIGHT` = ambient_light,
            `RED LIGHT` = red_light, `GREEN LIGHT` = green_light,
            `BLUE LIGHT` = blue_light, `IR LIGHT` = ir_light,
            `UVA LIGHT` = uva_light, `UVB LIGHT` = uvb_light,
            STATE = state)

    if (!is.null(header)) {
        checkmate::assert_file_exists(header)

        cli::cli_progress_step("Adding header")

        if (grepl("Condor Instruments Report",
                  readLines(header, n = 1))) {
            readLines(header, n = 25) %>% writeLines(file)

            cli::cli_progress_step("Writing data")

            out %>% readr::write_delim(
                file, delim = delim, na = "0", append = TRUE, col_names = TRUE)
        }
    } else {
        cli::cli_progress_step("Writing data")

        out %>% readr::write_delim(file, delim = delim, na = "0")
    }

    invisible(NULL)
}
