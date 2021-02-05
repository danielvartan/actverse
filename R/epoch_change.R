#' Return the epoch length of an actimetry dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A `data.frame` or `tibble` object.
#' @param timestamp A string indicating the name of the timestamp
#'   variable. The timestamp variable must be class `POSIXct`.
#'
#' @return A numeric value representing the epoch length in seconds.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' epoch(test_log)
#' }

epoch <- function(data,
                  timestamp = "timestamp") {

    checkmate::assert_data_frame(data, min.rows = 2)
    checkmate::assert_string(timestamp)
    checkmate::assert_subset(timestamp, names(data))
    checkmate::assert_posixct(data[[timestamp]], all.missing = FALSE)

    as.numeric(difftime(data[[timestamp]][2], data[[timestamp]][1],
                        units = "secs"))

}

#' Change the epoch length of an actimetry dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __UNDER DEVELOPMENT__
#'
#' Some computations may require a specific epoch length. This function change
#' the epoch length using a method defined in the `method` variable. Be aware
#' that this kind of data transformation can produce misleading results.
#'
#' Note that some methods are not valid when dealing with a lower or greater
#' epoch value than the original. Use [epoch()] to know the
#' epoch value of your data.
#'
#' @details
#'
#' ## `new_epoch` argument
#'
#' Note that when the new epoch is smaller than the original, the value must be
#' an integer divisor of the original epoch. (e.g. if the original epoch is 60,
#' the possible values are 30 (60 %% 30 = 0), 20, 15, 12, 10, 6, 5, 4, 3 , 2 and
#' 1)
#'
#' ## `method` argument
#'
#' This argument accepts only the following values.
#'
#' * `"divide"`: divide and equally distribute the value from the original epoch
#' into the new epochs created (only when new epoch `<` original epoch)
#' * `"same"`: assign the same value from the original epoch to the new epochs
#' created (only when new epoch `<` original epoch)
#' * `"first"`: use the first value from original epochs grouped in the new
#' epoch interval (only when new epoch `>` original epoch)
#' * `"last"`: use the last value from original epochs grouped in the new epoch
#' interval (only when new epoch `>` original epoch) (only when new epoch `>`
#' original epoch)
#' * `"sum"`: sum all the values in the new epoch interval (only when new epoch
#' `>` original epoch)
#' * `"mean"`: compute the mean value in the new epoch interval (only when new
#' epoch `>`original epoch)
#' * `"median"`: compute the median value in the new epoch interval (only when
#' new epoch `>`original epoch)
#'
#' Note that some methods are not valid when dealing with a lower or greater
#' epoch value than the original. You can use the [epoch()] function to know the
#' epoch value of your data.
#'
#' ## `variable` argument
#'
#'  When using a tidy dataset, the method will be used only for the activity and
#'  light exposure variables. The variables `orientation`, `body_temperature`,
#'  and `external_temperature` will be summarized by the `mean` method. If
#'  there's a `TRUE` value for the `event` variable in the time interval
#'  assessed, the final value will be `TRUE`. For the `state` variable, the
#'  first value of the time interval assessed will be used.
#'
#' @section Representations:
#'
#' The representations below can help you visualize the right kind of
#' transformation that you need.
#'
#'
#' * Example of an epoch length
#'
#' ```
#'        00:00:00   00:01:00   00:02:00
#' ----------|----------|----------|----------
#'           |----------|
#'              epoch
#' ```
#'
#' * Example for when New epoch `<` Original epoch
#'
#' ```
#'        00:00:00   00:00:30   00:01:00
#' ----------|----------|----------|----------
#'           |---------------------|
#'                original epoch
#'           |----------|
#'            new epoch
#' ```
#'
#' * Example for when New epoch `>` Original epoch
#'
#' ```
#'        00:00:00   00:00:30   00:01:00
#' ----------|----------|----------|----------
#'           |----------|
#'          original epoch
#'           |---------------------|
#'                   new epoch
#' ```
#'
#' @param data A `data.frame` or `tibble` object.
#' @param new_epoch An [integerish][checkmate::test_integerish()] `integer` or
#'   `numeric` object indicating the number of seconds of the new epoch.
#' @param method (optional) A string indicating the method that must be used for
#'   data summarization. See Details section to learn more.
#' @param timestamp (optional) A string indicating the name of the timestamp
#'   variable. The timestamp variable must have class `POSIXct`.
#' @param variable (optional) a string indicating a specific variable in `data`.
#'   If not assigned, its required that the data conforms to the proposed data
#'   structure of the [tidy_data()] function.
#'
#' @return A `tibble` object.
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom rlang !! := .data
#' @noRd
#'
#' @examples
#' \dontrun{
#' epoch_change(test_log)
#' }

epoch_change <- function(data,
                         new_epoch,
                         method = "sum",
                         timestamp = "timestamp",
                         variable = NULL) {

    # To do list:
    #
    # * Optimize the matching process

    # Check arguments -----

    checkmate::assert_data_frame(data, min.rows = 2)
    checkmate::assert_integerish(new_epoch)

    choices <- c("divide", "same", "first", "last", "sum", "mean", "median")
    checkmate::assert_choice(method, choices)

    check <- c("divide", "same", "sum", "mean", "median")

    if (!(is.null(variable)) && !(is.numeric(variable)) &&
        !(method %in% check)) {
        stop(paste(variable, "is not numeric. The method selected only works",
                   "with numeric variables."), call. = FALSE)
    }

    assert_any_na(data[[timestamp]])
    checkmate::assert_subset(timestamp, names(data))

    if (!is.null(variable)) {
        checkmate::assert_subset(variable, names(data))
    }

    # Select data -----

    if (!(is.null(variable))) {
        out <- data %>%
            dplyr::select(!!as.symbol(timestamp), !!as.symbol(variable))
    } else {
        out <- data
    }

    # Set values -----

    new_epoch <- as.integer(new_epoch)

    # Set epoch values -----

    epoch <- epoch(data)

    if (identical(new_epoch, epoch)) {
        return(out)
    }

    if (new_epoch < epoch && !(method %in% c("divide"))) {
        stop("method is not a valid. See documentation.", call. = FALSE)
    }

    if (new_epoch < epoch && !(epoch %% new_epoch == 0)) {
        stop(paste("new_epoch value is not a divisor of the original epoch.",
                   "See documentation."), call. = FALSE)
    }

    epoch_count <- as.numeric(lubridate::as.duration(
        dplyr::last(data[[timestamp]]) -
            dplyr::first(data[[timestamp]]))) / epoch

    if (!(length(data[[timestamp]]) < epoch_count * 1.01 &&
        length(data[[timestamp]]) > epoch_count * 0.9)) {
        stop(paste("Timestamp length don't match with epoch count.",
                   "Some of your data appears to be missing", call. = FALSE))
    }

    new_epoch_count <- ceiling(epoch_count * (epoch / new_epoch))


    # Set helper functions -----

    divide <- function(x) {
        sum(x) / (epoch / new_epoch)
    }

    same <- function(x) {
        sum(x)
    }

    # Change epoch length -----

    ## Set intervals

    intervals <- dplyr::tibble(
        onset = seq(data[[timestamp]][1], by = new_epoch,
                    length.out = new_epoch_count),
        offset = seq(data[[timestamp]][1] + lubridate::seconds(new_epoch - 1),
                     by = new_epoch,
                     length.out = new_epoch_count)) %>%
        dplyr::mutate(interval = lubridate::interval(start = .data$onset,
                                                     end = .data$offset),
                      n = seq(nrow(intervals))) %>%
        dplyr::relocate(.data$n)

    ## Match original epochs to new epochs

    out$interval <- NA

    for (i in seq_len(nrow(out))) {

        out$interval[i] <-
            which(out[[timestamp]][i] %within% intervals$interval)

    }

    ## Add new cases (if (new_epoch < epoch))

    if (new_epoch < epoch) {

        subset <- dplyr::tibble(!!as.symbol(timestamp) := intervals$onset,
                                interval = intervals$n)
        out <- dplyr::anti_join(subset, out, by = "interval") %>%
            dplyr::full_join(out, by = c("interval", timestamp)) %>%
            dplyr::arrange(timestamp)

    }

    ## Summarize data

    message("Summarizing data. This may take a while, don't be alarmed.")

    if (!(is.null(variable))) {

        out <- out %>%
            dplyr::group_by(.data$interval) %>%
            dplyr::summarise(
                dplyr::across(!!as.symbol(variable), get(method))) %>%
            dplyr::mutate(!!as.symbol(timestamp) := intervals$offset) %>%
            dplyr::select(-.data$interval) %>%
            dplyr::relocate(!!as.symbol(timestamp))

    } else {

        out <- out %>%
            dplyr::group_by(.data$interval) %>%
            dplyr::summarise(
                dplyr::across(c(.data$x_axis:.data$zcm_n,
                                .data$light:.data$uvb_light), sum),
                dplyr::across(.data$orientation:.data$external_temperature,
                              mean),
                dplyr::across(.data$event, ~ .x == TRUE),
                dplyr::across(.data$state, dplyr::first)) %>%
            dplyr::mutate(timestamp = intervals$offset,
                          pim_n = .data$pim / new_epoch,
                          tat_n = .data$tat / new_epoch,
                          zcm_n = .data$zcm / new_epoch) %>%
            dplyr::select(-.data$interval) %>%
            dplyr::relocate(.data$timestamp, .data$x_axis:.data$zcm_n,
                            .data$orientation:.data$external_temperature,
                            .data$light:.data$uvb_light,
                            .data$event:.data$state)

    }

    # Return output -----

    out

}
