#' Check the epoch length of an actimetry dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A data frame or tibble.
#' @param timestamp A string indicating the name of the timestamp
#'   variable (Note: the timestamp variable must be class `POSIXct`).
#' @return A numeric value representing the epoch length in seconds.
#' @importFrom utils head
#' @examples
#' \dontrun{
#' epoch(test_log)}
#' @export

epoch <- function(data,
                  timestamp = "timestamp") {

    # Check arguments --------------------

    if (!(is.data.frame(data))) {
        stop("data is not a data frame", call. = FALSE)
    }

    if (!(is.character(get(timestamp)))) {
        stop(paste(timestamp, "value is not class character"), call. = FALSE)
    }

    if (!(timestamp %in% names(data))) {
        stop(paste(timestamp, "were not found in data"), call. = FALSE)
    }

    if (!(lubridate::is.POSIXt(data[[timestamp]]))) {
        stop(paste(timestamp, "is not POSIXt"), call. = FALSE)
    }

    if (length(data[[timestamp]]) < 2) {
        stop(paste(timestamp, "length is less than 2"), call. = FALSE)
    }

    if (any(is.na(head(data[[timestamp]], 2)))) {
        stop(paste(timestamp, "first 2 values have NAs"), call. = FALSE)
    }

    # Compute epoch --------------------

    output <- as.numeric(
        lubridate::as.duration(data[[timestamp]][2] - data[[timestamp]][1]))

    # Return output --------------------

    output

}

#' Change the epoch length of an actimetry dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __UNDER DEVOLOPMENT__
#'
#' Some computations may require a specific epoch length. This function change
#' the epoch length using a method defined in the `method` variable. Be aware
#' that this kind of data transformation can produce misleading results.
#'
#' Note that some methods are not valid when dealing with a lower or greater
#' epoch value than the original. Use [epoch()] to know the
#' epoch value of your data.
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
#' @param data A data frame or tibble.
#' @param new_epoch A integer number indicating the number of seconds of the
#'   new epoch.
#'
#'   Note that when the new epoch is smaller than the original, the value
#'   must be an integer divisor of the original epoch. (e.g. if the original
#'   epoch is 60, the possible values are 30 (60 %% 30 = 0), 20,
#'   15, 12, 10, 6, 5, 4, 3 , 2 and 1)
#'
#' @param method A string indicating the method that must be used for data
#'   summarization.
#'
#' Note that some methods are not valid when dealing with a lower or greater
#' epoch value than the original. You can use the [epoch()] function to know
#' the epoch value of your data.
#'
#' This variable accepts only the following values.
#'
#' * `"divide"`: divide and equally distribute the value from the original
#'   epoch into the new epochs created (only when new epoch `<` original epoch)
#' * `"same"`: assign the same value from the original epoch to the new epochs
#'   created (only when new epoch `<` original epoch)
#' * `"first"`: use the first value from original epochs grouped in the
#'   new epoch interval (only when new epoch `>` original epoch)
#' * `"last"`: use the last value from original epochs grouped in the
#'   new epoch interval (only when new epoch `>` original epoch)
#'   (only when new epoch `>` original epoch)
#' * `"sum"`: sum all the values in the new epoch interval
#'   (only when new epoch `>` original epoch)
#' * `"mean"`: compute the mean value in the new epoch interval (only when new
#'   epoch `>`original epoch)
#' * `"median"`: compute the median value in the new epoch interval (only
#'   when new epoch `>`original epoch)
#'
#' @param timestamp A string indicating the name of the timestamp
#'   variable (Note: the timestamp variable must be class `POSIXct`).
#' @param variable a string indicating a specific variable in `data` (optional).
#'  If not assigned, its required that the data conforms to the proposed data
#'  structure of the [tidy_data()] function.
#'
#'  When using a tidy dataset, the method will be used only for the
#'  activity and light exposure variables. The variables `orientation`,
#'  `body_temperature`, and `external_temperature` will be summarized by
#'  the `mean` method. If there's a `TRUE` value for the `event` variable
#'  in the time interval assessed, the final value will be `TRUE`. For the
#'  `state` variable, the first value of the time interval assessed will be used.
#'
#' @return A tibble.
#' @examples
#' \dontrun{
#' epoch_change(test_log)}
#' @note
#'
#' To do list:
#'
#' * Optimize the matching process
#'
#' @importFrom magrittr %>%
#' @importFrom lubridate %within%
#' @importFrom dplyr first last
#' @importFrom rlang := .data is_true
#' @export

epoch_change <- function(data,
                         new_epoch,
                         method = "sum",
                         timestamp = "timestamp",
                         variable = NULL) {

    # Check arguments --------------------

    if (!(is.data.frame(data))) {
        stop("data is not a data frame", call. = FALSE)
    }

    check <- c("divide", "same", "first", "last", "sum", "mean", "median")
    if (!(method %in% check)) {
        stop("method is not a valid. See documentation.", call. = FALSE)
    }

    check <- c("divide", "same", "sum", "mean", "median")
    if (!(is.null(variable)) && !(is.numeric(variable)) &&
        !(method %in% check)) {
        stop(paste(variable, "is not numeric. The method selected only works",
                   "with numeric variables."), call. = FALSE)
    }

    if (is.null(variable) && !(is_tidy(data))) {
        stop(paste("data is not tidy"), call. = FALSE)
    }

    for (i in c("timestamp", "variable")) {
        if (i == "variable" && is.null(variable)) {
            next
        }

        if (!(get(i) %in% names(data))) {
            stop(paste(i, "were not found in data"), call. = FALSE)
        }
    }

    if (!(new_epoch %% 1 == 0)) {
        stop(paste("new_epoch must be a integer number"), call. = FALSE)
    }

    if (length(data[[timestamp]]) < 2) {
        stop(paste("timestamp variable length is less than 2"), call. = FALSE)
    }

    if (any(is.na(data[[timestamp]]))) {
        stop(paste("timestamp variable have NAs"), call. = FALSE)
    }

    # Select data --------------------

    if (!(is.null(variable))) {
        output <- data %>%
            dplyr::select(!!as.symbol(timestamp), !!as.symbol(variable))
    } else {
        output <- data
    }

    # Set epoch values --------------------

    epoch <- epoch(data)

    if (identical(new_epoch, epoch)) {
        return(output)
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


    # Set helper functions --------------------

    divide <- function(x) {
        sum(x) / (epoch / new_epoch)
    }

    same <- function(x) {
        sum(x)
    }

    # Change epoch length --------------------

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
        dplyr::relocate(n)

    ## Set progress bar

    pb <- progress::progress_bar$new(
        format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
        total = nrow(output),
        clear = FALSE,
        show_after = 0)

    pb$message(paste("Matching original epochs to new epochs.",
                     "This may take a while, don't be alarmed."))

    pb$tick(0)

    ## Match original epochs to new epochs

    output$interval <- NA

    for (i in seq_len(nrow(output))) {

        output$interval[i] <-
            which(output[[timestamp]][i] %within% intervals$interval)

        pb$tick()

    }

    ## Add new cases (if (new_epoch < epoch))

    if (new_epoch < epoch) {

        subset <- dplyr::tibble(!!as.symbol(timestamp) := intervals$onset,
                                interval = intervals$n)
        output <- dplyr::anti_join(subset, output, by = "interval") %>%
            dplyr::full_join(output, by = c("interval", timestamp)) %>%
            dplyr::arrange(timestamp)

    }

    ## Summarize data

    message("Summarizing data. This may take a while, don't be alarmed.")

    if (!(is.null(variable))) {

        output <- output %>%
            dplyr::group_by(interval) %>%
            dplyr::summarise(
                dplyr::across(!!as.symbol(variable), get(method))) %>%
            dplyr::mutate(!!as.symbol(timestamp) := intervals$offset) %>%
            dplyr::select(-interval) %>%
            dplyr::relocate(!!as.symbol(timestamp))

    } else {

        output <- output %>%
            dplyr::group_by(interval) %>%
            dplyr::summarise(
                dplyr::across(c(.data$x_axis:.data$zcm_n,
                                .data$light:.data$uvb_light), sum),
                dplyr::across(.data$orientation:.data$external_temperature,
                              mean),
                dplyr::across(.data$event, is_true),
                dplyr::across(.data$state, first)) %>%
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

    # Return output --------------------

    output

}
