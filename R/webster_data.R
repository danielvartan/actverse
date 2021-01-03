#' Classifies activity in stages of sleep and wakefulness using the Webster algorithm
#'
#' This function uses a already loaded raw actimetry dataframe.
#'
#' To use this package, the dataframe has to be tidy and organized, using the tidy_data function
#' You can still use the tools available without tiding your data,
#' but by doing that you may spend more time using the functions, and may be more prone to error.
#'
#' @param data A data frame.
#' @return The same data with two columns: STAGE and AWAKE.
#'
#' @note
#'
#' To do list:
#'
#' * Add new devices
#' * Add parameters for unconformed data loading
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang := .data
#' @importFrom utils head
#' @examples
#' webster_data(data)


webster_data <- function(data){
    data <- data%>%
        select(DATE, TIME, ZCM)
    data<- mutate(
        data
        , s51 = lag(ZCM, n = 1)
        , s52 = lag(ZCM, n = 2)
        , s53 = lag(ZCM, n = 3)
        , s54 = lag(ZCM, n = 4)
        , s55 = lag(ZCM, n = 5)
        , s41 = lead(ZCM, n = 1)
        , s42 = lead(ZCM, n = 2)
        , s43 = lead(ZCM, n = 3)
        , s44 = lead(ZCM, n = 4)

    )
    data$D =  0.036*((0.07 * data$s55) +
                         (0.08 * data$s54) +
                         (0.10 * data$s53) +
                         (0.11 * data$s52) +
                         (0.12 * data$s51) +
                         (0.14 * data$ZCM) +
                         (0.09 * data$s41) +
                         (0.09 * data$s42) +
                         (0.09 * data$s43) +
                         (0.10 * data$s44))

    data$STAGE <- ifelse(data$D >= 0, "sleep", "awake")
    data$AWAKE <- ifelse(data$D >= 0, 0, 1)

    data <- data%>%
        select(DATE, TIME,ZCM,STAGE,AWAKE)
    data
}
