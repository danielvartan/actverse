#' Classifies activity in stages of sleep and wakefulness using the Cole-Kripke
#' algorithm
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' This function uses a already loaded raw actimetry dataframe.
#'
#' @details
#'
#' To use this package, the dataframe has to be tidy and organized, using the
#' tidy_data function You can still use the tools available without tiding your
#' data, but by doing that you may spend more time using the functions, and may
#' be more prone to error.
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
#' sadeh_data(data)


cole_kripke_data <- function(data){
    data <- data%>%
        select(DATE, TIME, ZCM)
        data<- mutate(
            data
            , s51 = lag(ZCM, n = 1)
            , s52 = lag(ZCM, n = 2)
            , s53 = lag(ZCM, n = 3)
            , s54 = lag(ZCM, n = 4)
            , s91 = lead(ZCM, n = 1)
            , s92 = lead(ZCM, n = 2)
        )
        data$PS = 0.00001 * ((404 * data$s54) + (598 * data$s53) + (326 * data$s52) +
                                 (441 * data$s51) + (1408 * data$ZCM) +
                                 (508 * data$s91) + (350 * data$s92))


    data$STAGE <- ifelse(data$PS < 1, "sleep", "awake")
    #data$AWAKE <- ifelse(data$PS < 1, 0, 1)
    #aqui esta dando esse erro
    #Error in data$PS :     objeto de tipo 'closure' não possível dividir em subconjuntos

    data <- data%>%
        select(DATE, TIME,ZCM,STAGE)
    data
}


