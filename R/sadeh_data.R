#' Classifies activity in stages of sleep and wakefulness using the Sadeh
#' algorithm
#'
#' This function uses a already loaded raw actimetry dataframe.
#'
#' To use this package, the dataframe has to be tidy and organized, using the
#' tidy_data function You can still use the tools available without tiding your
#' data, but by doing that you may spend more time using the functions, and may
#' be more prone to error.
#'
#' @param data A data frame.
#' @param year Type of algorithm, published in the following year, 1 for the one published in 1989 and 2 for the one in 1994.
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


sadeh_data <- function(data, year){
    data <- data%>%
        select(DATE, TIME, ZCM)
    if (year == 1){
        data<- mutate(
            data
            , s51 = lag(ZCM, n = 1)
            , s52 = lag(ZCM, n = 2)
            , s53 = lag(ZCM, n = 3)
            , s54 = lag(ZCM, n = 4)
            , s55 = lag(ZCM, n = 5)
            , s91 = lead(ZCM, n = 1)
            , s92 = lead(ZCM, n = 2)
            , s93 = lead(ZCM, n = 3)
            , s94 = lead(ZCM, n = 4)
            , s95 = lead(ZCM, n = 5)
            , s96 = lead(ZCM, n = 6)
            , s97 = lead(ZCM, n = 7)
            , s98 = lead(ZCM, n = 8)
            , s99 = lead(ZCM, n = 9)
        )
        data$s5 = apply(data[,4:8],1,sd)
        data$s9 = apply(data[,9:17],1,sd)
        data$m2 = apply(data[,9:10],1,min)
        data$s2 = apply(data[,4:5],1,sd)
        data$PS = 4.532 - ((0.06828 * data$ZCM) -
                                  (0.0385 * data$s5) - (0.038 * data$s9)
                              + (0.0298 * data$m2) - (0.0299 * data$s2))
    }
    else if (year == 2){
        data<- mutate(
            data
            , p51 = lag(ZCM, n = 1)
            , p52 = lag(ZCM, n = 2)
            , p53 = lag(ZCM, n = 3)
            , p54 = lag(ZCM, n = 4)
            , p55 = lag(ZCM, n = 5)
            , s51 = lead(ZCM, n = 1)
            , s52 = lead(ZCM, n = 2)
            , s53 = lead(ZCM, n = 3)
            , s54 = lead(ZCM, n = 4)
            , s55 = lead(ZCM, n = 5)
        )

        data$W5 = apply(data[,3:13],1,mean)
        data <- data %>%
            rowwise() %>%
            mutate(NAT = sum(c_across(`ZCM` : `s55`) > 50 & c_across(`ZCM` : `s55`) < 100 )) %>%
            ungroup()
        data$SD6 <- apply(data[,3:8],1,sd)
        data$LOGACT <- log(data[,3]+1)
        data$PS <- 1
            7.601 - ((0.065 * data$W5) -
                                (1.08 * data$NAT) - (0.056 * data$SD6)
                            - (0.703 * data$LOGACT))
    }

    data$STAGE <- ifelse(data$PS >= 0, "sleep", "awake")
    data$AWAKE <- ifelse(data$PS >= 0, 0, 1)

    data <- data%>%
        select(DATE, TIME,ZCM,STAGE,AWAKE)
    data
}


