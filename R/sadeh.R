#' Classifies activity using the Sadeh algorithm
#'
#' @description
#'
#' `sadeh_data()` uses a already loaded raw actimetry dataframe.
#'
#' @param data A data frame.
#' @param year Type of algorithm, published in the following year, 1 for the one
#'   published in 1989 and 2 for the one in 1994.
#'
#' @return The same data with two columns: STAGE and AWAKE.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils head
#' @noRd
#'
#' @examples
#' \dontrun{
#' sadeh_data(data)
#' }
sadeh <- function(data, year) {

    # To do:
    #
    # * Add new devices
    # * Add parameters for unconformed data loading

    # Revisor: Daniel
    #
    # * Vetorize a função. Não use `data` como argumento.
    # * Melhore a documentação, conforme as instruções.
    # * Não use `data$`, é pouco eficiente. Use `mutate()`, se necessário.
    # * A função não functiona. Rever o código.
    # * Somente exporte uma função se ela funcionar.
    # * No R, as funções retornam a última expressão avaliada. Geralmente
    #   não é necessário aplicar o output a uma variável para depois chamá-la.
    # * Evite `ifelse()`, dê preferência par `dplyr::if_else()` ou
    #   `dplyr::case_when()`.
    # * Não esqueça de especificar as funções com `::`.
    # * Não use `apply()` ou `sapply()`, há side effects. Se for usar uma
    #   função do tipo `apply`, use somente `lapply()`, `vapply()` ou
    #   `mapply()` (com SIMPLIFY = FALSE)
    # * Veja diff para ver mais indicações.

    data <- data %>% dplyr::select(.data$DATE, .data$TIME, .data$ZCM)

    if (year == 1){
        data <- data %>%
            dplyr::mutate(
                s51 = dplyr::lag(.data$ZCM, n = 1),
                s52 = dplyr::lag(.data$ZCM, n = 2),
                s53 = dplyr::lag(.data$ZCM, n = 3),
                s54 = dplyr::lag(.data$ZCM, n = 4),
                s55 = dplyr::lag(.data$ZCM, n = 5),
                s91 = dplyr::lead(.data$ZCM, n = 1),
                s92 = dplyr::lead(.data$ZCM, n = 2),
                s93 = dplyr::lead(.data$ZCM, n = 3),
                s94 = dplyr::lead(.data$ZCM, n = 4),
                s95 = dplyr::lead(.data$ZCM, n = 5),
                s96 = dplyr::lead(.data$ZCM, n = 6),
                s97 = dplyr::lead(.data$ZCM, n = 7),
                s98 = dplyr::lead(.data$ZCM, n = 8),
                s99 = dplyr::lead(.data$ZCM, n = 9))

        data$s5 <- apply(data[,4:8], 1, stats::sd)
        data$s9 <- apply(data[,9:17], 1, stats::sd)
        data$m2 <- apply(data[,9:10], 1, min)
        data$s2 <- apply(data[,4:5], 1, stats::sd)

        data <- data %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                ps = 4.532 - ((0.06828 * .data$ZCM) - (0.0385 * .data$s5) -
                                  (0.038 * .data$s9) + (0.0298 * .data$m2) -
                                  (0.0299 * .data$s2))
            ) %>%
            dplyr::ungroup()
    } else if (year == 2) {
        data <- data %>%
            dplyr::mutate(
                p51 = dplyr::lag(.data$ZCM, n = 1),
                p52 = dplyr::lag(.data$ZCM, n = 2),
                p53 = dplyr::lag(.data$ZCM, n = 3),
                p54 = dplyr::lag(.data$ZCM, n = 4),
                p55 = dplyr::lag(.data$ZCM, n = 5),
                s51 = dplyr::lead(.data$ZCM, n = 1),
                s52 = dplyr::lead(.data$ZCM, n = 2),
                s53 = dplyr::lead(.data$ZCM, n = 3),
                s54 = dplyr::lead(.data$ZCM, n = 4),
                s55 = dplyr::lead(.data$ZCM, n = 5))

        data$w5 <- apply(data[, 3:13], 1, mean)

        data <- data %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
                nat = sum(dplyr::c_across("ZCM":"s55") > 50 &
                              dplyr::c_across("ZCM":"s55") < 100 )) %>%
            dplyr::ungroup()

        data$sd6 <- apply(data[, 3:8], 1, stats::sd)
        data$logact <- log(data[, 3] + 1)

        # ???
        # data$PS <- 1
        #     7.601 - ((0.065 * data$w5) -
        #                         (1.08 * data$nat) - (0.056 * data$sd6)
        #                     - (0.703 * data$logact))
    }

    data %>%
        dplyr::mutate(
            STAGE = dplyr::if_else(.data$ps >= 0, "Sleep", "Awake"),
            AWAKE = dplyr::if_else(data$ps >= 0, 0, 1)) %>%
        dplyr::select(.data$DATE, .data$TIME, .data$ZCM, .data$STAGE,
                      .data$AWAKE)

}
