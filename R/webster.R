#' Classifies activity using the Webster algorithm
#'
#' @description
#'
#' This function uses a already loaded raw actimetry dataframe.
#'
#' @param data A `data.frame` object.
#'
#' @return The same data with two columns: STAGE and AWAKE.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' data <- readr::read_csv2("./inst/extdata/test_log.txt", skip = 25)
#' data <- webster(data)
#' data$timestamp = lubridate::dmy_hms(paste(data$DATE, data$TIME))
#' ggplot2::qplot(timestamp, AWAKE, data = data, geom = "area",
#'                xlab = "Timestamp", ylab = "State")
#' }
webster <- function(data) {

    # To do list:
    #
    # * Add new devices
    # * Add parameters for unconformed data loading

    # Revisor: Daniel
    #
    # * Vetorize a função. Não use `data` como argumento.
    # * Melhore a documentação, conforme as instruções.
    # * Não use `data$`, é pouco eficiente. Use `mutate()`, se necessário.
    # * No R, as funções retornam a última expressão avaliada. Geralmente
    #   não é necessário aplicar o output a uma variável para depois chamá-la.
    # * Evite `ifelse()`, dê preferência par `dplyr::if_else()` ou
    #   `dplyr::case_when()`.
    # * Não esqueça de especificar as funções com `::`.
    # * Veja diff para ver mais indicações.

    data %>%
        dplyr::select(.data$DATE, .data$TIME, .data$ZCM) %>%
        dplyr::mutate(
            s51 = dplyr::lag(.data$ZCM, n = 1),
            s52 = dplyr::lag(.data$ZCM, n = 2),
            s53 = dplyr::lag(.data$ZCM, n = 3),
            s54 = dplyr::lag(.data$ZCM, n = 4),
            s55 = dplyr::lag(.data$ZCM, n = 5),
            s41 = dplyr::lead(.data$ZCM, n = 1),
            s42 = dplyr::lead(.data$ZCM, n = 2),
            s43 = dplyr::lead(.data$ZCM, n = 3),
            s44 = dplyr::lead(.data$ZCM, n = 4)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            d = 0.036 * sum((0.07 * .data$s55), (0.08 * .data$s54),
                            (0.10 * .data$s53), (0.11 * .data$s52),
                            (0.12 * .data$s51), (0.14 * .data$ZCM),
                            (0.09 * .data$s41), (0.09 * .data$s42),
                            (0.09 * .data$s43), (0.10 * .data$s44))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            STAGE = dplyr::if_else(.data$d < 1, "Sleep", "Awake"),
            AWAKE = dplyr::if_else(.data$d < 1, 0, 1)) %>%
        dplyr::select(.data$DATE, .data$TIME, .data$ZCM, .data$STAGE,
                      .data$AWAKE)

}
