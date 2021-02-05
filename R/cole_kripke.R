#' Classifies activity using the Cole-Kripke algorithm
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' __UNDER DEVELOPMENT__
#'
#' @param data A data frame.
#'
#' @return The same data with two columns: STAGE and AWAKE.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' data <- readr::read_csv2("./inst/extdata/test_log.txt", skip = 25)
#' data <- cole_kripke(data)
#' data$timestamp <- lubridate::dmy_hms(paste(data$DATE, data$TIME))
#' ggplot2::qplot(timestamp, AWAKE, data = data, geom = "area",
#'                xlab = "Timestamp", ylab = "State")
#' }
cole_kripke <- function(data) {

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
            s91 = dplyr::lead(.data$ZCM, n = 1),
            s92 = dplyr::lead(.data$ZCM, n = 2)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            ps = 0.00001 * sum((404 * .data$s54), (598 * .data$s53),
                               (326 * .data$s52), (441 * .data$s51),
                               (1408 * .data$ZCM), (508 * .data$s91),
                               (350 * .data$s92))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            STAGE = dplyr::if_else(.data$ps < 1, "Sleep", "Awake"),
            AWAKE = dplyr::if_else(.data$ps < 1, 0, 1)) %>%
        dplyr::select(.data$DATE, .data$TIME, .data$ZCM, .data$STAGE,
                      .data$AWAKE)

}
