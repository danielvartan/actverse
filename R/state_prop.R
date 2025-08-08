#' Compute the proportion of time spent in a specific state
#'
#' @description
#'
#' `state_prop()` computes the proportion of time spent in one or more specified
#' states from a [`tsibble`][tsibble::tsibble()] object. It quantifies the
#' fraction of time each day (or epoch) that the subject spends in the target
#' state(s), such as sleeping or awake, across the observed period.
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param state_col (optional) A string indicating which column of `data` have
#'   the states data (default: `"state"`).
#' @param state_values (optional) An
#'   [`integerish`][checkmate::assert_integerish] vector indicating which state
#'   values are considered as the target state(s) for calculating the proportion
#'   (default: `c(1, 2)`).
#'
#' @return A [`tsibble`][tsibble::tsibble()] object with the following columns:
#'
#' - `time`: An [`hms`][hms::hms()] object representing the time of day.
#' - `state`: A [`list`][base::list()] with a [`factor`][base::factor()] of the
#'   state at each time point, indicating whether it is the target state or not.
#' - `prop`: A [`numeric`][base::numeric()] vector representing the proportion
#'   of time spent in the target state at each time point. This value is
#'   calculated as the number of occurrences of the target state divided by
#'   the total number of observations at that time point.
#'
#' @family sleep statistics functions
#' @export
#'
#' @examples
#' library(curl)
#' library(dplyr)
#' library(ggplot2)
#' library(scales)
#'
#' if (has_internet()) {
#'   file <- get_from_zenodo(
#'     doi = "10.5281/zenodo.4898822",
#'     dir = tempdir(),
#'     file = "processed.txt"
#'   )
#'
#'   data <- read_acttrust(file, tz = "America/Sao_Paulo")
#'   state_prop_data <- data |> state_prop() # Sleeping + Resting states
#'
#'   state_prop_data |> print()
#'
#'   state_prop_data |> pull(prop) |> summary() |> print()
#'
#'   data |>
#'     actogram(
#'       col = "pim",
#'       days = -1,
#'       latitude = -23.55065,
#'       longitude = -46.63338,
#'       double_plot = TRUE
#'     )
#'
#'   state_prop_data |>
#'     mutate(per = prop * 100) |>
#'     ggplot(ggplot2::aes(x = time, y = per)) +
#'     geom_smooth(color = "#FC2913") +
#'     labs(
#'       x = "Time of day (Hour)",
#'       y = "Percentage of time asleep (%)",
#'     ) +
#'     scale_x_time(
#'       breaks = breaks_width("6 hours"),
#'       labels = label_time("%-H") # Use "%#H" for Windows
#'     ) +
#'     scale_y_continuous(limits = c(NA, 100)) +
#'     actverse:::get_actverse_theme()
#' }
state_prop <- function(
  data,
  state_col = "state",
  state_values = c(1, 2)
) {
  assert_tsibble(data)
  assert_regularity(data, strict = TRUE)
  checkmate::assert_choice(state_col, names(data))
  checkmate::assert_integerish(data[[state_col]])
  checkmate::assert_integerish(state_values, min.len = 1)
  checkmate::assert_subset(state_values, data[[state_col]])

  # R CMD Check variable bindings fix
  # nolint start
  timestamp <- time <- state <- NULL
  # nolint end

  data |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      timestamp = !!as.symbol(tsibble::index_var(data)),
      state = !!as.symbol(state_col)
    ) |>
    dplyr::mutate(
      time = timestamp |> hms::as_hms(),
      state = dplyr::if_else(state %in% state_values, "State", "Other")
    ) |>
    dplyr::summarize(state = list(state), .by = time) |>
    dplyr::mutate(
      prop = state |> purrr::map_dbl(\(x) prop(x, "State")),
      state =
        state |>
        purrr::map(
          \(x) factor(x, levels = c("Other", "State"), ordered = FALSE)
        )
    ) |>
    dplyr::arrange(time) |>
    tsibble::as_tsibble(index = time, regular = TRUE)
}
