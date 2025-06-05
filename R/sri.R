#' Compute Phillips et al.'s Sleep Regularity Index (SRI)
#'
#' @description
#'
#' `sri()` computes the Sleep Regularity Index (SRI) as described by Phillips et
#' al. (2017) from a [`tsibble`][tsibble::tsibble()] object, quantifying the
#' consistency of sleep/wake patterns across days.
#'
#' @details
#'
#' The Sleep Regularity Index (SRI) is a measure of sleep regularity, which
#' quantifies the consistency of sleep patterns over time. It is calculated by
#' comparing the state of sleep at a given time of day with the state of sleep
#' at the same time on the previous day.
#'
#' The index is computed as follows:
#'
#' 1. For each time point, compare the sleep state (Sleeping/Awake) with the
#'    state at the same time on the previous day.
#' 2. Calculate the proportion of time points where the states match (i.e.,
#'    agreement).
#' 3. Scale this proportion to a range from -100 to 100: a score of 100
#'    indicates perfectly regular sleep/wake patterns, while a score of 0
#'    indicates completely random patterns.
#'
#' Note: While SRI values below 0 are theoretically possible (e.g., in cases of
#' highly irregular patterns such as alternating 24-hour periods of sleep and
#' wake), such occurrences are extremely rare in practice.
#'
#' See Phillips et al. (2017) to learn more about the SRI and its
#' applications in sleep research.
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param state_col (optional) A string indicating which column of `data` have
#'   the states/categories data (default: `"state"`).
#' @param sleeping_states A [`integerish`][checkmate::assert_integerish] vector
#'   indicating which state values are considered as sleeping states
#'   (default: `1`).
#' @param awake_states A [`integerish`][checkmate::assert_integerish] vector
#'   indicating which states values are considered as awake states.
#'
#' @return A [`tsibble`][tsibble::tsibble()] object with the following columns:
#'
#' - `time`: An [`hms`][hms::hms()] object representing the time of day.
#' - `state`: A [`list`][base::list()] with a [`factor`][base::factor()] of the
#'    sleep state at each time point ("Sleeping" or "Awake").
#' - `previous_state`: A [`list`][base::list()] with a
#'   [`factor`][base::factor()] of the sleep state at the same time on the
#'   previous day.
#' - `agreement`: A [`list`][base::list()] with a [`logical`][base::logical()]
#'   vector indicating whether the state matches the previous day's state.
#' - `sri`: A [`numeric`][base::numeric()] vector representing the Sleep
#'   Regularity Index (SRI). See the Details section to learn more about how
#'   the SRI is computed.
#'
#' @template references_e
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
#'   sri_data <- data |> sri()
#'
#'   sri_data |> print()
#'
#'   sri_data |> pull(sri) |> summary() |> print()
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
#'   sri_data |>
#'     ggplot(ggplot2::aes(x = time, y = sri)) +
#'     geom_smooth(color = "#FC2913") +
#'     labs(
#'       x = "Time of day (Hour)",
#'       y = "Sleep Regularity Index (SRI)"
#'     ) +
#'     scale_x_time(
#'       breaks = breaks_width("6 hours"),
#'       labels = label_time("%-H") # Use "%#H" for Windows
#'     ) +
#'     scale_y_continuous(limits = c(0, NA)) +
#'     actverse:::get_actverse_theme()
#' }
sri <- function(
  data,
  state_col = "state",
  sleeping_states = 1,
  awake_states = c(0, 2)
) {
  assert_tsibble(data)
  assert_regularity(data, strict = TRUE)
  checkmate::assert_choice(state_col, names(data))
  checkmate::assert_integerish(data[[state_col]])
  checkmate::assert_integerish(sleeping_states, min.len = 1)
  checkmate::assert_subset(sleeping_states, data[[state_col]])
  checkmate::assert_integerish(awake_states, min.len = 1)
  checkmate::assert_subset(awake_states, data[[state_col]])
  prettycheck::assert_posixt(data[[tsibble::index_var(data)]])

  # R CMD Check variable bindings fix
  # nolint start
  timestamp <- time <- state <- previous_state <- agreement <- NULL
  # nolint end

  interval <- data |> find_epoch() |> magrittr::extract2("best_match")
  samples_per_day <- as.numeric(lubridate::ddays(1)) / interval

  data |>
    dplyr::as_tibble() |>
    dplyr::transmute(
      timestamp = !!as.symbol(tsibble::index_var(data)),
      state = !!as.symbol(state_col)
    ) |>
    dplyr::mutate(
      time = timestamp |> hms::as_hms(),
      state =
        dplyr::case_when(
          state %in% sleeping_states ~ "Sleeping",
          state %in% awake_states ~ "Awake",
          TRUE ~ NA
        ) |>
        factor(levels = c("Awake", "Sleeping"), ordered = FALSE),
      previous_state = dplyr::lag(state, samples_per_day),
      agreement = dplyr::case_when(
        is.na(state) ~ NA,
        is.na(previous_state) ~ NA,
        TRUE ~ state == previous_state
      )
    ) |>
    dplyr::summarize(
      state = list(state),
      previous_state = list(previous_state),
      agreement = list(agreement),
      .by = time
    ) |>
    dplyr::mutate(
      sri =
        agreement |>
        purrr::map_dbl(\(x) prop(x, TRUE, na_rm = TRUE)) |>
        scales::rescale(to = c(-100, 100), from = c(0, 1))
    ) |>
    dplyr::arrange(time) |>
    tsibble::as_tsibble(index = time, regular = TRUE)
}

check_sri <- function(sri_data, row) {
  assert_tsibble(sri_data)
  checkmate::assert_int(row, lower = 1, upper = nrow(sri_data))

  # R CMD Check variable bindings fix
  # nolint start
  time <- state <- previous_state <- agreement <- sri <- NULL
  # nolint end

  out <- sri_data |> dplyr::slice(row)

  dplyr::tibble(
    time = out |> dplyr::pull(time),
    state = out |> dplyr::pull(state) |> unlist(),
    previous_state = out |> dplyr::pull(previous_state) |> unlist(),
    agreement = out |> dplyr::pull(agreement) |> unlist(),
    unscaled_sri = agreement |> prop(TRUE),
    sri = out |> dplyr::pull(sri)
  )
}
