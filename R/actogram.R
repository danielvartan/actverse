# TO DO:
# - Work on API.
# - Add `summary_day()`.
# - `lens_chart()`.
# - Refactor and dismantle.
# - Add tests.

#' Create an actogram plot from actigraphy data
#'
#' @description
#'
#' `actogram()` creates an [actogram](https://en.wikipedia.org/wiki/Actogram)
#' plot from a [`tsibble`][tsibble::tsibble()] time series object, visualizing
#' activity patterns across multiple days.
#'
#' @details
#'
#' ## Vertical lines in the plot
#'
#' In some cases, you may notice thin vertical lines in the actogram
#' visualization. This is a rendering artifact that can depend on your graphic
#' device, graphics card, screen resolution, or the plot's size.
#'
#' If you encounter this, try resizing the plot window (for example, making it
#' more square or adjusting its dimensions), which often resolves the problem.
#' We also strongly recommend using the [`ragg`](https://ragg.r-lib.org/)
#' package for rendering graphics, as it can help avoid these artifacts.
#'
#' ## Light/Dark phase
#'
#' The light/dark phase is determined using the first timestamp of the time
#' series. The time zone is inferred from this timestamp as well. If the time
#' zone does not correspond to the provided latitude and longitude, the
#' calculated light/dark phases may be misaligned.
#'
#' ## `transform` argument
#'
#' `actogram()` allows you to provide a function for transforming the base
#' data. This is useful when dealing with extreme ranges.
#'
#' Example:
#'
#' ```{r roxygen2-actogram, fig.width=12, fig.height=5}
#' library(magrittr)
#'
#' trans_fun <- function(x) {
#'   log(x) |>
#'   inset(log(x) < 1, 1) |>
#'   subtract(1)
#' }
#'
#' acttrust |>
#'   actogram(
#'     col = "light",
#'     days = 1,
#'     transform = trans_fun,
#'     double_plot = FALSE
#'   )
#' ```
#'
#' ## `labels` argument
#'
#' The `labels` arguments allow you to customize the labels of the states in the
#' actogram. It must be a named character vector, where the names are the state
#' values and the values are the labels to be used in the legend. The order of
#' the labels will be the same as the order of the `labels` argument.
#'
#' Example (default setting):
#'
#' ```r
#' labels <- c(
#'   "1" = "Sleeping",
#'   "2" = "Resting",
#'   "4" = "Offwrist",
#'   "base" = "PIM",
#'   "lp" = "Light phase",
#'   "dp" = "Dark phase"
#' )
#' ```
#'
#' ## `colors` argument
#'
#' The `colors` arguments allow you to customize the colors of the states in
#' the actogram. Like the `label` argument, it must be a named character vector,
#' where the names are the state values and the values are the colors to be
#' used in the legend. The names of the `colors` must match the names of the
#' `labels`.
#'
#' The colors follow the conventions used in [`ggplot2`][ggplot2::ggplot].
#' You can specify either standard color names or
#' [hexadecimal color codes](https://en.wikipedia.org/wiki/Web_colors).
#' Click
#' [here](https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html)
#' to see the `ggplot2` color reference.
#'
#' Example (default setting):
#'
#' ```r
#' colors = c(
#'   "1" = "#410085",
#'   "2" = "#FFB426",
#'   "4" = "#FC2913",
#'   "base" = "#000040",
#'   "lp" = "#FFFFFF",
#'   "dp" = "#DBD7D3"
#' )
#' ```
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param col (optional) A string indicating which column of `data` to use for
#'   the area plot (default: `"pim"`).
#' @param days (optional) A number specifying how many days to display in the
#'   actogram. Use a negative value to exclude days from the end of the record.
#'   Set to `Inf` to plot all available days (default: `7`).
#' @param transform (optional) A transformation function to apply to
#'   `col` (default: [`identity`][base::identity()]).
#' @param state_col (optional) A string indicating which column of `data` have
#'   the states/categories data (default: `NULL`).
#' @param state_alpha (optional) A number, from `0` to `1`, indicating the
#'   transparency level of the state rectangles (default: `0.5`).
#' @param latitude (optional) A number indicating the latitude in decimal
#'   degrees that must be used to compute the light/dark phase. Set this
#'   argument to `NULL` when the light/dark is not needed. See the Details
#'   section to learn more (default: `NULL`).
#' @param longitude (optional) A number indicating the longitude in decimal
#'   degrees that must be used to compute the light/dark phase. Set this
#'   argument to `NULL` when the light/dark is not needed. See the Details
#'   section to learn more (default: `NULL`).
#' @param sun_stats_method (optional) A string indicating the method to use for
#'   computing the light/dark phase. See [`get_sun_stats()`][get_sun_stats()]
#'   for more details (default: `"suncalc"`).
#' @param labels (optional) A named character vector to rename states in the
#'   legend. Must include labels for any used state values, and `"base"` (label
#'   for `col`), `"lp"` (light phase), and `"dp"` (dark phase) if applicable.
#'   See the Details section to learn more.
#' @param colors (optional) A named character vector of colors to use for each
#'  label/state. Must include named colors for any states used in `labels`, as
#'  well as for `"base"` (color for `col`), `"lp"` (light phase), and `"dp"`
#'  (dark phase). See the Details section to learn more.
#' @param grid (optional) A [`logical`][logical()] flag indicating if the plot
#'   must have an overlay grid (default: `TRUE`).
#' @param minor_breaks (optional) A [`numeric`][base::numeric()] vector
#'   indicating the minor breaks on the x-axis
#'   (default: [`waiver()`][ggplot2::waiver()]).
#' @param date_format (optional) Format string passed to
#'   [`format()`][base::format()] for rendering date axis labels
#'   (default: `"%a %d/%m"`).
#' @param locale (optional) A string indicating the locale the
#'   [`LC_TIME`](https://en.wikipedia.org/wiki/Locale_(computer_software))
#'   environment variable must be set to. This is used to determine the language
#'   of the date labels (default: \code{`r get_en_locale()`}).
#' @param double_plot (optional) A [`logical`][base::logical()] flag indicating
#'   whether to stack 24-hour periods in a double-plot layout
#'   (default: `FALSE`).
#' @param x_label (optional) A string indicating the x-axis label
#'   (default: `NULL`).
#' @param y_label (optional) A string indicating the y-axis label
#'   (default: `NULL`).
#' @param print (optional) A [`logical`][logical()] flag indicating if the
#'   function must print the plot (default: `TRUE`).
#' @param ... (optional) Additional arguments passed to
#' [`theme()`][ggplot2::theme()].
#'
#' @return A [`ggplot`][ggplot2::ggplot()] object with the actogram plot.
#'
#' @family data visualization functions
#' @export
#'
#' @examples
#' library(curl)
#'
#' if (has_internet()) {
#'   file <- get_from_zenodo(
#'     doi = "10.5281/zenodo.4898822",
#'     dir = tempdir(),
#'     file = "processed.txt"
#'   )
#'
#'   data <- read_acttrust(file, tz = "America/Sao_Paulo")
#'
#'   data |>
#'     actogram(
#'       col = "pim",
#'       days = 7,
#'       # github.com/danielvartan/orbis
#'       # orbis::get_brazil_state_latitude("sp")
#'       latitude = -23.55065,
#'       # orbis::get_brazil_state_longitude("sp"),
#'       longitude = -46.63338,
#'       double_plot = TRUE
#'     )
#' }
actogram <- function(
  data,
  col = "pim",
  days = 7,
  transform = identity,
  state_col = "state",
  state_alpha = 0.5,
  latitude = NULL,
  longitude = NULL,
  sun_stats_method = "suncalc",
  labels = NULL,
  colors = NULL,
  grid = TRUE,
  minor_breaks = ggplot2::waiver(),
  date_format = "%a %d/%m",
  locale = get_en_locale(),
  double_plot = TRUE,
  x_label = NULL,
  y_label = NULL,
  print = TRUE,
  ...
) {
  assert_tsibble(data)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_numeric(data[[col]])
  checkmate::assert_number(days, null.ok = TRUE)
  checkmate::assert_function(transform)
  checkmate::assert_choice(state_col, names(data), null.ok = TRUE)
  checkmate::assert_number(state_alpha, lower = 0, upper = 1)
  checkmate::assert_number(latitude, lower = -90, upper = 90, null.ok = TRUE)
  checkmate::assert_number(longitude, lower = -180, upper = 180, null.ok = TRUE)
  checkmate::assert_string(sun_stats_method)
  assert_actogram_labels(labels, colors)
  checkmate::assert_flag(grid)
  checkmate::assert_string(date_format)
  checkmate::assert_string(locale)
  checkmate::assert_flag(double_plot)
  checkmate::assert_string(x_label, null.ok = TRUE)
  checkmate::assert_string(y_label, null.ok = TRUE)
  checkmate::assert_flag(print)

  if (!is.null(state_col)) {
    checkmate::assert_integerish(data[[state_col]], null.ok = TRUE)
  }

  if (is.null(labels)) labels <- get_actogram_default_labels(col)
  if (is.null(colors)) colors <- get_actogram_default_colors()

  current_theme <- ggplot2::theme_get()
  ggplot2::theme_set(get_actogram_default_theme(grid = grid, ...))
  withr::defer(ggplot2::theme_set(current_theme))

  index <- data |> tsibble::index_var()

  n_days <-
    data[[index]] |>
    lubridate::as_date() |>
    unique() |>
    length()

  if (days < 0) days <- n_days - abs(days)
  if (isTRUE(double_plot)) days <- days + 1

  if (is.null(days) || days > n_days) {
    days <- n_days
    all_days <- TRUE
  } else {
    all_days <- FALSE
  }

  if (
    (isFALSE(double_plot) && days < 0) ||
      (isTRUE(double_plot) && days == 1)
  ) {
    cli::cli_abort("The number of days must be greater than 0")
  }

  out <-
    data |>
    make_actogram_data(
      col = col,
      days = days,
      transform = transform,
      state_col = state_col,
      labels = labels,
      colors = colors,
      all_days = all_days
    ) |>
    plot_actogram(
      col = col,
      state_col = state_col,
      state_alpha = state_alpha,
      latitude = latitude,
      longitude = longitude,
      tz = lubridate::tz(data[[index]]),
      sun_stats_method = sun_stats_method,
      labels = labels,
      colors = colors,
      grid = grid,
      minor_breaks = minor_breaks,
      date_format = date_format,
      locale = locale,
      double_plot = double_plot,
      all_days = all_days,
      x_label = x_label,
      y_label = y_label,
      ...
    )

  if (isTRUE(print)) out |> print() |> shush()

  invisible(out)
}

make_actogram_data <- function(
  data,
  col = "pim",
  days = 7,
  transform = identity,
  state_col = "state",
  labels = NULL,
  colors = NULL,
  all_days = FALSE
) {
  assert_tsibble(data)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_numeric(data[[col]])
  checkmate::assert_int(days, null.ok = TRUE)
  checkmate::assert_function(transform)
  checkmate::assert_choice(state_col, names(data), null.ok = TRUE)
  assert_actogram_labels(labels, colors)
  checkmate::assert_flag(all_days)

  # R CMD Check variable bindings fix
  # nolint start
  timestamp <- time <- NULL
  # nolint end

  index <- data |> tsibble::index_var()

  filter_actogram_labels(data, state_col, labels, colors) |>
    list2env()

  if (!is.null(state_col)) {
    checkmate::assert_integerish(data[[state_col]], null.ok = TRUE)

    col_select <- c(index, col, state_col)
  } else {
    col_select <- c(index, col)
  }

  if (is.null(labels)) labels <- get_actogram_default_labels(col)
  if (is.null(colors)) colors <- get_actogram_default_colors()

  out <- data |> dplyr::select(dplyr::all_of(col_select))

  if (isFALSE(all_days)) {
    out <-
      out |>
      dplyr::filter(
        lubridate::date(!!as.symbol(index)) <=
          lubridate::date(dplyr::first(!!as.symbol(index))) +
            lubridate::days(days - 1)
      )
  }

  out <-
    out |>
    fill_actogram_data_tips() |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::all_of(col_select),
        ~ na_zero(.x)
      )
    ) |>
    dplyr::mutate(
      !!as.symbol(col) := transform(!!as.symbol(col))
    )

  out <-
    out |>
    dplyr::mutate(
      date = lubridate::as_date(!!as.symbol(index)),
      time = hms::as_hms(!!as.symbol(index)),
    )

  if (!is.null(state_col)) {
    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(state_col) := as.character(!!as.symbol(state_col)),
        !!as.symbol(state_col) := dplyr::if_else(
          !!as.symbol(state_col) %in% shush(
            drop_na(as.numeric(names(labels)))
          ),
          !!as.symbol(state_col),
          "Unassigned"
        )
      )

    for (i in seq_along(labels)) {
      if (is.na(shush(as.numeric(names(labels[i]))))) next

      out <-
        out |>
        dplyr::mutate(
          !!as.symbol(state_col) := dplyr::if_else(
            !!as.symbol(state_col) == names(labels[i]),
            unname(labels[i]),
            !!as.symbol(state_col)
          ),
        )
    }

    out <-
      out |>
      dplyr::mutate(
        !!as.symbol(state_col) := factor(
          !!as.symbol(state_col),
          levels =
            labels |>
            magrittr::extract(
              which(!names(labels) %in% c("lp", "dp"))
            ) |>
            unname(),
          ordered = TRUE
        )
      )
  }

  out |>
    dplyr::select(-timestamp) |>
    dplyr::relocate(date, time) |>
    dplyr::arrange(date, time)
}

filter_actogram_labels <- function(
  data,
  state_col = "state",
  labels = get_actogram_default_labels(),
  colors = get_actogram_default_colors()
) {
  checkmate::assert_multi_class(data, c("tbl_ts", "tbl_df"))
  assert_actogram_labels(labels, colors)

  if (!is.null(state_col)) {
    checkmate::assert_integerish(data[[state_col]], null.ok = TRUE)

    for (i in c("labels", "colors")) {
      j <-
        get(i) |>
        magrittr::extract(which(
          shush(as.numeric(names(get(i)))) %in%
            unique(data[[state_col]]) |
            names(get(i)) %in% c("lp", "dp")
        ))

      assign(i, j)
    }
  } else {
    for (i in c("labels", "colors")) {
      j <-
        get(i) |>
        magrittr::extract(which(names(get(i)) %in% c("lp", "dp")))

      assign(i, j)
    }
  }

  list(labels = labels, colors = colors)
}

fill_actogram_data_tips <- function(data) {
  assert_tsibble(data)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  index <- tsibble::index_var(data)
  epoch <- find_epoch(data)$best_match

  if (!is.null(epoch)) by <- epoch else by <- "min"

  out <- data

  if (!as.numeric(hms::as_hms(dplyr::first(data[[index]]))) == 0) {
    out <-
      data %>%
      dplyr::bind_rows(
        dplyr::tibble(
          !!as.symbol(index) := seq(
            from = flat_posixt_hour(
              dplyr::first(data[[index]]),
              base = hms::parse_hms("00:00:00"),
              force_tz = FALSE
            ),
            to = dplyr::first(data[[index]]),
            by = by
          )
        ),
        .
      )
  }

  if (
    !as.numeric(hms::as_hms(dplyr::last(data[[index]]))) >=
      hms::parse_hms("23:59:59")
  ) {
    out <-
      out |>
      dplyr::bind_rows(
        dplyr::tibble(
          !!as.symbol(index) := seq(
            from = dplyr::last(data[[index]]),
            to = flat_posixt_hour(
              dplyr::last(data[[index]]),
              base = hms::parse_hms("23:59:59"),
              force_tz = FALSE
            ),
            by = by
          )
        )
      )
  }

  out
}

plot_actogram <- function(
  data,
  col = "pim",
  state_col = "state",
  state_alpha = 0.5,
  latitude = NULL,
  longitude = NULL,
  tz = NULL,
  sun_stats_method = "suncalc",
  labels = NULL,
  colors = NULL,
  grid = TRUE,
  minor_breaks = ggplot2::waiver(),
  date_format = "%a %d/%m",
  locale = get_en_locale(),
  double_plot = TRUE,
  all_days = FALSE,
  x_label = NULL,
  y_label = NULL,
  ...
) {
  checkmate::assert_tibble(data)
  checkmate::assert_choice("time", names(data))
  prettycheck::assert_hms(data[["time"]])
  checkmate::assert_choice(col, names(data))
  checkmate::assert_numeric(data[[col]])
  checkmate::assert_choice(state_col, names(data), null.ok = TRUE)
  checkmate::assert_number(state_alpha, lower = 0, upper = 1)
  checkmate::assert_number(latitude, lower = -90, upper = 90, null.ok = TRUE)
  checkmate::assert_number(longitude, lower = -180, upper = 180, null.ok = TRUE)
  checkmate::assert_choice(tz, OlsonNames())
  checkmate::assert_string(sun_stats_method)
  assert_actogram_labels(labels, colors)
  checkmate::assert_flag(grid)
  checkmate::assert_string(date_format)
  checkmate::assert_string(locale)
  checkmate::assert_flag(double_plot)
  checkmate::assert_flag(all_days)
  checkmate::assert_string(x_label, null.ok = TRUE)
  checkmate::assert_string(y_label, null.ok = TRUE)

  # R CMD Check variable bindings fix
  # nolint start
  . <- time <- ld <- NULL
  # nolint end

  if (isFALSE(double_plot)) {
    day_length <- 24

    x_axis_labels <-
      c(seq(0, 23), 0) |>
      magrittr::extract(!seq(0, day_length) %% 2)
  } else {
    data <- data |> make_actogram_dp_data(col, all_days)
    day_length <- 48

    x_axis_labels <-
      c(seq(0, 23), seq(0, 23), 0) |>
      magrittr::extract(!seq(0, day_length) %% 2)
  }

  if ("base" %in% names(labels)) {
    base_label <- unname(labels["base"])
    base_color <- unname(colors["base"])
    labels <- labels[!names(labels) %in% c("base")]
    colors <- colors[!names(colors) %in% c("base")]
  } else {
    base_label <- col
    base_color <- "black"
  }

  ## Prepare the background

  if (!is.null(latitude) && !is.null(longitude) && !is.null(tz)) {
    out <-
      data |>
      make_actogram_ld_data(
        latitude = latitude,
        longitude = longitude,
        tz = tz,
        sun_stats_method = sun_stats_method,
        light_phase_label = labels["lp"],
        dark_phase_label = labels["dp"]
      ) |>
      ggplot2::ggplot(ggplot2::aes(x = time, y = !!as.symbol(col))) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = time,
          xmax = dplyr::case_when(
            time == dplyr::last(time) ~ time,
            dplyr::lead(time) <= time ~ time,
            TRUE ~ dplyr::lead(time),
          ),
          ymin = min(!!as.symbol(col), na.rm = TRUE),
          ymax = max(!!as.symbol(col), na.rm = TRUE),
          fill = ld
        ),
        na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(
        values = unname(colors),
        limits = unname(labels),
        na.value = NA
      )
  } else {
    out <-
      data |>
      ggplot2::ggplot(
        ggplot2::aes(x = time, y = !!as.symbol(col))
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = time,
          xmax = dplyr::case_when(
            time == dplyr::last(time) ~ time,
            dplyr::lead(time) <= time ~ time,
            TRUE ~ dplyr::lead(time),
          ),
          ymin = min(!!as.symbol(col), na.rm = TRUE),
          ymax = max(!!as.symbol(col), na.rm = TRUE)
        ),
        colour = "white",
        na.rm = TRUE
      )

    if (!is.null(state_col)) {
      out <-
        out +
        ggplot2::scale_fill_manual(
          values =
            colors |> #nolint
            magrittr::extract(
              which(!names(colors) %in% c("lp", "dp"))
            ) |>
            unname(),
          limits =
            labels |> #nolint
            magrittr::extract(
              which(!names(labels) %in% c("lp", "dp"))
            ) |>
            unname(),
          na.value = NA
        )
    }
  }

  ## Prepare the series/area plot

  out <-
    out +
    ggplot2::geom_area(
      mapping = ggplot2::aes(colour = base_label),
      fill = base_color
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(
      breaks =
        seq(0, day_length) |> #nolint
        magrittr::extract(!seq(0, day_length) %% 2) |>
        lubridate::dhours() |>
        as.numeric(),
      labels = x_axis_labels,
      minor_breaks = minor_breaks,
      limits = c(0, lubridate::hours(day_length) |> as.numeric()),
      expand = c(0, 0),
      sec.axis = ggplot2::dup_axis()
    ) +
    ggplot2::scale_colour_manual(
      ggplot2::element_blank(),
      breaks = base_label,
      values = base_color
    )

  ## Prepare the states rectangles

  if (!is.null(state_col)) {
    out <-
      out +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = time,
          xmax = dplyr::case_when(
            time == dplyr::last(time) ~ time,
            dplyr::lead(time) <= time ~ time,
            TRUE ~ dplyr::lead(time),
          ),
          ymin = min(!!as.symbol(col), na.rm = TRUE),
          ymax = max(!!as.symbol(col), na.rm = TRUE),
          fill = !!as.symbol(state_col)
        ),
        na.rm = TRUE,
        alpha = state_alpha
      )
  }

  ## Divide in facets by date

  if (isTRUE(double_plot)) {
    out <-
      out +
      ggplot2::geom_vline(
        xintercept = hms::parse_hm("24:00"),
        colour = "black",
        linewidth = 1
      )
  }

  ## Divide in facets by date

  if (is.null(x_label)) {
    if (isFALSE(double_plot)) {
      x_label <- "1 Day - Hours"
    } else {
      x_label <- "2 Days - Hours"
    }
  }

  if (is.null(y_label)) y_label <- "Days"

  out +
    ggplot2::facet_grid(
      row = ggplot2::vars(date),
      labeller = ggplot2::labeller(.rows = function(x) {
        withr::with_locale(
          new = c("LC_TIME" = locale),
          code = format(as.Date(x), date_format)
        )
      }),
      switch = "y"
    ) +
    ggplot2::labs(x = x_label, y = y_label)
}

make_actogram_dp_data <- function(
  data,
  col = "pim",
  all_days = FALSE
) {
  checkmate::assert_tibble(data)
  checkmate::assert_choice(col, names(data))
  checkmate::assert_numeric(data[[col]])
  checkmate::assert_choice("date", names(data))
  checkmate::assert_date(data[["date"]])
  checkmate::assert_choice("time", names(data))
  prettycheck::assert_hms(data[["time"]])
  checkmate::assert_flag(all_days)

  # R CMD Check variable bindings fix
  # nolint start
  time <- NULL
  # nolint end

  out <-
    data |>
    dplyr::filter(date > dplyr::first(date)) |>
    dplyr::mutate(
      date = date - lubridate::days(1),
      time = (time + hms::parse_hm("24:00")) |> hms::as_hms()
    ) |>
    dplyr::bind_rows(data) |>
    dplyr::arrange(date, time)

  if (isFALSE(all_days)) {
    out |> dplyr::filter(date < dplyr::last(date))
  } else {
    out
  }
}

make_actogram_ld_data <- function(
  data,
  latitude,
  longitude,
  tz = "UTC",
  sun_stats_method = "suncalc",
  light_phase_label = "Light phase",
  dark_phase_label = "Dark phase"
) {
  checkmate::assert_tibble(data)
  checkmate::assert_choice("date", names(data))
  checkmate::assert_choice("time", names(data))
  checkmate::assert_number(latitude, lower = -90, upper = 90)
  checkmate::assert_number(longitude, lower = -180, upper = 180)
  checkmate::assert_choice(tz, OlsonNames())
  checkmate::assert_string(sun_stats_method)
  checkmate::assert_string(light_phase_label, null.ok = TRUE)
  checkmate::assert_string(dark_phase_label, null.ok = TRUE)

  # R CMD Check variable bindings fix
  # nolint start
  time <- ld <- NULL
  # nolint end

  sun_stats <- get_sun_stats(
    latitude = latitude,
    longitude = longitude,
    date = data |> dplyr::pull(date) |> dplyr::first(),
    tz = tz,
    method = sun_stats_method
  )

  lp_start <- sun_stats |> magrittr::extract2("sunrise_start")
  lp_end <- sun_stats |> magrittr::extract2("sunset_end")

  out <-
    data |>
    dplyr::mutate(
      ld = dplyr::if_else(
        dplyr::between(
          time |> as.numeric(),
          lp_start |> as.numeric(),
          lp_end |> as.numeric()
        ),
        light_phase_label,
        dark_phase_label
      )
    )

  if (any(data$time > hms::parse_hm("24:00"))) {
    epoch <-
      data |>
      dplyr::pull(time) |>
      find_epoch() |>
      magrittr::extract2("best_match")

    out <-
      out |>
      dplyr::bind_rows(
        dplyr::tibble(
          date = dplyr::last(data$date),
          time = seq(
            from = dplyr::last(data$time) |> as.numeric(),
            to = lubridate::days(2) |> as.numeric(),
            by = epoch
          ) |>
            hms::as_hms()
        )
      ) |>
      dplyr::mutate(
        ld = dplyr::case_when(
          dplyr::between(
            time |> as.numeric(),
            hms::parse_hm("24:00") |> as.numeric(),
            (lp_start + hms::parse_hm("23:59")) |> as.numeric()
          ) ~ dark_phase_label,
          time > (lp_end + hms::parse_hm("24:00")) ~ dark_phase_label,
          dplyr::between(
            time |> as.numeric(),
            (lp_start + hms::parse_hm("24:00")) |> as.numeric(),
            (lp_end + hms::parse_hm("23:59")) |> as.numeric()
          ) ~ light_phase_label,
          TRUE ~ ld
        )
      )
  }

  out |>
    dplyr::mutate(
      ld = factor(
        ld,
        levels = c(light_phase_label, dark_phase_label),
        ordered = FALSE
      )
    )
}

get_actogram_default_theme <- function(grid = TRUE, ...) {
  checkmate::assert_flag(grid)

  out <-
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.ontop = TRUE,
      panel.background = ggplot2::element_rect(fill = NA),
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "black",
        linewidth = 0.1
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0, "npc"),
      axis.title.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0),
      strip.background = ggplot2::element_rect(
        colour = "black",
        fill = "white"
      ),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(color = "black")
    )

  if (isTRUE(grid)) {
    out <-
      out +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(
          colour = "black",
          linetype = "dashed",
          linewidth = 0.1
        ),
        panel.grid.minor.x = ggplot2::element_line(
          colour = "black",
          linetype = "dashed",
          linewidth = 0.1
        )
      )
  }

  out + ggplot2::theme(...)
}

get_actogram_default_labels <- function(col = "pim") {
  checkmate::assert_string(col)

  c(
    "1" = "Sleeping",
    "2" = "Resting",
    "4" = "Offwrist",
    "base" = ifelse(
      stringr::str_to_lower(col) %in% c("pim", "tat", "zcm"),
      stringr::str_to_upper(col),
      stringr::str_to_title(col)
    ),
    "lp" = "Light phase",
    "dp" = "Dark phase"
  )
}

get_actogram_default_colors <- function() {
  c(
    "1" = "#410085",
    "2" = "#FFB426",
    "4" = "#FC2913",
    "base" = "#000040",
    "lp" = "#FFFFFF",
    "dp" = "#DBD7D3"
  )
}

assert_actogram_labels <- function(labels, colors) {
  checkmate::assert_character(
    labels,
    any.missing = FALSE,
    unique = TRUE,
    names = "unique",
    null.ok = TRUE
  )
  checkmate::assert_character(
    colors,
    pattern = "^#.{6}$",
    any.missing = FALSE,
    unique = TRUE,
    names = "unique",
    null.ok = TRUE
  )
  checkmate::assert_set_equal(names(colors), names(labels))
}
