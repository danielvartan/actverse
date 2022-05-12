# actogram(data, "pim", days = 7, lat = -23.5489, lon = -46.6388)

#' Plot a actogram for an actigraphy record
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `actogram()` returns actogram plot for a [`tsibble`][tsibble::tsibble()]
#' object.
#'
#' @details
#'
#' ## Vertical lines
#'
#' Depending on your graphic cards, your screen resolution, and plot size,
#' some vertical lines can appear in the visualization. THe function cannot
#' fix this because it's a rendering issue.
#'
#' Resizing the plot (e.g., to a square shape) usually fix the problem.
#'
#' ## Light/Dark phase
#'
#' The light/dark phase is calculated by using the first timestamp of the
#' time series. Please note that the time zone will be retrieved from this same
#' timestamp. If the time zone doesn't match with the latitude and
#' longitude, the light/dark phase may shift.
#'
#' ## `trans` argument
#'
#' `actogram()` allows you to provide a function for transforming the base
#' data. That's is useful when dealing with extreme ranges.
#'
#' * Example:
#'
#' ```{r}
# trans_fun <- function(x) {
#     log(x) %>% `[<-`(log(x) < 1, 1) %>% `-`(1)
# }
#'
#' actogram(acttrust, "light", days = 1, trans = trans_fun)
#' ````
#' ## Labels order
#'
#' The labels order will be the same as the order of the `labels` argument.
#'
#' @param data A [`tsibble`][tsibble::tsibble()] object.
#' @param base_col A  string indicating which column of `data` to use for the
#'   basic data (area plot).
#' @param state_col (optional) a  string indicating which column of `data` have
#'   the states/categories data (default: `NULL`).
#' @param int (optional) a string indicating the interval unit. Valid values
#'   are: `“seconds”`, `“minutes”`, `“hours”`, `“days”`, `“weeks”`, `“months”`,
#'   `“quarters”`, and `“years”`) (default: `"days"`).
#' @param int_n (optional) an integer number indicating the size of the
#'   intervals, with the same unit as `int` (default: `7`).
#' @param days
#' @param state_alpha (optional) a number, from `0` to `1`, indicating the
#'   transparency level of the state rectangles (default: `0.5`).
#' @param base_label
#' @param date_format
#' @param lat (optional) a number indicating the latitude in decimal degrees
#' that must be used to compute the light/dark phase. Set this argument to
#' `NULL` when  the light/dark isn't need it. See the Details section
#' to learn more (default: `NULL`).
#' @param lon (optional) a number indicating the longitude in decimal degrees
#' that must be used to compute the light/dark phase. Set this argument to
#' `NULL` when  the light/dark isn't need it. See the Details section
#' to learn more (default: `NULL`).
#' @param grid (optional) a [`logical`][logical()] value indicating if the
#'   plot must have an overlay grid (default: `TRUE`).
#' @param colors
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the \eqn{Q_{p}}{Qp} plot (default: `TRUE`).
#'
#' @return a [`ggplot`][ggplot2::ggplot()] object with the actogram plot.
#'
#' @family visual analysis functions
#' @export
#'
#' @examples
#' actogram(acttrust, "pim", lat = -23.5489, lon = -46.6388)
actogram <- function(data, base_col, days = 7, trans = NULL,
                     state_col = "state", state_alpha = 0.5, lat = NULL,
                     lon = NULL,
                     labels = c(
                         "1" = "Sleeping", "2" = "Resting", "4" = "Offwrist",
                         "base" = base_col, "lp" = "Light phase",
                         "dp" = "Dark phase"
                     ),
                     colors = c(
                         "1" = viridis::viridis(3, alpha = NULL)[2],
                         "2" = viridis::viridis(3, alpha = NULL)[3],
                         "4" = viridis::viridis(3, alpha = NULL)[1],
                         "base" = "#000000", "lp" = "#faf3b4", "dp" = "#ebebeb"
                         ),
                     date_format ="%a %d/%m", double_plot = TRUE, grid = TRUE,
                     print = TRUE) {
    assert_tsibble(data)
    checkmate::assert_choice(base_col, names(data))
    checkmate::assert_numeric(data[[base_col]])
    checkmate::assert_int(days, null.ok = TRUE)
    checkmate::assert_function(trans, null.ok = TRUE)
    checkmate::assert_choice(state_col, names(data), null.ok = TRUE)
    checkmate::assert_number(state_alpha, lower = 0, upper = 1)
    checkmate::assert_number(lat, lower = -90, upper = 90, null.ok = TRUE)
    checkmate::assert_number(lon, lower = -180, upper = 180, null.ok = TRUE)
    checkmate::assert_character(labels, any.missing = FALSE,names = "unique")
    checkmate::assert_character(
        colors, pattern = "^#.{6}$", any.missing = FALSE, names = "unique"
        )
    checkmate::assert_set_equal(names(labels), names(colors))
    checkmate::assert_string(date_format)
    checkmate::assert_flag(double_plot)
    checkmate::assert_flag(grid)
    checkmate::assert_flag(print)

    if ((!is.null(lat) && is.null(lon)) || (is.null(lat) && !is.null(lon))) {
        cli::cli_abort(paste0(
            "{.strong {cli::col_red('lat')}} and ",
            "{.strong {cli::col_red('lon')}} must both be assigned ",
            "values or must be both {.strong NULL}."
        ))
    }

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- NULL

    # TO DO:
    #
    # * Resting x despertar
    # * Work on API
    # * Add douple-plot option.
    # * Add summary_day()
    # * lens_chart()

    # Order colors by names(labels)

    index <- data %>% tsibble::index_var()

    # Move to function
    n_days <- lubridate::interval(
        dplyr::first(data[[index]]), dplyr::last(data[[index]])
        ) %>%
        lubridate::time_length(unit = "days") %>%
        ceiling()

    if (is.null(days) || days > n_days) days <- n_days

    if ("base" %in% labels) {
        base_label <- unname(labels["base"])
        base_color <- unname(colors["base"])
    } else {
        base_label <- base_col
        base_color <- "#000000"
    }

    if (!is.null(state_col)) {
        checkmate::assert_integerish(data[[state_col]], null.ok = TRUE)

        for (i in c("labels", "colors")) {
            j <- c(
                get(i)[which(
                    shush(as.numeric(names(get(i)))) %in%
                        unique(data[[state_col]]) |
                        names(get(i)) %in% c("lp", "dp")
                )]
            )
            assign(i, j)
        }

        col_match <- paste0("^", index, "$|^", base_col, "$|^", state_col, "$")
    } else {
        for (i in c("labels", "colors")) {
            j <- c(
                get(i)[which(names(get(i)) %in% c("lp", "dp"))]
            )
            assign(i, j)
        }

        col_match <- paste0("^", index, "$|^", base_col, "$")
    }

    # Add base, lp and dp to 'labels' and 'colors'
    # if they are not assigned

    data <- data %>%
        dplyr::select(dplyr::matches(col_match)) %>%
        dplyr::filter(lubridate::date(!!as.symbol(index)) <=
                          lubridate::date(dplyr::first(!!as.symbol(index))) +
                          lubridate::days(days - 1)
        ) %>%
        fill_actogram_data_tips() %>%
        dplyr::mutate(dplyr::across(!dplyr::matches(col_match), ~ na_zero(.x)))


    if (!is.null(trans)) {
        data <- data %>%
            dplyr::mutate(
                !!as.symbol(base_col) := trans(!!as.symbol(base_col))
                )
    }

    data <- data %>%
        dplyr::mutate(
            date = lubridate::as_date(!!as.symbol(index)),
            time = hms::as_hms(!!as.symbol(index)),
        )

    if (!is.null(state_col)) {
        data <- data %>%
            dplyr::mutate(
                !!as.symbol(state_col) := as.character(!!as.symbol(state_col)),
                !!as.symbol(state_col) := dplyr::if_else(
                    !!as.symbol(state_col) %in% shush(
                        rm_na(as.numeric(names(labels)))
                        ),
                    !!as.symbol(state_col), "Unassigned"
                )
            )

        for (i in seq_along(labels)) {
            if (is.na(shush(as.numeric(names(labels[i]))))) next()

            data <- data %>%
                dplyr::mutate(
                    !!as.symbol(state_col) := dplyr::if_else(
                        !!as.symbol(state_col) == names(labels[i]),
                        unname(labels[i]), !!as.symbol(state_col)
                    ),
                )
        }

        data <- data %>%
            dplyr::mutate(
                !!as.symbol(state_col) := factor(
                    !!as.symbol(state_col),
                    levels = unname(labels[which(
                        !names(labels) %in% c("lp", "dp")
                    )]),
                    ordered = TRUE
                )
            )
    }

    out <- data %>%
        ggplot2::ggplot(ggplot2::aes(
            x = time, y = !!as.symbol(base_col)
        ))

    if (!is.null(lat) && !is.null(lon)) {
        require_pkg("suncalc")

        sun_stats <- sun_stats(
            lat = lat, lon = lon, date = lubridate::date(data[[index]][1]),
            tz = lubridate::tz(data[[index]][1])
        )

        tz <- lubridate::tz(data[[index]][1])
        sunrise <- lubridate::as_datetime(sun_stats$sunrise, tz = tz)
        sunset <- lubridate::as_datetime(sun_stats$sunset, tz = tz)

        dark_phase_1 <- lubridate::interval(
            lubridate::force_tz(lubridate::as_datetime(0), tzone = tz),
            sunrise - lubridate::dseconds()
        )
        light_phase <- lubridate::interval(sunrise, sunset)
        dark_phase_2 <- lubridate::interval(
            sunset + lubridate::dseconds(),
            lubridate::force_tz(lubridate::as_datetime(86400 - 1), tzone = tz)
        )

        ld_fill <- dplyr::tibble(
            index = flat_posixt_date(data[[index]]),
            ld = dplyr::case_when(
                index %within% dark_phase_1 ~ "Dark phase",
                index %within% light_phase ~ "Light phase",
                index %within% dark_phase_2 ~ "Dark phase"
            )
        ) %>%
            dplyr::mutate(
                ld = factor(ld, levels = c("Dark phase", "Light phase"))
            ) %>%
            magrittr::extract2("ld")

        out <- out +
            ggplot2::geom_rect(
                ggplot2::aes(
                    xmin = time,
                    xmax = dplyr::case_when(
                        time == dplyr::last(time) ~ time,
                        dplyr::lead(time) <= time ~ time,
                        TRUE ~ dplyr::lead(time),
                    ),
                    ymin = min(!!as.symbol(base_col), na.rm = TRUE),
                    ymax = max(!!as.symbol(base_col), na.rm = TRUE),
                    fill = ld_fill
                    ),
                na.rm = TRUE
            ) +
            ggplot2::scale_fill_manual(
                values = unname(colors), limits = unname(labels),
                na.value = NA
            )
    } else {
        out <- out +
            ggplot2::geom_rect(
                ggplot2::aes(
                    xmin = time,
                    xmax = dplyr::case_when(
                        time == dplyr::last(time) ~ time,
                        dplyr::lead(time) <= time ~ time,
                        TRUE ~ dplyr::lead(time),
                    ),
                    ymin = min(!!as.symbol(base_col), na.rm = TRUE),
                    ymax = max(!!as.symbol(base_col), na.rm = TRUE)
                    ),
                colour = "#ebebeb",
                na.rm = TRUE
            )

        if (!is.null(state_col)) {
            ggplot2::scale_fill_manual(
                values = unname(colors[which(
                    !names(colors) %in% c("lp", "dp")
                )]),
                limits = unname(labels[which(
                    !names(labels) %in% c("lp", "dp")
                )]),
                na.value = NA
            )
        }
    }

    out <- out +
        ggplot2::geom_area(
            ggplot2::aes(colour = base_label), fill = base_color
            ) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(
            breaks = as.numeric(lubridate::dhours(c(0:24)[!0:24 %% 2])),
            labels = c(0:23, 0)[!0:24 %% 2],
            minor_breaks = NULL, limits = c(0, 86400), expand = c(0.005, 0.005),
            sec.axis = ggplot2::dup_axis()
            ) +
        ggplot2::scale_colour_manual(
            ggplot2::element_blank(), breaks = base_label, values = base_color
            )

    if (!is.null(state_col)) {
        out <- out +
            ggplot2::geom_rect(
                ggplot2::aes(
                    xmin = time,
                    xmax = dplyr::case_when(
                        time == dplyr::last(time) ~ time,
                        dplyr::lead(time) <= time ~ time,
                        TRUE ~ dplyr::lead(time),
                    ),
                    ymin = min(!!as.symbol(base_col), na.rm = TRUE),
                    ymax = max(!!as.symbol(base_col), na.rm = TRUE),
                    fill = !!as.symbol(state_col)),
                na.rm = TRUE,
                alpha = state_alpha
            )
    }

    out <- out +
        ggplot2::facet_grid(
            row = ggplot2::vars(date),
            labeller = ggplot2::labeller(.rows = function(x) {
                withr::with_locale(
                    c("LC_TIME" = "en-US.utf8"),
                    format(as.Date(x), date_format)
                )
            }),
            switch = "y"
        ) +
        ggplot2::labs(x = "1 Day - Hours", y = "Days") +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = NA),
            panel.ontop = TRUE,
            panel.border = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            panel.spacing = ggplot2::unit(0.005, "npc"),
            axis.title.x.top = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            legend.position = "top",
            legend.title = ggplot2::element_blank(),
            strip.text.y.left = ggplot2::element_text(angle = 0)
        )

    if (isTRUE(grid)) {
        out <- out +
            ggplot2::theme(
                panel.grid.major.x = ggplot2::element_line(
                    colour = "gray", linetype="dashed", size = 0.1
                )
            )
    } else {
        out <- out +
            ggplot2::theme(
                panel.grid.major.x = ggplot2::element_blank()
            )
    }

    if (isTRUE(print)) shush(print(out))

    invisible(out)
}

fill_actogram_data_tips <- function(data) {
    assert_tsibble(data)

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    . <- NULL

    index <- tsibble::index_var(data)
    epoch <- find_epoch(data)$best_match

    if (!is.null(epoch)) by = epoch else by = "min"

    if (!as.numeric(hms::as_hms(dplyr::first(data[[index]]))) == 0) {
        data <- data %>%
            dplyr::bind_rows(
                dplyr::tibble(
                    !!as.symbol(index) := seq(
                        from = flat_posixt_hour(
                            dplyr::first(data[[index]]),
                            base = hms::parse_hms("00:00:00")
                        ),
                        to = dplyr::first(data[[index]]),
                        by = by
                    )),
                .
            )
    }

    if (!as.numeric(hms::as_hms(dplyr::last(data[[index]]))) >=
        hms::parse_hms("23:59:59")) {
        data <- data %>%
            dplyr::bind_rows(
                dplyr::tibble(
                    !!as.symbol(index) := seq(
                        from = dplyr::last(data[[index]]),
                        to = flat_posixt_hour(
                            dplyr::last(data[[index]]),
                            base = hms::parse_hms("23:59:59")),
                        by = by
                    ))
            )
    }
}

actogram_x_breaks <- function(...) {
    lubridate::dhours(0:24) %>%
        as.numeric() %>%
        hms::as_hms() %>%
        label_jump()
}

actogram_x_labels <- function(x) {
    lubridate::hour(x)
}

# Move to utils
label_jump <- function(x, type = "even") {
    checkmate::assert_atomic(x)
    checkmate::assert_choice(type, c("even", "odd"))

    if (type == "even") {
        x[!seq_along(x) %% 2 == 0]
    } else if (type == "odd") {
        x[seq_along(x) %% 2 == 0]
    }
}

# Move to utils
flat_posixt_date <- function(posixt, base = as.Date("1970-01-01")) {
    assert_posixt(posixt, null.ok = FALSE)
    checkmate::assert_date(base, len = 1, all.missing = FALSE)

    posixt %>% lubridate::`date<-`(base)
}

# Move to utils
flat_posixt_hour <- function(posixt, base = hms::parse_hms("00:00:00")) {
    assert_posixt(posixt)
    assert_hms(base, any.missing = FALSE)

    posixt %>%
        lubridate::date() %>%
        paste0(" ", base) %>%
        lubridate::as_datetime(tz = lubridate::tz(posixt))
}
