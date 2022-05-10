# actogram(data, "pim", days = 7, lat = -23.5489, lon = -46.6388)

#' Plot a actogram for an actigraphy record
#'
#' @description
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
#' @param var_lab
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
#' @param state_colors
#' @param print (optional) a [`logical`][logical()] value indicating if the
#'   function must print the \eqn{Q_{p}}{Qp} plot (default: `TRUE`).
#'
#' @return a [`ggplot`][ggplot2::ggplot()] object with the actogram plot.
#'
#' @family visual analysis functions
#' @export
#'
#' @examples
#' actogram(acttrust, "pim")
actogram <- function(data, base_col, state_col = "state", days = 7,
                     state_alpha = 0.5, var_lab = toupper(base_col),
                     date_format ="%a %d/%m", lat = NULL, lon = NULL,
                     base_color = "#000000",
                     state_names = c(
                         "1" = "Sleeping", "2" = "Awakening", "4" = "Offwrist",
                         "lp" = "Light phase", "dp" = "Dark phase"
                     ),
                     state_colors = c(
                         "1" = viridis::viridis(3, alpha = NULL)[2],
                         "2" = viridis::viridis(3, alpha = NULL)[3],
                         "4" = viridis::viridis(3, alpha = NULL)[1],
                         "lp" = "#faf3b4", "dp" = "#ebebeb"
                         ),
                     grid = TRUE, print = TRUE) {
    assert_tsibble(data)
    checkmate::assert_choice(base_col, names(data))
    checkmate::assert_numeric(data[[base_col]])
    checkmate::assert_choice(state_col, names(data))
    checkmate::assert_numeric(data[[state_col]])
    checkmate::assert_int(days, null.ok = TRUE)
    checkmate::assert_number(state_alpha, lower = 0, upper = 1)
    checkmate::assert_string(var_lab)
    checkmate::assert_string(date_format)
    checkmate::assert_number(lat, null.ok = TRUE)
    checkmate::assert_number(lon, null.ok = TRUE)
    checkmate::assert_string(base_color, pattern = "^#.{6}$")
    checkmate::assert_character(
        state_names, any.missing = FALSE,names = "unique")
    checkmate::assert_character(
        state_colors, pattern = "^#.{6}$", any.missing = FALSE, names = "unique"
        )
    checkmate::assert_set_equal(names(state_names), names(state_colors))
    checkmate::assert_flag(grid)
    checkmate::assert_flag(print)

    # TO DO:
    #
    # * Resting x despertar
    # * Work on API
    # * Add douple-plot option.
    # * Add solarimetric model.
    # * Add summary_day
    # * lens_chart()

    index <- data %>% tsibble::index_var()

    n_days <- lubridate::interval(
        dplyr::first(data[[index]]), dplyr::last(data[[index]])
        ) %>%
        lubridate::time_length(unit = "days") %>%
        ceiling()

    if (is.null(days) || days > n_days) days <- n_days

    for (i in c("state_names", "state_colors")) {
        j <- c(
            get(i)[which(
                shush(as.numeric(names(get(i)))) %in%
                    unique(data[[state_col]]) |
                    names(get(i)) %in% c("lp", "dp")
            )]
        )
        assign(i, j)
    }

    # Add lp and dp to 'state_names' and 'state_colors' if they are not assigned

    bind <- dplyr::tibble(
        !!as.symbol(index) := seq(
            from = flat_hour(dplyr::first(data$timestamp)),
            to = dplyr::first(data$timestamp), by = "min"
            )
    )

    data <- data %>%
        tsibble::as_tibble() %>%
        dplyr::select(
            dplyr::all_of(c(index, base_col, state_col))
            ) %>%
        dplyr::filter(lubridate::date(timestamp) <=
                          lubridate::date(dplyr::first(timestamp)) +
                          lubridate::days(days - 1)) %>%
        dplyr::bind_rows(bind) %>%
        dplyr::arrange(!!as.symbol(index)) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches(
                "^timestamp$|^orientation$|^event$|^state$"),
            ~ na_zero(.x)
        )) %>%
        dplyr::mutate(
            date = lubridate::as_date(timestamp),
            time = hms::as_hms(timestamp),
            state = as.character(state),
            state = dplyr::if_else(
                state %in% shush(rm_na(as.numeric(names(state_names)))),
                state, "Unassigned"
            )
        )

    for (i in seq_along(state_names)) {
        if (is.na(shush(as.numeric(names(state_names[i]))))) next()

        data <- data %>%
            dplyr::mutate(
                state = dplyr::if_else(
                    state == names(state_names[i]), unname(state_names[i]),
                    state
                    ),
            )
    }

    data <- data %>%
        dplyr::mutate(
            state = factor(
                state,
                levels = unname(state_names[which(
                    !names(state_names) %in% c("lp", "dp")
                    )]),
                ordered = TRUE
            )
        )

    out <- data %>%
        ggplot2::ggplot(ggplot2::aes(
            x = time, y = !!as.symbol(base_col)
        ))

    if (!is.null(lat) && !is.null(lon)) {
        require_pkg("suncalc")

        sun_stats <- get_sun_stats(
            lat = lat, lon = lon, date = lubridate::date(data$timestamp[1]),
            tz = lubridate::tz(data$timestamp[1])
        )

        tz <- lubridate::tz(data$timestamp[1])
        sunrise <- flat_posixt(sun_stats$sunrise, force_tz = FALSE)
        sunset <- flat_posixt(sun_stats$sunset, force_tz = FALSE)

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
            index = flat_posixt(data$timestamp, force_tz = FALSE),
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
                    ymin = 0,
                    ymax = max(!!as.symbol(base_col), na.rm = TRUE),
                    fill = ld_fill
                    ),
                na.rm = TRUE
            ) +
            ggplot2::scale_fill_manual(
                values = unname(state_colors), limits = unname(state_names),
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
                    ymin = 0,
                    ymax = max(!!as.symbol(base_col), na.rm = TRUE)
                    ),
                colour = "#ebebeb",
                na.rm = TRUE
            ) +
            ggplot2::scale_fill_manual(
                values = unname(state_colors[which(
                    !names(state_colors) %in% c("lp", "dp")
                )]),
                limits = unname(state_names[which(
                    !names(state_names) %in% c("lp", "dp")
                )]),
                na.value = NA
            )
    }

    out <- out +
        ggplot2::geom_area(ggplot2::aes(colour = var_lab), fill = base_color) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(
            breaks = as.numeric(lubridate::dhours(c(0:24)[!0:24 %% 2])),
            labels = c(0:23, 0)[!0:24 %% 2],
            minor_breaks = NULL, limits = c(0, 86400), expand = c(0.005, 0.005),
            sec.axis = ggplot2::dup_axis()
            ) +
        ggplot2::scale_colour_manual(
            ggplot2::element_blank(), breaks = var_lab, values = base_color
            ) +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = time,
                xmax = dplyr::case_when(
                    time == dplyr::last(time) ~ time,
                    dplyr::lead(time) <= time ~ time,
                    TRUE ~ dplyr::lead(time),
                ),
                ymin = 0, ymax = max(!!as.symbol(base_col), na.rm = TRUE),
                fill = state),
            na.rm = TRUE,
            alpha = state_alpha
        ) +
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

get_sun_stats <-function(lat, lon, date = Sys.Date(), tz = "UTC",
                         formatted = 0) {
    checkmate::assert_number(lat)
    checkmate::assert_number(lon)
    checkmate::assert_date(date)
    checkmate::assert_choice(tz, OlsonNames())
    checkmate::assert_int(formatted)

    # 'suncalc' package solution

    require_pkg("suncalc")

    suncalc::getSunlightTimes(date = date, lat = lat, lon = lon, tz = tz) %>%
        as.list()

    # # 'sunrise-sunset.org' solution
    # #
    # # https://sunrise-sunset.org/api solution (OK)
    #
    # require_pkg("curl", "jsonlite")
    #
    # if (isTRUE(curl::has_internet())) {
    #     get <- jsonlite::read_json(paste0(
    #         "https://api.sunrise-sunset.org/json?",
    #         "lat=", lat, "&lng=", lon, "&date=", date, "&formatted", formatted
    #     ))
    #
    #     if (!get$status == "OK") {
    #         NULL
    #     } else {
    #         get %>%
    #             magrittr::extract2("results") %>%
    #             purrr::map(
    #                 ~ paste0(as.character(date), " ", .x) %>%
    #                     lubridate::parse_date_time("Ymd IMS p") %>%
    #                     lubridate::with_tz(tzone = tz) %>%
    #                     hms::as_hms()
    #             ) %>%
    #             append(list(date = date), .)
    #     }
    # } else {
    #     NULL
    # }

    # NOAA / 'rnoaa' package solution
    #
    # https://gml.noaa.gov/grad/solcalc/
    # https://docs.ropensci.org/rnoaa

    # # NASA POWER solution / 'nasapower' package solution
    # #
    # # https://power.larc.nasa.gov/#resources
    # # https://power.larc.nasa.gov/docs/
    # # nasapower:::parameters$DAILY_AG
    # # rdocumentation.org/packages/nasapower/versions/3.0.1/topics/parameters
    #
    # nasapower::get_power(
    #     community = "ag",
    #     pars = c("ZENITH_LUMINANCE"),
    #     temporal_api = "daily",
    #     lonlat = c(lon, lat),
    #     dates = c("2022-05-01", "2022-05-05"),
    #     time_standard = "UTC"
    # )
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

label_jump <- function(x, type = "even") {
    checkmate::assert_atomic(x)
    checkmate::assert_choice(type, c("even", "odd"))

    if (type == "even") {
        x[!seq_along(x) %% 2 == 0]
    } else if (type == "odd") {
        x[seq_along(x) %% 2 == 0]
    }
}

flat_posixt <- function(posixt, base = as.Date("1970-01-01"),
                        force_tz = FALSE, tz = "UTC") {
    assert_posixt(posixt, null.ok = FALSE)
    checkmate::assert_date(base, len = 1, all.missing = FALSE)
    checkmate::assert_flag(force_tz)
    checkmate::assert_choice(tz, OlsonNames())

    lubridate::date(posixt) <- base

    if (isTRUE(force_tz)) {
        lubridate::force_tz(posixt, tz)
    } else {
        posixt
    }
}

flat_hour <- function(posixt, h = 0, m = 0, s = 0) {
    assert_posixt(posixt)

    posixt %>%
        lubridate::date() %>%
        lubridate::as_datetime(tz = lubridate::tz(posixt))
}
