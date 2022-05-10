# actverse:::require_pkg("mctq")

# actogram(data, "pim", days = 7, lat = -23.5489, lon = -46.6388)
actogram <- function(data, var_col, state_col = "state", days = 7, alpha = 0.5,
                     var_lab = toupper(var_col), date_format ="%a %d/%m",
                     lat = NULL, lon = NULL, grid = TRUE, fill_colors = NULL) {
    assert_tsibble(data)
    checkmate::assert_choice(var_col, names(data))
    checkmate::assert_numeric(data[[var_col]])
    checkmate::assert_int(days, null.ok = TRUE)
    checkmate::assert_number(alpha, lower = 0, upper = 1)
    checkmate::assert_string(var_lab)
    checkmate::assert_string(date_format)
    checkmate::assert_number(lat, null.ok = TRUE)
    checkmate::assert_number(lon, null.ok = TRUE)
    checkmate::assert_flag(grid)

    # TO DO:
    #
    # * Center the legend window
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

    bind <- dplyr::tibble(
        !!as.symbol(index) := seq(
            from = flat_hour(dplyr::first(data$timestamp)),
            to = dplyr::first(data$timestamp), by = "min"
            )
    )

    data <- data %>%
        tsibble::as_tibble() %>%
        dplyr::select(
            dplyr::all_of(c(index, var_col, state_col))
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
            state = dplyr::case_when(
                state == 0 ~ "Awake",
                state == 1 ~ "Sleeping",
                state == 2 ~ "Resting",
                state == 4 ~ "Offwrist",
                TRUE ~ "Awake"
            ),
            state = factor(
                state,
                levels = c("Awake", "Sleeping", "Resting", "Offwrist"),
                ordered = TRUE
            )
        )

    out <- data %>%
        ggplot2::ggplot(ggplot2::aes(
            x = time, y = !!as.symbol(var_col)
        ))

    if (!is.null(lat) && !is.null(lon)) {
        sun_stats <- get_sun_stats(
            lat = lat, lon = lon, date = lubridate::date(data$timestamp[1])
            )

        if (!is.null(sun_stats)) {
            sunrise <- sun_stats$sunrise %>%
                lubridate::parse_date_time("IMS p") %>%
                hms::as_hms()
            sunset <- sun_stats$sunset %>%
                lubridate::parse_date_time("IMS p") %>%
                hms::as_hms()

            dark_phase_1 <- lubridate::interval(
                lubridate::as_datetime(0),
                lubridate::as_datetime(hms::as_hms(
                    sunrise - lubridate::dseconds()
                    ))
            )
            light_phase <- lubridate::interval(
                lubridate::as_datetime(sunrise),
                lubridate::as_datetime(sunset)
            )
            dark_phase_2 <- lubridate::interval(
                lubridate::as_datetime(hms::as_hms(
                    sunset + lubridate::dseconds()
                    )),
                lubridate::as_datetime(86400 - 1)
            )

            ld_fill <- dplyr::tibble(
                index = mctq:::flat_posixt(data$timestamp),
                ld = dplyr::case_when(
                    index %within% dark_phase_1 ~ "Dark phase",
                    index %within% light_phase ~ "Light phase",
                    index %within% dark_phase_2 ~ "Dark phase"
                )
            ) %>%
                dplyr::mutate(
                    ld = factor(
                        ld, levels = c("Dark phase", "Light phase"),
                        ordered = FALSE
                    )
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
                        ymax = max(!!as.symbol(var_col), na.rm = TRUE),
                        fill = ld_fill),
                    na.rm = TRUE
                )
        }
    }

    out <- out +
        ggplot2::geom_area(ggplot2::aes(colour = var_lab)) +
        ggplot2::scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_x_continuous(
            breaks = as.numeric(lubridate::dhours(c(0:24)[!0:24 %% 2])),
            labels = c(0:23, 0)[!0:24 %% 2],
            minor_breaks = NULL, limits = c(0, 86400), expand = c(0.01, 0.01),
            sec.axis = ggplot2::dup_axis()
            ) +
        ggplot2::scale_colour_manual(
            ggplot2::element_blank(), breaks = var_lab, values = "#000000"
            ) +
        ggplot2::geom_rect(
            ggplot2::aes(
                xmin = time,
                xmax = dplyr::case_when(
                    time == dplyr::last(time) ~ time,
                    dplyr::lead(time) <= time ~ time,
                    TRUE ~ dplyr::lead(time),
                ),
                ymin = 0, ymax = max(!!as.symbol(var_col), na.rm = TRUE),
                fill = state),
            na.rm = TRUE,
            alpha = alpha
        ) +
        ggplot2::scale_fill_manual(
            values = c("#21918c", "#fde725", "#440154", "#ebebeb", "#faf3b4"),
            limits = c("Sleeping", "Resting", "Offwrist", "Dark phase",
                       "Light phase"),
            na.value = NA
        ) +
        ggplot2::facet_grid(
            row = ggplot2::vars(date),
            labeller = ggplot2::labeller(.rows = function(x) {
                x %>% as.Date() %>% format(date_format)
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

    print(out)

    invisible(out)
}

# get_sun_stats(
#     date = lubridate::date(data$timestamp[1]), lat = -23.5489, lon = -46.6388
#     )
get_sun_stats <-function(lat, lon, date = Sys.Date(), tz = "UTC",
                         formatted = 0) {
    checkmate::assert_number(lat)
    checkmate::assert_number(lon)
    checkmate::assert_date(date)
    checkmate::assert_choice(tz, OlsonNames())
    checkmate::assert_int(formatted)

    # # 'suncalc' package solution (not OK)
    #
    # suncalc::getSunlightTimes(
    #     date = Sys.Date(), lat = lat, lon = lon, tz = "UTC"
    #     )

    # 'sunrise-sunset.org' solution
    #
    # https://sunrise-sunset.org/api solution (OK)

    require_pkg("curl", "jsonlite")

    if (isTRUE(curl::has_internet())) {
        get <- jsonlite::read_json(paste0(
            "https://api.sunrise-sunset.org/json?",
            "lat=", lat, "&lng=", lon, "&date=", date, "&formatted", formatted
        ))

        if (!get$status == "OK") {
            NULL
        } else {
            get %>%
                magrittr::extract2("results") %>%
                purrr::map(
                    ~ paste0(as.character(date), " ", .x) %>%
                        lubridate::parse_date_time("Ymd IMS p") %>%
                        lubridate::with_tz(tzone = tz) %>%
                        hms::as_hms()
                ) %>%
                append(list(date = date), .)
        }
    } else {
        NULL
    }

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

# Sys.getlocale()
# Sys.setlocale(category = "LC_ALL", locale = "en-US.utf8")
actogram_strip_labels <- function(x) {
    x %>% as.Date() %>% format(format = "%a %d/%m")
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

flat_hour <- function(posixt, h = 0, m = 0, s = 0) {
    assert_posixt(posixt)

    posixt %>%
        lubridate::date() %>%
        lubridate::as_datetime(tz = lubridate::tz(posixt))
}
