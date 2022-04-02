# Refactor and export function
aggregate_index_mean <- function(data, unit, week_start = 1) {
    unit_choices <- c("second", "minute", "hour", "day", "week", "month",
                      "quarter", "year")
    unit_choices <- append(unit_choices, paste0(unit_choices, "s"))

    checkmate::assert_class(data, "tbl_ts")
    checkmate::assert_choice(unit, unit_choices)
    checkmate::assert_int(week_start, lower = 1, upper = 7)

    index_var <- tsibble::index_var(data)
    index <- data[[index_var]]

    if (unit == "weeks") {
        group <- tsibble::yearweek(index, week_start = week_start)
    } else if (unit == "months") {
        group <- tsibble::yearmonth(index)
    } else if (unit == "quarters") {
        group <- tsibble::yearquarter(index)
    } else {
        group <- lubridate::floor_date(index, unit)
    }

    data %>%
        tsibble::index_by(.index_placeholder = group) %>%
        dplyr::summarise(dplyr::across(dplyr::everything(),
                                       ~ mean(.x, na.rm = TRUE))) %>%
        dplyr::select(-tsibble::index_var(data)) %>%
        dplyr::rename_with(~ gsub(paste0("^", ".index_placeholder", "$"),
                                  index_var, .x))
}
