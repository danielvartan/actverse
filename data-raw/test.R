data <- actverse::read_acttrust()

data <- bkp %>%
    tsibble::as_tibble() %>%
    dplyr::transmute(
        DATE = as.character(lubridate::date(timestamp)),
        TIME = as.character(hms::as_hms(timestamp)),
        STATE = state,
        ZCM = zcm,
        PIM = pim,
        TAT = tat
    ) %>%
    sleep_select(x = c(1, 2), log_Data = ., type_Data = "actstudio") %>%
    sleep_statistic()
