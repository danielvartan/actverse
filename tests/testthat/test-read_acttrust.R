test_that("read_acttrust() | general test", {
    file <- tempfile()
    data <- paste0(
        "DATE/TIME;MS;EVENT;TEMPERATURE;EXT TEMPERATURE;ORIENTATION;PIM;",
        "PIMn;TAT;TATn;ZCM;ZCMn;LIGHT;AMB LIGHT;RED LIGHT;GREEN LIGHT;",
        "BLUE LIGHT;IR LIGHT;UVA LIGHT;UVB LIGHT;STATE",
        "\n",
        "01/01/2020 00:00:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0",
        "\n",
        "01/01/2020 00:01:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0"
    )

    writeLines(data, file)

    expected <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00",
                                             tz = "America/Sao_Paulo"),
                      lubridate::as_datetime("2020-01-01 00:01:00",
                                             tz = "America/Sao_Paulo")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp)

    object <- shush(read_acttrust(file, tz = "America/Sao_Paulo"))

    expect_equal(object, expected)
    expect_equal(lubridate::tz(object$timestamp), "America/Sao_Paulo")
})

test_that("read_acttrust() | error test", {
    file <- tempfile()
    writeLines("Temp", file)

    # checkmate::assert_string(file)
    expect_error(read_acttrust(file = 1, tz = "UTC", regularize = TRUE),
                 "Assertion on 'file' failed")

    # checkmate::assert_file_exists(file)
    expect_error(read_acttrust(file = "iMpoSSiBle.alien", tz = "UTC",
                               regularize = TRUE),
                 "Assertion on 'file' failed")

    # checkmate::assert_choice(tz, OlsonNames())
    expect_error(read_acttrust(file = file, tz = 1, regularize = TRUE),
                 "Assertion on 'tz' failed")

    # checkmate::assert_flag(regularize)
    expect_error(read_acttrust(file = file, tz = "UTC", regularize = 1),
                 "Assertion on 'regularize' failed")
})

test_that("read_acttrust_data() | general test", {
    file <- tempfile()
    data <- paste0(
        "DATE/TIME;MS;EVENT;TEMPERATURE;EXT TEMPERATURE;ORIENTATION;PIM;",
        "PIMn;TAT;TATn;ZCM;ZCMn;LIGHT;AMB LIGHT;RED LIGHT;GREEN LIGHT;",
        "BLUE LIGHT;IR LIGHT;UVA LIGHT;UVB LIGHT;STATE",
        "\n",
        "01/01/2020 00:00:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0",
        "\n",
        "01/01/2020 00:01:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0"
    )

    writeLines(data, file)

    expected <- dplyr::tibble(
        `DATE/TIME` = c("01/01/2020 00:00:00", "01/01/2020 00:01:00"),
        MS = "0", EVENT = "0", TEMPERATURE = "0", `EXT TEMPERATURE` = "0",
        ORIENTATION = "0", PIM = "0", PIMn = "0", TAT = "0", TATn = "0",
        ZCM = "0", ZCMn = "0", LIGHT = "0", `AMB LIGHT` = "0",
        `RED LIGHT` = "0", `GREEN LIGHT` = "0", `BLUE LIGHT` = "0",
        `IR LIGHT` = "0", `UVA LIGHT` = "0", `UVB LIGHT` = "0", STATE = "0"
    )

    expect_equal(shush(read_acttrust_data(file)), expected)

    header <- c("Condor Instruments Report", rep("\n", 12), "+--------+")
    data <- gsub(";", "\t", data)

    writeLines(c(header, data), file)
    expect_equal(shush(read_acttrust_data(file)), expected)
})

test_that("read_acttrust_data() | error test", {
    file <- tempfile()
    writeLines("Temp", file)

    # checkmate::assert_string(file)
    expect_error(read_acttrust_data(file = 1), "Assertion on 'file' failed")

    # checkmate::assert_file_exists(file)
    expect_error(read_acttrust_data(file = "iMpoSSiBle.alien"),
                 "Assertion on 'file' failed")
})

test_that("tidy_acttrust_data() | general test", {
    data <- dplyr::tibble(
        `DATE/TIME` = c("01/01/2020 00:00:00", "01/01/2020 00:01:00"),
        MS = "0", EVENT = "0", TEMPERATURE = "0.5", `EXT TEMPERATURE` = "0",
        ORIENTATION = "0", PIM = "0", PIMn = "0", TAT = "0", TATn = "0",
        ZCM = "0", ZCMn = "0", LIGHT = "0", `AMB LIGHT` = "0",
        `RED LIGHT` = "0", `GREEN LIGHT` = "0", `BLUE LIGHT` = "0",
        `IR LIGHT` = "0", `UVA LIGHT` = "0", `UVB LIGHT` = "0", STATE = "0"
    )

    expected <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00",
                                             tz = "America/Sao_Paulo"),
                      lubridate::as_datetime("2020-01-01 00:01:00",
                                             tz = "America/Sao_Paulo")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0.5,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    )

    expect_equal(shush(tidy_acttrust_data(data, tz = "America/Sao_Paulo")),
                 expected)

    data <- dplyr::tibble(
        DATE = c("01/01/2020", "01/01/2020"), TIME = c("00:00:00", "00:01:00"),
        MS = "0", EVENT = "0", TEMPERATURE = "0,5", `EXT TEMPERATURE` = "0",
        ORIENTATION = "0", PIM = "0", PIMn = "0", TAT = "0", TATn = "0",
        ZCM = "0", ZCMn = "0", LIGHT = "0", `AMB LIGHT` = "0",
        `RED LIGHT` = "0", `GREEN LIGHT` = "0", `BLUE LIGHT` = "0",
        `IR LIGHT` = "0", `UVA LIGHT` = "0", `UVB LIGHT` = "0", STATE = "0"
    )

    expect_equal(shush(tidy_acttrust_data(data, tz = "America/Sao_Paulo")),
                 expected)
})

test_that("tidy_acttrust_data() | error test", {
    # checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    expect_error(tidy_acttrust_data(1, tz = "UTC"),
                 "Assertion on 'data' failed")
    expect_error(tidy_acttrust_data(dplyr::tibble(), tz = "UTC"),
                 "Assertion on 'data' failed")
    expect_error(tidy_acttrust_data(dplyr::tibble(a = character()), tz = "UTC"),
                 "Assertion on 'data' failed")

    # checkmate::assert_choice(tz, OlsonNames())
    expect_error(tidy_acttrust_data(data = dplyr::tibble(a = 1), tz = 1),
                 "Assertion on 'tz' failed")
})

test_that("validate_acttrust_data() | general test", {
    data <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:00"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    )

    expected <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:00"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp)

    expect_equal(shush(validate_acttrust_data(data, regularize = TRUE)),
                 expected)

    data <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:02"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 100,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = c(0, 4, 0)
    )

    expected <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:02"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = c(0, NA, 0), tat = c(0, NA, 0), zcm = c(0, NA, 0),
        orientation = 0,
        wrist_temperature = c(as.numeric(NA), NA, NA),
        external_temperature = c(0, NA, 0), light = c(0, NA, 0),
        ambient_light = c(0, NA, 0), red_light = c(0, NA, 0),
        green_light = c(0, NA, 0), blue_light = c(0, NA, 0),
        ir_light = c(0, NA, 0), uva_light = c(0, NA, 0),
        uvb_light = c(0, NA, 0), event = 0, state = c(0, 4, 0)
    ) %>%
        tsibble::tsibble(index = timestamp, regular = FALSE)

    expect_equal(shush(validate_acttrust_data(data, regularize = TRUE)),
                 expected)
})

test_that("validate_acttrust_data() | error test", {
    # checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
    expect_error(validate_acttrust_data(1, regularize = TRUE),
                 "Assertion on 'data' failed")
    expect_error(validate_acttrust_data(dplyr::tibble(), regularize = TRUE),
                 "Assertion on 'data' failed")
    expect_error(validate_acttrust_data(dplyr::tibble(a = character()),
                                        regularize = TRUE),
                 "Assertion on 'data' failed")

    # checkmate::assert_flag(regularize)
    expect_error(validate_acttrust_data(data = dplyr::tibble(a = 1),
                                        regularize = 1),
                 "Assertion on 'regularize' failed")
})

test_that("regularize_acttrust_data() | general test", {
    data <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime(seq(0, 60 * 1000, by = 60)),
                      lubridate::as_datetime("1970-01-01 16:40:30"),
                      lubridate::as_datetime("1970-01-01 16:43:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = FALSE)

    expected <- data %>%
        dplyr::add_row(data[1, ]) %>%
        dplyr::mutate(
            timestamp = seq(lubridate::as_datetime(0),
                            lubridate::as_datetime(60180), by = 60)
        ) %>%
        dplyr::mutate(dplyr::across(
            !dplyr::matches("^timestamp$|^orientation$|^event$"),
            ~ dplyr::if_else(
                timestamp == lubridate::as_datetime(60060) |
                    timestamp == lubridate::as_datetime(60120),
                as.numeric(NA), .x)
        )) %>%
        dplyr::mutate(state = dplyr::if_else(is.na(pim), 9, state))


    expect_equal(shush(regularize_acttrust_data(data)), expected)

    data <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime(seq(0, 60 * 100, by = 60)),
                      lubridate::as_datetime(seq(6030, 6030 + (30 * 100),
                                                 by = 30))),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = FALSE)

    expect_equal(shush(regularize_acttrust_data(data)), data)
})

test_that("regularize_acttrust_data() | error test", {
    data <- dplyr::tibble(index = 1:10, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(regularize_acttrust_data(data = 1),
                 "Assertion on 'data' failed")

    # assert_index_class(data, c("Date", "POSIXt"))
    expect_error(regularize_acttrust_data(data = data),
                 "Assertion on 'data' failed")
})

test_that("find_offwrist_intervals() | general test", {
    data <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:02"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = c(0, 4, 0)
    ) %>%
        tsibble::tsibble(index = timestamp, regular = FALSE)

    expect_equal(shush(find_offwrist_intervals(data)),
                 lubridate::interval("2020-01-01 00:01:02",
                                     "2020-01-01 00:01:02"))

    data <- data %>% dplyr::mutate(state = 0)

    expect_equal(shush(find_offwrist_intervals(data)),
                 lubridate::as.interval(NA))
})

test_that("find_offwrist_intervals() | error test", {
    data_1 <- dplyr::tibble(
        index = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "day"),
        x = seq_along(index)
    ) %>%
        tsibble::tsibble(index = index)

    data_2 <- dplyr::tibble(index = 1:10, x = seq_along(index)) %>%
        tsibble::tsibble(index = index)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(find_offwrist_intervals(data = 1),
                 "Assertion on 'data' failed")

    # assert_index_class(data, c("Date", "POSIXt"))
    expect_error(find_offwrist_intervals(data = data_2),
                 "Assertion on 'data' failed")

    # checkmate::assert_subset(c("timestamp", "state"), names(data))
    expect_error(find_offwrist_intervals(data = data_1),
                 "Assertion on 'c\\(\"timestamp\", \"state\"\\)' failed")
})

test_that("read_acttrust_giperbio() | general test", {
    file <- tempfile()
    data <- paste0(
        "DATE/TIME;MS;EVENT;TEMPERATURE;EXT TEMPERATURE;ORIENTATION;PIM;",
        "PIMn;TAT;TATn;ZCM;ZCMn;LIGHT;AMB LIGHT;RED LIGHT;GREEN LIGHT;",
        "BLUE LIGHT;IR LIGHT;UVA LIGHT;UVB LIGHT;STATE",
        "\n",
        "01/01/2020 00:00:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0",
        "\n",
        "01/01/2020 00:01:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;4",
        "\n",
        "01/01/2020 00:02:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0"
    )

    writeLines(data, file)

    object <- suppressMessages(read_acttrust_giperbio(file, tz = "UTC"))

    expected <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime("2020-01-01 00:00:00"),
                      lubridate::as_datetime("2020-01-01 00:01:00"),
                      lubridate::as_datetime("2020-01-01 00:02:00")),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = c(0, 4, 0)
    ) %>%
        tsibble::tsibble(index = timestamp)

    expect_equal(object, expected)
})

test_that("read_acttrust_giperbio() | error test", {
    file <- tempfile()
    writeLines("Temp", file)

    # checkmate::assert_string(file)
    expect_error(read_acttrust_giperbio(file = 1, tz = "UTC"),
                 "Assertion on 'file' failed")

    # checkmate::assert_file_exists(file)
    expect_error(read_acttrust_giperbio(file = "iMpoSSiBle.alien", tz = "UTC"),
                 "Assertion on 'file' failed")

    # checkmate::assert_choice(tz, OlsonNames())
    expect_error(read_acttrust_giperbio(file = file, tz = 1),
                 "Assertion on 'tz' failed")
})
