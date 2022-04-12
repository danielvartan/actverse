test_that("write_acttrust | general test", {
    data_file <- tempfile()
    header_file <- tempfile()

    data_1 <- dplyr::tibble(
        timestamp = lubridate::as_datetime(0:1),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = TRUE)

    expected_1 <- c(
        paste0("DATE/TIME;MS;EVENT;TEMPERATURE;EXT TEMPERATURE;ORIENTATION;",
               "PIM;PIMn;TAT;TATn;ZCM;ZCMn;LIGHT;AMB LIGHT;RED LIGHT;GREEN ",
               "LIGHT;BLUE LIGHT;IR LIGHT;UVA LIGHT;UVB LIGHT;STATE"),
        "01/01/1970 00:00:00;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0",
        "01/01/1970 00:00:01;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0"
    )

    header <- c("Condor Instruments Report", rep("\n", 12))
    writeLines(header, header_file)
    shush(write_acttrust(data = data_1, file = data_file, delim = ";",
                         header = NULL))

    expect_equal(readLines(data_file), expected_1)

    shush(write_acttrust(data = data_1, file = data_file, delim = ";",
                         header = header_file))
    expected_2 <- append(c("Condor Instruments Report", rep("", 24)),
                         expected_1)

    expect_equal(readLines(data_file), expected_2)

    expected_3 <- gsub(";", "\t", expected_1)
    shush(write_acttrust(data = data_1, file = data_file, delim = "\t",
                         header = NULL))

    expect_equal(readLines(data_file), expected_3)

    data_2 <- dplyr::tibble(
        index = lubridate::as_datetime(0:1),
        .SoMeThInG = 0
    ) %>%
        tsibble::tsibble(index = index, regular = TRUE)

    expect_error(shush(write_acttrust(data = data_2, file = data_file,
                                      delim = "\t", header = NULL)))

    expect_equal(shush(write_acttrust(data = data_1, file = data_file,
                                      delim = "\t", header = NULL)),
                 NULL)
})

test_that("write_acttrust | error test", {
    data_1 <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime(seq(0, 60 * 1000, by = 60))),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = TRUE)

    data_2 <- dplyr::tibble(
        timestamp = 1:100,
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = TRUE)

    data_3 <- dplyr::tibble(
        timestamp = c(lubridate::as_datetime(seq(0, 60 * 100, by = 60)),
                      lubridate::as_datetime(seq(6030, 6030 + (30 * 100),
                                                 by = 30))),
        pim = 0, tat = 0, zcm = 0, orientation = 0, wrist_temperature = 0,
        external_temperature = 0, light = 0, ambient_light = 0, red_light = 0,
        green_light = 0, blue_light = 0, ir_light = 0, uva_light = 0,
        uvb_light = 0, event = 0, state = 0
    ) %>%
        tsibble::tsibble(index = timestamp, regular = TRUE)

    # assert_tsibble(data, min.rows = 2, min.cols = 2)
    expect_error(write_acttrust(data = 1, file = "", delim = ";",
                                header = NULL),
                 "Assertion on 'data' failed")
    expect_error(write_acttrust(data = data_1[1, ], file = "", delim = ";",
                                header = NULL),
                 "Assertion on 'data' failed")
    expect_error(write_acttrust(data = data_1[, 1], file = "", delim = ";",
                                header = NULL),
                 "Assertion on 'data' failed")

    # assert_index_class(data, c("POSIXt"))
    expect_error(write_acttrust(data = data_2, file = "", delim = ";",
                                header = NULL),
                 "Assertion on 'data' failed")

    # assert_clear_epoch(data, 0.9)
    expect_error(write_acttrust(data = data_3, file = "", delim = ";",
                                header = NULL),
                 "Assertion on 'data' failed")

    # checkmate::assert_string(file)
    expect_error(write_acttrust(data = data_1, file = 1, delim = ";",
                                header = NULL),
                 "Assertion on 'file' failed")

    # checkmate::assert_choice(delim, c(";", "\t"))
    expect_error(write_acttrust(data = data_1, file = "", delim = 1,
                                header = NULL),
                 "Assertion on 'delim' failed")

    # checkmate::assert_string(header, null.ok = TRUE)
    expect_error(write_acttrust(data = data_1, file = "", delim = ";",
                                header = 1),
                 "Assertion on 'header' failed")
})
