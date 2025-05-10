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

  data |> writeLines(file)

  expected <-
    dplyr::tibble(
      timestamp = c(
        lubridate::as_datetime(
          "2020-01-01 00:00:00",
          tz = "America/Sao_Paulo"
        ),
        lubridate::as_datetime(
          "2020-01-01 00:01:00",
          tz = "America/Sao_Paulo"
        )
      ),
      pim = 0,
      tat = 0,
      zcm = 0,
      orientation = 0,
      wrist_temperature = 0,
      external_temperature = 0,
      light = 0,
      ambient_light = 0,
      red_light = 0,
      green_light = 0,
      blue_light = 0,
      ir_light = 0,
      uva_light = 0,
      uvb_light = 0,
      event = 0,
      state = 0
    ) |>
    tsibble::tsibble(index = timestamp)

  object <-
    file |>
    read_acttrust(tz = "America/Sao_Paulo") |>
    shush()

  expect_equal(object, expected)
  expect_equal(lubridate::tz(object$timestamp), "America/Sao_Paulo")
})

test_that("read_acttrust() | Error test", {
  file <- tempfile()

  "Temp" |> writeLines(file)

  # checkmate::assert_string(file)
  read_acttrust(file = 1, tz = "UTC", regularize = TRUE) |>
    expect_error("Assertion on 'file' failed")

  # checkmate::assert_file_exists(file)
  read_acttrust(
    file = "iMpoSSiBle.alien", tz = "UTC",
    regularize = TRUE
  ) |>
    expect_error("Assertion on 'file' failed")

  # checkmate::assert_choice(tz, OlsonNames())
  read_acttrust(file = file, tz = 1, regularize = TRUE) |>
    expect_error("Assertion on 'tz' failed")

  # checkmate::assert_flag(regularize)
  read_acttrust(file = file, tz = "UTC", regularize = 1) |>
    expect_error("Assertion on 'regularize' failed")
})

test_that("read_acttrust_data() | General test", {
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

  data |> writeLines(file)

  expected <- dplyr::tibble(
    `DATE/TIME` = c("01/01/2020 00:00:00", "01/01/2020 00:01:00"),
    MS = "0",
    EVENT = "0",
    TEMPERATURE = "0",
    `EXT TEMPERATURE` = "0",
    ORIENTATION = "0",
    PIM = "0",
    PIMn = "0",
    TAT = "0",
    TATn = "0",
    ZCM = "0",
    ZCMn = "0",
    LIGHT = "0",
    `AMB LIGHT` = "0",
    `RED LIGHT` = "0",
    `GREEN LIGHT` = "0",
    `BLUE LIGHT` = "0",
    `IR LIGHT` = "0",
    `UVA LIGHT` = "0",
    `UVB LIGHT` = "0",
    STATE = "0"
  )

  file |>
    read_acttrust_data() |>
    shush() |>
    expect_equal(expected)

  header <- c("Condor Instruments Report", rep("\n", 12), "+--------+")
  data <- gsub(";", "\t", data)

  c(header, data) |> writeLines(file)

  file |>
    read_acttrust_data() |>
    shush() |>
    expect_equal(expected)
})

test_that("read_acttrust_data() | Error test", {
  file <- tempfile()

  writeLines("Temp", file)

  # checkmate::assert_string(file)
  read_acttrust_data(file = 1) |>
    expect_error("Assertion on 'file' failed")

  # checkmate::assert_file_exists(file)
  read_acttrust_data(file = "iMpoSSiBle.alien") |>
    expect_error("Assertion on 'file' failed")
})

test_that("tidy_acttrust_data() | General test", {
  data <- dplyr::tibble(
    `DATE/TIME` = c("01/01/2020 00:00:00", "01/01/2020 00:01:00"),
    MS = "0",
    EVENT = "0",
    TEMPERATURE = "0.5",
    `EXT TEMPERATURE` = "0",
    ORIENTATION = "0",
    PIM = "0", PIMn = "0",
    TAT = "0", TATn = "0",
    ZCM = "0", ZCMn = "0",
    LIGHT = "0",
    `AMB LIGHT` = "0",
    `RED LIGHT` = "0",
    `GREEN LIGHT` = "0",
    `BLUE LIGHT` = "0",
    `IR LIGHT` = "0",
    `UVA LIGHT` = "0",
    `UVB LIGHT` = "0",
    STATE = "0"
  )

  expected <-
    dplyr::tibble(
      timestamp = c(
        lubridate::as_datetime(
          "2020-01-01 00:00:00",
          tz = "America/Sao_Paulo"
        ),
        lubridate::as_datetime(
          "2020-01-01 00:01:00",
          tz = "America/Sao_Paulo"
        )
      ),
      pim = 0,
      tat = 0,
      zcm = 0,
      orientation = 0,
      wrist_temperature = 0.5,
      external_temperature = 0,
      light = 0,
      ambient_light = 0,
      red_light = 0,
      green_light = 0,
      blue_light = 0,
      ir_light = 0,
      uva_light = 0,
      uvb_light = 0,
      event = 0,
      state = 0
    )

  data |>
    tidy_acttrust_data(tz = "America/Sao_Paulo") |>
    shush() |>
    expect_equal(expected)

  data <- dplyr::tibble(
    DATE = c("01/01/2020", "01/01/2020"),
    TIME = c("00:00:00", "00:01:00"),
    MS = "0",
    EVENT = "0",
    TEMPERATURE = "0,5",
    `EXT TEMPERATURE` = "0",
    ORIENTATION = "0",
    PIM = "0",
    PIMn = "0",
    TAT = "0",
    TATn = "0",
    ZCM = "0",
    ZCMn = "0",
    LIGHT = "0",
    `AMB LIGHT` = "0",
    `RED LIGHT` = "0",
    `GREEN LIGHT` = "0",
    `BLUE LIGHT` = "0",
    `IR LIGHT` = "0",
    `UVA LIGHT` = "0",
    `UVB LIGHT` = "0",
    STATE = "0"
  )

  data |>
    tidy_acttrust_data(tz = "America/Sao_Paulo") |>
    shush() |>
    expect_equal(expected)
})

test_that("tidy_acttrust_data() | Error test", {
  # checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
  tidy_acttrust_data(1, tz = "UTC") |>
    expect_error("Assertion on 'data' failed")

  tidy_acttrust_data(dplyr::tibble(), tz = "UTC") |>
    expect_error("Assertion on 'data' failed")

  tidy_acttrust_data(dplyr::tibble(a = character()), tz = "UTC") |>
    expect_error("Assertion on 'data' failed")

  # checkmate::assert_choice(tz, OlsonNames())
  tidy_acttrust_data(data = dplyr::tibble(a = 1), tz = 1) |>
    expect_error("Assertion on 'tz' failed")
})

test_that("validate_acttrust_data() | General test", {
  data <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:00"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = 0
  )

  expected <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:00"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = 0
  ) |>
    tsibble::tsibble(index = timestamp)

  data |>
    validate_acttrust_data(regularize = TRUE) |>
    shush() |>
    expect_equal(expected)

  data <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:02"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 100,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = c(0, 4, 0)
  )

  expected <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:02"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = c(0, NA, 0),
    tat = c(0, NA, 0),
    zcm = c(0, NA, 0),
    orientation = 0,
    wrist_temperature = c(as.numeric(NA), NA, NA),
    external_temperature = c(0, NA, 0),
    light = c(0, NA, 0),
    ambient_light = c(0, NA, 0),
    red_light = c(0, NA, 0),
    green_light = c(0, NA, 0),
    blue_light = c(0, NA, 0),
    ir_light = c(0, NA, 0),
    uva_light = c(0, NA, 0),
    uvb_light = c(0, NA, 0),
    event = 0,
    state = c(0, 4, 0)
  ) |>
    tsibble::tsibble(index = timestamp, regular = FALSE)

  data |>
    validate_acttrust_data(regularize = TRUE) |>
    shush() |>
    expect_equal(expected)
})

test_that("validate_acttrust_data() | Error test", {
  # checkmate::assert_tibble(data, min.cols = 1, min.rows = 1)
  1 |>
    validate_acttrust_data(regularize = TRUE) |>
    expect_error("Assertion on 'data' failed")

  dplyr::tibble() |>
    validate_acttrust_data(regularize = TRUE) |>
    expect_error("Assertion on 'data' failed")

  dplyr::tibble(a = character()) |>
    validate_acttrust_data(regularize = TRUE) |>
    expect_error("Assertion on 'data' failed")

  # checkmate::assert_flag(regularize)
  dplyr::tibble(a = 1) |>
    validate_acttrust_data(regularize = 1) |>
    expect_error("Assertion on 'regularize' failed")
})

test_that("regularize_acttrust_data() | General test", {
  data <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime(seq(0, 60 * 1000, by = 60)),
      lubridate::as_datetime("1970-01-01 16:40:30"),
      lubridate::as_datetime("1970-01-01 16:43:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = 0
  ) |>
    tsibble::tsibble(index = timestamp, regular = FALSE)

  expected <-
    data |>
    dplyr::add_row(data[1, ]) |>
    dplyr::mutate(
      timestamp = seq(
        from = lubridate::as_datetime(0),
        to = lubridate::as_datetime(60180),
        by = 60
      )
    ) |>
    dplyr::mutate(dplyr::across(
      !dplyr::matches("^timestamp$|^orientation$|^event$"),
      ~ dplyr::if_else(
        timestamp == lubridate::as_datetime(60060) |
          timestamp == lubridate::as_datetime(60120),
        as.numeric(NA),
        .x
      )
    )) |>
    dplyr::mutate(
      state = dplyr::if_else(is.na(pim), 9, state)
    )

  data |>
    regularize_acttrust_data() |>
    shush() |>
    expect_equal(expected)

  data <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime(
        seq(from = 0, to = 60 * 100, by = 60)
      ),
      lubridate::as_datetime(
        seq(from = 6030, to = 6030 + (30 * 100), by = 30)
      )
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = 0
  ) |>
    tsibble::tsibble(
      index = timestamp,
      regular = FALSE
    )

  data |>
    regularize_acttrust_data() |>
    shush() |>
    expect_equal(data)
})

test_that("regularize_acttrust_data() | Error test", {
  data <-
    dplyr::tibble(
      index = 1:10,
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(data, min_rows = 2, min_cols = 2)
  1 |>
    regularize_acttrust_data() |>
    expect_error("Assertion on 'data' failed")

  # assert_index_class(data, c("Date", "POSIXt"))
  data |>
    regularize_acttrust_data() |>
    expect_error("Assertion on 'data' failed")
})

test_that("find_offwrist_intervals() | General test", {
  data <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:02"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = c(0, 4, 0)
  ) |>
    tsibble::tsibble(
      index = timestamp,
      regular = FALSE
    )

  data |>
    find_offwrist_intervals() |>
    shush() |>
    expect_equal(
      lubridate::interval(
        "2020-01-01 00:01:02",
        "2020-01-01 00:01:02"
      )
    )

  data <- data |> dplyr::mutate(state = 0)

  data |>
    find_offwrist_intervals() |>
    shush() |>
    expect_equal(lubridate::as.interval(NA))
})

test_that("find_offwrist_intervals() | Error test", {
  data_1 <-
    dplyr::tibble(
      index = seq(
        from = as.Date("2020-01-01"),
        to = as.Date("2023-01-01"),
        by = "day"
      ),
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  data_2 <-
    dplyr::tibble(
      index = 1:10,
      x = seq_along(index)
    ) |>
    tsibble::tsibble(index = index)

  # assert_tsibble(data, min_rows = 2, min_cols = 2)
  find_offwrist_intervals(data = 1) |>
    expect_error("Assertion on 'data' failed")

  # assert_index_class(data, c("Date", "POSIXt"))
  find_offwrist_intervals(data = data_2) |>
    expect_error("Assertion on 'data' failed")

  # checkmate::assert_subset(c("timestamp", "state"), names(data))
  find_offwrist_intervals(data = data_1) |>
    expect_error("Assertion on 'c\\(\"timestamp\", \"state\"\\)' failed")
})

test_that("read_acttrust_() | General test", {
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

  object <-
    file |>
    read_acttrust_(tz = "UTC") |>
    shush()

  expected <- dplyr::tibble(
    timestamp = c(
      lubridate::as_datetime("2020-01-01 00:00:00"),
      lubridate::as_datetime("2020-01-01 00:01:00"),
      lubridate::as_datetime("2020-01-01 00:02:00")
    ),
    pim = 0,
    tat = 0,
    zcm = 0,
    orientation = 0,
    wrist_temperature = 0,
    external_temperature = 0,
    light = 0,
    ambient_light = 0,
    red_light = 0,
    green_light = 0,
    blue_light = 0,
    ir_light = 0,
    uva_light = 0,
    uvb_light = 0,
    event = 0,
    state = c(0, 4, 0)
  ) |>
    tsibble::tsibble(index = timestamp)

  expect_equal(object, expected)
})

test_that("read_acttrust_() | Error test", {
  file <- tempfile()
  writeLines("Temp", file)

  # checkmate::assert_string(file)
  read_acttrust_(file = 1, tz = "UTC") |>
    expect_error("Assertion on 'file' failed")

  # checkmate::assert_file_exists(file)
  read_acttrust_(file = "iMpoSSiBle.alien", tz = "UTC") |>
    expect_error("Assertion on 'file' failed")

  # checkmate::assert_choice(tz, OlsonNames())
  read_acttrust_(file = file, tz = 1) |>
    expect_error("Assertion on 'tz' failed")
})
