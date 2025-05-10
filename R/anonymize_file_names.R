# library(tools)
# library(utils)

anonymize_file_names <- function(
  dir = tcltk::tk_choose.dir(),
  algo = "md5",
  seed = sample(1:1000, 1),
  chars = letters,
  n_chars = 5L
) {
  choices_algo <- c(
    "md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64",
    "murmur32", "spookyhash", "blake3"
  )

  checkmate::assert_directory_exists(dir)
  checkmate::assert_choice(algo, choices_algo)
  checkmate::assert_integerish(seed)
  checkmate::assert_character(chars)
  checkmate::assert_integerish(n_chars)

  # R CMD Check variable bindings fix
  # nolint start
  . <- NULL
  # nolint end

  for (i in list.files(dir)) {
    file <- fs::path(dir, i)

    if (checkmate::test_file_exists(file)) {
      anonymize_file <-
        file |>
        anonymizer::anonymize(
          .algo = algo,
          .seed = seed,
          .chars = chars,
          .n_chars = n_chars
        ) %>%
        fs::path(dir, .) %>%
        paste0(".", fs::path_ext(i))

      fs::file_move(file, anonymize_file)
    }
  }

  invisible()
}

list_files_to_clipboard <- function(
  dir = tcltk::tk_choose.dir()
) {
  checkmate::assert_directory_exists(dir)

  if (length(list.files(dir)) == 0) {
    cli::cli_alert_warning("No files were found.")

    invisible()
  } else {
    out <- character()

    for (i in list.files(dir)) {
      if (checkmate::test_file_exists(file.path(dir, i))) {
        out <- c(out, i)
      }
    }

    if (length(out) == 0) {
      clipr::write_clip("NULL")

      cli::cli_alert_warning("No files were found.")
    } else {
      clipr::write_clip(out)

      print(out)
    }

    invisible()
  }
}

list_newest_and_oldest_data <- function(
  dir = tcltk::tk_choose.dir()
) {
  checkmate::assert_directory_exists(dir)

  if (length(list.files(dir)) == 0) {
    cli::cli_alert_warning("No files were found.")

    invisible()
  } else {
    out <- character()

    for (i in list.files(dir)) {
      file <- fs::path(dir, i)

      if (checkmate::test_file_exists(file)) {
        data <-
          file |>
          read_acttrust_data() |>
          tidy_acttrust_data() |>
          shush()

        out <- c(out, data$timestamp[1])
      }
    }

    out <- out |> sort()

    if (length(out) == 0) {
      cli::cli_alert_warning("No files were found.")
    } else {
      cli::cli_alert_info("{.strong Number of files}: {length(out)}")
      cli::cli_alert_info("{.strong Oldest file}: {out[1]}")
      cli::cli_alert_info("{.strong Newest file}: {out[length(out)]}")
    }

    invisible()
  }
}

remove_acttrust_header <- function(
  dir = tcltk::tk_choose.dir()
) {
  checkmate::assert_directory_exists(dir)

  for (i in list.files(dir)) {
    file <- fs::path(dir, i)

    if (checkmate::test_file_exists(file)) {
      end_line <- find_acttrust_report_end_line(file)

      if (!is.null(end_line)) {
        lines <- readLines(file, warn = FALSE)

        readr::write_lines(lines[-seq(end_line)], file)
      }
    }
  }
}

find_acttrust_report_end_line <- function(file, n = 50) {
  checkmate::assert_file_exists(file)

  lines <- readr::read_lines(file, n = n)
  i <- NULL

  if (grepl("Condor Instruments Report", lines[1])) {
    for (i in seq(2, length(lines))) {
      if (grepl("^\\+\\-\\-\\-", lines[i]) &&
            grepl("\\-\\-\\-\\+$", lines[i])) {
        break
      }
    }
  }

  i
}
