# library(actverse)
# library(anonymizer)
# library(checkmate)
# library(cli)
# library(lubridate)
# library(magrittr)
# library(tools)
# library(utils)

# anonymize_file_names(dir = normalizePath(readClipboard(), "/",
#                                          mustWork = FALSE))

anonymize_file_names <- function(dir = utils::choose.dir(), algo = "md5",
                                 seed = sample(1:1000, 1), chars = letters,
                                 n_chars = 5L) {
    choices_algo <- c(
        "md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64",
        "murmur32", "spookyhash", "blake3"
    )

    checkmate::assert_directory_exists(dir)
    checkmate::assert_choice(algo, choices_algo)
    checkmate::assert_integerish(seed)
    checkmate::assert_character(chars)
    checkmate::assert_integerish(n_chars)

    # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
    # nolint start: object_usage_linter.
    . <- NULL
    # nolint end

    for (i in list.files(dir)) {
        file <- file.path(dir, i)

        if (checkmate::test_file_exists(file)) {
            file_ext <- tools::file_ext(i)

            anonymize_file <- file %>%
                anonymizer::anonymize(
                    .algo = algo, .seed = seed, .chars = chars,
                    .n_chars = n_chars
                    ) %>%
                file.path(dir, .) %>%
                paste0(".", file_ext)

            file.rename(file, anonymize_file)
        }
    }

    invisible(NULL)
}

# list_files_to_clipboard(dir = normalizePath(readClipboard(), "/",
#                                             mustWork = FALSE))

list_files_to_clipboard <- function(dir = utils::choose.dir()) {
    checkmate::assert_directory_exists(dir)

    if (length(list.files(dir)) == 0) {
        cli::cli_alert_warning(paste0("No files were found."))

        return(invisible(NULL))
    }

    out <- character()

    for (i in list.files(dir)) {
        if (checkmate::test_file_exists(file.path(dir, i))) {
            out <- append(out, i)
        }
    }

    if (length(out) == 0) {
        utils::writeClipboard("NULL")
        cli::cli_alert_warning(paste0("No files were found."))
    } else {
        utils::writeClipboard(out)
        print(out)
    }

    invisible(NULL)
}

# list_newest_and_oldest_data(dir = normalizePath(readClipboard(), "/",
#                                                 mustWork = FALSE))

list_newest_and_oldest_data <- function(dir = utils::choose.dir()) {
    checkmate::assert_directory_exists(dir)

    if (length(list.files(dir)) == 0) {
        cli::cli_alert_warning(paste0("No files were found."))

        return(invisible(NULL))
    }

    out <- character()

    for (i in list.files(dir)) {
        file <- file.path(dir, i)

        if (checkmate::test_file_exists(file)) {
            data <- file %>%
                actverse:::read_acttrust_data() %>%
                actverse:::tidy_acttrust_data() %>%
                actverse:::shush()

            out <- append(out, data$timestamp[1])
        }
    }

    out <- out %>% sort()

    if (length(out) == 0) {
        cli::cli_alert_warning(paste0("No files were found."))
    } else {
        cli::cli_alert_info(paste0(
            "{.strong Number of files}: {length(out)}"
        ))

        cli::cli_alert_info(paste0(
            "{.strong Oldest file}: {out[1]}"
        ))

        cli::cli_alert_info(paste0(
            "{.strong Newest file}: {out[length(out)]}"
        ))
    }

    invisible(NULL)
}

remove_acttrust_header <- function(dir = utils::choose.dir()) {
    checkmate::assert_directory_exists(dir)

    for (i in list.files(dir)) {
        file <- file.path(dir, i)

        if (checkmate::test_file_exists(file)) {
            end_line <- find_acttrust_report_end_line(file)

            if (!is.null(end_line)) {
                lines <- readLines(file, warn = FALSE)
                writeLines(lines[- seq(end_line)], file)
            }
        }
    }
}

find_acttrust_report_end_line <- function(file, n = 50) {
    checkmate::assert_file_exists(file)

    lines <- readLines(file, n = n)
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
