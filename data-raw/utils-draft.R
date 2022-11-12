# library(anonymizer)
# library(checkmate)
# library(magrittr)
# library(tools)
# library(utils)

anonymize_file_names <- function(dir = utils::choose.dir(), algo = "md5",
                                 seed = 0, chars = letters, n_chars = 5L) {
    choices_algo = c(
        "md5", "sha1", "crc32", "sha256", "sha512", "xxhash32", "xxhash64",
        "murmur32", "spookyhash", "blake3"
    )

    checkmate::assert_atomic_vector(dir)
    checkmate::assert_choice(algo, choices_algo)
    checkmate::assert_integerish(seed)
    checkmate::assert_character(chars)
    checkmate::assert_integerish(n_chars)

    for (i in list.files(dir)) {
        file <- file.path(dir, i)

        if (checkmate::test_file_exists(file)) {
            file_ext <- tools::file_ext(i)

            anonymize_file <- file %>%
                anonymizer::anonymize(.algo = algo, .seed = seed, .chars = chars,
                                      .n_chars = n_chars) %>%
                file.path(dir, .) %>%
                paste0(".", file_ext)

            file.rename(file, anonymize_file)
        }
    }

    NULL
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
