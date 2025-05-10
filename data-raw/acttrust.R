# library(actverse)
# library(dplyr)
# library(readr)
# library(usethis)

#' Build and save an ActTrust data file
#'
#' @description
#'
#' `build_acttrust()` reads, tidy, validate, and saves an ActTrust
#' example dataset to the `actverse` package. The raw data can be found with
#' `get_raw_data("acttrust.txt")`.
#'
#' See [`?read_acttrust`][actverse::read_acttrust()] to learn more about the
#' tiding and validating processes. See [`?acttrust`][actverse::acttrust] to
#' learn more about the data origin and structure.
#'
#' @param write_csv (optional) A [`logical`][logical()] value indicating if the
#'   function must write a `acttrust.csv` file to `"./data-raw/"`
#'   (default: `FALSE`).
#' @param write_rda (optional) A [`logical`][logical()] value indicating if the
#'   function must write a `acttrust.rda` file to `"./data/"`
#'   (default: `FALSE`).
#'
#' @return A [`tibble`][dplyr::tibble()] with a tidied and validated data from
#'   a ActTrust log.
#'
#' @family data functions
#' @noRd
#'
#' @examples
#' \dontrun{
#'   build_acttrust()
#' }
build_acttrust <- function(write_csv = FALSE, write_rda = FALSE) {
  checkmate::assert_flag(write_rda)
  checkmate::assert_flag(write_csv)

  acttrust <-
    actverse::get_raw_data("acttrust.txt") |>
    actverse::read_acttrust(
      tz = "America/Sao_Paulo",
      regularize = TRUE
    )

  if (isTRUE(write_rda)) usethis::use_data(acttrust, overwrite = TRUE)

  if (isTRUE(write_csv)) {
    acttrust |>
      tsibble::as_tibble() |>
      dplyr::mutate(timestamp = as.character(timestamp)) |>
      readr::write_csv("./data-raw/acttrust.csv")
  }

  acttrust
}
