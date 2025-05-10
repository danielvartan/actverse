# library(beepr)
# library(cffr)
# library(codemetar)
# library(fs)
# library(groomr) # https://github.com/danielvartan/groomr
# library(here)
# library(readr)
# library(rutils) # https://github.com/danielvartan/rutils

# Remove empty lines from `README.md` -----

here::here("README.md") |> groomr::remove_blank_line_dups()

# Fix image links in `README.md` -----

readme_files_dir <- here::here("README_files")
readme_image_dir <- fs::path(readme_files_dir, "figure-commonmark")
pkg_image_dir <- here::here("man", "figures")

if (checkmate::test_directory_exists(readme_files_dir)) {
  if (checkmate::test_directory_exists(readme_image_dir)) {
    fs::dir_map(
      path = readme_image_dir,
      \(x) fs::file_move(x, fs::path(pkg_image_dir, basename(x)))
    )
  }

  fs::dir_delete(readme_files_dir)
}

file <- here::here("README.md")

file |>
  readr::read_lines() |>
  stringr::str_replace_all(
    pattern = "README_files/figure-commonmark/",
    replacement = "man/figures/"
  ) |>
  readr::write_lines(file)

# Update package versions in `DESCRIPTION` -----

rutils::update_pkg_versions()

# Update package year in `LICENSE`, `LICENSE.md`, and `inst/CITATION` -----

rutils::update_pkg_year()

# Update `cffr` and `codemeta` -----

cffr::cff_write()
codemetar::write_codemeta()

# Check if the script ran successfully -----

beepr::beep(1)

Sys.sleep(3)
