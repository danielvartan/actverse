# These functions were created to be used with the 'mockr' package.
# Sort by type or alphabetical order.

has_internet <- function(...) {
    require_pkg("curl")

    curl::has_internet()
}

is_interactive <- function(...) {
    interactive()
}

read_json <- function(path, simplifyVector = FALSE, ...) {
    require_pkg("jsonlite")

    jsonlite::read_json(path, simplifyVector = FALSE, ...)
}

require_namespace <- function(x, ..., quietly = TRUE) {
    requireNamespace(x, ..., quietly = quietly)
}
