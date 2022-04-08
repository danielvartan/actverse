# Sort by type or by alphabetical order.
# This functions were created to be used with the `mockr` package.

is_interactive <- function(...) {
    interactive()
}

require_namespace <- function(x, ..., quietly = TRUE) {
    requireNamespace(x, ..., quietly = quietly)
}
