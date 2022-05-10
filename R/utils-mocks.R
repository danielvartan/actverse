# These functions were created to be used with the 'mockr' package.
# Sort by type or alphabetical order.

is_interactive <- function(...) {
    interactive()
}

require_namespace <- function(x, ..., quietly = TRUE) {
    requireNamespace(x, ..., quietly = quietly)
}
