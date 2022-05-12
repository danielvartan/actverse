# Sort by type or alphabetical order.

label_jump <- function(x, type = "even") {
    checkmate::assert_atomic(x)
    checkmate::assert_choice(type, c("even", "odd"))

    if (type == "even") {
        x[!seq_along(x) %% 2 == 0]
    } else if (type == "odd") {
        x[seq_along(x) %% 2 == 0]
    }
}
