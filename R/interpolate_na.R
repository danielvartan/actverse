# Export function
interpolate_na <- function(x, index, method = "locf", mode = TRUE) {
    method_choices <- c("approx", "locf", "overall_mean", "spline")

    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    checkmate::assert_multi_class(index, c("Date", "POSIXt"))
    # assert_temporal_atomic
    checkmate::assert_choice(method, method_choices)
    require_pkg("zoo")

    mode_check <- shush(identical(x, as.integer(x))) || !is.numeric(x)

    if (isTRUE(mode) && mode_check) {
        # Return value that has highest number of occurrences (mode)
        unique <- unique(x)
        mode_value <- unique[which.max(tabulate(match(x, unique(x))))]
        x[which(is.na(x))] <- mode_value
    } else {
        data <- zoo::zoo(x = x, order.by = index)

        if (method == "approx") {
            data %>% zoo::na.approx() %>% as.numeric()
        } else if (method == "locf") {
            data %>% zoo::na.locf() %>% as.numeric()
        } else if (method == "overall_mean") {
            data %>% zoo::na.aggregate() %>% as.numeric()
        } else if (method == "spline") {
            data %>% zoo::na.spline() %>% as.numeric()
        }
    }
}
