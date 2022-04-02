# Export function
interpolate_na <- function(x, index, method = "locf") {
    method_choices <- c("approx", "locf", "overall_mean", "spline")

    checkmate::assert_numeric(x, min.len = 1, all.missing = FALSE)
    checkmate::assert_multi_class(index, c("Date", "POSIXt"))
    # assert_temporal_atomic
    checkmate::assert_choice(method, method_choices)
    require_pkg("zoo")

    data <- zoo::zoo(x = x, order.by = index)

    if (method == "locf") {
        data %>% zoo::na.locf() %>% as.numeric()
    } else if (method == "spline") {
        data %>% zoo::na.spline() %>% as.numeric()
    } else if (method == "approx") {
        data %>% zoo::na.approx() %>% as.numeric()
    } else if (method == "overall_mean") {
        data %>% zoo::na.aggregate() %>% as.numeric()
    }
}
