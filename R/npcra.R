#' Non-Parametric Function M10 (Most Active 10 Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "M10 represents activity during the most active period of the day. This
#' measure may be influenced by daytime napping" - WITTING, W. (1990)
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the 10 most active hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param timestamp String with the name of the column that contains the
#'   dataframe dates.
#' @param method 1 = whole period, 2 = average day, 3 = each day
#' @param fast True: It makes the M10 quick calculation, prone to errors; False:
#'   Scans completely and returns the correct M10 value, but more slowly
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window in the period in the second position.
#'
#' @family NPCRA functions
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10(test_log, "pim")}
npcra_m10 <- function(data, col_name = "pim", timestamp="timestamp", method=1, fast=TRUE) {
    time_begin <- Sys.time()
    npcra_test_args(data, col_name, timestamp, method, fast)

    m10 <- data.frame()
    if (method==1) {
        m10 <- npcra_m10_whole_period(data, col_name, timestamp)
    }
    if (method==2) {
       m10 <- npcra_m10_average_day(data, col_name, timestamp)
    }
    if (method==3) {
        m10 <- npcra_m10_each_day(data, col_name, timestamp)
    }

    duration <- Sys.time() - time_begin
    message("M10 was calculated in ", duration, " seconds")

    m10
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for each day
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "M10 represents activity during the most active period of the day. This
#' measure may be influenced by daytime napping" - WITTING, W. (1990) This
#' calculation method considers the 10-hour window most active for each day.
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the 10 most active hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param timestamp String with the name of the column that contains the
#'   dataframe dates.
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window by day in the second position.
#'
#' @family NPCRA functions
#' @importFrom lubridate hours
#' @importFrom lubridate years
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10_each_day(test_log, fast = FALSE)}
npcra_m10_each_day <- function(data, col_name = "pim", timestamp="timestamp"){
    time_begin <- Sys.time()
    valid_data <- data %>%
        dplyr::select(timestamp, col_name) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_name) %>%
        dplyr::mutate(day = lubridate::ymd(strftime(timestamp, format="%y%m%d")),
                      endWindow = timestamp+hours(10),
                      hourEndWindow = as.numeric(strftime(endWindow,format="%H")),
                      mean_10_hours = 0,
                      )

    unique_days <- unique(valid_data$day)
    index_first_days <- match(unique_days,valid_data$day)
    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    quant_days <- length(unique_days)

    for (index_day in seq(quant_days)){
        index = index_first_days[index_day]
        value_to_remove <- 0
        sum_in_10_hours <- 0
        window_index <- index
        current_day <- day(unique_days[index_day])
        while (index < index_last_valid_register & valid_data$hourEndWindow[index] != 0) {
            end_window <- valid_data$endWindow[index]
            window_sum <- 0
            while (valid_data$timestamp[window_index] <= end_window) {
                window_sum <- window_sum + valid_data$x[window_index]
                window_index <- window_index+1
            }
            sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
            valid_data$mean_10_hours[index] <- sum_in_10_hours/(window_index-index)
            value_to_remove <- valid_data$x[index]
            index<-index+1
        }
    }

    m10 <- tapply(valid_data$mean_10_hours, valid_data$day, max)
    m10_first_date <- valid_data$timestamp[match(m10,valid_data$mean_10_hours)]
    m10_each_day <- cbind.data.frame(m10, unique_days, m10_first_date)
    m10_each_day <- m10_each_day %>%
        dplyr::rename("day"=unique_days, "start_date"=m10_first_date) %>%
        dplyr::as_tibble()

    duration <- round(Sys.time()-time_begin,digits = 2)
    message("Time spent: ", duration, " seconds")
    m10_each_day
}

#' Non-Parametric Function M10 (Most Active 10 Hours) for the entire period
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "M10 represents activity during the most active period of the day. This
#' measure may be influenced by daytime napping" - WITTING, W. (1990) This
#' calculation method considers the 10-hour window most active for all data.
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the 10 most active hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param timestamp String with the name of the column that contains the
#'   dataframe dates.
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window in the period in the second position.
#'
#' @family NPCRA functions
#' @importFrom lubridate hours
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_m10_whole_period(test_log, fast = FALSE)}

npcra_m10_whole_period <- function(data, col_name = "pim", timestamp="timestamp") {
    time_begin <- Sys.time()
    valid_data <- data %>%
        dplyr::select(timestamp, col_name) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_name) %>%
        dplyr::mutate(endWindow = timestamp+lubridate::hours(10)) %>%
        dplyr::mutate(m10 = 0)

    index_last_valid_register <- which.min(valid_data$endWindow < last(valid_data$timestamp))
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0

    for (index in seq_len(index_last_valid_register-1)) {
        window_sum <- 0
        while (valid_data$timestamp[window_index] < valid_data$endWindow[index]) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
        }
        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$m10[index] <- sum_in_10_hours / (window_index-index)
    }

    max_m10 <- max(valid_data$m10)
    m10_data <- valid_data %>%
        dplyr::select(m10, timestamp) %>%
        dplyr::filter(m10 == max_m10) %>%
        dplyr::rename("start_date"=timestamp)

    duration <- round(Sys.time()-time_begin,digits = 2)
    message("Time spent: ", duration, " seconds")
    m10_data
}

npcra_m10_average_day <- function(data, col_name = "pim", timestamp="timestamp") {
    time_begin <- Sys.time()
    valid_data <- data %>%
        dplyr::select(timestamp, col_name) %>%
        dplyr::rename("timestamp" = timestamp, "x" = col_name) %>%
        dplyr::mutate(hour = as.POSIXct(format(timestamp, format="%H:%M:%S"),
                                        format="%T", tz = "GMT")) %>%
        dplyr::mutate(m10 = 0) %>%
        dplyr::mutate(endWindow = as.POSIXct(format(timestamp+lubridate::hours(10), format="%H:%M:%S"),
                                        format="%T", tz = "GMT")) %>%
        dplyr::select(x, hour, endWindow, m10) %>%
        dplyr::arrange(hour)
    return (valid_data)
    first_10_hours <- valid_data %>%
        dplyr::filter(hour<=10 & hour>=0)

    valid_data <- rbind.data.frame(valid_data, first_10_hours)
    index<-1
    window_index <- 1
    value_to_remove <- 0
    sum_in_10_hours <- 0
    while (index <= dplyr::n_distinct(valid_data)) {
        window_sum <- 0
        #TO DO: ajuste da janela à meia-noite
        while (difftime(valid_data$hour[window_index], valid_data$hour[index], units = "hour") <10) {
            window_sum <- window_sum + valid_data$x[window_index]
            window_index <- window_index + 1
        }
        sum_in_10_hours <- sum_in_10_hours - value_to_remove + window_sum
        value_to_remove <- valid_data$x[index]
        valid_data$m10[index] <- sum_in_10_hours / (window_index-index)

        message(index, " | ", valid_data$hour[index], " | ", valid_data$m10[index], " | ", window_index)
        index <- index+1
    }
    duration <- round(Sys.time()-time_begin,digits = 2)
    message("Time spent: ", duration, " seconds")
    valid_data
}

#' Non-Parametric Function L5 (Least Active 5  Hours)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "L5 represents movement-activity during sleep plus nighttime arousals" -
#' WITTING, W. (1990)
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the least Active 5  Hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param method 1 = whole period, 2 = average day, 3 = each day
#' @param fast True: It makes the L5 quick calculation, prone to errors; False:
#'   Scans completely and returns the correct L5 value, but more slowly
#'
#' @return a Dataframe with the value of M10 in the first position and the start
#'   date of the 10 most active window in the period in the second position.
#'
#' @family NPCRA functions
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_l5(test_log, "pim")}
npcra_l5 <- function(data, col_name = "pim", timestamp="timestamp", method=1, fast=TRUE) {
    time_begin <- Sys.time()
    npcra_test_args(data, col_name, timestamp, method, fast)

    l5 <- data.frame()

    #CALCULATION OF L5 ACCORDING TO THE CHOSEN METHOD
    if(method==1){
        l5 <- npcra_l5_whole_period(data, col_name, timestamp, fast)
    }

    duration <- Sys.time() - time_begin
    message("L5 was calculated in ", duration, " seconds")

    l5
}

#' Non-Parametric Function L5 (Least Active 5  Hours) for the entire period
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "L5 represents movement-activity during sleep plus nighttime arousals" -
#' WITTING, W. (1990) This calculation method considers the 5-hour window less
#' active for all data.
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the least Active 5  Hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param timestamp String with the name of the column that contains the
#'   dataframe dates.
#' @param fast True: It makes the L5 quick calculation, prone to errors; False:
#'   Scans completely and returns the correct L5 value, but more slowly
#'
#' @return a Dataframe with the value of L5 in the first position and the start
#'   date of the 5 least active window in the period in the second position.
#'
#' @family NPCRA functions
#' @importFrom lubridate hours
#' @export
#' @examples
#' \dontrun{
#' npcra_l5_whole_period(test_log, fast = FALSE)}
npcra_l5_whole_period <- function(data, col_name = "pim", timestamp="timestamp", fast=TRUE) {
    sum_in_5_hours <- 0
    window_index <- 0
    index <- 1

    valid_data <- cbind.data.frame(data[, timestamp], data[, col_name])
    colnames(valid_data) <- c("timestamp", "x")

    end_first_window <- valid_data$timestamp[index] + hours(5)
    while (valid_data$timestamp[index + window_index] <= end_first_window) {
        sum_in_5_hours <- sum_in_5_hours + valid_data$x[index + window_index]
        window_index <- window_index + index
    }
    l_5 <- sum_in_5_hours / (window_index - 1)
    l_5_first_date <- valid_data$timestamp[index]

    last_valid_register <- last(valid_data$timestamp) - hours(5)
    index <- 2
    if (fast) {
        while (valid_data$timestamp[index] <= last_valid_register) {
            sum_in_5_hours <- sum_in_5_hours - valid_data$x[index - 1]
            sum_in_5_hours <- sum_in_5_hours + valid_data$x[index + window_index]
            if (sum_in_5_hours / (window_index) < l_5) {
                l_5 <- sum_in_5_hours / (window_index)
                l_5_first_date <- valid_data$timestamp[index]
            }
            index <- index + 1
        }
    }
    else {
        while (valid_data$timestamp[index] <= last_valid_register) {
            window_sum <- 0
            finish_window <- valid_data$timestamp[index] + hours(5)
            while (valid_data$timestamp[window_index] < finish_window) {
                window_sum <- window_sum + valid_data$x[window_index]
                window_index <- window_index + 1
            }
            sum_in_5_hours <-
                sum_in_5_hours - valid_data$x[index - 1] + window_sum
            if (sum_in_5_hours / (window_index - index) < l_5) {
                l_5 <- sum_in_5_hours / (window_index - index)
                l_5_first_date <- valid_data$timestamp[index]
                message("New L5: ", l_5, " on date ", l_5_first_date)
            }
            index <- index + 1
        }
    }

    data.frame(l_5, l_5_first_date)
}

#' Non-Parametric Function IS (Interdaily Stability )
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "Alterations in IS may indicate a  loose coupling between the rest-activity
#' rhythm and its supposedly stable "Zeitgebers," as IS decreases with higher
#' day-to-day variation (i.e. ,  a  looser coupling) of the activity patterns" -
#' WITTING, W. (1990)
#'
#' @param data Dataframe containing the column of numeric values that will be
#'   used as X in the calculation.
#' @param col_x String with the name of the column that will be used in the
#'   calculation.
#' @param col_date String with the name of the column with date values.
#'
#' @return A numeric value.
#' @family NPCRA functions
#' @importFrom lubridate day
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_is(test_log, "pim", "timestamp")}
npcra_is <- function(data, col_x = "pim", col_date = "timestamp"){
    #Attention: Wrong answer yet
    x_values <- as.vector(rbind(data[,col_x]))
    x_values <- unlist(x_values)

    n <- length(x_values)

    day_of_x <- day(data$timestamp)
    p <- as.integer(mean(table(day(data$timestamp))))

    avg_x <- mean(x_values)
    denominator <- sum((x_values - avg_x)^2)
    denominator <- n*denominator

    dados <- data.frame(cbind(x_values), day_of_x)

    #media do grupo
    dados$sqr_diff_from_day <- with(dados,x_values-ave(x_values,day_of_x))

    numerator <- p*sum(dados$sqr_diff_from_day^2)

    is <- numerator/denominator
    print(n)
    print(p)
    print(numerator)
    print(denominator)
    is
}

#' Non-Parametric Function IV (Intradaily  Variability)
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' "IV was included to detect fragmentation of the rest-activity rhythms. High
#' IV may be an indicative of daytime napping and/or nighttime arousals" -
#' WITTING, W. (1990)
#'
#' @param data Dataframe containing the column of numeric values that will be
#'   used as X in the calculation.
#' @param col_x String with the name of the column that will be used in the
#'   calculation.
#'
#' @return A numeric value.
#' @family NPCRA functions
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_iv(test_log, "body_temperature")}
npcra_iv <- function(data, col_x = "pim"){
    #creates a vector containing only col_x data
    data <- as.vector(rbind(data[col_x]))
    data <- unlist(data)

    n <- length(data)

    #numerator of function iv
    #sum the squares of the differences of an element and its next
    square_diff <- diff(data)
    square_diff <- square_diff^2
    sum_diff <- sum(square_diff)
    numerator <- n*sum_diff

    #numerator of function iv
    #calculates the data variance and multiplies by (n-1)
    #var() function divides by n-1 by default, so it was squared
    denonimator <- var(data)
    denonimator <- denonimator*(n-1)^2

    iv <- numerator/denonimator
    iv
}

#' Testing the arguments of nonparametric functions
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Performs the verification of the types of expected values as arguments to the
#' npcra functions.
#'
#' @param data Dataframe that contains the date variable and the variable that
#'   will be used to identify the 10 most active hours.
#' @param col_name String with the name of the column that will be used in the
#'   calculation.
#' @param timestamp String with the name of the column that contains the
#'   dataframe dates.
#' @param method 1 = whole period, 2 = average day, 3 = each day
#' @param fast True: It makes the M10 quick calculation, prone to errors; False:
#'   Scans completely and returns the correct M10 value, but more slowly
#'
#' @return If no test stops, TRUE will be returned.
#' @family NPCRA functions
#' @export
#'
#' @examples
#' \dontrun{
#' npcra_test_args(test_log, "pim", "timestamp", 1, TRUE)}
npcra_test_args <- function(data, col_name, timestamp, method, fast) {
    data_col_names <- colnames(data)

    if (!is.data.frame(data)) {
        stop("Parameter 'data' expects a dataframe but received a ", class(data))
    }
    if (!is.element(col_name, data_col_names)) {
        stop("Parameter 'col_name' = ", col_name, " is not a column of the given dataframe")
    }
    if (!is.element(timestamp, data_col_names)) {
        stop("Parameter 'timestamp' = ", timestamp, " is not a column of the given dataframe")
    }
    if (!is.element(method, c(1,2,3))) {
        stop("Parameter 'method' expects an integer value equal to 1,2 or 3, but received ", method, " (classe ", class(method), ")")
    }
    if (!is.element(fast, c(TRUE,FALSE))) {
        stop("Parameter 'fast' expects an logical value (TRUE or FALSE), but received ", fast, " (classe ", class(fast), ")")
    }
    if(any(is.na(data[,col_name]))){
        stop("Column 'col_name' = ", col_name, " has NA values")
    }
    if(any(is.na(data[,timestamp]))){
        stop("Column 'timestamp' = ", timestamp, " has NA values")
    }

    col_name_class <- lapply(data[, col_name], class)
    col_timestamp_class <- unlist(col_name_class)
    if (!is.element("numeric", col_name_class)) {
        stop("col_name must be a column that has only 'numeric' values. col_name is composed of: ", col_name_class)
    }

    col_timestamp_class <- lapply(data[, timestamp], class)
    col_timestamp_class <- unlist(col_timestamp_class)
    if (!is.element("POSIXct", col_timestamp_class) || !is.element("POSIXt", col_timestamp_class)) {
        stop("timestamp must be a column that has 'POSIXct' or 'POSIXt' values. timestamp is composed of: ", col_timestamp_class)
    }

    TRUE
}
