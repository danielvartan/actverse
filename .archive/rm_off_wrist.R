rm_off_wrist <- function(data, initial_date, final_date, timestamp="timestamp"){
    off_wrist <- subset(data, data$timestamp >= initial_date & data$timestamp <= final_date)
    data_without_off_wrist <- data[!(data$timestamp %in% off_wrist$timestamp),]

    return (data_without_off_wrist)
}
