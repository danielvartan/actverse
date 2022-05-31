data_actstudio <- function(file = file.choose()) {
  fileNames <- list.files(path = file, full.names = TRUE)

  utils::read.csv(
      file = x,
      header = T,
      sep = ";",
      dec = ".",
      stringsAsFactors = FALSE
    )
}

sleep_select <- function(x, log_Data, type_Data) {
    checkmate::assert_choice(c, 1:10)
    checkmate::assert_choice(type_Data, "actstudio")

    if (type_Data == "actstudio") {
        df <- log_Data %>% dplyr::select(DATE,TIME,STATE,ZCM,PIM,TAT) %>%
            dplyr::filter(STATE %in% x) %>%
            apply(1, function(x) x [x!= ""])
    }
}
