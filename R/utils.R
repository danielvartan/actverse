timeToDec <- function(data_collumn) {
  data_collumn <- as.character(data_collumn)

  dat<- c(data_collumn)
  data_collumn <- sapply(strsplit(dat,":"),
                         function(x) {
                           x <- as.numeric(x)
                           x[1]+x[2]/60+x[3]/3600
                         }
  )

  data_collumn
}

placeholder <- function() message("Hello")
