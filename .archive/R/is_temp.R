# IS

library(tidyverse)
library(lubridate)

data <- test_log
x <- "pim"
timestamp <- "timestamp"

n <- length(data[[x]])
p <- 24 * 60 * 60
mean <- mean(data[[x]])


## Compute hourly means

hourly_means <- tibble(
  onset = numeric(),
  offset = numeric(),
  sum = numeric(),
  mean = numeric()
)

for (i in data[[timestamp]]) {
  onset <- i
  offset <- i + as.numeric(minutes(600 - 1))

  subset <- data %>% filter(
    !!as.symbol(timestamp) >= onset,
    !!as.symbol(timestamp) <= offset
  )

  if (!(length(subset[[timestamp]]) == 600)) {
    next()
  }

  if (offset > last(data[[timestamp]])) {
    break()
  }

  sum <- subset[[x]] %>% sum()
  mean <- subset[[x]] %>% mean()

  hourly_means <- hourly_means %>%
    add_row(onset = onset, offset = offset, sum = sum, mean = mean)
}

hourly_mean <- mean(hourly_means$mean)
