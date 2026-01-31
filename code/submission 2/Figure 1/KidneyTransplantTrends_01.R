rm(list = ls())
pacman::p_load(readr, dplyr, ggplot2, lubridate, here, stringr)
file <- file.path(here(), "code", "submission 2", "KidneyTransplantTrends_01.csv")

data <- read_csv(file)

counts <- apply(data[,-1], 2, sum)
total <- sum(apply(data[,-1], 2, sum))

result <- paste0(counts, " (", round(100*counts/total, 1), ")")
names(result) <- names(counts)
result