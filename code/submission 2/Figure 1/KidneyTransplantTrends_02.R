rm(list = ls())
pacman::p_load(readr, dplyr, ggplot2, lubridate, here, stringr)
file <- file.path(here(), "code", "submission 2", "KidneyTransplantTrends_02.csv")

df1 <- read_tsv(file, show_col_types = FALSE)

## Jan 2015
df1 |> filter(pick(1)[[1]] == 1 & pick(2)[[1]] == 1) |> select(Model)

## May 2023
df1 |> filter(pick(1)[[1]] == 1 & pick(2)[[1]] == 101) |> select(Model)
