pacman::p_load(here, tidyverse, haven, lubridate, 
               ggpubfigs, ggsci, 
               readxl)

#source(file.path(here(), "code/helper.R"))
save = FALSE

#### finding changepoints (loess instead of joinpoint) ####

data <- read_csv(file.path(here(), "data/clean_01_joinpoint/USDDKT_except_PR06232025.csv"))

data$Month <- floor_date(ymd("2015-01-01") + months(data$Month - 1), "month")
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c(1, 2, 3, 4, 5, 6), 
                               labels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other"))
data$outcome <- (1e6*(data$Tx_Count/data$Population))

data <- data |> 
  add_row(data |> 
            group_by(Month) |> 
            summarize(Population = min(Population),
                      outcome = sum(outcome), 
                      Mechanism_Death = "Overall", 
                      Tx_Count = sum(Tx_Count)))

data$outcome_se <- (1e6*(data$Tx_Count/data$Population)*(1 - (data$Tx_Count/data$Population))/data$Population)
data$outcome_se <- sprintf("%.15f", data$outcome_se)

data <- data |> 
  inner_join(as_tibble(sort(unique(data$Month))) |> 
               add_column(index = 1:120) |> rename(Month = value))

write_csv(data |> select(c(index, Mechanism_Death, outcome, outcome_se)), 
          file.path(here(), "data/clean_01_joinpoint/2025_07_20_forJoinpoint.csv"))


# data <- data |> 
#   group_by(Mechanism_Death) |> 
#   arrange(Month, .by_group = TRUE) |> 
#   mutate(smooth_outcome = stats::filter(outcome, rep(1/12, 12), sides = 1)) |> 
#   drop_na() |> 
#   ungroup()


change_points <- data |> 
  mutate(Month_numeric = as.numeric(Month)) |> 
  group_by(Mechanism_Death) |> 
  mutate(fitted = predict(loess(outcome ~ Month_numeric, span = 1))) |> 
  mutate(slope = (fitted - lag(fitted))) |> 
  mutate(slope_sign = sign(slope),
         slope_sign_prev = lag(slope_sign),
         slope_change = !is.na(slope_sign_prev) & slope_sign != slope_sign_prev,
         slope_before = ifelse(slope_change, lag(slope), NA),
         slope_after = ifelse(slope_change, slope, NA),
         change_magnitude = ifelse(slope_change, abs(slope_after - slope_before), NA)) |> 
  filter(slope_change) |> 
  select(Month, slope_before, slope_after, outcome, fitted) |> 
  ungroup()

plot <- data |> 
  ggplot(aes(x = Month, y = outcome, group = Mechanism_Death)) + 
  geom_point(aes(color = Mechanism_Death), alpha = 0.5) + 
  geom_smooth(aes(color = Mechanism_Death), se = FALSE) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = change_points, aes(xintercept = Month, color = Mechanism_Death), 
             linetype = "dashed", linewidth = 0.75,
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_jama() + 
  labs(color = "Mechanism of death", 
       y = "Monthly kidney transplant rate (per million)", x = "Date", 
       title = "Monthly Kidney Transplant Rates by Mechanism of Death in the United States", 
       subtitle = "Temporal trends from 2015-2024 with identified change points in transplant rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows monthly kidney transplant rates per million population by donor death mechanism.\n", 
                        "2. Points represent observed rates, solid lines show smoothed trends using loess regression (span = 1), and dashed vertical lines mark time points where the direction of the trend changed.\n", 
                        "3. Change points were identified by examining the slope of the smoothed trend lines over time.\n", 
                        "4. Source: USDDKT database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1))
  
if(save){
  ggsave(file.path("documents", "2025_07_15_timeSeriesPlot.pdf"), 
         plot, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
         )
}






#### modeling change over time ####

data <- read_csv(file.path(here(), "data/raw", "VSRR_Provisional_Drug_Overdose_Death_Counts.csv"))

population_pre2021 <- read_xlsx(file.path(here(), "data/raw", "US CENSUS data 2010-2020.xlsx"), skip = 3) |> 
  select(c(1, 2)) |> 
  drop_na() |> 
  mutate(time = seq_along(`2010`)) |> 
  select(-1) |> 
  rename(population = 1) |> 
  mutate(date = months(time-1) + ymd("2010-04-01"))|> 
  select(-c(2))

population_post2020 <- read_xlsx(file.path(here(), "data/raw", "US CENSUS data 2020-.xlsx"), skip = 13) |> 
  select(c(1, 2)) |> 
  drop_na() |> 
  mutate(time = seq_along(`2021`)) |> 
  select(-1) |> 
  rename(population = 1) |> 
  mutate(date = months(time-1) + ymd("2021-01-01")) |> 
  select(-c(2))

population <- rbind(population_pre2021, 
                    population_post2020)

indicator_types <- data |> pull(Indicator) |> unique()
target_indicators <- indicator_types[c(9, 1, 11, 8)]


data <- data |> 
  filter(State == "US") |> 
  filter(Indicator %in% target_indicators) |> 
  mutate(date = ymd(paste0(Year, "-", Month, "-01"))) |> 
  select(c(date, `Data Value`, Indicator)) |> 
  rename(deaths = `Data Value`) |> 
  inner_join(population) |> 
  mutate(Indicator = factor(Indicator, 
                            levels = target_indicators[c(4, 1, 2, 3)], 
                            labels = c("All drugs overdose deaths", "Opioids", "Cocaine", "Psychostimulants"))) |> 
  mutate(death_rate = 1e6*deaths/population) 

change_points <- data |> 
  mutate(date_numeric = as.numeric(date)) |> 
  group_by(Indicator) |> 
  mutate(fitted = predict(loess(death_rate ~ date_numeric, span = 0.25))) |> 
  mutate(slope = (fitted - lag(fitted))) |> 
  mutate(slope_sign = sign(slope),
         slope_sign_prev = lag(slope_sign),
         slope_change = !is.na(slope_sign_prev) & slope_sign != slope_sign_prev,
         slope_before = ifelse(slope_change, lag(slope), NA),
         slope_after = ifelse(slope_change, slope, NA),
         change_magnitude = ifelse(slope_change, abs(slope_after - slope_before), NA)) |> 
  filter(slope_change) |> 
  select(date, slope_before, slope_after, death_rate, fitted) |> 
  filter(date == max(date)) |> 
  ungroup()

plot <- data |> 
  ggplot(aes(x = date, y = death_rate, group = Indicator)) + 
  geom_point(aes(color = Indicator), alpha = 0.5) + 
  geom_line(aes(color = Indicator), linewidth = 1) + 
  theme_bw() + 
  geom_vline(data = change_points, aes(xintercept = date, color = Indicator), 
             linetype = "dashed", linewidth = 0.75, alpha = 0.5,
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_jama() + 
  labs(color = "Mechanism of overdose", 
       y = "12-month rolling death rate (per million)", x = "Date", 
       title = "12-month rolling drug overdose-related death rate by mechanism of overdose in the United States", 
       subtitle = "Temporal trends from 2015-2024 with identified change points in death rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows 12-month rolling death rates per million population by overdose mechanism.\n", 
                        "2. Points represent observed rates, solid lines show trends, and dashed vertical lines mark time points where the direction of the trend changed.\n", 
                        "3. Change points were identified by examining the slope of the trend lines over time.\n", 
                        "4. Source: XXXXX database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1))


if(save){
  ggsave(file.path("documents", "2025_07_15_timeSeriesPlot2.pdf"), 
         plot, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}




