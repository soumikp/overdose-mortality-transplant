rm(list = ls())
pacman::p_load(here, tidyverse, haven, lubridate, 
               ggpubfigs, ggsci, 
               readxl)

#source(file.path(here(), "code/helper.R"))
save = TRUE

#### finding changepoints (loess instead of joinpoint) ####

data <- read_csv(file.path(here(), "data/clean_01_joinpoint/archive/USDDKT_except_PR06232025.csv"))

data$time <- floor_date(ymd("2015-01-01") + months(data$Month - 1), "month")
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c(1, 2, 3, 4, 5, 6), 
                               labels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other"))

data <- data |> 
  add_row(data |> group_by(time) |> 
            summarise(Population = min(Population ), 
                      Tx_Count = sum(Tx_Count), 
                      Month = min(Month), 
                      Mechanism_Death = "Overall"))

data |> 
  #filter(time >= as.Date("2017-01-01")) |> 
  #mutate(Month = Month - 24) |> 
  write_csv(file.path(here(), "data/clean_01_joinpoint/2025_07_20_forJoinpoint_from2015.csv"))

data |> 
  filter(time >= as.Date("2017-01-01")) |> 
  mutate(Month = Month - 24) |> 
  write_csv(file.path(here(), "data/clean_01_joinpoint/2025_07_20_forJoinpoint_from2017.csv"))


#data$outcome <- (1e6*(data$Tx_Count/data$Population))
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other", "Overall"))


pre_joinpoint_from2017 <- data |> filter(time >= as.Date("2017-01-01")) |> 
  filter(Mechanism_Death != "Overall") |> 
  ggplot(aes(x = time, y = Tx_Count, fill = fct_rev(Mechanism_Death))) + 
  geom_col(position = "fill") + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_fill_jama() + 
  labs(fill = "Mechanism of death", 
       title = "Organ Transplantation by Donor Death Cause",    
       subtitle = "Percentage of transplanted organs from different causes of donor death from 2017-2024",    
       y = "Percentage of Total Transplants",    
       x = "", 
       caption = "Horizontal reference line marks the return of drug intoxication death percentage to early 2019 values, suggesting a decline in this donor category by late 2024.\nSource: USDDKT database (accessed MM-DD-YYYY).") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(), breaks = seq(0, 1, 0.10)) + 
  geom_hline(yintercept = 1-0.875, color="white", linetype = "dotted", linewidth = 2) #+ 
#geom_vline(xintercept = as.Date("2023-06-01"), color="white", linetype = "dotted", linewidth = 1)


pre_joinpoint_from2015 <- data |> filter(time >= as.Date("2015-01-01")) |> 
  filter(Mechanism_Death != "Overall") |> 
  ggplot(aes(x = time, y = Tx_Count, fill = fct_rev(Mechanism_Death))) + 
  geom_col(position = "fill") + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_fill_jama() + 
  labs(fill = "Mechanism of death", 
       title = "Organ Transplantation by Donor Death Cause",    
       subtitle = "Percentage of transplanted organs from different causes of donor death from 2015-2024",    
       y = "Percentage of Total Transplants",    
       x = "", 
       caption = "Horizontal reference line marks the return of drug intoxication death percentage to early 2019 values, suggesting a decline in this donor category by late 2024.\nSource: USDDKT database (accessed MM-DD-YYYY).") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) + 
  scale_x_date(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(), breaks = seq(0, 1, 0.10)) + 
  geom_hline(yintercept = 1-0.875, color="white", linetype = "dotted", linewidth = 2) #+ 
#geom_vline(xintercept = as.Date("2023-06-01"), color="white", linetype = "dotted", linewidth = 1)


if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_preJoinpoint_from2017.pdf"), 
         pre_joinpoint_from2017, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
  
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_preJoinpoint_from2015.pdf"), 
         pre_joinpoint_from2015, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}