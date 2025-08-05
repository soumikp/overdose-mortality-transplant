rm(list = ls())
pacman::p_load(here, tidyverse, haven, lubridate, 
               ggpubfigs, ggsci, stringr, 
               readxl)

#source(file.path(here(), "code/helper.R"))
save = TRUE

#### finding joinpoint - from 2015 ####

data <- read_tsv(file.path(here(), "data/clean_01_joinpoint/2025_07_20_from2015/export.Export.Data.txt"))
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c(0, 1, 2, 3, 4, 5, 6), 
                               labels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other", "Overall"))
data$Month <- floor_date(ymd("2015-01-01") + months(data$Month - 1), "month")


plot_main_from2015 <- data |> 
  filter(Mechanism_Death %in% c("Drug intoxication", "Overall")) |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = `Crude Rate`, group = Mechanism_Death, color = Mechanism_Death), alpha = 0.75) + 
  geom_line(aes(y = Model, color = Mechanism_Death), linewidth = 1) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
               filter(Joinpoints == `Final Selected Model`) |> 
               filter(Mechanism_Death %in% c("Drug intoxication", "Overall")), 
             aes(xintercept = Month, color = Mechanism_Death), linetype = "dashed", 
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_manual(values = pal_jama("default")(7)[c(6, 7)]) + 
  labs(color = "Mechanism of death", 
       y = "Monthly kidney transplant rate (per million)", x = "Date", 
       title = "Monthly Kidney Transplant Rates by Mechanism of Death in the United States", 
       subtitle = "Temporal trends from 2015-2024 with identified change points in transplant rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows monthly kidney transplant rates per million population by donor death mechanism.\n", 
                        "2. Points represent observed rates, solid lines show smoothed trends using joinpoint analysis; dashed vertical lines mark change points where the trend changed.\n", 
                        "3. Source: USDDKT database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1))


plot_supp_from2015 <- data |> 
  filter(Mechanism_Death != "Overall") |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = `Crude Rate`, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), alpha = 0.5) + 
  geom_smooth(aes(y = Model, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), se = FALSE, linewidth = 1, span = 0.2) + 
  #geom_line(aes(y = Model, color = fct_rev(Mechanism_Death)), linewidth = 1) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
               filter(Joinpoints == `Final Selected Model`) |> 
               filter(Mechanism_Death != "Overall"), 
             aes(xintercept = Month, color = fct_rev(Mechanism_Death)), linetype = "dashed", 
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_manual(values = (pal_jama("default")(7))) + 
  labs(color = "Mechanism of death", 
       y = "Monthly kidney transplant rate (per million)", x = "Date", 
       title = "Monthly Kidney Transplant Rates by Mechanism of Death in the United States", 
       subtitle = "Temporal trends from 2015-2024 with identified change points in transplant rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows monthly kidney transplant rates per million population by donor death mechanism.\n", 
                        "2. Points represent observed rates, solid lines show smoothed trends using joinpoint analysis; dashed vertical lines mark change points where the trend changed.\n", 
                        "3. Source: USDDKT database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1, reverse = TRUE))


if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointMain_from2015.pdf"), 
         plot_main_from2015, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}

if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointSupp_from2015.pdf"), 
         plot_supp_from2015, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}




#### finding joinpoint - from 2017 ####

data <- read_tsv(file.path(here(), "data/clean_01_joinpoint/2025_07_20_from2017/export.Export.Data.txt"))
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c(0, 1, 2, 3, 4, 5, 6), 
                               labels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other", "Overall"))
data$Month <- floor_date(ymd("2017-01-01") + months(data$Month - 1), "month")


plot_main_from2017 <- data |> 
  filter(Mechanism_Death %in% c("Drug intoxication", "Overall")) |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = `Crude Rate`, group = Mechanism_Death, color = Mechanism_Death), alpha = 0.75) + 
  geom_smooth(aes(y = Model, group = Mechanism_Death, color = Mechanism_Death), linewidth = 1, se = FALSE, span = 0.3) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
               filter(Joinpoints == `Final Selected Model`) |> 
               filter(Mechanism_Death %in% c("Drug intoxication", "Overall")), 
             aes(xintercept = Month, color = Mechanism_Death), linetype = "dashed", 
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_manual(values = pal_jama("default")(7)[c(6, 7)]) + 
  labs(color = "Mechanism of death", 
       y = "Monthly kidney transplant rate (per million)", x = "Date", 
       title = "Monthly Kidney Transplant Rates by Mechanism of Death in the United States", 
       subtitle = "Temporal trends from 2017-2024 with identified change points in transplant rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows monthly kidney transplant rates per million population by donor death mechanism.\n", 
                        "2. Points represent observed rates, solid lines show smoothed trends using joinpoint analysis; dashed vertical lines mark change points where the trend changed.\n", 
                        "3. Source: USDDKT database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1))


plot_supp_from2017 <- data |> 
  filter(Mechanism_Death != "Overall") |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = `Crude Rate`, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), alpha = 0.5) + 
  geom_smooth(aes(y = Model, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), se = FALSE, linewidth = 1, span = 0.3) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
               filter(Joinpoints == `Final Selected Model`) |> 
               filter(Mechanism_Death != "Overall"), 
             aes(xintercept = Month, color = fct_rev(Mechanism_Death)), linetype = "dashed", 
             show.legend = FALSE) + 
  theme(
    legend.position = "bottom", 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(color = "white"), 
    plot.caption = element_text(hjust = 0)
  ) + 
  scale_color_manual(values = (pal_jama("default")(7))) + 
  labs(color = "Mechanism of death", 
       y = "Monthly kidney transplant rate (per million)", x = "Date", 
       title = "Monthly Kidney Transplant Rates by Mechanism of Death in the United States", 
       subtitle = "Temporal trends from 2017-2024 with identified change points in transplant rate", 
       caption = paste0("Notes:\n", 
                        "1. Data shows monthly kidney transplant rates per million population by donor death mechanism.\n", 
                        "2. Points represent observed rates, solid lines show smoothed trends using joinpoint analysis; dashed vertical lines mark change points where the trend changed.\n", 
                        "3. Source: USDDKT database (accessed MM-DD-YYYY).")) +
  guides(colour = guide_legend(nrow = 1, reverse = TRUE))


if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointMain_from2017.pdf"), 
         plot_main_from2017, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}

if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointSupp_from2017.pdf"), 
         plot_supp_from2017, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}
