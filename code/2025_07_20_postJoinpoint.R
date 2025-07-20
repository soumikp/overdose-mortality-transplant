pacman::p_load(here, tidyverse, haven, lubridate, 
               ggpubfigs, ggsci, stringr, 
               readxl)

#source(file.path(here(), "code/helper.R"))
save = TRUE

#### finding changepoints (loess instead of joinpoint) ####

data <- read_tsv(file.path(here(), "data/clean_01_joinpoint/2025_07_20_joinpoint.Export.Data.txt"))
data$Mechanism_Death <- factor(data$Mechanism_Death, 
                               levels = c(0, 1, 2, 3, 4, 5, 6), 
                               labels = c("Drug intoxication", "Cardiovascular", 
                                          "Blunt injury", "Stroke", 
                                          "Gunshot wound", "Other", "Overall"))
data$Month <- floor_date(ymd("2015-01-01") + months(data$index - 1), "month")




plot_main <- data |> 
  filter(index > 24) |>
  filter(Mechanism_Death %in% c("Drug intoxication", "Overall")) |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = outcome, group = Mechanism_Death, color = Mechanism_Death), alpha = 0.5) + 
  geom_smooth(aes(y = Model, group = Mechanism_Death, color = Mechanism_Death), se = FALSE, linewidth = 1, span = 0.175) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
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


plot_supp <- data |> 
  filter(index > 24) |>
  filter(Mechanism_Death != "Overall") |> 
  ggplot(aes(x = Month)) + 
  geom_point(aes(y = outcome, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), alpha = 0.5) + 
  geom_smooth(aes(y = Model, group = Mechanism_Death, color = fct_rev(Mechanism_Death)), se = FALSE, linewidth = 1, span = 0.2) + 
  facet_wrap(~Mechanism_Death, ncol = 2, scales = "free_y") + 
  theme_bw() + 
  geom_vline(data = data |> filter(str_detect(Flag, "Joinpoint")) |> 
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
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointMain.pdf"), 
         plot_main, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}

if(save){
  ggsave(file.path(here(), "documents", "2025_07_20_joinpoint", "2025_07_20_postJoinpointSupp.pdf"), 
         plot_supp, 
         height = 8.5, 
         width = 11, 
         units = "in", 
         device = pdf
  )
}
