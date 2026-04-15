rm(list = ls())
pacman::p_load(readr, dplyr, ggplot2, lubridate, here, stringr)
file <- file.path(here(), "code", "submission 2", "Figure 1", "data", "Kidney OD nonOD JP data.csv")

df <- read_tsv(file, show_col_types = FALSE)

names(df) <- gsub("\ufeff", "", names(df))
names(df) <- gsub("Ôªø", "", names(df))
names(df) <- trimws(names(df))

if (!"Month" %in% names(df)) {
  month_col <- grep("Month", names(df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(month_col)) {
    names(df)[names(df) == month_col] <- "Month"
  }
}

# ---------------------------
# 2. Filter to MoD 1, 2, 7 & create dates
# ---------------------------

mod_labels <- c(
  "1" = "Drug Overdose",
  "2" = "Non-drug Overdose",
  "7" = "Overall"
)

df_plot <- df %>%
  filter(MoD %in% c(1, 2, 7)) %>%
  mutate(
    Flag = ifelse(Flag == "NA" | is.na(Flag), NA_character_, Flag),
    Date = as.Date("2015-01-01") %m+% months(Month - 1),
    Observed = `Crude Rate`,
    Fitted = Model,
    Panel = factor(mod_labels[as.character(MoD)],
                   levels = c("Drug Overdose", "Non-drug Overdose", "Overall"))
  )

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2024-12-01")

df_plot <- df_plot %>%
  filter(Date >= start_date, Date <= end_date)

# ---------------------------
# 3. Build smooth log-linear curves between joinpoints
# ---------------------------

get_segment_endpoints <- function(data) {
  data %>%
    arrange(Date) %>%
    mutate(
      is_joinpoint = grepl("Joinpoint", Flag, ignore.case = TRUE),
      is_start = row_number() == 1,
      is_end = row_number() == n()
    ) %>%
    filter(is_joinpoint | is_start | is_end) %>%
    select(Date, Fitted, Panel)
}

interpolate_exp_curve <- function(date1, y1, date2, y2, n_points = 100) {
  
  t1 <- as.numeric(date1)
  t2 <- as.numeric(date2)
  
  if (y1 <= 0 | y2 <= 0) {
    t_seq <- seq(t1, t2, length.out = n_points)
    y_seq <- seq(y1, y2, length.out = n_points)
  } else {
    b <- log(y2 / y1) / (t2 - t1)
    t_seq <- seq(t1, t2, length.out = n_points)
    y_seq <- y1 * exp(b * (t_seq - t1))
  }
  
  data.frame(
    Date = as.Date(t_seq, origin = "1970-01-01"),
    Fitted_smooth = y_seq
  )
}

smooth_lines_list <- list()

for (panel_name in levels(df_plot$Panel)) {
  
  panel_data <- df_plot %>% filter(Panel == panel_name)
  
  segment_points <- get_segment_endpoints(panel_data)
  
  smooth_curves <- segment_points %>%
    arrange(Date) %>%
    mutate(
      Date_end = lead(Date),
      Fitted_end = lead(Fitted),
      segment_id = row_number()
    ) %>%
    filter(!is.na(Date_end))
  
  for (i in 1:nrow(smooth_curves)) {
    row <- smooth_curves[i, ]
    
    curve_points <- interpolate_exp_curve(
      date1 = row$Date,
      y1 = row$Fitted,
      date2 = row$Date_end,
      y2 = row$Fitted_end,
      n_points = 100
    )
    
    curve_points$segment_id <- paste(panel_name, row$segment_id, sep = "_")
    curve_points$Panel <- panel_name
    
    smooth_lines_list[[length(smooth_lines_list) + 1]] <- curve_points
  }
}

smooth_lines_df <- bind_rows(smooth_lines_list)

smooth_lines_df$Panel <- factor(
  smooth_lines_df$Panel,
  levels = c("Drug Overdose", "Non-drug Overdose", "Overall")
)

# ---------------------------
# 4. Axis ticks
# ---------------------------

breaks <- as.Date(c(
  "2015-01-01",
  "2017-01-01",
  "2019-01-01",
  "2021-01-01",
  "2023-01-01",
  "2024-12-01"
))

labels <- c(
  "Jan\n2015", "Jan\n2017", "Jan\n2019",
  "Jan\n2021", "Jan\n2023", "Dec\n2024"
)

# ---------------------------
# 5. Faceted Plot (NO dotted lines)
# ---------------------------

p <- ggplot(df_plot, aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  geom_smooth(
    data = smooth_lines_df,
    aes(x = Date, y = Fitted_smooth, group = segment_id, color = Panel),
    linewidth = 1
  ) +
  geom_vline(data = smooth_lines_df |>  group_by(segment_id) |> filter(Date == min(Date)) |> filter(str_detect(segment_id, "_2")), 
             aes(color = Panel, xintercept = Date), 
             linetype = "dashed",
             linewidth = 1)+
  scale_color_manual(
    values = c(
      "Drug Overdose" = "blue",
      "Non-drug Overdose" = "red",
      "Overall" = "darkgreen"
    )
  ) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = c(0.5, 1.0, 2, 3, 4, 5),
    labels = c("0.5", "1.0", "2", "3", "4", "5")
  ) +
  labs(
    x = "Month",
    y = "Kidney transplants (per million)"
  ) +
  facet_wrap(~ Panel, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    #strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines"), 
    strip.background = element_rect(fill = "black"), 
    strip.text = element_text(face = "bold", color = "white")
  ) 

# ---------------------------
# 6. Print plot
# ---------------------------

print(p)
if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "Figure 1", "Figure 1.pdf"), 
         p, 
         device = "pdf", 
         height = 11, 
         width = 8.5, 
         units = "in")
}

