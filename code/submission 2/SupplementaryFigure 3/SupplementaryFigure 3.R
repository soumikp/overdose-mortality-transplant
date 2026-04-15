rm(list = ls())

pacman::p_load(readr, dplyr, ggplot2, lubridate, here, patchwork)

# ---------------------------
# 1. Read & clean data
# ---------------------------

file <- file.path(here(), "code", "submission 2", 
                  "SupplementaryFigure 3", "data", "Joinpoint_data_spk_slk.csv")

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

standardize_col <- function(df, pattern, new_name) {
  idx <- grep(pattern, names(df), ignore.case = TRUE)[1]
  if (!is.na(idx) && !new_name %in% names(df)) {
    names(df)[idx] <- new_name
  }
  df
}

df <- standardize_col(df, "Donor.*MoD|^MoD$", "Donor_MoD")
df <- standardize_col(df, "Crude.*Rate", "Crude_Rate")
df <- standardize_col(df, "^Model$", "Model")
df <- standardize_col(df, "Flag", "Flag")

# ---------------------------
# 2. Recode & create dates
# ---------------------------

df_plot <- df %>%
  mutate(
    Flag = ifelse(Flag == "NA" | is.na(Flag), NA_character_, Flag),
    Transplant_Type = case_when(
      Donor_MoD == 1 ~ "SPK",
      Donor_MoD == 2 ~ "SLK"
    ),
    Transplant_Type = factor(Transplant_Type, levels = c("SPK", "SLK")),
    Date = as.Date("2015-01-01") %m+% months(Month - 1),
    Observed = Crude_Rate,
    Fitted = Model
  )

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2024-12-01")

df_plot <- df_plot %>%
  filter(Date >= start_date, Date <= end_date)

# ---------------------------
# 3. Build smooth log-linear curves
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
    select(Date, Fitted, Transplant_Type)
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

for (type in levels(df_plot$Transplant_Type)) {
  type_data <- df_plot %>% filter(Transplant_Type == type)
  segment_points <- get_segment_endpoints(type_data)
  
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
      row$Date,
      row$Fitted,
      row$Date_end,
      row$Fitted_end,
      n_points = 100
    )
    
    curve_points$segment_id <- paste(type, row$segment_id, sep = "_")
    curve_points$Transplant_Type <- type
    
    smooth_lines_list[[length(smooth_lines_list) + 1]] <- curve_points
  }
}

smooth_lines_df <- bind_rows(smooth_lines_list)

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
# 5. Plots (matched aesthetics)
# ---------------------------

base_theme <- theme_bw(base_size = 16) +
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

# p_spk <- ggplot(df_plot %>% filter(Transplant_Type == "SPK"), aes(x = Date)) +
#   geom_point(aes(y = Observed), color = "black", size = 1.5) +
#   geom_line(
#     data = smooth_lines_df %>% filter(Transplant_Type == "SPK"),
#     aes(y = Fitted_smooth, group = segment_id),
#     linewidth = 1,
#     color = "red"
#   ) +
#   scale_x_date(
#     breaks = breaks,
#     labels = labels,
#     limits = c(start_date, as.Date("2024-12-31") + 30),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   labs(
#     title = "ODD-associated\nsimultaneous pancreas-kidney transplants",
#     x = "Month",
#     y = "Transplants (per million)"
#   ) +
#   base_theme
# 
# p_slk <- ggplot(df_plot %>% filter(Transplant_Type == "SLK"), aes(x = Date)) +
#   geom_point(aes(y = Observed), color = "black", size = 1.5) +
#   geom_line(
#     data = smooth_lines_df %>% filter(Transplant_Type == "SLK"),
#     aes(y = Fitted_smooth, group = segment_id),
#     linewidth = 1,
#     color = "blue"
#   ) +
#   scale_x_date(
#     breaks = breaks,
#     labels = labels,
#     limits = c(start_date, as.Date("2024-12-31") + 30),
#     expand = expansion(mult = c(0.01, 0.05))
#   ) +
#   labs(
#     title = "ODD-associated\nsimultaneous liver-kidney transplants",
#     x = "Month",
#     y = "Transplants (per million)"
#   ) +
#   base_theme

p <- df_plot |>
  mutate(Transplant_Type = case_when(Transplant_Type == "SPK" ~ "(A) Pancreas-Kidney", 
                                     Transplant_Type == "SLK" ~ "(B) Liver-Kidney")) |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  geom_line(
   data = smooth_lines_df |> mutate(Transplant_Type = case_when(Transplant_Type == "SPK" ~ "(A) Pancreas-Kidney", 
                                                            Transplant_Type == "SLK" ~ "(B) Liver-Kidney")),
    aes(y = Fitted_smooth, group = segment_id, color = Transplant_Type),
    linewidth = 1
  ) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0))
  ) +
  labs(
    title = stringr::str_wrap("ODD-Associated Simultaneous Transplants", width=60), 
    x = "Month",
    y = "Transplants (per million)"
  ) +
  base_theme + 
  facet_grid(rows = vars(Transplant_Type)) + scale_color_aaas()


if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "SupplementaryFigure 3", "SupplementaryFigure 3.pdf"), 
         p, 
         device = "pdf", 
         height = 11, 
         width = 8.5, 
         units = "in")
}
