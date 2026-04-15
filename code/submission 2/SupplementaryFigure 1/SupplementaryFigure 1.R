library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# -----------------------------
# 1. Read & clean data
# -----------------------------

file <- file.path(here(), "code", "submission 2", "Figure 1", "data", "DDKT_joinpoint_data.csv")

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

# -----------------------------
# 2. Recode MoD & create dates
# -----------------------------

df_plot <- df %>%
  mutate(
    Flag = ifelse(Flag == "NA" | is.na(Flag), NA_character_, Flag),
    Mechanism = case_when(
      MoD == 1 ~ "Drug Overdose",
      MoD == 2 ~ "Cardiovascular",
      MoD == 3 ~ "Gunshot wound",
      MoD == 4 ~ "Blunt Injury",
      MoD == 5 ~ "ICH/stroke",
      MoD == 6 ~ "Others",
      MoD == 7 ~ "Overall"
    ),
    Mechanism = factor(
      Mechanism,
      levels = c(
        "Drug Overdose",
        "Overall",
        "Cardiovascular",
        "Gunshot wound",
        "Blunt Injury",
        "ICH/stroke",
        "Others"
      )
    ),
    Date     = as.Date("2015-01-01") %m+% months(Month - 1),
    Observed = `Crude Rate`,
    Fitted   = Model
  )

start_date <- as.Date("2015-01-01")
end_date   <- as.Date("2024-12-01")

df_plot <- df_plot %>%
  filter(Date >= start_date, Date <= end_date)

# -----------------------------
# 3. Build smooth log-linear curves between joinpoints
# -----------------------------

get_segment_endpoints <- function(data) {
  data %>%
    group_by(Mechanism) %>%
    arrange(Date) %>%
    mutate(
      is_joinpoint = grepl("Joinpoint", Flag, ignore.case = TRUE),
      is_start = row_number() == 1,
      is_end = row_number() == n()
    ) %>%
    filter(is_joinpoint | is_start | is_end) %>%
    select(Mechanism, Date, Fitted) %>%
    ungroup()
}

segment_points <- get_segment_endpoints(df_plot)

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

smooth_curves <- segment_points %>%
  group_by(Mechanism) %>%
  arrange(Date) %>%
  mutate(
    Date_end = lead(Date),
    Fitted_end = lead(Fitted),
    segment_id = row_number()
  ) %>%
  filter(!is.na(Date_end)) %>%
  ungroup()

smooth_lines_list <- list()

for (i in 1:nrow(smooth_curves)) {
  row <- smooth_curves[i, ]
  
  curve_points <- interpolate_exp_curve(
    date1 = row$Date,
    y1 = row$Fitted,
    date2 = row$Date_end,
    y2 = row$Fitted_end,
    n_points = 100
  )
  
  curve_points$Mechanism <- row$Mechanism
  curve_points$segment_id <- row$segment_id
  
  smooth_lines_list[[i]] <- curve_points
}

smooth_lines_df <- bind_rows(smooth_lines_list) %>%
  mutate(group_id = interaction(Mechanism, segment_id))

# -----------------------------
# 4. Axis ticks & colors
# -----------------------------

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

cols <- c(
  "Drug Overdose"  = "red",
  "Overall"        = "blue",
  "Cardiovascular" = "darkorange",
  "Gunshot wound"  = "darkgreen",
  "Blunt Injury"   = "purple",
  "ICH/stroke"     = "brown",
  "Others"         = "grey30"
)

# -----------------------------
# 5. Plot 1: Drug Overdose + Overall (NO dotted lines)
# -----------------------------

df_drug_overall <- df_plot %>%
  filter(Mechanism %in% c("Drug Overdose", "Overall"))

smooth_drug_overall <- smooth_lines_df %>%
  filter(Mechanism %in% c("Drug Overdose", "Overall"))

p_drug_overall <- ggplot(df_drug_overall, aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  geom_line(
    data = smooth_drug_overall,
    aes(x = Date, y = Fitted_smooth, color = Mechanism, group = group_id),
    linewidth = 1
  ) +
  scale_color_manual(values = cols) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    x = "Month",
    y = "Kidney transplants (per million)"
  ) +
  facet_wrap(~ Mechanism, ncol = 1, scales = "free_y") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text     = element_text(face = "bold"),
    strip.text    = element_text(face = "bold", size = 16)
  )

# -----------------------------
# 6. Plot 2: MoD 2–6 (NO dotted lines)
# -----------------------------

non_overall_mechs <- c(
  "Cardiovascular", "Gunshot wound", "Blunt Injury", "ICH/stroke", "Others"
)

df_mech_2to6 <- df_plot %>%
  filter(Mechanism %in% non_overall_mechs)

smooth_mech_2to6 <- smooth_lines_df %>%
  filter(Mechanism %in% non_overall_mechs)

p_mech_2to6 <- ggplot(df_mech_2to6, aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  geom_line(
    data = smooth_mech_2to6,
    aes(x = Date, y = Fitted_smooth, color = Mechanism, group = group_id),
    linewidth = 1
  ) +
  scale_color_manual(values = cols) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    x = "Month",
    y = "Kidney transplants (per million)"
  ) +
  facet_wrap(~ Mechanism, ncol = 2) +
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

# -----------------------------
# 7. Print plots
# -----------------------------
print(p_mech_2to6)
if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "SupplementaryFigure 1", "SupplementaryFigure 1.pdf"), 
         p_mech_2to6, 
         device = "pdf", 
         height = 11, 
         width = 8.5, 
         units = "in")
}
