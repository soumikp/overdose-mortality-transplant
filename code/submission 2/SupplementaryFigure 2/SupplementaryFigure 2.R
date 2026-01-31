rm(list = ls())

pacman::p_load(readr, dplyr, ggplot2, lubridate, here)

# ---------------------------
# 1. Read & clean data
# ---------------------------

file <- file.path(here(), "code", "submission 2", 
                  "SupplementaryFigure 2", "data", "Liver OD nonOD JP data.csv")  # tab-delimited

df <- read_tsv(file, show_col_types = FALSE)

names(df) <- gsub("\ufeff", "", names(df))
names(df) <- gsub("Ôªø", "", names(df))  # Handle BOM rendered as text
names(df) <- trimws(names(df))

# Ensure Month column exists (handle various encodings)
if (!"Month" %in% names(df)) {
  month_col <- grep("Month", names(df), value = TRUE, ignore.case = TRUE)[1]
  if (!is.na(month_col)) {
    names(df)[names(df) == month_col] <- "Month"
  }
}

# ---------------------------
# 2. Filter to MoD 2 & create dates
# ---------------------------

df_plot <- df %>%
  filter(Donor_MoD == 2) %>%
  mutate(
    # Clean Flag column - convert string "NA" to actual NA
    Flag = ifelse(Flag == "NA" | is.na(Flag), NA_character_, Flag),
    Date = as.Date("2015-01-01") %m+% months(Month - 1),
    Observed = `Crude Rate`,
    Fitted = Model
  )

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2024-12-01")

df_plot <- df_plot %>%
  filter(Date >= start_date, Date <= end_date)

# ---------------------------
# 3. Build smooth log-linear curves between joinpoints
# ---------------------------

# Function to extract segment endpoints
get_segment_endpoints <- function(data) {
  data %>%
    arrange(Date) %>%
    mutate(
      is_joinpoint = grepl("Joinpoint", Flag, ignore.case = TRUE),
      is_start = row_number() == 1,
      is_end = row_number() == n()
    ) %>%
    filter(is_joinpoint | is_start | is_end) %>%
    select(Date, Fitted)
}

# Get segment endpoints
segment_points <- get_segment_endpoints(df_plot)

# Function to interpolate exponential curve between two points
interpolate_exp_curve <- function(date1, y1, date2, y2, n_points = 100) {
  
  # Convert dates to numeric (days since origin)
  t1 <- as.numeric(date1)
  t2 <- as.numeric(date2)
  
  # Handle edge case where y1 or y2 is zero or negative
  if (y1 <= 0 | y2 <= 0) {
    # Fall back to linear interpolation
    t_seq <- seq(t1, t2, length.out = n_points)
    y_seq <- seq(y1, y2, length.out = n_points)
  } else {
    # Calculate exponential parameters
    b <- log(y2 / y1) / (t2 - t1)
    
    # Generate sequence of time points
    t_seq <- seq(t1, t2, length.out = n_points)
    
    # Calculate y values along exponential curve
    y_seq <- y1 * exp(b * (t_seq - t1))
  }
  
  data.frame(
    Date = as.Date(t_seq, origin = "1970-01-01"),
    Fitted_smooth = y_seq
  )
}

# Build smooth curves for each segment
smooth_curves <- segment_points %>%
  arrange(Date) %>%
  mutate(
    Date_end = lead(Date),
    Fitted_end = lead(Fitted),
    segment_id = row_number()
  ) %>%
  filter(!is.na(Date_end))

# Generate interpolated points for all segments
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
  
  curve_points$segment_id <- row$segment_id
  
  smooth_lines_list[[i]] <- curve_points
}

smooth_lines_df <- bind_rows(smooth_lines_list)

# ---------------------------
# 4. Joinpoint locations (for vertical lines)
# ---------------------------

jp_df <- df_plot %>%
  filter(grepl("Joinpoint", Flag, ignore.case = TRUE)) %>%
  distinct(Date)

# ---------------------------
# 5. Axis ticks
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
# 6. Plot
# ---------------------------

p <- ggplot(df_plot, aes(x = Date)) +
  geom_point(aes(y = Observed), color = "black", size = 1.5) +
  # Smooth log-linear curves
  geom_line(
    data = smooth_lines_df,
    aes(x = Date, y = Fitted_smooth, group = segment_id),
    color = "blue",
    linewidth = 1
  ) +
  geom_vline(
    data = jp_df,
    aes(xintercept = Date),
    linetype = "dotted"
  ) +
  scale_x_date(
    breaks = breaks,
    labels = labels,
    limits = c(start_date, as.Date("2024-12-31") + 30),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    title = "Non-drug overdose",
    x = "Month",
    y = "Liver transplants (per million)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )

# ---------------------------
# 7. Print plot
# ---------------------------

print(p)

ggsave(file.path(here(), "code", "submission 2", "SupplementaryFigure 2", "SupplementaryFigure 2.pdf"), 
       p, 
       device = "pdf", 
       height = 8.5, 
       width = 8.5, 
       units = "in")