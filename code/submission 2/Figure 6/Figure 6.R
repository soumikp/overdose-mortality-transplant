###############################################################################
# Interrupted Time Series (ITS) Segmented Model for Monthly Liver Waitlist Size
# Dataset: monthly_liver_waitlist_size.csv
# Columns (required):
#   - month                (e.g., "1/1/15")
#   - ActiveWaitlistSize   (counts)
#   - Resident_population  (counts)
#
# We compute per-million internally:
#   WaitlistPM = ActiveWaitlistSize / (Resident_population / 1e6)
#
# Output:
#   - ITS analysis with Newey–West (prewhite = FALSE) CIs and plot
#   - EXACT totals for Sep 2023 – Aug 2024
#
# Update:
#   - Post-intervention CI bands for factual and counterfactual
#   - CI bands derived from Newey–West robust vcov
###############################################################################
rm(list = ls())
pacman::p_load(tidyverse, lubridate, lmtest, sandwich, here)

csv_file          <- file.path(here(), "code", "submission 2", 
                               "Figure 6", "data", "monthly_liver_waitlist_size.csv")
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2024-12-01")
intervention_date <- as.Date("2023-08-01")
knots <- c(60, 64, 82)

intervention_label <- "Overdose decedent\ndonor liver transplants decline"

if (!file.exists(csv_file)) stop("CSV file not found: ", csv_file)

df_raw <- read_csv(csv_file, show_col_types = FALSE)
names(df_raw) <- str_trim(names(df_raw))

req_cols <- c("month","ActiveWaitlistSize","Resident_population")
missing_cols <- setdiff(req_cols, names(df_raw))
if (length(missing_cols) > 0) stop("Required columns not found: ", paste(missing_cols, collapse = ", "))

df <- df_raw %>%
  transmute(
    Date = as.Date(.data[["month"]], format = "%m/%d/%y"),
    ActiveWaitlistSize = as.double(.data[["ActiveWaitlistSize"]]),
    Resident_population = as.double(.data[["Resident_population"]])
  ) %>%
  mutate(WaitlistPM = ActiveWaitlistSize / (Resident_population / 1e6)) %>%
  arrange(Date) %>%
  filter(Date >= start_date & Date <= end_date) %>%
  mutate(
    time = row_number(),
    break_idx = match(intervention_date, Date),
    time_after = pmax(0, time - break_idx),
    knot60 = pmax(0, time - knots[1]),
    knot64 = pmax(0, time - knots[2]),
    knot82 = pmax(0, time - knots[3])
  )

model <- lm(WaitlistPM ~ time + knot60 + knot64 + knot82 + time_after, data = df)
vcov_nw <- NeweyWest(model, prewhite = FALSE)
tcrit <- qt(0.975, df.residual(model))

# Predictions

df_exp <- df %>% mutate(time_after = 0L)

df <- df %>%
  mutate(
    fitted = predict(model, newdata = df),
    expected = predict(model, newdata = df_exp)
  )

# CI computation

b <- coef(model)
V <- vcov_nw[names(b), names(b)]

X_fit <- model.matrix(model, df)
X_exp <- model.matrix(model, df_exp)

fit_var <- rowSums((X_fit %*% V) * X_fit)
exp_var <- rowSums((X_exp %*% V) * X_exp)

fit_se <- sqrt(pmax(fit_var, 0))
exp_se <- sqrt(pmax(exp_var, 0))


df <- df %>%
  mutate(
    fitted_lwr = fitted - tcrit * fit_se,
    fitted_upr = fitted + tcrit * fit_se,
    expected_lwr = expected - tcrit * exp_se,
    expected_upr = expected + tcrit * exp_se
  )

# Post-intervention only

df_post <- df %>% filter(Date >= intervention_date)

# --- Intervention label placement controls ---
label_anchor <- "line"   # "line" or "right"
label_x_offset_days <- 145   # positive = right, negative = left
label_y_offset_frac <- 0.40   # fraction down from top of y-range
label_size <- 5

y_range <- range(df$WaitlistPM, na.rm = TRUE)
y_lab <- y_range[2] - label_y_offset_frac * diff(y_range)

label_x <- if (label_anchor == "right") {
  end_date + days(label_x_offset_days)
} else {
  intervention_date + days(label_x_offset_days)
}

x_limit_end <- if (label_anchor == "right") {
  end_date + days(max(0, label_x_offset_days + 5))
} else end_date

p <- ggplot(df, aes(Date)) +
  geom_ribbon(data = df_post,
              aes(ymin = fitted_lwr, ymax = fitted_upr),
              fill = "steelblue", alpha = 0.38) +
  geom_ribbon(data = df_post,
              aes(ymin = expected_lwr, ymax = expected_upr),
              fill = "red", alpha = 0.18) +
  geom_point(aes(y = WaitlistPM), size = 2, color = "gray30") +
  geom_line(aes(y = fitted), color = "steelblue", size = 1) +
  geom_line(aes(y = expected), linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = intervention_date, linetype = "dotted") +
  geom_text(
    data = tibble(Date = label_x, y = y_lab),
    aes(x = Date, y = y-5, label = paste0("August 2023: ", intervention_label)),
    angle = 90, lineheight = 0.95,
    vjust = -0.3, hjust = 0,
    size = label_size, fontface = "bold"
  ) +
  scale_x_date(limits = c(start_date, x_limit_end)) +
  labs(title = "Monthly Active Waitlist Census: Liver",
       x = "Month", y = "Patients per million") +
  theme_bw(base_size = 16)

print(p)


if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "Figure 6", "Figure 6.pdf"), 
         p, 
         device = "pdf", 
         height = 8.5, 
         width = 8.5, 
         units = "in")
}


###############################################################################
# --- OUTPUTS MATCHING ORIGINAL SCRIPT ---
###############################################################################

cat("\n--- ITS Model (Newey–West robust SE; prewhite = FALSE) ---\n")
print(coeftest(model, vcov. = vcov_nw), digits = 2)

# --- Segment slopes with 95% CI ---

slope_ci <- function(vars) {
  ests <- coef(model)[vars]
  covs <- vcov_nw[vars, vars]
  slope <- sum(ests)
  se <- sqrt(sum(covs))
  ci <- slope + c(-1, 1) * tcrit * se
  tibble(Estimate = slope, Lower = ci[1], Upper = ci[2])
}

segments <- list(
  "Pre-knot60"   = c("time"),
  "Knot60-64"    = c("time","knot60"),
  "Knot64-82"    = c("time","knot60","knot64"),
  "Knot82-Int"   = c("time","knot60","knot64","knot82"),
  "Post-Int"     = c("time","knot60","knot64","knot82","time_after")
)

cat("\n--- Segment Slopes (units/month, 95% CI) ---\n")
seg_tbl <- purrr::imap_dfr(segments, ~{
  s <- slope_ci(.x)
  tibble(Segment = .y,
         Estimate = s$Estimate,
         Lower = s$Lower,
         Upper = s$Upper)
}) %>%
  mutate(`Estimate (95% CI)` = sprintf("%.2f (%.2f to %.2f)", Estimate, Lower, Upper)) %>%
  select(Segment, `Estimate (95% CI)`)

print(seg_tbl, n = Inf, width = Inf)

# --- Post-intervention slope p-value ---
final_vars <- c("time","knot60","knot64","knot82","time_after")
ests <- coef(model)[final_vars]
covs <- vcov_nw[final_vars, final_vars]
final_slope <- sum(ests)
final_se <- sqrt(sum(covs))
final_t <- final_slope / final_se
final_p <- 2 * pt(-abs(final_t), df.residual(model))

cat("\nFinal (Post-Intervention) Slope:\n")
cat(sprintf("Estimate = %.4f, SE = %.4f, t = %.3f, p = %.4g\n",
            final_slope, final_se, final_t, final_p))

###############################################################################
# --- EXACT TOTALS (Sep 2023–Aug 2024): modeled vs expected with CIs ---
###############################################################################

post_start <- as.Date("2023-09-01")
post_end <- as.Date("2024-08-01")

post <- df %>%
  filter(Date >= post_start, Date <= post_end) %>%
  mutate(w = Resident_population / 1e6)

fm <- formula(model)
b <- coef(model)
V <- vcov_nw[names(b), names(b)]

X_mod <- model.matrix(fm, data = post)
post_exp <- post %>% mutate(time_after = 0L)
X_exp <- model.matrix(fm, data = post_exp)

L_mod <- colSums(post$w * X_mod)
L_exp <- colSums(post$w * X_exp)
L_dif <- L_mod - L_exp

lincom <- function(L) {
  est <- as.numeric(crossprod(L, b))
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  c(lwr = est - tcrit * se, est = est, upr = est + tcrit * se)
}

tot_exp <- lincom(L_exp)
tot_mod <- lincom(L_mod)
tot_dif <- lincom(L_dif)

num_hat <- as.numeric(crossprod(L_dif, b))
den_hat <- as.numeric(crossprod(L_exp, b))
rel_hat <- 100 * num_hat / den_hat

grad <- 100 * ((L_dif / den_hat) - (num_hat / den_hat^2) * L_exp)
se_rel <- sqrt(as.numeric(t(grad) %*% V %*% grad))
ci_rel <- c(rel_hat - tcrit * se_rel,
            rel_hat,
            rel_hat + tcrit * se_rel)

summary_tbl <- tibble(
  Metric = c("Expected total (counts, no intervention)",
             "Modeled total (counts)",
             "Absolute difference (counts)",
             "Relative difference (%)"),
  `95% CI Lower` = c(tot_exp["lwr"], tot_mod["lwr"], tot_dif["lwr"], ci_rel[1]),
  Estimate = c(tot_exp["est"], tot_mod["est"], tot_dif["est"], ci_rel[2]),
  `95% CI Upper` = c(tot_exp["upr"], tot_mod["upr"], tot_dif["upr"], ci_rel[3])
)

cat("\n=== Sep 2023 – Aug 2024 Totals ===\n")
print(mutate(summary_tbl, across(-Metric, ~round(., 2))), n = Inf)