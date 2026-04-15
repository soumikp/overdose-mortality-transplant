rm(list = ls())
pacman::p_load(tidyverse, lmtest, sandwich, here)
df <- read_csv(file.path(here(), "code", "submission 2", "Figure 2", "data", "Mortality_Vs_Transplant.csv"), 
                 col_types = cols(
                   Month                  = col_integer(),  # YYYYMM
                   ODD_Kidney_Transplants = col_double(),   # rate per million
                   ODD_Liver_Transplants  = col_double(),   # rate per million
                   ODD_deaths             = col_double()    # rate per million
                 )
)

df <- df %>%
  mutate(
    Year      = floor(Month / 100),
    Month_num = Month %% 100,
    Date      = as.Date(sprintf("%d-%02d-01", Year, Month_num)),
    .keep     = "unused"
  ) %>%
  arrange(Date)

# First differences
df_diff <- df %>%
  mutate(
    dKidney = ODD_Kidney_Transplants - lag(ODD_Kidney_Transplants),
    dLiver  = ODD_Liver_Transplants  - lag(ODD_Liver_Transplants),
    dDeaths = ODD_deaths             - lag(ODD_deaths)
  )

# ------------------------------------------------------------------------------
# 2. Analysis windows based on joinpoints
# ------------------------------------------------------------------------------

# Kidney peak at May 2023 → decline begins June 2023
df_kidney <- df_diff %>%
  filter(Date >= as.Date("2023-06-01")) %>%
  drop_na(dKidney, dDeaths)

# Liver peak at August 2023 → decline begins September 2023
df_liver <- df_diff %>%
  filter(Date >= as.Date("2023-09-01")) %>%
  drop_na(dLiver, dDeaths)

# ------------------------------------------------------------------------------
# 3. Helper: compute Newey–West lag
# ------------------------------------------------------------------------------

get_nw_lag <- function(model) {
  n <- length(residuals(model))
  auto_lag <- floor(4 * (n / 100)^(2/9)) # default HAC rule
  used_lag <- min(auto_lag, n - 1)
  tibble(n = n, auto_lag = auto_lag, used_lag = used_lag)
}

# ------------------------------------------------------------------------------
# 4. HAC regression + HAC plot
# ------------------------------------------------------------------------------

run_hac_model <- function(data, y_var, title_main, label_y) {
  y <- rlang::sym(y_var)
  
  cat("\n=============================================\n")
  cat("Series:", title_main, "\n")
  cat("=============================================\n")
  
  # --- Pearson correlation
  cat("=== Pearson Correlation ===\n")
  print(cor.test(dplyr::pull(data, !!y), data$dDeaths))
  
  # --- Linear regression
  formula <- as.formula(paste(y_var, "~ dDeaths"))
  lm_model <- lm(formula, data = data)
  
  # --- Newey–West lag reporting
  lag_info <- get_nw_lag(lm_model)
  cat("\n=== Newey–West Lag Info ===\n")
  print(lag_info)
  
  # --- Newey–West covariance
  nw_cov <- NeweyWest(lm_model, prewhite = FALSE)
  nw_res <- coeftest(lm_model, vcov. = nw_cov)
  
  cat("\n=== Regression with Newey–West Robust SE (auto lag) ===\n")
  print(nw_res)
  
  # --- Slope CI
  beta_hat <- coef(lm_model)["dDeaths"]
  se_hat   <- sqrt(nw_cov["dDeaths", "dDeaths"])
  df_resid <- lm_model$df.residual
  t_crit   <- qt(0.975, df_resid)
  
  ci_low  <- beta_hat - t_crit * se_hat
  ci_high <- beta_hat + t_crit * se_hat
  
  cat("\n=== Slope (per 1 death per million) ===\n")
  cat(sprintf("β = %.5f  (95%% CI: %.5f, %.5f)\n", beta_hat, ci_low, ci_high))
  
  cat("\n=== Slope (per 100 deaths per million) ===\n")
  cat(sprintf("β = %.2f  (95%% CI: %.2f, %.2f)\n",
              100 * beta_hat, 100 * ci_low, 100 * ci_high))
  
  # --- HAC prediction band
  grid <- tibble(dDeaths = seq(min(data$dDeaths),
                               max(data$dDeaths),
                               length.out = 200))
  X_new   <- model.matrix(~ dDeaths, data = grid)
  pred    <- X_new %*% coef(lm_model)
  pred_se <- sqrt(diag(X_new %*% nw_cov %*% t(X_new)))
  
  grid <- grid %>%
    mutate(
      fit = as.numeric(pred),
      lwr = fit - 1.96 * pred_se,
      upr = fit + 1.96 * pred_se
    )
  
  # --- Plot
  p <- ggplot(data, aes(x = dDeaths, y = !!y)) +
    geom_point(size = 4, colour = "black", alpha = 0.8) +
    geom_ribbon(
      data = grid,
      aes(x = dDeaths, ymin = lwr, ymax = upr),
      inherit.aes = FALSE,
      alpha = 0.2,
      fill  = "blue"
    ) +
    geom_line(
      data = grid,
      aes(x = dDeaths, y = fit),
      inherit.aes = FALSE,
      colour = "blue",
      linewidth = 1.2
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    labs(
      title = title_main,
      x     = "Change in 12-month-ending deaths (per million)",
      y     = label_y
    ) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
  
  # --- Residual diagnostics
  residuals_lm <- residuals(lm_model)
  
  p_res <- ggplot(data %>% mutate(Residuals = residuals_lm),
                  aes(Date, Residuals)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(
      title = paste("Regression Residuals Over Time –", title_main),
      x     = "Month",
      y     = "Residual"
    ) +
    theme_minimal()
  
  print(p_res)
  
  cat("\n=== Ljung-Box Test (lag = 4) ===\n")
  print(Box.test(residuals_lm, lag = 4, type = "Ljung-Box"))
  
  invisible(list(
    model    = lm_model,
    nw_cov   = nw_cov,
    lag_info = lag_info,
    grid     = grid,
    plot     = p
  ))
}

# ------------------------------------------------------------------------------
# 5. Run models: Kidney and Liver
# ------------------------------------------------------------------------------

kidney_results <- run_hac_model(
  data      = df_kidney,
  y_var     = "dKidney",
  title_main = stringr::str_wrap("Monthly Change in Overdose Deaths vs. Overdose Decedent Donor Kidney Transplants", width = 60),
  label_y   = "Change in 12-month-ending kidney transplants (per million)"
)

p <- kidney_results$plot

if(FALSE){
  ggsave(file.path(here(), "code", "submission 2", "Figure 2", "Figure 2.pdf"), 
         p, 
         device = "pdf", 
         height = 8.5, 
         width = 8.5, 
         units = "in")
}


liver_results <- run_hac_model(
  data      = df_liver,
  y_var     = "dLiver",
  title_main = "Monthly Change in Overdose Deaths vs. ODD Liver Transplants",
  label_y   = "Change in 12-month-ending liver transplants (per million)"
)
