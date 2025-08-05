library(dplyr)
library(purrr)

# detect slope change points for a single group
detect_slope_changes <- function(x, y, span = 0.3, grid_points = 500, 
                                 min_threshold = 0.001, return_all = FALSE) {
  
  # Handle edge cases
  if (length(x) < 4 || length(unique(x)) < 3) {
    if (return_all) {
      return(data.frame(
        change_point = numeric(0),
        slope_before = numeric(0),
        slope_after = numeric(0),
        change_magnitude = numeric(0)
      ))
    } else {
      return(numeric(0))
    }
  }
  
  # Sort by x to ensure proper order
  order_idx <- order(x)
  x_sorted <- x[order_idx]
  y_sorted <- y[order_idx]
  
  # Fit loess
  tryCatch({
    loess_fit <- loess(y_sorted ~ x_sorted, span = span)
    
    # Create fine grid for derivative calculation
    x_seq <- seq(min(x_sorted), max(x_sorted), length.out = length(x_sorted))
    y_pred <- predict(loess_fit, newdata = data.frame(x_sorted = x_seq))
    
    # Calculate first derivative (slope)
    dx <- diff(x_seq)
    dy <- diff(y_pred)
    slope <- dy / dx
    
    # Find slope sign changes
    slope_signs <- sign(slope)
    sign_changes <- which(diff(slope_signs) != 0)
    
    if (length(sign_changes) == 0) {
      if (return_all) {
        return(data.frame(
          change_point = numeric(0),
          slope_before = numeric(0),
          slope_after = numeric(0),
          change_magnitude = numeric(0)
        ))
      } else {
        return(numeric(0))
      }
    }
    
    # Get change points
    change_points <- x_seq[sign_changes + 1]
    
    # Filter by minimum threshold if specified
    if (!is.null(min_threshold)) {
      slope_magnitude <- abs(slope[sign_changes])
      significant_changes <- slope_magnitude > min_threshold
      change_points <- change_points[significant_changes]
      sign_changes <- sign_changes[significant_changes]
    }
    
    if (return_all && length(change_points) > 0) {
      # Calculate slopes before and after each change point
      slope_before <- slope[sign_changes]
      slope_after <- slope[sign_changes + 1]
      change_magnitude <- abs(slope_after - slope_before)
      
      return(data.frame(
        change_point = change_points,
        slope_before = slope_before,
        slope_after = slope_after,
        change_magnitude = change_magnitude
      ))
    } else {
      return(change_points)
    }
    
  }, error = function(e) {
    warning(paste("Error in loess fitting:", e$message))
    if (return_all) {
      return(data.frame(
        change_point = numeric(0),
        slope_before = numeric(0),
        slope_after = numeric(0),
        change_magnitude = numeric(0)
      ))
    } else {
      return(numeric(0))
    }
  })
}

# Function to apply to grouped tibble
find_group_slope_changes <- function(data, x_col, y_col, group_cols, 
                                     span = 0.3, grid_points = 500, 
                                     min_threshold = 0.001, return_all = FALSE) {
  
  # Convert column names to symbols for tidy evaluation
  x_sym <- sym(x_col)
  y_sym <- sym(y_col)
  group_syms <- syms(group_cols)
  
  # Apply function to each group
  results <- data %>%
    group_by(!!!group_syms) %>%
    summarise(
      change_data = list(detect_slope_changes(
        x = !!x_sym, 
        y = !!y_sym, 
        span = span,
        grid_points = grid_points,
        min_threshold = min_threshold,
        return_all = return_all
      )),
      .groups = "keep"
    )
  
  if (return_all) {
    # Unnest the detailed results
    results <- results %>%
      unnest(change_data) %>%
      ungroup()
  } else {
    # Unnest just the change points
    results <- results %>%
      unnest(change_data) %>%
      rename(change_point = change_data) %>%
      ungroup()
  }
  
  return(results)
}

# Example usage:
# 
# # Simple usage - just get change points
# change_points <- find_group_slope_changes(
#   data = your_tibble,
#   x_col = "time_variable",
#   y_col = "outcome_variable", 
#   group_cols = c("group1", "group2"),
#   span = 0.3
# )
#
# # Detailed usage - get slopes before/after and magnitudes
# detailed_changes <- find_group_slope_changes(
#   data = your_tibble,
#   x_col = "time_variable",
#   y_col = "outcome_variable",
#   group_cols = c("group1", "group2"),
#   span = 0.3,
#   return_all = TRUE
# )