rm(list = ls())
pacman::p_load(here, tidyverse, haven, lubridate, 
               ggpubfigs, ggsci, stringr, 
               readxl)

###############################################################################
# 2. Analysis parameters -----------------------------------------------------
###############################################################################
active_status_codes <- c(4010, 4050, 4060, 5010)   # <- ONLY these four codes
start_date <- ymd("2015-01-01")
end_date   <- ymd("2024-12-01")
all_months <- seq.Date(from = start_date, to = end_date, by = "month")

###############################################################################
# 3. Read and preprocess the STATHIST_KIPA export ---------------------------
###############################################################################
status_df <- read_csv(file.path(here(), "data/clean_02_waitlist", 
                                "Status_File_for_Analysis07122025.csv"),
  col_types = cols(.default = "c")          # read all columns as character
) %>%
  # Organ filter: Kidney (KI) or Kidney–Pancreas (KP)
  filter(WL_ORG %in% c("KI", "KP")) %>%
  # Coerce variables
  mutate(
    CANHX_BEGIN_DT = mdy(CANHX_BEGIN_DT),
    CANHX_END_DT   = mdy(CANHX_END_DT),
    CANHX_STAT_CD  = as.numeric(CANHX_STAT_CD),
    PERS_ID        = as.character(PERS_ID)
  ) %>%
  # Keep only rows with the four active status codes
  filter(CANHX_STAT_CD %in% active_status_codes) %>%
  # Treat missing end-dates as “still active”
  mutate(
    CANHX_END_DT = replace_na(CANHX_END_DT, ymd("9999-12-31"))
  )

###############################################################################
# 4. Monthly active-waitlist counts (unique PERS_ID) -------------------------
###############################################################################
monthly_active_counts <- map_dfr(all_months, function(month_start) {
  
  month_end <- ceiling_date(month_start, "month") - days(1)
  
  n_active <- status_df %>%
    filter(
      CANHX_BEGIN_DT <= month_end,   # spell has started
      CANHX_END_DT   >= month_start  # spell has not ended
    ) %>%
    distinct(PERS_ID) %>%            # one row per patient per month
    nrow()
  
  tibble(
    Month              = month_start,
    ActiveWaitlistSize = n_active
  )
})

###############################################################################
# 5. Inspect and export ------------------------------------------------------
###############################################################################
print(head(monthly_active_counts, 12))   # first 12 months
print(tail(monthly_active_counts, 12))   # last 12 months

write_csv(
  monthly_active_counts,
  "Active_Kidney_Waitlist_Monthly_Counts_07122025_PERSID.csv"
)