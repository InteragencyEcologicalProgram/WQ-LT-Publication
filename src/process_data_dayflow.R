# WQ-LT Drought Publication
# Purpose: Process Delta Inflow and Outflow from DAYFLOW model for analysis
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(tidyverse)
library(fs)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())

# Source global data processing functions
source(here("src/global_data_proc_func.R"))

# Import Dayflow data for 1970-2021
fp_dayflow <- dir_ls(here("data/external"), regexp = "dayflow.+\\.csv$")
ls_dayflow <- map(fp_dayflow, ~ read_csv(.x, col_types = list(.default = "c")))

# Combine Dayflow data - only keep Delta Inflow and Outflow
df_dayflow <- ls_dayflow %>%
  map(
    ~ select(
      .x,
      Date,
      InflowTotal = TOT,
      Outflow = OUT
    )
  ) %>%
  bind_rows() %>%
  # Convert date column to date and Inflow and Outflow to numeric
  mutate(
    Date = date(parse_date_time(Date, c("mdY", "Ymd"))),
    across(contains("flow"), as.numeric)
  )

# Finish cleaning Dayflow data
dayflow <- df_dayflow %>%
  # Add Water Year and water day of year columns
  mutate(
    WaterYear = calc_wy(Date),
    WYday = calc_wy_day(Date)
  ) %>%
  # Filter to WY 1975-2021
  filter(WaterYear %in% 1975:2021) %>%
  select(
    Date,
    WaterYear,
    WYday,
    InflowTotal,
    Outflow
  ) %>%
  arrange(Date)

# Export Dayflow data frame as csv file
dayflow %>% write_csv(here("data/processed/dayflow.csv"))

# Export Dayflow data frame as rds file
dayflow %>% saveRDS(here("data/processed/dayflow.rds"))

