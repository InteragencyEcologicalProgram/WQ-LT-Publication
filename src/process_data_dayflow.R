# WQ-LT Drought Publication
# Purpose: Process Delta Inflow and Outflow from DAYFLOW model for analysis
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(tidyverse)
library(fs)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter(), dplyr::lag())

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
    across(contains("flow"), as.numeric),
    # Add Water Year and water day of year columns
    WaterYear = calc_wy(Date),
    WYday = calc_wy_day(Date)
  ) %>%
  # Filter to WY 1975-2021
  filter(WaterYear %in% 1975:2021)

# Calculate cumulative sums and totals for each water year
df_dayflow_sum <- df_dayflow %>%
  group_by(WaterYear)%>%
  arrange(WYday)%>%
  mutate(across(c(InflowTotal, Outflow), list(cum = ~ cumsum(.x), sum = ~ sum(.x)))) %>%
  ungroup()

# Import year assignments
df_yr_type <- read_csv(here("data/raw/year_assignments.csv"))

# Calculate period years for each drought classification
df_drt_period_yrs <- df_yr_type %>%
  select(Year, Drought) %>%
  arrange(Drought, Year) %>%
  group_by(Drought) %>%
  mutate(Lag = Year - lag(Year, order_by = Year)) %>%
  ungroup() %>%
  mutate(
    Start = if_else(is.na(Lag) | Lag > 1, TRUE, FALSE),
    Period_ID = 1:n(),
    Period_ID = if_else(Start, Period_ID, NA_integer_),
    Period_ID = as.integer(as.factor(Period_ID))
  ) %>%
  fill(Period_ID, .direction = "down") %>%
  group_by(Period_ID) %>%
  arrange(Year) %>%
  mutate(
    Period_year = 1:n(),
    Period_year_cat = if_else(Period_year >= 3, "3+", as.character(Period_year))
  ) %>%
  ungroup() %>%
  select(Year, Drought, starts_with("Period_year"))

# Add period years and drought classifications to summed Dayflow data and finish cleaning
dayflow <- df_dayflow_sum %>%
  left_join(df_drt_period_yrs, by = join_by(WaterYear == Year)) %>%
  select(
    Date,
    WaterYear,
    WYday,
    Drought,
    starts_with("Period"),
    starts_with("InflowTotal"),
    starts_with("Outflow")
  ) %>%
  arrange(Date)

# Export Dayflow data frame as csv file
dayflow %>% write_csv(here("data/processed/dayflow.csv"))

# Export Dayflow data frame as rds file
dayflow %>% saveRDS(here("data/processed/dayflow.rds"))

