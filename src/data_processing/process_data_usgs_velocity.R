# WQ-LT Drought Publication
# Purpose: Process USGS instantaneous velocity data for analysis
# Authors: Liz Stumpner, Dave Bosworth
# Contacts: Elizabeth.Stumpner@water.ca.gov; David.Bosworth@water.ca.gov

library(tidyverse)
library(qs)
library(imputeTS)
library(here)
library(fs)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())


# Functions ---------------------------------------------------------------

# Godin filter:
# It includes three passes in time domain, with windows 24, 24, and 25 hrs long
  # will assume 15-min data here, so for first pass, 48 points have to be skipped
  # and last 44 window is 12-1-11 hrs in length, i.e. use previous 12 hours and
  # subsequent 11 hrs to define result at 13th hour.
godin_filt <- function(tint, xint) {
  xfilt <- rep_len(NA, length.out = length(xint))

  # do first pass, with window of 12, 1, 11 hrs
  for (i in seq(from = 49, to = (length(xint) - 44))) {
    xfilt[i] <- sum(xint[(i - 48):(i + 44)]) / 93
  }

  xfilt1 <- xfilt
  # now 2nd pass, same approach but window is 11, 1, 12 hrs
  for (i in seq(from = 45, to = (length(xint) - 48))) {
    xfilt1[i] <- sum(xfilt[(i - 44):(i + 48)]) / 93
  }
  xfilt <- xfilt1
  # now 3rd pass with 12-1-12 window
  for (i in seq(from = 49, to = (length(xint) - 48))) {
    xfilt[i] <- sum(xfilt1[(i - 48):(i + 48)]) / 97
  }
  # Now throw away 36 hrs at each end
  xfilt[1:(36 * 4)] <- NA
  isize <- length(xfilt)
  xfilt[(isize - 36 * 4):isize] <- NA

  return(xfilt)
}

# Source global data processing functions
source(here("src/data_processing/global_data_proc_func.R"))


# Import Instantaneous Data -----------------------------------------------

# Import instantaneous velocity data, each station as an element in a list
fp_vel_inst <- dir_ls(here("data/external"), regexp = "uv[[:upper:]]{3}\\.qs$")
names(fp_vel_inst) <- str_extract(names(fp_vel_inst), "(?<=uv)[:upper:]{3}(?=\\.qs$)")

ls_vel_inst <- map(fp_vel_inst, qread)


# Prepare Instantaneous Data for Summarizing ------------------------------

# Define the start month for the adjusted year
adj_year_month<-10

# Fill in time stamps using linear interpolation for gaps up to 2 hours
ls_vel_inst_c1 <- ls_vel_inst %>%
  map(
    ~ drop_na(.x, velocity_ft_s) %>%
      # Remove a few timestamps not collected on a standard 15-minute interval
      filter(minute(dateTime) %in% c(0, 15, 30, 45)) %>%
      complete(dateTime = seq.POSIXt(min(dateTime), max(dateTime), by = "15 min")) %>%
      mutate(velocity_ft_s = na_interpolation(velocity_ft_s, maxgap = 8)) %>%
      # Convert velocity to metric units
      mutate(velocity_m_s = velocity_ft_s / 3.2808399) %>%
      select(-velocity_ft_s)
  )

# Apply Godin filter to calculate net velocity, then calculate tidal velocity
ls_vel_inst_c2 <- ls_vel_inst_c1 %>%
  map(
    ~ mutate(.x, NetVel = godin_filt(dateTime, velocity_m_s)) %>%
      drop_na(NetVel) %>%
      mutate(TidalVel = velocity_m_s - NetVel)
  )


# Calculate Weekly Values -------------------------------------------------

# First, calculate daily min, max and max abs values of tidal velocity and daily
  # average net velocity
ls_vel_dv <- ls_vel_inst_c2 %>%
  map(
    ~ mutate(.x, Date = date(dateTime)) %>%
      summarize(
        MinTidalVel = min(TidalVel),
        MaxTidalVel = max(TidalVel),
        MaxAbsTidalVel = max(abs(TidalVel)),
        MeanNetVel = mean(NetVel),
        n_ts = n(),
        .by = Date
      ) %>%
      # Remove days that have greater than 2 hours of missing data
      filter(n_ts >= 96 - 8) %>%
      select(-n_ts)
  )

# Combine Cache Slough data - stations RYI and RYF. Last day for RYI is
  # 3/30/2019, use RYF from 3/31/2019 - end of record
df_vel_dv_ryi <- ls_vel_dv$RYI
df_vel_dv_ryf <- ls_vel_dv$RYF %>% filter(Date >= "2019-03-31")
ls_vel_dv_cache <- list(bind_rows(df_vel_dv_ryi, df_vel_dv_ryf)) %>% set_names("Cache")

# Add combined Cache Slough data back to the main list and remove RYI and RYF
ls_vel_dv_c <- ls_vel_dv %>%
  discard_at(c("RYI", "RYF")) %>%
  append(ls_vel_dv_cache)

# Calculate weekly min, max and max abs values of tidal velocity and weekly
  # average net velocity from the daily values
ls_vel_wk <- ls_vel_dv_c %>%
  map(
    ~ mutate(.x, Week = floor_date(Date, "week")) %>%
      summarize(
        MinTidalVel = min(MinTidalVel),
        across(starts_with("Max"), max),
        MeanNetVel = mean(MeanNetVel),
        n_day = n(),
        .by = Week
      ) %>%
      # Remove weeks that have less than 4 days of data available
      filter(n_day >= 4) %>%
      select(-n_day) %>%
      # Add sign column for tidal velocity
      mutate(TideSign = if_else(abs(MinTidalVel) > MaxTidalVel, "negative", "positive")) %>%
      # Remove min and max tidal velocity since they are no longer needed
      select(-c(MinTidalVel, MaxTidalVel))
  )

# Combine data into one data frame
df_vel_wk <- bind_rows(ls_vel_wk, .id = "Station")


# Add Weekly Avg Delta Outflow from Dayflow -------------------------------

# Import Dayflow data for 1970-2021
fp_dayflow <- dir_ls(here("data/external"), regexp = "dayflow.+\\.csv$")
ls_dayflow <- map(fp_dayflow, ~ read_csv(.x, col_types = list(.default = "c")))

# Combine Dayflow data - only keep Delta Outflow
df_outflow <- ls_dayflow %>%
  map(~ select(.x, Date, Outflow = OUT)) %>%
  bind_rows() %>%
  # Convert date column to date and Outflow to numeric (metric units)
  mutate(
    Date = date(parse_date_time(Date, c("mdY", "Ymd"))),
    Outflow = as.numeric(Outflow),
    Outflow = Outflow / 35.315
  )

# Calculate weekly average Delta Outflow
df_outflow_wk <- df_outflow %>%
  mutate(Week = floor_date(Date, "week")) %>%
  summarize(
    Outflow = mean(Outflow),
    n_day = n(),
    .by = Week
  ) %>%
  # Remove weeks that have less than 4 days of data available
  filter(n_day >= 4) %>%
  select(-n_day)

# Join weekly average Delta Outflow to weekly velocity data
df_vel_wk_c1 <- left_join(df_vel_wk, df_outflow_wk, by = join_by(Week))


# Final Data Processing and Export ----------------------------------------

# Import year assignments
df_yr_type <- read_csv(here("data/raw/year_assignments.csv"))

velocity <- df_vel_wk_c1 %>%
  # Add Water Year and water day of year columns
  mutate(
    WaterYear = calc_wy(Week, adj_year_month),
    WYday = calc_wy_day(Week, adj_year_month)
  ) %>%
  # Filter to WY 2008-2021
  filter(WaterYear %in% 2008:2021) %>%
  # Add year assignments
  left_join(df_yr_type, by = join_by(WaterYear == Year)) %>%
  select(
    Station,
    WaterYear,
    YearType,
    Drought,
    Week,
    WYday,
    MaxAbsTidalVel,
    TideSign,
    MeanNetVel,
    Outflow
  )

# Export velocity data frame as csv file
velocity %>% write_csv(here("data/processed/hydrology/velocity.csv"))

# Export velocity data frame as rds file
velocity %>% saveRDS(here("data/processed/hydrology/velocity.rds"))

