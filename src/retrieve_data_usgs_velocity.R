# WQ-LT Drought Publication
# Purpose: Retrieve USGS instantaneous velocity data and save copy in
  # data/external for continued processing
# Authors: Liz Stumpner, Dave Bosworth
# Contacts: Elizabeth.Stumpner@water.ca.gov; David.Bosworth@water.ca.gov

library(dataRetrieval)
library(dplyr)
library(tibble)
library(purrr)
library(qs)
library(here)

# Import data using NWIS web services:
# USGS 11455350 CACHE SLOUGH A RYER ISLAND (inactive)
# USGS 11455385 CACHE SLOUGH AB RYER ISLAND FERRY NR RIO VISTA CA (active)
# USGS 11337190 SAN JOAQUIN R A JERSEY POINT CA
# USGS 11312676 MIDDLE R AT MIDDLE RIVER CA
# USGS 11313405 OLD R A BACON ISLAND CA

# Function to download USGS instantaneous velocity data in the PST time zone
dwnld_inst_vel <- function(site_num) {
  readNWISuv(site_num, parameterCd = "72255", endDate = "2021-12-31", tz = "Etc/GMT+8")
}

# Download data separately since purrr::map doesn't seem to play well with the
  # dataRetrieval R package
uvRYI <- dwnld_inst_vel("11455350")
uvRYF <- dwnld_inst_vel("11455385")
uvSJJ <- dwnld_inst_vel("11337190")
uvMDM <- dwnld_inst_vel("11312676")
uvOLD <- dwnld_inst_vel("11313405")

# Keep only the necessary columns and export instantaneous velocity data
lst(uvRYI, uvRYF, uvSJJ, uvMDM, uvOLD) %>%
  map(~ select(.x, dateTime, velocity_ft_s = `X_72255_00000`)) %>%
  map(as_tibble) %>%
  iwalk(\(x, idx) qsave(x, file = here("data/external", paste0(idx, ".qs"))))

