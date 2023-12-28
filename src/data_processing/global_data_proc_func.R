# WQ-LT Drought Publication
# Purpose: Global functions and crosswalk tables to be used across data
  # processing for the WQ-LT drought publication
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Calculate Water Year from a date
calc_wy <- function(date_var) {
  yr <- lubridate::year(date_var)
  dplyr::if_else(lubridate::month(date_var) >= 10, yr + 1, yr)
}

# Calculate day of Water Year from a date
calc_wy_day <- function(date_var) {
  leap_yr <- lubridate::leap_year(date_var)
  doy <- lubridate::yday(date_var)
  dplyr::case_when(
    leap_yr == TRUE & doy >= 275 ~ doy - 274,
    leap_yr == TRUE & doy < 275 ~ doy + 92,
    leap_yr == FALSE & doy >= 274 ~ doy - 273,
    leap_yr == FALSE & doy < 274 ~ doy + 92
  )
}

# Crosswalk table for converting parameter names to those used in the
  # publication
df_param_cw <- tibble::tribble(
  ~ Parameter, ~ Parameter_publ,
  "Temperature", "water temperature",
  "Salinity", "salinity",
  "Secchi", "Secchi depth",
  "DissAmmonia", "ammonium",
  "DissNitrateNitrite", "nitrate + nitrite",
  "DissOrthophos", "ortho-phosphate",
  "Chlorophyll", "chlorophyll"
)

# Crosswalk table for survey names used in the publication
df_survey_cw <- tibble::tribble(
  ~ Source, ~ Source_publ, ~ Survey_name, ~ Operator,
  "20mm", "20 mm", "20 mm Survey", "CDFW",
  "Baystudy", "Bay Study", "Bay Study", "CDFW",
  "FMWT", "FMWT", "Fall Midwater Trawl", "CDFW",
  "SKT", "SKT", "Spring Kodiak Trawl", "CDFW",
  "STN", "STN", "Summer Townet Survey", "CDFW",
  "DJFMP", "DJFMP", "Delta Juvenile Fish Monitoring Program", "USFWS",
  "EMP", "EMP", "Environmental Monitoring Program", "CDWR",
  "NCRO", "NCRO", "North Central Region Office monitoring program", "CDWR",
  "SDO", "SDO", "Stockton Dissolved Oxygen monitoring", "CDWR",
  "Suisun", "Suisun", "Suisun Marsh Fish Study", "UCD",
  "USGS_CAWSC", "USGS CAWSC", "Sacramento River at Freeport, California (11447650)", "USGS",
  "USGS_SFBS", "USGS SFBS", "San Francisco Bay Water Quality Program", "USGS"
)
