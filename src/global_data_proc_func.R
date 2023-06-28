# WQ-LT Drought Publication
# Purpose: Global functions to be used across data processing for the WQ-LT
  # drought publication
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
    leap_yr == TRUE & doy >= 275 ~ doy -274,
    leap_yr == TRUE & doy < 275 ~ doy + 92,
    leap_yr == FALSE & doy >= 274 ~ doy - 273,
    leap_yr == FALSE & doy < 274 ~ doy + 92
  )
}

