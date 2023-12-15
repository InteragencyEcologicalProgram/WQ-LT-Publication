# WQ-LT Drought Publication
# Purpose: Global functions to be used across data processing for the WQ-LT
  # drought publication
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Calculate Water Year from a date
calc_wy <- function(date_var, month) {
  yr <- lubridate::year(date_var)
  dplyr::if_else(lubridate::month(date_var) >= month, yr + 1, yr)
}

# Calculate day of Water Year from a date
calc_wy_day <- function(date_var, month) {
  if(!month%in%3:12){
    stop("sorry, I only coded this to work with start months of March through Dec (really just for Oct or Dec start months).
         You'll have to do something more clever with the leap year adjustments to work for Jan and Feb.")
  }

  year_adj_day<-yday(ymd(paste("2023", month, "01", sep="-"))) # Start day of the adjusted year, for a non-leap year

  leap_yr <- lubridate::leap_year(date_var)
  doy <- lubridate::yday(date_var)
  dplyr::case_when(
    leap_yr == TRUE & doy >=(year_adj_day+1) ~ doy -year_adj_day,
    leap_yr == TRUE & doy <(year_adj_day+1) ~ doy + (365-year_adj_day+1),
    leap_yr == FALSE & doy >=year_adj_day ~ doy -(year_adj_day-1),
    leap_yr == FALSE & doy <year_adj_day ~ doy + (365-year_adj_day+1)
  )
}

