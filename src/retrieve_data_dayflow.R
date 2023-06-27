# WQ-LT Drought Publication
# Purpose: Retrieve Delta Inflow and Outflow from DAYFLOW model for water years
  # 1975-2021 and save copy in data/external for continued processing
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(readr)
library(tibble)
library(purrr)
library(here)

# Download DAYFLOW Data from CNRA portal: https://data.cnra.ca.gov/dataset/dayflow
dayflow_1970_1983 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")
dayflow_1984_1996 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")
dayflow_1997_2020 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv")
dayflow_2021 <- read_csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/83122ce7-e7f5-4ad1-b5e9-6a7032cb1117/download/dayflowcalculations2021.csv")

# Export data as .csv files
lst(dayflow_1970_1983, dayflow_1984_1996, dayflow_1997_2020, dayflow_2021) %>%
  iwalk(\(x, idx) write_csv(x, file = here("data/external", paste0(idx, ".csv"))))

