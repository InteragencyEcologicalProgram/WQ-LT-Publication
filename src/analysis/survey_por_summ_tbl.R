# WQ-LT Drought Publication
# Purpose: Create table of the parameters collected by each survey used in the
  # publication and their periods of record. This table is used in the
  # publication.
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(tidyverse)
library(qs)
library(fs)
library(here)
# library(conflicted)

# Import discrete measurement data for all WQ, nutrient, and chlorophyll parameters
fp_dwq <- dir_ls(here("data/interim"), regexp = "raw.+\\.qs$")
df_dwq <- map(fp_dwq, qread) %>% list_rbind()

# Define factor level for Parameter
param_lev <- c(
  "Temperature",
  "Salinity",
  "Secchi",
  "DissAmmonia",
  "DissNitrateNitrite",
  "DissOrthophos",
  "Chlorophyll"
)

# Summarize periods of record for each survey-parameter combination
df_survey_summ <- df_dwq %>%
  mutate(Parameter = factor(Parameter, levels = param_lev)) %>%
  summarize(
    min_yr = min(YearAdj),
    max_yr = max(YearAdj),
    .by = c(Source, Parameter)
  ) %>%
  arrange(Source, Parameter)

# Export survey summary table to be used in the publication.
df_survey_summ %>% write_csv(here("outputs/survey_summary_table.csv"))

