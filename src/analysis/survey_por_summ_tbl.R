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

# Source global data processing functions
source(here("src/data_processing/global_data_proc_func.R"))

# Import discrete measurement data for all WQ, nutrient, and chlorophyll parameters
fp_dwq <- dir_ls(here("data/interim"), regexp = "raw.+\\.qs$")
df_dwq <- map(fp_dwq, qread) %>% list_rbind()

# Summarize periods of record for each survey-parameter combination
df_survey_summ <- df_dwq %>%
  summarize(
    min_yr = min(YearAdj),
    max_yr = max(YearAdj),
    .by = c(Source, Parameter)
  ) %>%
  # Join crosswalk tables for parameter and survey names
  left_join(df_param_cw, by = join_by(Parameter)) %>%
  left_join(df_survey_cw, by = join_by(Source)) %>%
  select(-c(Source, Parameter)) %>%
  arrange(min_yr, Parameter_publ, .locale = "en") %>%
  # Create period of record column combining min and max years
  unite("Period_of_record", min_yr, max_yr, sep = " - ") %>%
  # Combine parameters into one column for each unique combination of records
  chop(Parameter_publ) %>%
  mutate(Parameters_collected = map_chr(Parameter_publ, ~ str_c(.x, collapse = ", "))) %>%
  # Clean up
  transmute(
    Survey = Survey_name,
    Abbreviation = factor(Source_publ, levels = df_survey_cw$Source_publ),
    Operator,
    Parameters_collected,
    Period_of_record
  ) %>%
  arrange(Abbreviation)

# Export survey summary table to be used in the publication.
df_survey_summ %>% write_csv(here("results/tables/survey_summary_table.csv"))

