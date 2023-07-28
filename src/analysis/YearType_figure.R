# WQ-LT Drought Publication
# Purpose: Create figure of SV water year indices and drought classifications
  # for publication
# Authors: Rosie Hartman, Dave Bosworth
# Contacts: Rosemary.Hartman@water.ca.gov; David.Bosworth@water.ca.gov

library(tidyverse)
library(here)

# Import water year assignments
df_yr_type <- read_csv(here("data/raw/year_assignments.csv"))

# Create a data frame of SV Index cutoffs for Year Types to be used on the figure
df_cutoffs <- tibble(
  Yr_type = c("Critical", "Dry", "Below \nNormal", "Above \nNormal", "Wet"),
  cutval = c(0, 5.4, 6.5, 7.8, 9.2)
)

# Define color palette for drought classifications
pal_drought <- c(D = "#FDE333", N = "#53CC67", W = "#00588B")

# Create bar chart figure for publication
plt_yr_type <- df_yr_type %>%
  ggplot() +
  geom_col(aes(x = Year, y = SVIndex, fill = Drought)) +
  scale_fill_manual(
    name = "Drought Classification: ",
    values = pal_drought,
    labels = c("Drought", "Neutral", "Wet")
  ) +
  theme_bw() +
  geom_hline(data = df_cutoffs, aes(yintercept = cutval), linetype = 2) +
  geom_text(
    data = df_cutoffs,
    aes(x = 1970, y = cutval + 0.2, label = Yr_type),
    vjust = 0,
    hjust = 0,
    size = 3,
    lineheight = 0.9
  ) +
  scale_x_continuous(breaks = seq.int(1975, 2020, by = 5)) +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "top"
  ) +
  ylab("Sacramento Valley Water Year Index") +
  xlab("Water Year")

# Export Year Type Figure
ggsave(
  here("results/figures/yr_type.jpg"),
  plot = plt_yr_type,
  width = 6,
  height = 5,
  units = "in"
)

