# WQ-LT Drought Publication
# Purpose: Process spatial data used in map figures
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

library(tidyverse)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# install.packages("devtools")
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(sf)
library(qs)
library(fs)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())


# Station Coordinates -----------------------------------------------------

# UGSS velocity stations:
# Import coordinates for the UGSS velocity stations
df_usgs_vel_coord <- read_csv(here("data/external/vel_coord.csv"))

# Keep only the necessary columns and rename stations
df_usgs_vel_coord_c <- df_usgs_vel_coord %>%
  select(
    Station = MonitoringLocationIdentifier,
    Latitude = LatitudeMeasure,
    Longitude = LongitudeMeasure
  ) %>%
  mutate(
    Station = case_match(
      Station,
      "USGS-11455350" ~ "Cache-RYI",
      "USGS-11455385" ~ "Cache-RYF",
      "USGS-11337190" ~ "Jersey",
      "USGS-11312676" ~ "Middle",
      "USGS-11313405" ~ "Old"
    )
  )

# Convert to sf object
sf_usgs_vel_coord <- df_usgs_vel_coord_c %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Discrete WQ sampling locations:
# Import discrete measurement data for all WQ, nutrient, and chlorophyll parameters
fp_dwq <- dir_ls(here("data/interim/"), regexp = "raw.+\\.qs$")
df_dwq <- map(fp_dwq, qread) %>% list_rbind()

# Find unique combinations of Station, Latitude and Longitude
df_dwq_c <- df_dwq %>% distinct(Source, Station, Latitude, Longitude)

# Convert to sf object
sf_dwq <- df_dwq_c %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Region Polygons ---------------------------------------------------------

# Import region assignments
df_regions <- read_csv(here("data/raw/region_assignments.csv"))

# Load Delta subregions shapefile from EDSM, add region assignments, and
  # dissolve subregions into regions
sf_regions <- R_EDSM_Subregions_Mahardja_FLOAT %>%
  select(SubRegion) %>%
  inner_join(df_regions, by = join_by(SubRegion)) %>%
  # Add a 0.5 meter buffer around each subregion to eliminate slivers within
  # polygons when they're dissolved
  st_buffer(0.5) %>%
  group_by(Region) %>%
  summarize()


# Export Data -------------------------------------------------------------

# UGSS velocity station coordinates
sf_usgs_vel_coord %>% write_sf(here("data/processed/spatial/velocity_coords.shp"))

# Discrete WQ station coordinates
sf_dwq %>% write_sf(here("data/processed/spatial/discrete_wq_coords.shp"))

# Region polygons
sf_regions %>% write_sf(here("data/processed/spatial/regions.shp"))

