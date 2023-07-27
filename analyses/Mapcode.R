# WQ-LT Drought Publication
# Purpose: Create map of sampling stations for publication
# Authors: Rosie Hartman, Dave Bosworth
# Contacts: Rosemary.Hartman@water.ca.gov; David.Bosworth@water.ca.gov

library(tidyverse)
library(sf)
# Make sure we are using `deltamapr` version 1.0.0, commit d0a6f9c22aa074f906176e99a0ed70f97f26fffd
# install.packages("devtools")
# devtools::install_github("InteragencyEcologicalProgram/deltamapr", ref = "d0a6f9c22aa074f906176e99a0ed70f97f26fffd")
library(deltamapr)
library(cowplot)
library(ggsn)
library(tigris)
library(here)
library(conflicted)

# Declare package conflict preferences
conflicts_prefer(dplyr::filter())


# Import and Prepare Data -------------------------------------------------

# Import station coordinates for velocity and discrete WQ stations
sf_vel <- read_sf(here("data/processed/spatial/velocity_coords.shp"))
sf_dwq <- read_sf(here("data/processed/spatial/discrete_wq_coords.shp"))

# Add numeric ID's to velocity coordinates to be used as map labels
sf_vel_c <- sf_vel %>%
  mutate(
    ID = case_match(
      Station,
      "Cache-RYI" ~ 1,
      "Cache-RYF" ~ 2,
      "Jersey" ~ 3,
      "Old" ~ 4,
      "Middle" ~ 5
    )
  )

# Remove the EMP EZ stations from the discrete WQ coordinates since they are not
  # fixed and overwhelm the map
sf_dwq_c <- sf_dwq %>% filter(!str_detect(Station, "^EMP EZ"))

# Import Delta region polygons
sf_regions <- read_sf(here("data/processed/spatial/regions.shp"))

# Add nudge variable to region polygons to move region labels outside boxes
sf_regions_c <- sf_regions %>% mutate(nudge = c(-.1, .1, -.2, -.09, .09))


# Create Map Figure -------------------------------------------------------

# Start with large map of all sampling locations along with regions
map_wq <-
  ggplot() +
  geom_sf(data = WW_Delta, fill = "lightskyblue", color = "grey") +
  geom_sf(data = sf_regions_c, aes(fill = Region), alpha = 0.2) +
  scale_fill_viridis_d(option = "H", guide = NULL) +
  geom_sf_label(
    data = sf_regions_c,
    aes(label = Region),
    position = position_nudge(y = sf_regions_c$nudge)
  ) +
  geom_sf(data = sf_dwq_c, aes(shape = Source)) +
  scale_shape_manual(
    name = "Discrete\nSamples",
    values = c(15, 16, 17, 18, 22, 23, 24, 25, 11, 1, 2, 3)
  ) +
  geom_sf(
    data = sf_vel_c,
    aes(color = "Continous\nVelocity"),
    shape = 16,
    size = 5
  ) +
  geom_sf_text(data = sf_vel_c, aes(label = ID)) +
  scale_color_discrete(name = NULL) +
  theme(legend.position = "none") +
  scalebar(
    y.min = 37.8,
    y.max = 38.6,
    x.min = -122.2,
    x.max = -121.2,
    transform = TRUE,
    dist = 10,
    st.size = 4,
    dist_unit = "km",
    model = "WGS84",
    location = "bottomleft"
  ) +
  north(
    y.min = 37.8,
    y.max = 38.4,
    x.min = -122.2,
    x.max = -121.2,
    symbol = 2
  ) +
  theme_bw() +
  ylab(NULL) +
  xlab(NULL) +
  coord_sf(
    xlim = c(-122.2, -121.2),
    ylim = c(37.7, 38.6)
  )

# Create map of California for inset
# Retrieve the outline of California
sf_us_states <- states(cb = FALSE, class = "sf")
sf_ca <- filter(sf_us_states, NAME == "California")

# Create bounding box of area of interest
bb_delta <- st_as_sfc(st_bbox(sf_regions_c))

# Put the box on the map of California
map_inset <-
  ggplot() +
  geom_sf(data = sf_ca, fill = "white", size = 0.2) +
  geom_sf(data = WW_Watershed, fill = "white", color = "grey") +
  geom_sf(data = bb_delta, fill = NA, color = "blue", size = 1.2) +
  theme(
    axis.title = element_blank(),
    plot.background = element_rect(fill = "white"),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

# Add the inset to the map of sampling locations
map_wq_f <-
  ggdraw() +
  draw_plot(map_wq) +
  draw_plot(
    map_inset,
    x = 0.08,
    y = 0.7,
    width = 0.2,
    height = 0.2
  )


# Export Map Figure -------------------------------------------------------

ggsave(
  here("plots/wq_map.jpg"),
  plot = map_wq_f,
  width = 8,
  height = 8,
  units = "in"
)

