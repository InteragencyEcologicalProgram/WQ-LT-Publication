#Map of sampling stations


library(sf)
library(tidyverse)
library(readxl)
library(deltamapr)
library(dataRetrieval)
library(cowplot)
library(ggsn)
library(tigris)

load("data/DroughtRegions.RData")

#move region labels outside boxes
Regions = mutate(Regions, nudge = c(-.1, .1, -.2, -.09, .09))


######################################################################
#map for water quality paper


#all the ddiscrete locations (except the EZ stations)
stas = read_csv("data/drought_wq_station_coords.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(!Station %in% c("EMP EZ2", "EMP EZ6", "EMP EZ2-SJR", "EMP EZ6-SJR"))


siteNumbers <- c(
  'USGS-11455350', # CACHE SLOUGH A RYER ISLAND (inactive)
  'USGS-11455385', # CACHE SLOUGH AB RYER ISLAND FERRY NR RIO VISTA CA (active)
  'USGS-11337190', # SAN JOAQUIN R A JERSEY POINT CA
  'USGS-11313405', # OLD R A BACON ISLAND CA
  'USGS-11312676' # MIDDLE R AT MIDDLE RIVER CA
)

vel_coords <- dataRetrieval::whatWQPsites(siteid = siteNumbers)%>%
  dplyr::select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
vels = st_as_sf(vel_coords, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326) %>%
  mutate(ID = c(1,2,3,4,5), name = c("Cache Slough at Ryer Island (inactive)",
                                     "Cache Slough above Ryer Island Ferry (active)",
                                     "San Joaquin River at Jersey Point",
                                     "Old River at Bacon Island",
                                     "Middle River at Middle River"),
         Abbreviation = c("Cache", "Cache", "Jersey", "Old", "Middle"),
         USGSnumbs = siteNumbers)

#write.csv(vels, "data/Velocitystations.csv")

wqmap = ggplot()+
  geom_sf(data = WW_Delta, fill = "lightskyblue", color = "grey")+
  geom_sf(data = Regions,
          aes(fill=Region), alpha = 0.2)+
  geom_sf_label(data = Regions,
                aes(label = Region), position = position_nudge(y=Regions$nudge))+
  theme_bw()+
  geom_sf(data = stas, aes(shape = Source))+
  scale_shape_manual(values = c(15,16,17,18,22,23,24,25,11,1, 2, 3), name = "Discrete \nSamples")+
  geom_sf(data = vels, shape = 16, size = 5,aes(color = "Continous\nVelocity"))+
  geom_sf_text(data = vels, aes(label = ID))+
  scale_color_discrete(name = NULL)+
  theme(legend.position="none")+

  scale_fill_viridis_d(option = "H", guide = NULL)+
  scalebar( y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2,
            transform = TRUE, dist = 10, st.size = 4,
            dist_unit = "km", model = "WGS84", location = "bottomleft") +
  north(y.min = 37.8, y.max = 38.4, x.min = -122.2, x.max = -121.2,  symbol = 2) +
  theme_bw()+ylab("")+xlab("")+

  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.7, 38.6))

wqmap



########################################################################
#map of california for inset

#get the outlie of california
us_states = states(cb = FALSE, class = "sf")
Cal = filter(us_states, NAME == "California")

#bounding box of area of interest
deltabb = st_as_sfc(st_bbox(Regions))

#put the box on the map of california
ggm3 = ggplot() +
  geom_sf(data = Cal, fill = "white", size = 0.2) +
  geom_sf(data = WW_Watershed, fill = "white", color = "grey")+
  geom_sf(data = deltabb, fill = NA, color = "blue", size = 1.2) +
  theme(axis.title = element_blank(), plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(), axis.text = element_blank())

ggm3

#now add the inset to the map
wqmapinste= ggdraw() +
  draw_plot(wqmap) +
  draw_plot(ggm3, x = 0.1, y = 0.7, width = 0.2, height = 0.2)

wqmapinste
ggsave("plots/WQmap.tiff", device = "tiff", width = 8, height = 8)
ggsave("plots/WQmap.pdf", device = "pdf", width = 8, height = 8)
