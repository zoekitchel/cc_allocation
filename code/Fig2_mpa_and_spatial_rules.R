
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data"
plotdir <- "figures"

# US EEZ
eez <- marineregions::eezs_lr
eez_use <- eez %>% 
  filter(sovereign1=="United States")


# Setup
################################################################################

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(returnclass = "sf") 

# Base theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # EEZ
  geom_sf(data=eez_use, fill="lightblue", color=NA) +
  # Land
  geom_sf(data=foreign, fill="grey80", color="white") +
  geom_sf(data=usa, fill="grey80", color="white") +
  # Crop
  coord_sf(xlim = c(-128, -65), ylim = c(15, 50)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g



# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_map_and_spatial_rules.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


###


# https://stackoverflow.com/questions/69970029/st-crop-on-pacific-centred-naturalearth-world-map

worldMap <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid()

target_crs <- st_crs("+proj=eqc +x_0=0 +y_0=0 +lat_0=0 +lon_0=133")

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of world
# Centered in lon 133

offset <- 180 - 133


polygon <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)


# modify world dataset to remove overlapping portions with world's polygons
world2 <- worldMap %>% st_difference(polygon)
eez2 <- eez %>% st_make_valid() %>% st_difference(polygon)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

# Transform
world3 <- world2 %>% st_transform(crs = target_crs)

ggplot(data = world3, aes(group = admin)) +
  geom_sf(fill = "grey")




# Load necessary packages
library(ggplot2)
library(maps)
library(mapdata)

# Get the map data for USA (including Alaska)
alaska_map <- map_data("world2", region = "USA:Alaska")

# Plot the map using ggplot2
ggplot() +
  geom_polygon(data = alaska_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  coord_fixed(xlim = c(170, -130), ylim = c(50, 75), ratio = 1.3) +  # Adjust x and y limits to span the Date Line
  theme_minimal() +
  labs(title = "Map of Alaska including the Aleutian Islands",
       x = "Longitude",
       y = "Latitude")



