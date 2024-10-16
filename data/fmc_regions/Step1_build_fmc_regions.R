
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(rnaturalearth)
library(ggplot2)
library(lwgeom)

# Directories
outdir <- "data/fmc_regions"
plotdir <- "figures"

# EEZ
eez <- marineregions::eezs_lr


# Build data
################################################################################

# US EEZ
eez_us <- eez %>% 
  # Reduce to US
  filter(sovereign1=="United States") %>% 
  # Simplify
  select(geoname, geometry) %>% 
  # Rename
  rename(eez=geoname) %>% 
  # Format
  mutate(eez=gsub(" Exclusive Economic Zone", "", eez) %>% stringr::str_squish(.)) %>% 
  # Remove overlapping claim
  filter(!grepl("Overlapping|Joint", eez)) %>% 
  # Split
  sf::st_cast(to="POLYGON") %>% 
  # Add council
  mutate(council=recode(eez,
                        "American Samoa"="Western Pacific",                          
                        "Palmyra Atoll"="Western Pacific",                         
                        "Puerto Rican"="Caribbean",                         
                        "Virgin Islander"="Caribbean",                          
                        "Johnston Atoll"="Western Pacific", 
                        "Guam"="Western Pacific",        
                        "Northern Mariana"="Western Pacific",           
                        "Wake Island"="Western Pacific",              
                        "Jarvis Island"="Western Pacific",           
                        "Howland and Baker Islands"="Western Pacific",   
                        "United States (Hawaii)"="Western Pacific",                
                        "United States (Alaska)"="North Pacific",                
                        "United States"="Mainland")) %>% 
  # Label 
  mutate(council=ifelse(council %in% "Mainland", make.unique(council), council),
         council=recode(council, 
                        "Mainland.1"="Pacific"))  %>% 
  # Format EEZ
  mutate(eez=case_when(council=="Pacific" ~ "West Coast",
                     eez=="United States (Alaska)" ~ "Alaska",
                     eez=="United States (Hawaii)" ~ "Hawaii",
                     eez=="United States" ~ "East Coast",
                     T ~ eez))


# Seperate East Coast for splitting
eez_east <- eez_us %>% 
  filter(eez=="East Coast")

# Cape Hatteras: 35.2225, -75.53	
# Near Point Judith: 41.379058, -71.479020
# Florida: 25.510183, -80.672610; 23.040815, -79.711237


# Define the splitting line with two end points
hatteras <- sf::st_sfc(sf::st_linestring(rbind(c(-80, 35.2225), c(-65, 35.2225))), 
                         crs = sf::st_crs(eez_east))
pt_judith <- sf::st_sfc(sf::st_linestring(rbind(c(-80, 41.379058), c(-60, 41.379058))), 
                       crs = sf::st_crs(eez_east))
florida <- sf::st_sfc(sf::st_linestring(rbind(c(-80.672610, 25.510183), c(-79.711237, 23.040815))), 
                        crs = sf::st_crs(eez_east))

# Split by Hatteras
eez_east_split1 <- lwgeom::st_split(eez_east, hatteras)
eez_east_split1_poly <- sf::st_collection_extract(eez_east_split1, type = "POLYGON") 

# Split by Judith
eez_east_split2 <- lwgeom::st_split(eez_east_split1_poly, pt_judith)
eez_east_split2_poly <- sf::st_collection_extract(eez_east_split2, type = "POLYGON") 

# Split by Florida
eez_east_split3 <- lwgeom::st_split(eez_east_split2_poly, florida)
eez_east_split3_poly <- sf::st_collection_extract(eez_east_split3, type = "POLYGON") 

# Finalize
eez_east_split <- eez_east_split3_poly %>% 
  mutate(id=1:n() %>% as.character()) 

# Assume 'polygons' is your sf polygon object
# Calculate the centroids
centroids <- st_centroid(eez_east_split)

# Extract the coordinates of the centroids
centroid_coords <- st_coordinates(centroids)

# Add the latitude and longitude as new columns
eez_east_split_final <- eez_east_split %>% 
  mutate(long_dd = centroid_coords[, 1],  # Longitude (X)
         lat_dd = centroid_coords[, 2]) %>%    # Latitude (Y) %>% 
  mutate(council=case_when(id %in% c(1) ~ "New England",
                           id %in% c(2, 3, 4, 5, 6, 10, 12) ~ "Mid-Atlantic",
                           id %in% c(7, 11, 9) ~ "South Atlantic",
                           id %in% c(8) ~ "Gulf of Mexico",
                           T ~ id))


# Plot east
ggplot(eez_east_split_final, aes(fill=council)) +
  # Plot regions
  geom_sf() +
  # Plot centroids
  geom_text(aes(x=long_dd, y=lat_dd, label=id)) + 
  # Crop
  # coord_sf(ylim=c(y=41, 42), xlim=c(-74, -70)) # NE/MA
  # coord_sf(ylim=c(y=41, 42), xlim=c(-71, -70)) # NE/MA
  # coord_sf(ylim=c(y=35.1, 35.3), xlim=c(-76.7, -75.5)) # NE/MA
  coord_sf(ylim=c(y=24.5, 25.5), xlim=c(-82, -79)) # Florida


# Merge
council_eez <- eez_us %>% 
  # Remove East Coast
  filter(eez!="East Coast") %>% 
  # Add formatted East Coast
  bind_rows(eez_east_split_final) %>% 
  # Arrange
  select(-id) %>% 
  select(council, eez, everything())


# Export data
################################################################################

# Export
saveRDS(council_eez, file.path(outdir, "US_EEZ_by_council.Rds"))

# Plot data
################################################################################

# World
usa <- rnaturalearth::ne_states(country = c("United States of America", "Puerto Rico"), returnclass = "sf")
foreign <- rnaturalearth::ne_countries(returnclass = "sf") 

# Shift to pacific-centered
usa_shifted <- lwgeom::st_wrap_x(usa, 0, 360)
foreign_shifted <- lwgeom::st_wrap_x(foreign, 0, 360)
council_eez_shifted <- lwgeom::st_wrap_x(council_eez, 0, 360)

# Bounding box
sf::st_bbox(council_eez_shifted)

# Base theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_blank(),
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

# Plot it
g <- ggplot() + 
  # Plot data
  geom_sf(data = foreign_shifted, fill="grey90", color="white", lwd=0.2)+ 
  geom_sf(data = usa_shifted, fill="grey80", color="grey40", lwd=0.2)+ 
  geom_sf(data = council_eez_shifted, mapping=aes(fill=council), color=NA, show.legend = F) + #
  # Crop
  coord_sf(xlim=c(145, 290), ylim=c(-17, 72)) +
  # coord_sf(xlim=c(263, 294), ylim=c(22, 50)) + 
  # Legend
  scale_fill_discrete(name="Council") +
  # Theme
  theme_bw() + my_theme
g


