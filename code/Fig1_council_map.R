
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
datadir <- "data"
plotdir <- "figures"

# Councils
councils <- c("New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico",
              "Caribbean", "Pacific", "North Pacific", "Western Pacific")
colors <- c("#1f78b4", "#a6cee3", "#ff7f00", "#e31a1c",
            "#fb9a99",  "#b2df8a", "#33a02c", "#fdbf6f")

# Read EEZ file
eez_us <- readRDS("data/fmc_regions/US_EEZ_by_council.Rds") %>% 
  mutate(council=factor(council, levels=councils))

# Read database for stock totals
data <- readRDS("data/database/processed/quota_allocations_database.Rds")
table(data$council_lead)

# Build data
################################################################################

# World
usa <- rnaturalearth::ne_states(country = c("United States of America", "Puerto Rico"), returnclass = "sf")
foreign <- rnaturalearth::ne_countries(returnclass = "sf") 

# Shift to pacific-centered
usa_shifted <- lwgeom::st_wrap_x(usa, 0, 360)
foreign_shifted <- lwgeom::st_wrap_x(foreign, 0, 360)
eez_us_shifted <- lwgeom::st_wrap_x(eez_us, 0, 360)


# All EEZs in 1 panel
################################################################################

# Bounding box
sf::st_bbox(eez_us_shifted)

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
  geom_sf(data = eez_us_shifted, mapping=aes(fill=council), color=NA, show.legend = F) + #
  # Crop
  coord_sf(xlim=c(145, 290), ylim=c(-17, 72)) +
  # coord_sf(xlim=c(172, 292), ylim=c(17, 72)) +
  # Legend
  scale_fill_manual(name="Council", values=colors) +
  # Theme
  theme_bw() + my_theme
g



# Seperate EEZs
################################################################################

# Base theme
my_theme <-  theme(axis.ticks=element_blank(),
                   axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# FMC labels
labels_fmc <- matrix(data=c("Pacifiic", -130, 30,
                            "Gulf of Mexico", -90, 23,
                            "Caribbean", -75, 17,
                            "South Atlantic", -73, 30,
                            "Mid-Atlantic", -66, 36,
                            "New England", -73, 50), ncol=3, byrow=T) %>%
  as.data.frame() %>% 
  setNames(c("council", "long", "lat")) %>% 
  mutate_at(vars(long:lat), as.numeric)

  
# Plot continent
g1 <- ggplot() + 
  # Plot data
  geom_sf(data = foreign, fill="grey90", color="white", lwd=0.2)+ 
  geom_sf(data = usa, fill="grey80", color="grey40", lwd=0.2)+ 
  geom_sf(data = eez_us, mapping=aes(fill=council), color=NA, show.legend = F) + 
  # FMC titles
  # geom_text(data=labels_fmc, mapping=aes(color=council, x=long, y=lat, label=council),
  #           size=1.5, fontface="bold", show.legend = F) +
  # Crop
  coord_sf(xlim=c(-130, -65), ylim=c(15, 50)) +
  # Legend
  scale_fill_manual(name="Council", values=colors) +
  # Theme
  theme_bw() + my_theme
g1

# Alaska labels
labels_ak <- tibble(region=c("Gulf of\nAlaska",
                             "Bering Sea/\nAleutian Islands",
                             "Arctic"),
                    long=c(215, 190, 207),
                    lat=c(58.5, 57.5, 72.5))

# Plot Alaska
g2 <- ggplot() + 
  # Plot data
  geom_sf(data = foreign_shifted, fill="grey90", color="white", lwd=0.2)+
  geom_sf(data = usa_shifted, fill="grey80", color="grey40", lwd=0.2)+
  geom_sf(data = eez_us_shifted, mapping=aes(fill=council), color=NA, show.legend = F) + 
  # Plot region labels
  geom_text(data=labels_ak, mapping=aes(x=long, y=lat, label=region), 
            fontface="italic", size=1.8) +
  # Crop
  coord_sf(xlim=c(166, 234), ylim=c(48.6, 74)) +
  # Legend
  scale_fill_manual(name="Council", values=colors) +
  # Theme
  theme_bw() + my_theme
g2

# Pacific labels
labels_pac <- eez_us_shifted %>% 
  # Reduce to WP
  filter(council=="Western Pacific") %>% 
  # Record centrodi
  sf::st_centroid() %>% 
  mutate(long=sf::st_coordinates(.)[,"X"],
         lat=sf::st_coordinates(.)[,"Y"])  %>% 
  select(eez, long, lat) %>% 
  sf::st_drop_geometry() %>% 
  # Fix longitudes so [0, 360]
  mutate(long=ifelse(long<0, long+360, long)) %>% 
  # Remove extra Hawaai
  filter(!(eez=="Hawaii" & lat>=24)) %>% 
  # Format EEZs
  mutate(eez=recode(eez, 
                    "American Samoa"="American\nSamoa",         
                    "Palmyra Atoll"="Palmyra\nAtoll",          
                    "Johnston Atoll"="Johnston\nAtoll", 
                    "Northern Mariana"="Northern\nMariana", 
                    "Wake Island"="Wake\nIsland", 
                    "Jarvis Island"="Jarvis\nIsland", 
                    "Howland and Baker Islands"="Howland and\nBaker Islands"))

# Plot Pacific islands
g3 <- ggplot() + 
  # Plot data
  geom_sf(data = foreign_shifted, fill="grey90", color="white", lwd=0.2)+
  geom_sf(data = usa_shifted, fill="grey80", color="grey40", lwd=0.2)+
  geom_sf(data = eez_us_shifted, mapping=aes(fill=council), color=NA, show.legend = F) + 
  # Plot region labels
  geom_text(data=labels_pac, mapping=aes(x=long, y=lat, label=eez), 
            fontface="italic", size=1.8, lineheight = 0.9) +
  # Crop
  coord_sf(xlim=c(142, 210), ylim=c(-17, 35)) + 
  # Legend
  scale_fill_manual(name="Council", values=colors) +
  # Theme
  theme_bw() + my_theme
g3

# Merge plots
layout_matrix <- matrix(data=c(1,2,
                               3,2), nrow=2, byrow=T)
g <- gridExtra::grid.arrange(g2, g1, g3,
                             layout_matrix=layout_matrix,
                             widths=c(0.3, 0.7))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_council_map.png"), 
       width=6.5, height=3, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig1_council_map.pdf"), 
       width=6.5, height=3, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig1_council_map.tiff"), 
       width=6.5, height=3, units="in", dpi=600)
