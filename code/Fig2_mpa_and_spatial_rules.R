
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

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_map_and_spatial_rules.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





