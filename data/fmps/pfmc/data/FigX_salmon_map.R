
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
datadir <- "data/fmps/pfmc/data"
plotdir <- "figures"

# Read data
mouthes <- readxl::read_excel(file.path(datadir, "PFMC_SalmonFMP_stocks.xlsx"), sheet=2)


# Build data
################################################################################

# World
usa <- rnaturalearth::ne_states(country = c("United States of America"), returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf") 


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
  # Leadbetter point
  geom_hline(yintercept = 46.659951, linetype="dashed") +
  # Cape Falcon
  geom_hline(yintercept = 45.767667, linetype="dashed") +
  # Plot data
  geom_sf(data = foreign, fill="grey90", color="white", lwd=0.2) + 
  geom_sf(data = usa, fill="grey90", color="white", lwd=0.2) + 
  # Sites
  geom_point(data=mouthes, mapping=aes(x=long_dd, y=lat_dd), size=1.1, color="grey50") +
  # Site labels
  ggrepel::geom_text_repel(data=mouthes, mapping=aes(x=long_dd, y=lat_dd, label=river), 
                           size=2.2, segment.color='grey50') +
  # Crop
  coord_sf(xlim=c(-116, -128), ylim=c(37, 48.5)) +
  # Legend
  scale_fill_manual(name="Council", values=colors) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(datadir, "FigX_salmon_map.png"), 
       width=4.5, height=6, units="in", dpi=600)


