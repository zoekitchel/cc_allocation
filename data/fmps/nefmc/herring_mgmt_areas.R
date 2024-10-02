
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/nefmc"

# Read data
data <- sf::st_read(file.path(datadir, "Herring_Management_Areas/Herring_Management_Areas.shp"))


# Setup
################################################################################

# Plot data
ggplot() +
  geom_sf(data=data) +
  theme_bw()
