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

# Read CVA data
cva_orig <- readRDS("data/cva/processed/cva_data.Rds")

# Read Morley data
proj_orig <- readRDS("data/morley/processed/morley_projections.Rds")

# Fix many-to-one problem
# Add sample size to plot
# Incorporate North Pacific


# Build data
################################################################################

# Inspect regions
table(cva_orig$region)
table(proj_orig$region)

# Format CVA
cva <- cva_orig %>% 
  # Remove Pacific salmon
  filter(region!="Pacific salmon")

# Build data
data <- proj_orig %>% 
  # Simplify
  select(rcp:uncertainty) %>% 
  # Format region to match CVA
  mutate(cva_region=recode(region,
                           "US West Coast"="Pacific",
                           "Eastern Bering Sea"="North Pacific",
                           "Gulf of Alaska"="North Pacific")) %>% 
  # Add CVA results
  left_join(cva_orig, by=c("cva_region"="region", "species"="species")) %>% # FIX MERGE
  # Remove rows without data
  filter(!is.na(dist_change))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(y=dist_change, x=shift_km)) +
  facet_grid(rcp~region, scales="free_x") +
  geom_boxplot() +
  # Labels
  labs(x="End-of-century centroid shift (km)", y="Propensity for\ndistribution change") +
  # Theme
  theme_bw()
g

# Export
# ggsave(g, filename=file.path(plotdir, "FigX_cva_analysis.png"), 
#        width=6.5, height=6.5, units="in", dpi=600)



