
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/database/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "quota_allocations_database.Rds"))
fmp_key <- readxl::read_excel("data/database/raw/fmp_key_bad.xlsx")

# Build
data <- data_orig %>% 
  left_join(fmp_key) %>% 
  mutate(shift_km=runif(n=nrow(.), 0, 200),
         cva=runif(n=nrow(.), 0, 5)) 

stats <- data %>% 
  group_by(council_lead, fmp_short) %>% 
  summarize(shift_med=median(shift_km)) %>% 
  ungroup() %>% 
  arrange(council_lead, desc(shift_med))

data_ordered <- data %>% 
  mutate(fmp_short=factor(fmp_short, levels=stats$fmp_short))


# Overall
################################################################################


# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=9),
                    plot.tag = element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(data_ordered, aes(y=fmp_short, x=shift_km)) +
  facet_grid(council_lead~., space="free_y", scale="free_y") +
  geom_boxplot() +
  # Labels
  labs(x="Shift (km/decade)", y="") +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "fmp_prioritzation_shift.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



