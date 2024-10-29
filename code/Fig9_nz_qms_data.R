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

# Read data
data_orig <- readxl::read_excel("data/new_zealand/2025_QMS_catch_limits.xlsx")


# Format data
data <- data_orig %>% 
  # Arrange
  select(spp_code, species,target_yn, bycatch_yn, qms_yn, everything()) %>% 
  # Gather
  gather(key="fishery", value="catch_kg", 6:ncol(.)) %>% 
  mutate(fishery=recode_factor(fishery,
                              "comm_kg"="Commercial\n(87% average)", 
                              "rec_kg"="Recreational\n(8% average)", 
                              "custom_kg"="Customary\n(5% average)")) %>% 
  # Calculate percent
  group_by(spp_code, species) %>% 
  mutate(catch_perc=catch_kg/sum(catch_kg))

data %>% 
  group_by(fishery) %>% 
  summarize(catch_perc=mean(catch_perc, na.rm=T)*100)

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
g <- ggplot(data, aes(y=fishery, x=catch_perc)) +
  geom_boxplot() +
  # Labels
  labs(x="Percent of quota, 2025", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig9_nz_qms_data.png"), 
       width=3.5, height=2, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig9_nz_qms_data.pdf"), 
       width=3.5, height=2, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig9_nz_qms_data.tiff"), 
       width=3.5, height=2, units="in", dpi=600)
