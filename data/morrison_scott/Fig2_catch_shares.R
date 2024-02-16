
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/morrison_scott"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Morrison_Scott_Appendix1.xlsx"), sheet=2)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
 # Remove anything without years
  filter(!is.na(period)) %>% 
 # Format councils
  mutate(council=recode_factor(council,
                               "NPFMC"="North\nPacific",
                               "PFMC"="Pacific",
                               "NEFMC"="New\nEngland",
                               "MAFMC"="Mid-\nAtlantic",
                               "SAFMC"="South\nAtlantic",
                               "GMFMC"="Gulf of\nMexico")) %>% 
  # Break years
  separate(col=period, into=c("year1", "year2"), convert=T, remove=F) %>% 
  # Format historical
  mutate(hist_catch_basis=recode(hist_catch_basis,
                                 "yes"="Entirely",
                                 "mostly"="Mostly",
                                 "partly"="Partly"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data) +
  facet_grid(council~., scales="free_y", space="free_y") +
  geom_segment(mapping=aes(x=year1, 
                           xend=year2, 
                           color=hist_catch_basis,
                           y=species, 
                           yend=species),
               linewidth=0.8) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(lim=c(NA, 2020), breaks=seq(1975,2020,5)) +
  # Legend
  scale_color_manual(name="Based on historical catch?", values=RColorBrewer::brewer.pal(9, "Blues")[c(5,7,9)] %>% rev()) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.key.size = unit(0.3, "cm"),
        legend.box.margin = margin(-3,0,-7,0))
g


# Export
ggsave(g, filename=file.path(datadir, "FigX_catch_shares.png"), 
       width=6.5, height=3.5, units="in", dpi=600)



