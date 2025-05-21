
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
data_orig <- readxl::read_excel(file.path(datadir, "season_allocation_database.xlsx"))


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Factor council
  mutate(council=recode_factor(council, 
                               "New England"="New England", 
                               "Mid-Atlantic"="Mid-Atlantic", 
                               "South Atlantic"="South Atlantic", 
                               "Pacific"="Pacific")) %>% 
  # Format date
  mutate(start=lubridate::ymd(start),
         end=lubridate::ymd(end)) %>% 
  # Calculate midpoint
  mutate(midpoint = as.Date((as.numeric(start) + as.numeric(end)) / 2, origin = "1970-01-01")) %>% 
  # Label
  mutate(percent_label=paste0(percent*100, "%")) %>% 
  # Arrange
  arrange(council, fmp, species) %>% 
  mutate(order=1:n()) %>% 
  # Mark herring
  mutate(species=recode(species,
                        "Atlantic herring"="Atlantic herring*"))
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   strip.text.y = element_text(angle = 0),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, mapping=aes(y=reorder(species, order), x=start, xend=end, 
                              linewidth=percent, color=percent)) + 
  facet_grid(council~., space="free_y", scales="free_y") +
  geom_segment() +
  # Reference line
  geom_vline(xintercept=ymd("2024-01-01"), color="grey30", lwd=0.2, linetype="dashed") +
  # Add text
  # geom_text(mapping=aes(y=species, x=midpoint, label=percent_label), 
  #           inherit.aes = F, show.legend = F, color="grey50", size=2) + # color="black", size=2.2
  # Axis
  scale_x_date(breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "3 months"),
               date_label = "%b") +
  # Legend
  scale_linewidth_continuous(name="% of quota", labels=scales::percent_format()) +
  scale_color_gradientn(name="% of quota", 
                        colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                        labels=scales::percent_format()) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"))
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig8_seasonal_allocations.png"),
       width=6.5, height=3.5, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig8_seasonal_allocations.pdf"),
       width=6.5, height=3.5, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig8_seasonal_allocations.tiff"),
       width=6.5, height=3.5, units="in", dpi=600)



