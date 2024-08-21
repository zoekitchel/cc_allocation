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

# Build data
################################################################################

# Read data
data1 <- readxl::read_excel(file.path(datadir, "figX_data.xlsx"), sheet=1) %>% 
  # Format metric
  mutate(metric=recode_factor(metric,
                              "Resource distribution"="Resource\ndistribution",
                              "Quota allocation"="Quota\nallocation"))

# Read data
data2 <- readxl::read_excel(file.path(datadir, "figX_data.xlsx"), sheet=2) %>% 
  # Format metric
  mutate(scenario=recode_factor(scenario,
                              "Historical access"="Maintain\nhistorical access,\ndespite resource shift",
                              "Contemporary distribution"="Adjust to\ncurrent availability,\ndespite historical reliance"))


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Map
g1 <- ggplot() +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Management zone
  geom_hline(yintercept=40, linetype="dashed") +
  annotate(geom="text", x=-60, y=40, hjust=1, vjust=c(-0.5, 1.5), 
           label=c("North", "South"), color=c("darkred", "navy")) +
  # Labels 
  labs(tag="A", x="", y="") +
  # Extent / axes
  coord_sf(xlim = c(-77, -60), ylim = c(33, 50)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y = element_blank())
g1

# Plot bars
g2 <- ggplot(data1, aes(x=as.character(year), y=percent, fill=zone)) +
  facet_wrap(~metric) +
  geom_bar(stat="identity", alpha=0.5, color="black") +
  # Labels
  labs(x="Year\n\n", y="% of value", tag="B") +
  # Legend
  scale_fill_manual(name="Zone", values=c("darkred", "navy")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot gradient
g3 <- ggplot(data2, aes(x=scenario, y=percent, fill=zone)) +
  geom_bar(stat="identity", alpha=0.5, color="black") +
  # Labels
  labs(x="", y="% of value", title="\n", tag="C") +
  # Legend
  scale_fill_manual(name="Zone", values=c("darkred", "navy")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3


# Merge 
g <- gridExtra::grid.arrange(g1, g2, g3, widths=c(0.3, 0.25, 0.45), nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_geographic_shifts.png"), 
       width=6.5, height=2.5, units="in", dpi=600)





