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
  # Format year
  mutate(year=paste0(year, "\n \n ")) %>% 
  # Format metric
  mutate(metric=recode_factor(metric,
                              "Resource distribution"="Resource\ndistribution",
                              "Quota allocation"="Quota\nallocation"))

# Read data
data2 <- readxl::read_excel(file.path(datadir, "figX_data.xlsx"), sheet=2) %>% 
  # Format metric
  mutate(scenario=recode_factor(scenario,
                              "Historical access"="Maintain\nhistorical access,\ndespite resource shift",
                              "Trading"=" \n ", 
                              "Contemporary distribution"="Adjust to\ncurrent availability,\ndespite historical reliance"))

# Helper function
################################################################################

# Function to generate ellipse points
generate_ellipse <- function(X, Y, a, b, angle_deg, n = 100) {
  
  # Convert angle from degrees to radians
  angle_rad <- angle_deg * pi / 180
  
  # Generate theta values for parametric equations
  theta <- seq(0, 2 * pi, length.out = n)
  
  # Parametric equations for the ellipse
  x_ellipse <- a * cos(theta)
  y_ellipse <- b * sin(theta)
  
  # Rotation matrix
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), 
                              sin(angle_rad), cos(angle_rad)), 
                            nrow = 2, byrow = TRUE)
  
  # Rotate the ellipse
  rotated_ellipse <- t(rotation_matrix %*% rbind(x_ellipse, y_ellipse))
  
  # Translate the ellipse to the specified centroid
  x_translated <- rotated_ellipse[,1] + X
  y_translated <- rotated_ellipse[,2] + Y
  
  # Combine into a data frame
  ellipse_data <- data.frame(x = x_translated, y = y_translated)
  
  return(ellipse_data)
}


# Plot data
################################################################################

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", scale="large", returnclass = "sf")

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Strip
                   strip.background=element_rect(colour="white", fill="white"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Simulate fishing grounds
fg1 <- generate_ellipse(X=-71, Y=38.5, a=4.5, b=1.5, angle_deg=40) %>% 
  rename(long_dd=x, lat_dd=y) %>% 
  mutate(region="South")
fg2 <- generate_ellipse(X=-67.5, Y=41, a=4.5, b=1.5, angle_deg=20) %>% 
  rename(long_dd=x, lat_dd=y) %>% 
  mutate(region="North")
fg <- rbind(fg1, fg2)
fg_text <- tibble(year=c(1985, 2025),
                  lat_dd=c(36, 43),
                  long_dd=c(-70, -62))

# Map
g1 <- ggplot() +
  facet_wrap(~" \n ") +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Management zone
  geom_hline(yintercept=40, linetype="dashed", lwd=0.5) +
  annotate(geom="text", x=-60, y=40, hjust=1, vjust=c(-0.5, 1.5), 
           label=c("North", "South"), color=c("darkred", "navy"), size=2) +
  # Plot fishing grounds
  geom_polygon(data=fg, aes(x=long_dd, y=lat_dd, fill=region), alpha=0.3) +
  geom_text(data=fg_text, aes(x=long_dd, y=lat_dd, label=year), size=2, color=c("navy", "darkred")) +
  # Legend
  scale_fill_manual(name="", values=c("darkred", "navy")) +
  # Labels 
  labs(tag="A", x=" ", y="") +
  # Extent / axes
  coord_sf(xlim = c(-77, -60), ylim = c(33, 50)) +
  scale_x_continuous(breaks=seq(-75,-60, 5),
                     labels=paste(rev(seq(60, 75, 5)), "°W\n \n ")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title.y = element_blank())
g1

# Plot bars
q_label <- tibble(metric=factor("Quota\nallocation",levels=levels(data1$metric)), 
                  year="2025\n \n ", 
                  percent=50,
                  label="?")
g2 <- ggplot(data1, aes(x=year, y=percent, fill=zone)) +
  facet_wrap(~metric) +
  geom_bar(stat="identity", alpha=0.3, color="black", lwd=0.3) +
  # Plot ?
  geom_text(data=q_label, mapping=aes(x=year, y=percent, label=label), inherit.aes = F) +
  # Labels
  labs(x="Year", y="% of value", tag="B") +
  # Legend
  scale_fill_manual(name="Zone", values=c("darkred", "navy", "white")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot gradient
g3 <- ggplot(data2, aes(x=scenario, y=percent, fill=zone)) +
  facet_wrap(~" \n ") +
  geom_bar(stat="identity", alpha=0.3, color="black", lwd=0.3, width = 0.25) +
  # Labels
  labs(x="Allocation strategy", y="% of quota", tag="C") +
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
       width=6.5, height=3, units="in", dpi=600)





