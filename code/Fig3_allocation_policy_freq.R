
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
data <- readRDS(file.path(datadir, "quota_allocations_database.Rds")) %>% 
  filter(council_lead!="IPHC")

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

# Overall
################################################################################

# Overall
stats1 <- data %>% 
  # Simplify
  select(stock, allocation_yn_use, spatial_yn, sector_yn, subsector_yn, season_yn, shares_yn) %>% 
  # Gather
  gather(key="alloc_type", value="alloc_yn", 2:ncol(.)) %>% 
  # Summarize
  group_by(alloc_type) %>% 
  summarize(n=sum(alloc_yn=="yes"),
            n_tot=n(),
            p=n/n_tot) %>% 
  # Format type
  mutate(alloc_type=recode(alloc_type,
                           "allocation_yn_use"="Any", 
                           "spatial_yn"="Spatial", 
                           "sector_yn"="Sector", 
                           "subsector_yn"="Subsector", 
                           "season_yn"="Seasonal", 
                           "shares_yn"="Catch shares"))  %>% 
  # Arrange
  arrange(desc(p)) %>% 
  # Add label
  mutate(label=paste0(round(p*100,1), "% (", n, " stocks)"))


# Plot overall
g1 <- ggplot(stats1, aes(y=reorder(alloc_type, desc(p)), x=p)) +
  geom_bar(stat="identity") +
  # Label
  geom_text(mapping=aes(label=label), hjust=-0.1, size=2, color="grey60") +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent, lim=c(0,0.55)) +
  # Theme
  theme_bw() + base_theme
g1


# By council
################################################################################


# Overall
stats2 <- data %>% 
  # Simplify
  select(council_lead, stock, allocation_yn_use, spatial_yn, sector_yn, subsector_yn, season_yn, shares_yn) %>% 
  # Gather
  gather(key="alloc_type", value="alloc_yn", 3:ncol(.)) %>% 
  # Summarize
  group_by(council_lead, alloc_type) %>% 
  summarize(n=sum(alloc_yn=="yes"),
            n_tot=n(),
            p=n/n_tot) %>% 
  # Format type
  mutate(alloc_type=recode(alloc_type,
                           "allocation_yn_use"="Any", 
                           "spatial_yn"="Spatial", 
                           "sector_yn"="Sector", 
                           "subsector_yn"="Subsector", 
                           "season_yn"="Seasonal", 
                           "shares_yn"="Catch shares")) %>% 
  # Format council
  mutate(council_lead=recode(council_lead,
                             "WPFMC"="Western Pacific",
                             "SAFMC"="South Atlantic",
                             "PFMC"="Pacific",
                             "NPFMC"="North Pacific",
                             "MAFMC"="Mid-Atlantic",
                             "SAFMC"="South Atlantic",
                             "GMFMC"="Gulf of Mexico",
                             "NEFMC"="New England",
                             "CFMC"="Caribbean"))

# Council order
council_order <- stats2 %>% 
  filter(alloc_type=="Any") %>% 
  arrange(desc(p))

# Order data
stats2_ordered <- stats2 %>% 
  # Order types
  mutate(alloc_type=factor(alloc_type, levels=stats1$alloc_type)) %>% 
  # Order councils
  mutate(council_lead=factor(council_lead, levels=council_order$council_lead))

# Plot data
g2 <- ggplot(stats2_ordered, aes(y=council_lead, x=p)) +
  facet_wrap(~alloc_type, scales="free_x") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + base_theme
g2


# Merge data
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.3, 0.7))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_policy_frequency.png"), 
       width=6.5, height=5.5, units="in", dpi=600)

