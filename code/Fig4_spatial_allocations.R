
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


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify columns
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         subsector_yn:subsector_list_rec) %>% 
  # Reduce to stocks with subsector allocations
  filter(subsector_yn=="yes") %>% 
  # Recode council
  mutate(council_lead=recode(council_lead, 
                            "NPFMC"="North Pacific",
                            "NEFMC"="New England",
                            "SAFMC"="South Atlantic",
                            "MAFMC"="Mid-Atlantic",
                            "Atlantic HMS"="Atlantic HMS",
                            "GMFMC"="Gulf of Mexico"))

# 
range(data$subsector_n)

table(data$subsector_type)

# Build stats
stats <- data %>% 
  group_by(council_lead, subsector_type) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Factor 
  mutate(council_lead=factor(council_lead, levels=c("North Pacific", "New England", "South Atlantic", 
                                                    "Mid-Atlantic", "Atlantic HMS", "Gulf of Mexico"))) %>% 
  # Order type by frequency
  mutate(subsector_type=stringr::str_to_sentence(subsector_type),
         subsector_type=factor(subsector_type, levels=c("Organizations", "Permit types", "Gears", 
                                                        "End uses", "Target species", "Vessel tiers", "Recreational")))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.title=element_blank(),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot subsector type
g1 <- ggplot(stats, aes(y=council_lead, x=n, fill=subsector_type)) +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="Number of stocks", y="", tag="A") +
  # Legend
  scale_fill_discrete(name="Subsector type\n(in decreasing frequency)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.7, 0.7),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot number of subsectors
g2 <- ggplot(data, aes(y=factor(council_lead, levels=levels(stats$council_lead)), x=subsector_n)) +
  geom_boxplot(fill="grey80") +
  # Labels
  labs(x="Number of subsectors", y="", tag="B") +
  lims(x=c(0, NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_subsector_allocations.png"), 
       width=6.5, height=3, units="in", dpi=600)


