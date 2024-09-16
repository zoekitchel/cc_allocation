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

# Read FMP species key
fmp_spp_key <- readRDS("data/fmps/US_FMP_species_list.Rds")

# Read FMP short key
fmp_short_key <- readxl::read_excel("data/fmps/fmp_key.xlsx")
freeR::which_duplicated(fmp_short_key$fmp_short)

# Build data
################################################################################

# Inspect
table(cva_orig$region)
table(fmp_spp_key$council)

# Process CVA
check <- cva_orig %>% 
  count(region, species)
cva <- cva_orig %>% 
  # Remove Pacific salmon
  filter(region!="Pacific salmon") %>% 
  # Simplify
  select(region, species, vulnerability) %>% 
  arrange(region, species, desc(vulnerability)) %>% 
  # Select the most severe designation for the species
  group_by(region, species) %>% 
  slice(1) %>% 
  ungroup() 

# Build data
data <- fmp_spp_key %>% 
  # Reduce to target species
  filter(type=="Target") %>% 
  # Remove councils without CVA
  filter(!council %in% c( "CFMC")) %>% 
  # Mark CVA region
  mutate(cva_region=recode(council,
                           "GFMC"="Gulf of Mexico",
                           "MAFMC"="Northeast",
                           "NEFMC"="Northeast",
                           "NPFMC"="North Pacific",
                           "PFMC"="Pacific",
                           "SAFMC"="South Atlantic",
                           "WPFMC"="Western Pacific")) %>% 
  # Add CVA
  left_join(cva, by=c("cva_region"="region", "species"="species")) %>% 
  # Fill missing vulnerability and recode
  mutate(vulnerability=as.character(vulnerability),
         vulnerability=ifelse(is.na(vulnerability), "Unknown", vulnerability),
         vulnerability=factor(vulnerability, 
                              levels=c("Unknown", "Low", "Moderate", "High", "Very high")))

freeR::check_names(data$species)

# Summarize
data_sum <- data %>% 
  # Count 
  group_by(council, fmp, vulnerability) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Prop
  group_by(council, fmp) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Add FMP short
  left_join(fmp_short_key %>% select("fmp", "fmp_short"), by="fmp") %>% 
  # Recode council
  mutate(council=recode_factor(council,
                               "NEFMC"="New\nEngland",
                               "MAFMC"="Mid-Atlantic",
                               "SAFMC"="South\nAtlantic", 
                               "GFMC"="Gulf of\nMexico",
                               "PFMC"="Pacific",
                               "NPFMC"="North\nPacific",
                               "WPFMC"="Western\nPacific")) %>% 
  # Add vulnerability score
  mutate(vulnerability_score=recode(vulnerability,
                                    "Low"="0",
                                    "Moderate"="1", 
                                    "High"="2",
                                    "Very high"="3") %>% as.numeric(.))

# Stats
stats <- data_sum %>% 
  filter(!is.na(vulnerability_score)) %>% 
  group_by(council, fmp_short) %>% 
  summarize(vulnerability_score=mean(vulnerability_score)) %>% 
  ungroup() %>% 
  arrange(council, desc(vulnerability_score))

# Order data
data_sum_ordered <- data_sum %>% 
  mutate(fmp_short=factor(fmp_short, levels=stats$fmp_short))

# Labels
labs <- data_sum_ordered %>% 
  group_by(council, fmp_short) %>% 
  summarize(n=sum(n)) %>% 
  ungroup() %>% 
  mutate(fmp_short=factor(fmp_short, levels=stats$fmp_short))


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
g <- ggplot(data_sum_ordered, aes(y=fmp_short, x=prop, fill=vulnerability)) +
  facet_grid(council~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Sample size
  geom_text(data=labs, aes(x=1.02, y=fmp_short, label=paste0("n=", n)), hjust=0, inherit.aes = F, size=2.2, color="grey40") +
  # Labels
  labs(x="Proportion of stocks", y="") +
  scale_x_continuous(labels=scales::percent_format(), lim=c(0, 1.1), breaks=seq(0,1,0.25)) +
  # Legend
  scale_fill_manual(name="Vulnerability",
                    values=c("grey90", RColorBrewer::brewer.pal(4, "Spectral") %>% rev())) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_cva_analysis.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



