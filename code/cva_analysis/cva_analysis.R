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

# Read database
data_orig <- readRDS("data/database/processed/quota_allocations_database.Rds")

# Read FMP short key
fmp_key <- readxl::read_excel("data/database/raw/fmp_short_key.xlsx")
freeR::which_duplicated(fmp_key$fmp_short)


# Build data
################################################################################

# Inspect
table(cva_orig$region)
table(data_orig$council_lead)

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
data <- data_orig %>% 
  # Remove councils without CVA
  filter(!council_lead %in% c("Atlantic HMS", "Pacific HMS", "CFMC", "IPHC")) %>% 
  # Species in each FMP
  select(council_lead, fmp, comm_name, Species1:Species28) %>% 
  unique() %>% 
  gather(key="species_num", value="species", 4:ncol(.)) %>% 
  select(-species_num) %>% 
  filter(!is.na(species)) %>% 
  unique() %>% 
  # Recode some species
  mutate(species=recode(species,
                        "Bathyrara trachura" = "Bathyraja trachura",
                        "Beringraja binoculata" = "Raja binoculata",
                        "Caliraja rhina" = "Beringraja rhina",
                        "Clidodoerma asperrimum" = "Clidoderma asperrimum",
                        "Epinephelus flavolimbatus" = "Hyporthodus flavolimbatus",
                        "Epinephelus mystacinus" = "Hyporthodus mystacinus",
                        "Epinephelus nigritus"  = "Hyporthodus nigritus",
                        "Epinephelus niveatus" = "Hyporthodus niveatus",
                        "Haemulon margaritaceum" = "",
                        "Haemulon plumieri"  = "Haemulon plumierii",
                        # "Hymenodora rosa"  = "",
                        "Lepidopsetta billineta" = "Lepidopsetta bilineata",
                        # "Scadella vimbo"  = "",
                        # "Scorpaoides xanthodes"  = "",
                        # "Sebastes diaconus" = "",
                        "Macrozoarces americanus"  = "Zoarces americanus")) %>% 
  # Mark CVA region
  mutate(cva_region=recode(council_lead,
                           "GMFMC"="Gulf of Mexico",
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
  group_by(council_lead, fmp, vulnerability) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Prop
  group_by(council_lead, fmp) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Add FMP short
  left_join(fmp_key %>% select("fmp_long", "fmp_short"), by=c("fmp"="fmp_long")) %>% 
  # Recode council
  mutate(council_lead=recode_factor(council_lead,
                                   "NEFMC"="Northeast",
                                   "MAFMC"="Mid-Atlantic",
                                   "SAFMC"="South\nAtlantic", 
                                   "GMFMC"="Gulf of\nMexico",
                                   "PFMC"="Pacific",
                                   "NPFMC"="North\nPacific",
                                   "WPFMC"="Western\nPacific")) %>% 
  # Add vulnerability score
  mutate(vulnerability_score=recode(vulnerability,
                                    "Low"=0,
                                    "Moderate"=1, 
                                    "High"=2,
                                    "Very high"=3) %>% as.numeric(.)) %>% 
  # Remove uninteresting FMPs 
  filter(!fmp_short %in% c("Sargassum", "Corals", "Gulf Corals"))

# Stats
stats <- data_sum %>% 
  group_by(council_lead, fmp_short) %>% 
  summarize(vulnerability_score=mean(vulnerability_score, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(council_lead, desc(vulnerability_score))

# Order data
data_sum_ordered <- data_sum %>% 
  mutate(fmp_short=factor(fmp_short, levels=stats$fmp_short))

# Labels
labs <- data_sum_ordered %>% 
  group_by(council_lead, fmp_short) %>% 
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
  facet_grid(council_lead~., space="free_y", scales="free_y") +
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



