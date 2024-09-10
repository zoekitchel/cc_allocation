# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/cva/raw"
outdir <- "data/cva/processed"
plotdir <- "figures"

# Read data
data_ne <- readRDS(file.path(indir, "northeast/Hare_etal_2016_cva.Rds")) %>% mutate(region="Northeast")
data_sa <- readRDS(file.path(indir, "gulf_of_mexico/Quinlan_etal_2023_cva.Rds")) %>% mutate(region="Gulf of Mexico")
data_gom <- readRDS(file.path(indir, "south_atlantic/Burton_etal_2023_cva.Rds")) %>% mutate(region="South Atlantic")
data_np <- readRDS(file.path(indir, "north_pacific/Spencer_etal_2019_cva.Rds")) %>% mutate(region="North Pacific")
data_pac <- readRDS(file.path(indir, "pacific/McClure_etal_2023_cva.Rds")) %>% mutate(region="Pacific")
data_wp <- readRDS(file.path(indir, "western_pacific/Giddens_etal_2022_cva.Rds")) %>% mutate(region="Western Pacific")

# Things to do:
# 1. Add Pacific salmon
# 2. Harmonize functional groups
# 3. Distribution shift, directional shift, sub-scores


# Build data
################################################################################

# Merge data
data <- bind_rows(data_ne, data_sa, data_gom, data_np, data_pac, data_wp) %>% 
  # Arrange
  select(region, functional_group, comm_name, species, 
         vulnerability, sensitivity, exposure, everything()) %>% 
  # Format scores
  mutate(exposure=factor(exposure, levels=c("Low", "Moderate", "High", "Very high")),
         sensitivity=factor(sensitivity, levels=c("Low", "Moderate", "High", "Very high")),
         vulnerability=factor(vulnerability, levels=c("Low", "Moderate", "High", "Very high"))) %>% 
  # Format functional group
  mutate(functional_group=stringr::str_to_sentence(functional_group),
         functional_group=recode(functional_group,
                                 "Invertebrate"="Invertebrates",
                                 "Coastal"="Coastal pelagics",
                                 "Coastal pelagic"="Coastal pelagics",
                                 "Coastal pelagic fish"="Coastal pelagics",
                                 "Diadromous"="Diadromous fish",
                                 "Elasmobranch"="Elasmobranchs",
                                 "Groundish"="Groundfish",
                                 "Pelagic"="Pelagic fish",
                                 "Pelagics"="Pelagic fish")) %>% 
  # Format species
  mutate(species=recode(species,
                        "Reinhardtius stomias"="Atheresthes stomias",
                        "Anchoa hepsetus & A. mitchilli"="Anchoa hepsetus / Anchoa mitchilli",
                        "Ammodytes americanus & Ammodytes dubius"="Ammodytes americanus / Ammodytes dubius",
                        "Acipenser oxyrhynchus"="Acipenser oxyrinchus",
                        "Penaeus setiferus"="Litopenaeus setiferus",
                        "Micropogonias undulates"="Micropogonias undulatus",
                        "Penaeus aztecus"="Farfantepenaeus aztecus",
                        "Farfantapenaeus aztecus"="Farfantepenaeus aztecus",
                        "Penaeus robustus"="Farfantepenaeus duorarum",
                        "Farfantapenaeus duoarum"="Farfantepenaeus duorarum",
                        "Tetrapturus audax"="Kajikia audax",
                        "Anguilla oceanica"="Conger oceanicus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Dasyatis Sabina"="Dasyatis sabina",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus nigritus"="Hyporthodus nigritus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Euthynneus alletteratus"="Euthynnus alletteratus",
                        "Haelichoeres bivittatum"="Halichoeres bivittatus",
                        "Haemulon plumieri"="Haemulon plumierii",
                        "Melanogrammus aegleinus"="Melanogrammus aeglefinus",
                        "Sebastes dalli"="Sebastes dallii"),
         species=ifelse(comm_name=="Gulf sturgeon", "Acipenser oxyrhynchus desotoi", species)) %>% 
  # Format name
  mutate(comm_name=recode(comm_name, 
                          "Scamp"="Scamp grouper",
                          "Tilefish"="Golden tilefish",
                          "Dolphin"="Dolphinfish",
                          "Sand tiger"="Sand tiger shark",
                          "Eastern Bering Sea Pacific cod"="Pacific cod",
                          "Mahimahi"="Dolphinfish",
                          "Mullet"="Striped mullet",
                          "Scalloped hammerhead"="Scalloped hammerhead shark"),
         comm_name=case_when(species=="Anchoa hepsetus / Anchoa mitchilli" ~ "Broad-striped anchovy / Bay anchovy",
                             species=="Squalus suckleyi" ~ "Pacific spiny dogfish",
                             species=="Panulirus penicillatus" ~ "Pronghorn spiny lobster",
                             T ~ comm_name))

# Inspect
freeR::complete(data)
table(data$region)
table(data$functional_group) # could improve

# Scores
table(data$vulnerability)
table(data$exposure)
table(data$sensitivity)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

freeR::check_names(spp_key$species) # Nicholsina usta, Palola viridis are both correct


# Plot data
################################################################################

# Totals
stats <- data %>% 
  group_by(region, functional_group, vulnerability) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  group_by(region, functional_group) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  mutate(region=factor(region,
                       levels=c("Northeast", "South Atlantic", "Gulf of Mexico",
                                "Pacific", "North Pacific", "Western Pacific")))

# Plot
ggplot(stats, aes(y=functional_group, x=prop, fill=vulnerability)) +
  facet_grid(region~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Proportion of species", y="") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Vulnerability",
                    values=RColorBrewer::brewer.pal(4, "YlOrRd")) +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "cva_data.Rds"))

