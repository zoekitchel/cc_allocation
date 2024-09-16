
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/nefmc"

# Setup
################################################################################

# Build data
data_orig <- readxl::read_excel(file.path(datadir, "NEFMC_species.xlsx"))

# Format data
data <- data_orig %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Correct names
  mutate(species=recode(species_orig,
                        "Clupea harengus harengus"="Clupea harengus",
                        "Dipturis laevis"="Dipturus laevis",        
                        "Macrozoarces americanus"="Zoarces americanus",
                        "Pleuronectes americanus"="Pseudopleuronectes americanus",
                        "Pleuronectes ferruginea"="Limanda ferruginea"))

# Check names
freeR::check_names(data$species)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "NEFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "NEFMC_species_list.csv"), row.names=F)


