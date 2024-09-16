
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/safmc"

# Setup
################################################################################

# Build data
data_orig <- readxl::read_excel(file.path(datadir, "SAFMC_species.xlsx"))

# Format data
data <- data_orig %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Correct names
  mutate(species=species_orig)

# Check names
freeR::check_names(data$species)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "SAFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "SAFMC_species_list.csv"), row.names=F)



