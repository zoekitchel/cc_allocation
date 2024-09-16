
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/gfmc"

# Setup
################################################################################

# Build data
data_orig <- readxl::read_excel(file.path(datadir, "GFMC_species.xlsx"))

# Format data
data <- data_orig %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Correct names
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  mutate(species=species_orig)

# Check names
freeR::check_names(data$species)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "GFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "GFMC_species_list.csv"), row.names=F)


