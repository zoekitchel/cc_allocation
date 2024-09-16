
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps"

# Read data
data_ne <- readRDS("data/fmps/nefmc/NEFMC_species_list.Rds")
data_ma <- readRDS("data/fmps/mafmc/MAFMC_species_list.Rds")
data_sa <- readRDS("data/fmps/safmc/SAFMC_species_list.Rds")
data_gm <- readRDS("data/fmps/gfmc/GFMC_species_list.Rds")
data_c <- readRDS("data/fmps/cfmc/CFMC_species_list.Rds")
data_p <- readRDS("data/fmps/pfmc/data/PFMC_species_list.Rds")
data_np <- readRDS("data/fmps/npfmc/NPFMC_species_list.Rds")
data_wp <- readRDS("data/fmps/wpfmc/WPFMC_species_list.Rds")


# Merge data
################################################################################

# Build data
data <- bind_rows(data_ne, data_ma, data_sa, data_c, data_gm, data_p, data_np, data_wp) %>% 
  # Format type
  mutate(type=stringr::str_to_title(type)) %>% 
  # Format common name
  mutate(comm_name=recode(comm_name,
                          "Atlantic goliath grouper"="Goliath grouper",
                          "Arrowtooth flounder (turbot)"="Arrowtooth flounder",
                          "Boccacio"="Bocaccio",
                          "Coney grouper"="Coney",
                          "Darkblotch rockfish"="Darkblotched rockfish",
                          "Dolphin"="Dolphinfish",
                          "Graysby grouper"="Graysby",
                          "Margate grunt"="Margate",
                          "Scup porgy"="Scup",
                          "Scamp grouper"="Scamp",
                          "Spiny lobster"="Caribbean spiny lobster",
                          "Wenchman snapper"="Wenchman")) %>% 
  # Arrange
  select(-c(criterion, notes, family, "#"))
  

# Inspect
freeR::complete(data)

# Inspect
table(data$council)
table(data$type)
table(data$category)

# FMP key
fmp_key <- data %>% 
  filter(type=="Target") %>% 
  count(council, fmp)

# Species key
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Check names
freeR::check_names(spp_key$species)

# FMP key
write.csv(fmp_key, file=file.path(datadir, "fmp_key.csv"), row.names = F)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "US_FMP_species_list.Rds"))

