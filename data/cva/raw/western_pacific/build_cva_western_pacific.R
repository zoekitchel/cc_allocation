# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1 <- readxl::read_excel("data/cva/raw/western_pacific/Giddens_etal_2022_tables.xlsx", sheet=1)
data2 <- readxl::read_excel("data/cva/raw/western_pacific/Giddens_etal_2022_tables.xlsx", sheet=2)


# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color) %>% 
  # Select
  select(functional_group, comm_name, species, exposure, sensitivity, vulnerability)

# Inspect
freeR::which_duplicated(data$species)
freeR::which_duplicated(data$comm_name)
freeR::check_names(data$species)

# Export data
saveRDS(data, file="data/cva/raw/western_pacific/Giddens_etal_2022_cva.Rds")
