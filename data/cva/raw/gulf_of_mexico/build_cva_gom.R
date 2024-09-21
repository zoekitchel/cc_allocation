# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1 <- readxl::read_excel("data/cva/raw/gulf_of_mexico/Quinlan_etal_2023_tables.xlsx", sheet=1)
data2 <- readxl::read_excel("data/cva/raw/gulf_of_mexico/Quinlan_etal_2023_tables.xlsx", sheet=2)
data3 <- readxl::read_excel("data/cva/raw/gulf_of_mexico/Quinlan_etal_2023_tables.xlsx", sheet=3)
data4 <- readxl::read_excel("data/cva/raw/gulf_of_mexico/Quinlan_etal_2023_tables.xlsx", sheet=4)


# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color) %>% 
  # Add distribution
  left_join(data3, by="comm_name") %>% 
  # Add directional effects
  left_join(data4, by="comm_name")

# Inspect
freeR::complete(data)

# Export data
saveRDS(data, file="data/cva/raw/gulf_of_mexico/Quinlan_etal_2023_cva.Rds")
