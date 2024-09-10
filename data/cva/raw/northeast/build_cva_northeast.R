# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1 <- readxl::read_excel("data/cva/raw/northeast/Hare_etal_2016_tables.xlsx", sheet=1) %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name))
data2 <- readxl::read_excel("data/cva/raw/northeast/Hare_etal_2016_tables.xlsx", sheet=2)


# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color)

# Export data
saveRDS(data, file="data/cva/raw/northeast/Hare_etal_2016_cva.Rds")
