# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1 <- readxl::read_excel("data/cva/raw/pacific/McClure_etal_2023_tables.xlsx", sheet=1) %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_squish(),
         comm_name=recode(comm_name,
                          "Bocaccio rockfish - puget sound"="Bocaccio rockfish - Puget Sound",
                          "Canary rockfish - puget sound"="Canary rockfish - Puget Sound",
                          "Yelloweye rockfish - puget sound"="Yelloweye rockfish - Puget Sound",
                          "Eulachon - southern"="Eulachon - Southern",
                          "North pacific albacore"="North Pacific albacore"))

# Vulnerability
data2 <- readxl::read_excel("data/cva/raw/pacific/McClure_etal_2023_tables.xlsx", sheet=2)

# Direction/distribution
data3 <- readxl::read_excel("data/cva/raw/pacific/McClure_etal_2023_tables.xlsx", sheet=3) %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_squish(),
         comm_name=recode(comm_name,
                          "Bocaccio rockfish - puget sound"="Bocaccio rockfish - Puget Sound",
                          "Canary rockfish - puget sound"="Canary rockfish - Puget Sound",
                          "Eulachon - southern"="Eulachon - Southern",
                          "North pacific albacore"="North Pacific albacore"))


# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color) %>% 
  # Add distribution/direction
  left_join(data3 %>% select(comm_name, dir_effect, dist_change), by="comm_name") %>% 
  # Format 
  mutate(dist_change=stringr::str_to_sentence(dist_change)) %>% 
  # Add missing taxa info
  mutate(functional_group=case_when(comm_name=="Bocaccio rockfish - Puget Sound" ~ "Rockfish", 
                                    comm_name=="Canary rockfish - Puget Sound" ~ "Rockfish", 
                                    T ~ functional_group),
         species=case_when(comm_name=="Bocaccio rockfish - Puget Sound" ~ "Sebastes paucispinis", 
                           comm_name=="Canary rockfish - Puget Sound" ~ "Sebastes pinniger", 
                           T ~ species))

# Inspect
freeR::complete(data)

# Export data
saveRDS(data, file="data/cva/raw/pacific/McClure_etal_2023_cva.Rds")
