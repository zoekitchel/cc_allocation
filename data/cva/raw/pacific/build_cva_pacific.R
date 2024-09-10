# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
data1 <- readxl::read_excel("data/cva/raw/pacific/McClure_etal_2023_tables.xlsx", sheet=1) %>% 
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "North pacific albacore"="North Pacific albacore"))
data2 <- readxl::read_excel("data/cva/raw/pacific/McClure_etal_2023_tables.xlsx", sheet=2)


# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color) %>% 
  # Add missing taxa stuff
  mutate(functional_group=case_when(comm_name=="Bocaccio rockfish - Puget Sound" ~ "Rockfish", 
                                    comm_name=="Canary rockfish - Puget Sound" ~ "Rockfish", 
                                    T ~ functional_group),
         species=case_when(comm_name=="Bocaccio rockfish - Puget Sound" ~ "Sebastes paucispinis", 
                           comm_name=="Canary rockfish - Puget Sound" ~ "Sebastes pinniger", 
                           T ~ species))

# Export data
saveRDS(data, file="data/cva/raw/pacific/McClure_etal_2023_cva.Rds")
