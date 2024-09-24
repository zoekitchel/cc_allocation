# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Read data from paper
data1 <- readxl::read_excel("data/cva/raw/north_pacific/Spencer_etal_2019_tables.xlsx", sheet=1)
data2 <- readxl::read_excel("data/cva/raw/north_pacific/Spencer_etal_2019_tables.xlsx", sheet=2)

# Read direction data from Paul
data3 <- read.csv("data/cva/raw/north_pacific/directional_analysis_bins.csv") %>% 
  select(-Group) %>% 
  rename(comm_name=Stock,
         dir_effect=Direction) %>% 
  mutate(comm_name=recode(comm_name,
                          "Togiak herring"="Pacific herring"))

# Read distribution data from Paul
data4 <- read.csv("data/cva/raw/north_pacific/distributional_analysis_bins.csv") %>% 
  select(-Group) %>% 
  rename(comm_name=Stock,
         dist_change=Dist_change)%>% 
  mutate(comm_name=recode(comm_name,
                          "Togiak herring"="Pacific herring"),
         dist_change=recode(dist_change,
                          "Very High"="Very high"))

# Build data
################################################################################

# Merge data
data <- data2 %>% 
  # Add species info
  left_join(data1, by="comm_name") %>% 
  select(-vulnerability_color) %>% 
  # Add direction
  left_join(data3, by=c("comm_name")) %>% 
  # Add distribution
  left_join(data4, by=c("comm_name"))

# Export data
saveRDS(data, file="data/cva/raw/north_pacific/Spencer_etal_2019_cva.Rds")
