
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/database/raw"
outdir <- "data/database/processed"

# Read data
data_orig <- readRDS(file.path(outdir, "quota_allocations_database.Rds"))

# Read FMP short key
fmp_key <- readxl::read_excel(file.path(indir, "fmp_short_key.xlsx"))


# Setup
################################################################################

# Build key
data <- data_orig %>% 
  # Remove IPHC
  filter(council!="IPHC") %>% 
  # Simplify
  select(council, council_lead, fmp, comm_name, species) %>% 
  unique() %>% 
  # Add FMP short
  left_join(fmp_key %>% select(-council_lead), by=c("fmp"="fmp_long"))
