
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("data/fmps/pfmc/data/GMT015-final specifications-2024.xlsx",
                                skip=6, na="-")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Clean name
  janitor::clean_names("snake")

# Inspect
str(data)
