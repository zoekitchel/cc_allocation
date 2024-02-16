
# Packages
library(tidyverse)

# Read data
data <- readxl::read_excel("data/fmps/pfmc/data/pfmc_rockfish.xlsx")

# Check
freeR::check_names(data$sci_name)
