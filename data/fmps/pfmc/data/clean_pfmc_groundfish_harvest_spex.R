
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

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
  janitor::clean_names("snake") %>% 
  rename(year=spex_year,
         stock=stock_or_complex,
         status=stock_status,
         pstar=probability,
         abc_buffer=abc_buffer_fraction,
         ofl_mt=ofl,
         abc_mt=abc,
         acl_mt=annual_catch_limit, 
         hg_mt=fhg,
         act_mt=act, 
         # Set asides
         tribal_mt=tribal,
         efp_mt=efp,
         research_mt=research,
         open_access_mt=oa,
         buffer_mt=buffer,
         incidental_mt=incidental,
         set_aside_total_mt=set_aside_total) %>% 
  # Remove empty
  select(-c(notes, ofl_document, notes, parent_row, spr)) %>% 
  # Arrange
  select(year, stock, area, acl_code,
         assessment_year, assessment_type, assessment_author,
         status, category, pstar, abc_buffer,
         ofl_mt, abc_mt, acl_mt, hg_mt, act_mt, 
         everything())

# Inspect
str(data)
freeR::complete(data)
