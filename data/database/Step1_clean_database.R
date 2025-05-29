
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
data_orig <- readxl::read_excel(file.path(indir, "Allocations database.xlsx"), sheet="Data")


# Helper functions
################################################################################

# Count items in list
count_items <- function(x){
  n_vec <- purrr::map_int(x, function(x){
    if(is.na(x)){
      n <- 0
    }else{
      n <- str_count(x, ',')+1
    }
  })
  n_vec
}


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Format common name
  mutate(comm_name=stringr::str_trim(comm_name) %>% stringr::str_to_sentence(.)) %>% 
  # Clean allocation yes/nos
  mutate(spatial_yn=ifelse(is.na(spatial_yn), "no", spatial_yn),
         country_yn=ifelse(is.na(country_yn), "no", country_yn),
         state_yn=ifelse(is.na(state_yn), "no", state_yn),
         area_yn=ifelse(is.na(area_yn), "no", area_yn),
         sector_yn=ifelse(is.na(sector_yn), "no", sector_yn),
         subsector_yn=ifelse(is.na(subsector_yn), "no", subsector_yn),
         season_yn=ifelse(is.na(season_yn), "no", season_yn),
         shares_yn=ifelse(is.na(shares_yn), "no", shares_yn)) %>% 
  # Add spatial type
  mutate(spatial_type=paste(country_yn, state_yn, area_yn, sep="-"),
         spatial_type=recode(spatial_type,
                             "no-no-no"="none", 
                             "no-no-yes"="area", 
                             "no-yes-no"="state", 
                             "yes-no-no"="country",
                             "yes-no-yes"="country-area")) %>% 
  # Format allocation
  mutate(allocation_yn_use=ifelse(spatial_yn=="yes" | sector_yn=="yes" | subsector_yn=="yes" | season_yn=="yes" | shares_yn=="yes", "yes", "no")) %>% 
  # Arrange
  select(council:allocation_yn, allocation_yn_use,
         spatial_yn, spatial_type,
         country_yn, country_n, country_list, country_notes,
         state_yn, state_n, state_list, state_yrs, state_notes,
         area_yn, area_n, area_list, 
         sector_yn, sector_type, sector_setaside_list, sector_n, sector_list, sector_yrs, sector_notes,
         subsector_yn, subsector_type, subsector_n, subsector_yrs, subsector_notes,
         subsector_list_comm, subsector_list_rec,
         season_yn, season_n, season_list, season_yrs, season_notes,
         everything()) 

# Inspect
freeR::complete(data)

# Spatial type
table(data$spatial_type)
table(data$shares_yn)

# Common names
sort(unique(data$comm_name))

# Inspect more
unique(data$country_list)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "quota_allocations_database.Rds"))




