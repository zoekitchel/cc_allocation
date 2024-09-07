
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
data_orig <- readxl::read_excel(file.path(indir, "Allocations database.xlsx"), sheet="allocations")


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
  # Split stock into common name and area
  separate(stock, into=c("comm_name", "area"), sep=" - ", remove=F) %>% 
  # Format common name
  mutate(comm_name=stringr::str_trim(comm_name) %>% stringr::str_to_sentence(.)) %>% 
  # Format common name
  mutate(area=stringr::str_trim(area)) %>% 
  # Add country columns
  mutate(country_yn=ifelse(!is.na(country_list), "yes", "no"),
         country_n=count_items(country_list)) %>%  
  # Add state columns
  mutate(state_yn=ifelse(!is.na(state_list), "yes", "no"),
         state_n=count_items(state_list)) %>%  
  # Add area columns
  mutate(area_yn=ifelse(!is.na(area_list), "yes", "no"),
         area_n=count_items(area_list)) %>%  
  # Add spatial yes/no
  mutate(spatial_yn=ifelse(country_yn=="yes" | state_yn=="yes" | area_yn=="yes", "yes", "no"),
         spatial_type=paste(country_yn, state_yn, area_yn, sep="-"),
         spatial_type=recode(spatial_type,
                             "no-no-no"="none", 
                             "no-no-yes"="area", 
                             "no-yes-no"="state", 
                             "yes-no-no"="country")) %>% 
  # Add sector columns
  mutate(sector_yn=ifelse(!is.na(sector_list), "yes", "no"),
         sector_n=count_items(sector_list)) %>%  
  # Add subsector columns
  mutate(subsector_yn=ifelse(!is.na(subsector_list_rec) | !is.na(subsector_list_comm), "yes", "no"),
         subsector_rec_yn=ifelse(!is.na(subsector_list_rec), "yes", "no"),
         subsector_comm_yn=ifelse(!is.na(subsector_list_comm), "yes", "no"),
         subsector_rec_n=count_items(subsector_list_rec),
         subsector_comm_n=count_items(subsector_list_comm),
         subsector_n=subsector_rec_n + subsector_comm_n) %>% 
  # Add season columns
  mutate(season_yn=ifelse(!is.na(season_list), "yes", "no"),
         season_n=count_items(season_list)) %>%  
  # Format shares columns
  mutate(shares_yn=ifelse(shares_yn=="y" & !is.na(shares_yn), "yes", "no")) %>% 
  # Format allocation
  mutate(allocation_yn_use=ifelse(spatial_yn=="yes" | sector_yn=="yes" | subsector_yn=="yes" | season_yn=="yes" | shares_yn=="yes", "yes", "no")) %>% 
  # Arrange
  select(council:allocation_yn, allocation_yn_use,
         spatial_yn, spatial_type,
         country_yn, country_n, country_list, country_notes,
         state_yn, state_n, state_list, state_yrs, state_notes,
         area_yn, area_n, area_list, 
         sector_yn, sector_type, sector_n, sector_list, sector_yrs, sector_notes,
         subsector_yn, subsector_n, subsector_yrs, subsector_notes,
         subsector_comm_yn, subsector_comm_n, subsector_list_comm,
         subsector_rec_yn, subsector_rec_n, subsector_list_rec,
         season_yn, season_n, season_list, season_yrs, season_notes,
         everything()) %>% 
  # Remove useless
  select(-c(completed_by, QC_by))

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




