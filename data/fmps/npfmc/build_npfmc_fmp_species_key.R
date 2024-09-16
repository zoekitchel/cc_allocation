
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/npfmc"

# Setup
################################################################################

# Build data
data_orig <- purrr::map_df(1:6, function(x){
  df <- readxl::read_excel(file.path(datadir, "NPFMC_species.xlsx"), sheet=x)
})

# Format data
data <- data_orig %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Correct names
  mutate(species=recode(species_orig,
                        # "Gadus chalcogrammus" = "", 
                        # "Microstomus bathybius" = "", 
                        "Sebastes brevispinus" = "Sebastes brevispinis",  
                        "Sebastes paucispinus" = "Sebastes paucispinis"))

# Check name
freeR::check_names(data$species)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "NPFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "NPFMC_species_list.csv"), row.names=F)

