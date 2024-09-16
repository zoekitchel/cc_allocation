
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/cfmc"

# Build data
data1 <- readxl::read_excel(file.path(datadir, "CFMC_PuertoRicoFMP_species.xlsx"))
data2 <- readxl::read_excel(file.path(datadir, "CFMC_StCroixFMP_species.xlsx"))
data3 <- readxl::read_excel(file.path(datadir, "CFMC_StThomasStJohnFMP_species.xlsx"))


# Setup
################################################################################

# Format data
data <- bind_rows(data1, data2, data3) %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Format category
  mutate(category=stringr::str_to_sentence(category),
         category=recode(category,
                         "All corals"="Corals",
                         "Triggerfish"="Triggerfishes")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=gsub("\\*", "", comm_name),
         comm_name=recode(comm_name,
                          "Red hind grouper"="Red hind")) %>% 
  # Format species
  mutate(species_orig=gsub("\\*", "", species_orig)) %>% 
  # Correct names
  mutate(species=species_orig,
         species=recode(species, 
                        "Cephalopholis cruentatus" = "Cephalopholis cruentata", 
                        # "Hypanus americanus" = "", # this is correct 
                        "Lobatus (Strombus) gigas" = "Lobatus gigas"))
  
# Inspect
freeR::complete(data)

# Inspect
table(data$council)
table(data$fmp)
table(data$category)
table(data$criterion)

# Species
spp_key <- data %>% 
  count(comm_name, species)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Check names
freeR::check_names(data$species)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "CFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "CFMC_species_list.csv"), row.names=F)


