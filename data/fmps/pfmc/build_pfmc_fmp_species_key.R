
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/pfmc/data"

# Setup
################################################################################

# Build data
data_orig <- purrr::map_df(1:5, function(x){
  df <- readxl::read_excel(file.path(datadir, "PFMC_species.xlsx"), sheet=x)
})

# Format data
data <- data_orig %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Correct names
  mutate(species=species_orig) %>% 
  mutate(species=recode(species_orig,
                        "Dasyetis violacea" = "Pteroplatytrygon violacea",
                        "Etrumeus teres" = "Etrumeus sadina",
                        "Galeorhinus zyopterus" = "Galeorhinus galeus",
                        "Scorpaena gutatta"  = "Scorpaena guttata"))

# Check name
freeR::check_names(data$species) # THESE ARE CORRECT: Sebastes crocotulus, Sebastes diaconus

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "PFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "PFMC_species_list.csv"), row.names=F)




