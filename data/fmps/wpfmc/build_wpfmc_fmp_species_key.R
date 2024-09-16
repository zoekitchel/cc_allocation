
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/wpfmc"

# Build data
data1 <- readxl::read_excel(file.path(datadir, "WPFMC_RemoteIslandsFMP_species.xlsx"))
data2 <- readxl::read_excel(file.path(datadir, "WPFMC_AmericanSamoaFMP_species.xlsx"))
data3 <- readxl::read_excel(file.path(datadir, "WPFMC_HawaiiFMP_species.xlsx"))
data4 <- readxl::read_excel(file.path(datadir, "WPFMC_PelagicFMP_species.xlsx"))
data5 <- readxl::read_excel(file.path(datadir, "WPFMC_MarianaFMP_species.xlsx"))


# Setup
################################################################################

# Format data
data <- bind_rows(data1, data2, data3, data4, data5) %>% 
  # Rename
  rename(species_orig=species) %>% 
  # Trim
  mutate_all(stringr::str_trim) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>% 
  # Format scientific name
  mutate(species_orig=gsub("\\*", "", species_orig),
         species_orig=gsub("\r\n", " ", species_orig),
         species_orig=gsub("dae ", "dae, ", species_orig),
         species_orig=gsub("Family ", "", species_orig),
         species_orig=recode(species_orig,
                             "Other Bivalves"="Other bivalves",
                             "Hydrozoans and Bryzoans" = "Hydrozoans, Bryzoans",                            
                             "Kyphosus cinerascens Kyphosus biggibus" = "Kyphosus cinerascens, Kyphosus biggibus")) %>% 
  # Fix species
  mutate(species=recode(species_orig,
                        "Antipathes ulex" = "Myriopathes ulex",
                        "Corallium laauense" = "Hemicorallium laauense",                         
                        "Corallium regale" = "Hemicorallium regale",
                        "Epinephelus quernus" = "Hyporthodus quernus",
                        "Fistularia commersoni" = "Fistularia commersonii",
                        "Mulloidichthys pfleugeri" = "Mulloidichthys pfluegeri",
                        "Naso unicornus" = "Naso unicornis",                           
                        "Octopus ornatus" = "Callistoctopus ornatus",
                        "Oxycheilinus diagrammus" = "Oxycheilinus digramma",                   
                        "Parupeneus cyclostomas" = "Parupeneus cyclostomus",                    
                        "Parupeneus multifaciatus" = "Parupeneus multifasciatus",
                        "Pristipomoides seiboldii" = "Pristipomoides sieboldii",
                        "Upeneus arge" = "Upeneus taeniopterus",
                        # Round 2
                        "Alopias superciliousus" = "Alopias superciliosus",
                        "Balistoides rectanulus" = "Rhinecanthus rectangulus",
                        "Cheilinus undulates" = "Cheilinus undulatus",
                        "Etelis quernus" = "Hyporthodus quernus",
                        "Halichoeres margaritacous" = "Halichoeres margaritaceus",
                        "Hologynmosus doliatus" = "Hologymnosus doliatus",
                        "Kyphosus biggibus" = "Kyphosus bigibbus",
                        "Kyphosus vaigienses" = "Kyphosus vaigiensis",
                        "Makaira indica" = "Istiompax indica",
                        "Ommastrephes bartamii" = "Ommastrephes bartramii",
                        "Parupeneus bifasciatus" = "Parupeneus trifasciatus",
                        "Pseudopentaceros wheeleri" = "Pentaceros wheeleri",
                        "Siganus aregenteus" = "Siganus argenteus",                                 
                        "Siganus aregentus" = "Siganus argenteus",
                        "Sufflamen fraenatus" = "Sufflamen fraenatum",
                        "Tetrapturus audax" = "Kajikia audax",
                        "Xyrichtys aneitensis" = "Iniistius aneitensis",                                
                        "Xyrichtys pavo" = "Iniistius pavo")) %>% 
  # Format category
  mutate(category=recode(category,
                         "Coral Reef Ecosystem MUs, Potentially Harvested"="Coral Reef Ecosystem MUs, potentially harvested",
                         "Coral Reef Ecosystem MUs, Currently Harvested"="Coral Reef Ecosystem MUs, currently harvested",
                         "Coral Reef Ecosystem MUs Species, Currently Harvested"="Coral Reef Ecosystem MUs, currently harvested")) %>% 
  # Add type
  mutate(type=ifelse(grepl("potentially", tolower(category)), "Ecosystem Component", "Target")) %>% 
  # Arrange
  select(-c(table, comm_name_local)) %>% 
  select(council, fmp, type, category, comm_name, species_orig, species, everything())
  
# Inspect
freeR::complete(data)

# Inspect
table(data$council)
table(data$fmp)
table(data$category)

# Species
spp_key <- data %>% 
  count(comm_name, species) %>% 
  mutate(nwords=freeR::nwords(species)) %>% 
  filter(nwords>=2)
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$species)

# Check names
freeR::check_names(spp_key$species) # Correct: Hemicorallium laauense, Hemicorallium regale


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(datadir, "WPFMC_species_list.Rds"))
write.csv(data, file=file.path(datadir, "WPFMC_species_list.csv"), row.names=F)


