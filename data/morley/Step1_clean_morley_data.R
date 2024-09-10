# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
indir <- "data/morley/raw"
outdir <- "data/morley/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Morley_etal_2018_appendix1.xlsx"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(shift_km=shift,
         shift_km_sd=sd_shift,
         uncertainty=uncert,
         r2_presence=dev_pa,
         r2_biomass=dev_biom,
         habitat_perc_delta=percent_hab, 
         habitat_perc_delta_sd=sd_hab) %>% 
  # Remove headers
  filter(rcp!="RCP") %>% 
  # Fill values
  fill(species, .direction = "down") %>% 
  fill(r2_presence, .direction = "down") %>% 
  fill(r2_biomass, .direction = "down") %>% 
  fill(region, .direction = "down") %>% 
  # Convert to numeric
  mutate_at(vars(r2_presence, r2_biomass, shift_km, shift_km_sd, 
                 habitat_perc_delta, habitat_perc_delta_sd), as.numeric) %>% 
  # Format RCP
  mutate(rcp=recode(rcp,
                    "26"="RCP 2.6",
                    "85"="RCP 8.5")) %>% 
  # Format region
  mutate(region=recode(region,
                       "E. Canada"="Canada East Coast", 
                       "E. Bering S."="Eastern Bering Sea",        
                       "G. Mexico"="Gulf of Mexico",      
                       "G. Alaska"="Gulf of Alaska",         
                       "NE U.S."="Northeast",
                       "SE U.S."="South Atlantic",
                       "West U.S."="US West Coast")) %>% 
  # Fix scientific name
  mutate(species=stringr::str_to_sentence(species)) %>% 
  mutate(species=recode(species,
                        "Achelous spinicarpus" = "Portunus spinicarpus",
                        "Aspidophoroides bartoni" = "Aspidophoroides monopterygius",
                        "Astropecten duplius" = "Astropecten articulatus",
                        # "Buccinum polare" = "",
                        "Calappa sula" = "Calappa sulcata",
                        "Clupea pallasii" = "Clupea pallasii pallasii",
                        "Cross papposus" = "Crossaster papposus",
                        # "Crossaster borealis" = "",
                        "Etrumeus teres" = "Etrumeus sadina",
                        # "Eualus macrophthalmus" = "",
                        # "Gibbesia neglecta" = "",
                        "Gymothorax igromargiatus" = "Gymnothorax nigromarginatus",
                        # "Homaxinella amphispicula" = "",
                        # "Liponema brevicorne" = "",
                        "Loligo pealeii" = "Doryteuthis pealeii",
                        # "Moreiradromia antillensis" = "",
                        "Myliobatis freminvillii" = "Myliobatis freminvillei",
                        # "Neptunea borealis" = "",
                        "Ophidion welshi" = "Ophidion josephi",
                        "Ovalipes floridaus" = "Ovalipes floridanus",
                        "Parapasiphaea sulifrons" = "Parapasiphae sulcatifrons", # not matched to FB
                        "Parastichopus californicus" = "Apostichopus californicus",
                        # "Parthenopoides massena" = "",
                        "Plicifusus kroeyeri"  = "Plicifusus kroyeri", # not working
                        "Podochela sidneyi" = "Coryrhynchus sidneyi",
                        "Polydactylus octoemus" = "Polydactylus octonemus",
                        "Reilla mulleri" = "Renilla muelleri",
                        # "Serratiflustra serrulata" = "",
                        "Spirontocaris lilljeborgi" = "Spirontocaris liljeborgii", # not matched to FB
                        # "Stegophiura ponderosa"  = "",
                        "Stenocionops furus" = "Stenocionops furcatus",
                        # "Stereomastis sculpta"  = "",
                        # "Stomias ferox"  = "",
                        # "Symphurus civitatum"  = "",
                        "Talismania bifura" = "Talismania bifurcata",
                        # "Trachinocephalus myops" = "",
                        "Triglops forfius" = "Triglops forficatus",
                        "Ulvaria subbifura" = "Ulvaria subbifurcata",
                        "Zenopsis conchifera" = "Zenopsis conchifer")) %>%
  # Arrange
  select(rcp, region, species, shift_km, shift_km_sd, uncertainty,
         habitat_perc_delta, habitat_perc_delta_sd, r2_presence, r2_biomass, everything()) %>% 
  arrange(rcp, region, species)

# Inspect
str(data)
freeR::complete(data)

# Inspect
freeR::uniq(data$region)
freeR::uniq(data$uncertainty)

# Species key
spp_key <- data %>% 
  count(species)

freeR::check_names(spp_key$species)

# Lookup names
names < freeR::fb_comm_name(spp_key$species)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "morley_projections.Rds"))



