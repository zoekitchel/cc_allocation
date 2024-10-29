# CREATION DATE 7 Sep 2024
# MODIFIED DATE 22 Oct 2024

# AUTHOR: zoe.j.kitchel@gmail.com

# PURPOSE: Link Morley projections with catch share database

#############################
##Setup
#############################
library(data.table)
library(stringr)
library(ggplot2)

source(file.path("functions","return_spptaxonomy_function.R"))

########################
##Load data
########################

#####Bring in projections 

    #Species projections from bottom trawl survey data for North America (Morley et al. 2018)
    morley_projections <- data.table(readRDS(file.path("data","morley","processed","morley_projections.Rds")))
    
#####Bring in key to link FMPs to scientific species names
    
    #Cleaned by CMF
    US_FMP_species_list <- data.table(readRDS(file.path("data","fmps","US_FMP_species_list.Rds")))
    
    #Limit to type == "Target
    US_FMP_species_list.r <- US_FMP_species_list[type == "Target",]
    
    #Calculate number of target species in each FMP
    US_FMP_species_list.r[,count_target_spp := uniqueN(species),fmp]
    
#####Bring in database
    #Cleaned by CMF
    quota_allocations_database <- data.table(readRDS(file.path("data","database","processed","quota_allocations_database.Rds")))
    
    #Reduce to essential columns
    quota_allocations_database.r <- quota_allocations_database[,.(council, council_lead, fmp, stock_orig, stock, area, allocation_yn, spatial_yn, spatial_type,
                                                                  country_yn, country_n, country_list, state_yn, state_n, state_list, area_yn, area_n, area_list)]
    
#####Bring in species~stock key
    stock_name_species_key <- fread(file.path("data","database","stock_name_species_key.csv"))
    
#####Bring in key linking database FMP names to US_FMP_species_list names
    fmp_name_key <- fread(file.path("data","database","fmp_name_key.csv"))
    
#####Add US_FMP_species_list names for FMPs to database
    quota_allocations_database.r <- fmp_name_key[quota_allocations_database.r, on = c("allocation_database_fmp"="fmp")]

    
########################
##Modify FMP, Database, and Morley Projection data tables to match by species
########################
    
#####Check taxonomy of Morley projections
  #  projection_taxa <- get_taxa(unique(morley_projections$species))
    
      #Save, this takes a bit
   #   saveRDS(projection_taxa, file.path("data","morley","processed","projection_taxa.Rds"))
      
      projection_taxa <- readRDS(file.path("data","morley","processed","projection_taxa.Rds"))
    
    #Link "correct species name" back to morley projections by spp
    morley_projections_clean <- projection_taxa[morley_projections, on = c("query" = "species")]
    
    #Manually enter those with no match
    morley_projections_clean[query == "Gadus ogac"]$taxa <- "Gadus macrocephalus"
    morley_projections_clean[query == "Gadus ogac"]$worms_id <- 254538
    morley_projections_clean[query == "Cheiraster dawsoni"]$taxa <- "Cheiraster (Luidiaster) dawsoni"
    morley_projections_clean[query == "Cheiraster dawsoni"]$worms_id <- 380798
    morley_projections_clean[query == "Laqueus californianus"]$taxa <- "Laqueus erythraeus"
    morley_projections_clean[query == "Laqueus californianus"]$worms_id <- 235590
    morley_projections_clean[query == "Portunus spinicarpus"]$taxa <- "Achelous spinicarpus"
    morley_projections_clean[query == "Portunus spinicarpus"]$worms_id <- 557889
    morley_projections_clean[query == "Nearchaster aciculosus"]$taxa <- "Nearchaster (Nearchaster) aciculosus"
    morley_projections_clean[query == "Nearchaster aciculosus"]$worms_id <- 380816
    
#####Check taxonomy of FMPs
    #Check FMP taxonomy to match with database, and projections
   # fmp_taxa_clean <- get_taxa(unique(US_FMP_species_list.r$species))
    
    #Save, this takes a bit
  #  saveRDS(fmp_taxa_clean, file.path("data","fmps","fmp_taxa_clean.Rds"))
    
    fmp_taxa_clean <- readRDS(file.path("data","fmps","fmp_taxa_clean.Rds"))
    
    #Reduce to essential columns (fishbase ID, species name)
    fmp_taxa_clean.r <- fmp_taxa_clean[,.(query,worms_id, taxa,common_name)]
    
    #Which did not match up?
    setdiff(unique(US_FMP_species_list.r$species), fmp_taxa_clean$query)
    
    #Manually add in correct query/taxa combo for Embassichythys bathybius and Gerardia spp.
    fmp_taxa_clean.r[taxa == "Microstomus bathybius"]$query <- "Embassichthys bathybius"
    fmp_taxa_clean.r[query == "Gerardia"]$query <- "Gerardia spp."
    
    #Link with FMPs
    US_FMP_species_list_clean <- fmp_taxa_clean.r[US_FMP_species_list.r, on = c("query" = "species")]
    
    #Delete any without specific taxa affiliations (i.e. shark complex)
    US_FMP_species_list_clean <- US_FMP_species_list_clean[!is.na(query),]
    
    #Rename council as council_lead
    setnames(US_FMP_species_list_clean, "council","council_lead")
    
    #Reduce to essential columns
    US_FMP_species_list_clean.r <- US_FMP_species_list_clean[,.(worms_id, taxa, council_lead, fmp, comm_name, count_target_spp)]
    
    
###Check taxonomy of database stock~species key
    #Flip wide to long with extra species
    stock_name_species_key.l <- melt(stock_name_species_key, id.vars = c("council_lead","fmp","stock"),
                                     variable.name = "Species_num",
                                     value.name = "species",
                                     na.rm = T)
    
   # stock_spp_clean <- get_taxa(unique(stock_name_species_key.l$species))
    
    #Save, this takes a bit
  #  saveRDS(stock_spp_clean, file.path("data","database","stock_spp_clean.Rds"))
    
    stock_spp_clean <- readRDS(file.path("data","database","stock_spp_clean.Rds"))
    
    #Which did not match up?
    setdiff(unique(stock_name_species_key.l$species), stock_spp_clean$query)

########################
##Link up projections with FMP key
########################

      #Add region column to US_FMP_species_list_clean.r to match with morley
    reg_key <- data.table(fmp = c(
                                  "Fishery Management Plan for Atlantic Salmon",                                                       
                                 "Spiny Dogfish Fishery Management Plan",
                                 "Fishery Management Plan for Deep-Sea Red Crab",
                                 "Northeast Multispecies Fishery Management Plan for Small Mesh Multispecies",
                                 "Northeast Skate Complex Fishery Management Plan",
                                 "Atlantic Herring Fishery Management Plan",
                                 "Monkfish Fishery Management Plan",
                                 "Fishery Management Plan for Atlantic Sea Scallops",
                                 "Fishery Management Plan for the Northeast Multi-species Fishery",
                                   "Summer Flounder, Scup, and Black Sea Bass Fishery Management Plan",
                                   "Mackerel, Squid, Butterfish Fishery Management Plan",
                                   "Atlantic Surfclam and Ocean Quahog",
                                   "Bluefish Fishery Management Plan",
                                   "Tilefish Fishery Management Plan",
                                   "South Atlantic Snapper-Grouper Fishery Management Plan",
                                   "Gulf of Mexico and South Atlantic Coastal Migratory Pelagic Fishery Management Plan",
                                   "Dolphin Wahoo Fishery Management Plan",
                                   "Golden Crab Fishery Management Plan",
                                   "South Atlantic Shrimp Fishery Management Plan",
                                   "Spiny Lobster Fishery Management Plan",
                                   "Puerto Rico",
                                   "St. Croix",
                                   "St. Thomas and St. John",
                                   "Fishery Management Plan for Reef Fish Resources of the Gulf of Mexico",
                                   "Gulf of Mexico Red Drum Fishery Management Plan",
                                   "Gulf of Mexico Shrimp Fishery Management Plan",
                                   "Pacific Coast Groundfish Fishery Management Plan",
                                   "Coastal Pelagic Species Fishery Management Plan",
                                   "Pacific Coast Salmon Fishery Management Plan",
                                   "Fishery Management Plan for U.S. West Coast Fisheries for Highly Migratory Species",
                                   "Fishery Management Plan for the Scallop Fishery off Alaska",
                                   "Fishery Management Plan for the Salmon Fisheries in the EEZ Off Alaska",
                                   "Fishery Management Plan for Bering Sea/Aleutian Islands King and Tanner Crabs",
                                   "Fishery Management Plan for Groundfish of the Gulf of Alaska",
                                   "Fishery Management Plan Plan for Groundfish of the Bering Sea and Aleutian Islands Management Area",
                                   "Fishery Management Plan for Fish Resources of the Arctic Management Area",
                                   "Fishery Ecosystem Plan for the Pacific Remote Island Areas",
                                   "Fishery Ecosystem Plan for the American Samoa Archipelago",
                                   "Fishery Ecosystem Plan for the Hawaii Archipelago",
                                   "Fishery Ecosystem Plan for Pacific Pelagic Fisheries of the Western Pacific Region",
                                   "Fishery Ecosystem Plan for the Mariana Archipelago"),
                          region = c("Northeast",                                                       
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "Northeast",
                                     "South Atlantic",
                                     "South Atlantic",
                                     "South Atlantic",
                                     "South Atlantic",
                                     "South Atlantic",
                                     "Gulf of Mexico_South Atlantic",
                                     NA,
                                     NA,
                                     NA,
                                     "Gulf of Mexico",
                                     "Gulf of Mexico",
                                     "Gulf of Mexico",
                                     "US West Coast",
                                     "US West Coast",
                                     "US West Coast",
                                     "US West Coast",
                                     "Eastern Bering Sea_Gulf of Alaska",
                                     "Eastern Bering Sea_Gulf of Alaska",
                                     "Eastern Bering Sea",
                                     "Gulf of Alaska",
                                     "Eastern Bering Sea",
                                     "Eastern Bering Sea",
                                     NA,
                                     NA,
                                     NA,
                                     NA,
                                     NA),
                          fmp_label = c(
                            "Atlantic Salmon",                                                       
                            "Spiny Dogfish",
                            "Deep-Sea Red Crab",
                            "NE Small-Mesh Multispecies (Whiting)",
                            "NE Skate Complex",
                            "Atlantic Herring",
                            "Monkfish",
                            "Atlantic Sea Scallops",
                            "NE Multispecies Fishery",
                            "Summer Flounder, Scup, and Black Sea Bass",
                            "Mackerel, Squid, and Butterfish",
                            "Atlantic Surfclam and Ocean Quahog",
                            "Bluefish",
                            "Tilefish",
                            "Snapper-Grouper",
                            "Coastal Migratory Pelagics",
                            "Dolphin-Wahoo",
                            "Golden Crab",
                            "Shrimp",
                            "Spiny Lobster",
                            "Puerto Rico",
                            "St. Croix",
                            "St. Thomas and St. John",
                            "Reef Fish Resources",
                            "Red Drum",
                            "Shrimp",
                            "Pacific Groundfish",
                            "Coastal Pelagic Species",
                            "Pacific Salmon",
                            "Pacific Highly Migratory Species",
                            "Alaska Scallops",
                            "Alaska Salmon",
                            "King and Tanner Crabs",
                            "GOA Groundfish",
                            "BSAI Groundfish",
                            "Arctic Fish Resources",
                            "Fishery Ecosystem Plan for the Pacific Remote Island Areas",
                            "Fishery Ecosystem Plan for the American Samoa Archipelago",
                            "Fishery Ecosystem Plan for the Hawaii Archipelago",
                            "Fishery Ecosystem Plan for Pacific Pelagic Fisheries of the Western Pacific Region",
                            "Fishery Ecosystem Plan for the Mariana Archipelago"))
    
    US_FMP_species_list_clean.region <- US_FMP_species_list_clean.r[reg_key, on = "fmp"]
    
    #Only keep those with link to Morley region (in continental US)
    US_FMP_species_list_clean.region <- US_FMP_species_list_clean.region[!is.na(region),]
    
    #For those few stocks that cross multiple regions, add extra rows
   
     # Function to split regions with underscores
    split_regions <- function(dt) {
      # Identify rows where 'region' contains an underscore
      underscore_rows <- dt[grepl("_", region)]
      
      # Split these rows by the underscore and create new rows
      split_rows <- underscore_rows[, strsplit(region, "_"), by = .(worms_id, taxa, council_lead, fmp,fmp_label, comm_name,count_target_spp)]
      
      # Create a new data.table with the individual regions
      dt_split <- split_rows[, .(worms_id, taxa, council_lead, fmp, fmp_label, comm_name,count_target_spp, region = V1)]
      
      # Keep original rows without underscores
      dt_no_underscore <- dt[!grepl("_", region)]
      
      # Combine the original rows with the new split rows
      dt_final <- rbind(dt_no_underscore, dt_split)
      
      return(dt_final)
    }
    
    # Apply the function to the data.table
    US_FMP_species_list_clean.region.split <- split_regions(US_FMP_species_list_clean.region)
    
################################
####Link projections with stocks, fmps, and fmcs
################################   
    
    stock_fmp_spp_merge <- morley_projections_clean[US_FMP_species_list_clean.region.split, on = c("region","taxa","worms_id")]
    
    #Add more useful council names
    stock_fmp_spp_merge[,council_lead := factor(council_lead, levels = c("NPFMC","PFMC","NEFMC","SAFMC","MAFMC","GFMC"),
                                                labels = c("North Pacific","Pacific","New England",
                                                           "South Atlantic","Mid-Atlantic","Gulf of Mexico"))]
    
    #reduce to low/med uncertainty, and only those with matches
    stock_fmp_spp_merge.med_highcertain <- stock_fmp_spp_merge[complete.cases(stock_fmp_spp_merge[,.(worms_id, region,shift_km)])][uncertainty != "high"]
    
    #Calculate number of species per fmp in plot
    stock_fmp_spp_merge.med_highcertain[,spp_fmp_proj := uniqueN(worms_id),.(fmp)]
    
    #Add new label with FMP and with n
    stock_fmp_spp_merge.med_highcertain[,fmp_label_n := paste0(fmp_label," (n = ",spp_fmp_proj,")")]
    
    #Add dummy row so that red crab bumps left (missing rcp 2.6)
    row <- stock_fmp_spp_merge.med_highcertain[comm_name == "Deep-sea red crab"]
    row$rcp <- "RCP 2.6"
    row$shift_km <- -400
    
    #Link back
    stock_fmp_spp_merge.med_highcertain <- rbind(stock_fmp_spp_merge.med_highcertain, row)
    
    
    
  
    (Fig14_projections_fmps <- ggplot() +
        geom_hline(yintercept = 71.12, color = "grey75", linetype = "dashed") +
        geom_hline(yintercept = 189.4, color = "grey75", linetype = "dashed") +
        geom_hline(yintercept = 405.8, color = "grey75", linetype = "dashed") +
        geom_boxplot(data = stock_fmp_spp_merge.med_highcertain, aes(x = reorder(fmp_label,-shift_km), y = shift_km, fill = factor(rcp)), lwd = 0.3, outlier.size = 1, position = position_dodge(preserve = "single")) +
        geom_text(data = stock_fmp_spp_merge.med_highcertain, aes(x = reorder(fmp_label,-shift_km), y = -100,label = spp_fmp_proj), size = 3, check_overlap = T, fontface = "bold") +
        geom_text(data = stock_fmp_spp_merge.med_highcertain, aes(x = reorder(fmp_label,-shift_km), y = -175,label = count_target_spp), size = 3, check_overlap = T) +
        geom_text(data = stock_fmp_spp_merge.med_highcertain, aes(x = reorder(fmp_label,-shift_km), y = -135,label = "—"), size = 3, check_overlap = T) +
        facet_grid(~council_lead, scales = "free_x", space = "free") +
        labs(x = "Fishery Management Plan", y = "Shift by the end of 21st century (km)", fill = "RCP") +
        scale_fill_manual(values = c("grey","white"), labels = c("2.6","8.5"), drop = FALSE) +
        coord_cartesian(ylim = c(-190,1800)) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 11),   # Increase x-axis text size
              axis.text.y = element_text(size = 11),                                      # Increase y-axis text size
              axis.title = element_text(size = 14),                                       # Increase axis title size
              legend.position = c(0.9, 0.9),                                              # Keep legend position
              legend.text = element_text(size = 11),                                      # Increase legend text size
              legend.title = element_text(size = 14),                                      # Increase legend title size
              strip.text = element_text(size = 11),
              legend.direction = "horizontal"
        ))
    
    ggsave(Fig14_projections_fmps, path = "figures",filename = "Fig14_projections_fmps.jpg", width = 12, height = 8, units = "in")
    ggsave(Fig14_projections_fmps, path = "figures",filename = "Fig14_projections_fmps.tiff", width = 12, height = 8, units = "in")
    ggsave(Fig14_projections_fmps, path = "figures",filename = "Fig14_projections_fmps.png", width = 12, height = 8, units = "in")
    ggsave(Fig14_projections_fmps, path = "figures",filename = "Fig14_projections_fmps.pdf", width = 12, height = 8, units = "in")
    ggsave(Fig14_projections_fmps, path = "figures",filename = "Fig14_projections_fmps.eps", width = 12, height = 8, units = "in")
        
################################
####Link projections with database
################################
    
projections_cc_database_allocations <- morley_projections_correctedtaxa[database_correctedtaxa.reglink.l, on = c("Region","taxa","worms_id","taxa","kingdom","phylum","class","order","family","genus","rank","common_name" )]
    
    #How many total stocks
    length(unique(projections_cc_database_allocations$stock_orig))
      #477
    #How many stocks do we have at least one projection for?
    stock_projection <- unique(projections_cc_database_allocations[!is.na(Shift),.(stock_orig)])
      #109 stocks
    #How many total species?
    length(unique(projections_cc_database_allocations$taxa))
    #365
    #How many species do we have at least one projection for?
    spp_projection <- unique(projections_cc_database_allocations[!is.na(Shift),.(taxa)])
    #88
    
    #How many stocks are allocated? (170, excluding pacific)
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y",.(stock_orig)]))
   
     #How many species are allocated? (171)
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y",.(taxa)]))
    
    #Of those that are allocated, how many stocks do we have projections for? 57
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y" & !is.na(Shift),.(stock_orig)]))
    
    #Of those that are allocated, how many species do we have projections for? 50
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y" & !is.na(Shift),.(taxa)]))
    
    projection_database_linkages <- unique(projections_cc_database_allocations[,.(Region, stock_orig, taxa, RCP, Shift)])
    
#Reduce to ones that successfully linked to projections
projections_cc_database_allocations.shiftsonly <- unique(projections_cc_database_allocations[!is.na(taxa) & !is.na(Region) & !is.na(Shift),.(council_lead,council_label, fmp,fmp_label, stock_orig, taxa, RCP,Shift, `Uncert.`) ])

#Set order of Councils
projections_cc_database_allocations.shiftsonly[,council_label := factor(council_label,
                                                                           levels = c("North Pacific","Pacific",
                                                                                      "New England","Mid-Atlantic","South Atlantic","Gulf of Mexico"))]

#Visualize distribution of shifts for both RCPs for each FMP (removing any with high uncertainty)
#Displays 102 stocks and 122 unique taxa~stock combinations
(projections_federal_stocks <- ggplot() +
  geom_boxplot(data = projections_cc_database_allocations.shiftsonly[`Uncert.` != "high"], aes(x = fmp_label, y = Shift, fill = factor(RCP)), lwd = 0.3, outlier.size = 1) +
  facet_grid(~council_label, scales = "free_x", space = "free") +
  labs(x = "Fisheries Management Plan", y = "Shift by the end of 21st century (km)", fill = "RCP") +
  scale_fill_manual(values = c("turquoise3","orange2"), labels = c("2.6","8.5")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)))

ggsave(projections_federal_stocks, path = "figures",filename = "projections_by_FMP.jpg", width = 8, height = 5, units = "in")















#Federally managed groundfish species with highest projected shifts (2.5 RCP) with: (excluding those that don't link up with Morley (HMS, Caribbean), and Pacific which is still TBD, etc.)
####Some type of spatial allocation
#New columns to identify stocks with country, state, and other area allocations
projections_cc_database_allocations[,country_allocation := ifelse(is.na(country_list),F,T)][,state_allocation := ifelse(is.na(state_list),F,T)][,other_area_allocation := ifelse(is.na(area_list),F,T)]


spatial_alloc_RCP26 <- projections_cc_database_allocations[RCP == 26 & Uncert.%in% c("medium","low") & (country_allocation == T | state_allocation == T | other_area_allocation == T),]

###Rockfish all projected to shift far

##Sebastes zacentrus, S. proriger, S. brevispinis, S. babcocki, S. pinniger (9 out of top 10 are Sebastes in the Gulf of Alaska, 300-400 km shift)
##Scomber japonicus is also in top 10, chub mackerel, West_USA, Mexico/USA split allocation
##Sebastes variabilis (light dusky rockfish) is biggest shifter in EBS (274.3 km)
##Scup is fastest stock in Mid-Atlantic (NEUS) (183.8 km)
##Scomberomorus cavalla (King mackerel) is top shifter with spatial allcoation in South Atlantic (20.2 km)
##Lutjanus campechanus (northern red snapper) is top shifter with spatial allocation in Gulf of Mexico (16.2 km)

####Some type of sector allocation
#Most sector allocations are rec/commercial, and occur in Gulf of Mexico and South Atlantic

sector_alloc_RCP26 <- projections_cc_database_allocations[RCP == 26 & Uncert.%in% c("medium","low") & !is.na(sector_list),]

#Stenotomus chrysops (NEUS Scup)
###Projected 183.8 km shift and 43% increase in habitat area
###MAFMC
###Summer Flounder, Scup and Black Sea Bass Scup - Atlantic Coast
###commercial (65%), recreational (35%) 

#Haemulon plumierii (white grunt in grunts complex in South Atlantic)
### Projected 81.8 km shift
###SAFMC
###Snapper-Grouper Fishery of South Atlantic
###rec/commercial split

#Yellowedge grouper

#Lutjanus campechanus (Red snapper)
###Projected 74.5 km shift and 95% increase in habitat area
###SAFMC
###Snapper-Grouper Fishery of the South Atlantic Region Red snapper 
###commercial (28.07%), recreational (71.93%) 


#Scomber scombrus (Atlantic mackerel)
###Projected 66.2 km shift and -0.1% decrease in habitat area
###MAFMC
###Mackerel, Squid and Butterfish
###recreational (1209 mt), commercial (remainder)     


#Centropristis striata (black sea bass)
###Projected 65.8 km shift and -2.9% decrease in habitat area
###SAFMC
###Snapper-Grouper Fishery of the South Atlantic Region
###recreational (57%), commercial (43%)   

#Oncorhynchus tshawytscha (Chinook salmon)
###Projected 54.3 km shift and 6.9% increase in habitat area
###NPFMC
###Salmon Fisheries in the EEZ off the Coast of Alaska
###Purse seine; drift gillnets; set gillnets; troll 
