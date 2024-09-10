# CREATION DATE 7 Sep 2024

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

#####Bring in projections and database and make sure species names match

    #Species projections from bottom trawl survey data for North America (Morley et al. 2018/2019)
    morley_projections_byspp <- fread(file.path("data","morley_projections_byspp.csv"))
    
    #New column with full species name
    morley_projections_byspp[,spp := paste(str_to_title(Genus), species, sep = " ")]
    
    #Check taxonomy
   # projection_taxa <- get_taxa(unique(morley_projections_byspp$spp))
    
      #Save, this takes a bit
    #  saveRDS(projection_taxa, file.path("data","projection_taxa.Rds"))
      
      projection_taxa <- readRDS(file.path("data","projection_taxa.Rds"))
    
    #Link "correct species name" back to morley projections by spp
    morley_projections_correctedtaxa <- projection_taxa[morley_projections_byspp, on = c("query" = "spp")]
    
    #Allocations database upload
    database <- fread(file.path("data","sep9_db_download.csv"))
    
    #Make all empty cells in database NAs to allow for melt
    database[ , names(database) := lapply(.SD, function(x) { x[is.na(x) | x == ""] <- NA; x })]
    
    #Make sure column 46 is 'notes' for this next step to work correctly
    stopifnot(colnames(database[,46])== "notes")
    
    #Wide to long so that each species (even within a complex) gets it's own row, na.rm = T prevents NAs in Species name column from having fewer than 28 spp in complex
    database.l <- melt(database, measure.vars = colnames(database[,47:ncol(database)]), variable.name = "Species_num_complex", value.name = "Species_name", na.rm = T)
    
    #Check database taxonomy to match with projections
   # database_taxa <- get_taxa(unique(database.l$Species_name))
    
    #Save, this takes a bit
   # saveRDS(database_taxa, file.path("data","database_taxa.Rds"))
    
    database_taxa <- readRDS(file.path("data","database_taxa.Rds"))
    
    #Link "correct species name" back to database by Species_name
    database_correctedtaxa <- database_taxa[database.l, on = c("query" = "Species_name")]

######Link on region
    #Currently, the projections have 7 regions : East_Canada, EBS (includes AI), GMEX, GOA, NEUS, SEUS, West_USA 
    #Currently, the database has FMPs with regionally inspired names
    #We'll make a key to link projections with FMPs
    #Note that some FMPs overlap with multiple regions, therefore, we will include two columns

    #Made a key manually, now load up
    fmp_region_key <- fread(file.path("data","fmp_region_key.csv"), strip.white = T)
    #Find and replace \n
    fmp_region_key <- fmp_region_key[, lapply(.SD, function(x) if (is.character(x)) gsub("\\\\n", "\n", x) else x)]
    
    #Add to database
    database_correctedtaxa.reglink <- fmp_region_key[database_correctedtaxa, on = c("council_lead", "fmp")]

    #Wide to long (two regions for some stocks)
    database_correctedtaxa.reglink.l <- melt(database_correctedtaxa.reglink, measure.vars = c("Reg1","Reg2"), variable.name = "Region_number",value.name = "Region")
    
################################
####Link projections with database
################################
    
projections_cc_database_allocations <- morley_projections_correctedtaxa[database_correctedtaxa.reglink.l, on = c("Region","taxa")]
    
    #How many total stocks
    length(unique(projections_cc_database_allocations$stock_orig))
      #477
    #How many stocks do we have at least one projection for?
    stock_projection <- unique(projections_cc_database_allocations[!is.na(Shift),.(stock_orig)])
      #112 stocks
    #How many total species?
    length(unique(projections_cc_database_allocations$taxa))
    #370
    #How many species do we have at least one projection for?
    spp_projection <- unique(projections_cc_database_allocations[!is.na(Shift),.(taxa)])
    #90
    
    #How many stocks are allocated? (170, excluding pacific)
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y",.(stock_orig)]))
   
     #How many species are allocated? (174)
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y",.(taxa)]))
    
    #Of those that are allocated, how many stocks do we have projections for? 60
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y" & !is.na(Shift),.(stock_orig)]))
    
    #Of those that are allocated, how many species do we have projections for? 51
    nrow(unique(projections_cc_database_allocations[allocation_yn == "y" & !is.na(Shift),.(taxa)]))
    
    projection_database_linkages <- unique(projections_cc_database_allocations[,.(Region, stock_orig, taxa, RCP, Shift)])
    
#Reduce to ones that successfully linked to projections
projections_cc_database_allocations.shiftsonly <- unique(projections_cc_database_allocations[!is.na(taxa) & !is.na(Region) & !is.na(Shift),.(council_lead,council_label, fmp,fmp_label, stock_orig, taxa, RCP,Shift, `Uncert.`) ])

#Set order of Councils
projections_cc_database_allocations.shiftsonly[,council_label := factor(council_label,
                                                                           levels = c("North Pacific","Pacific",
                                                                                      "New England","Mid-Atlantic","South Atlantic","Gulf of Mexico"))]

#Visualize distribution of shifts for both RCPs for each FMP
(projections_federal_stocks <- ggplot() +
  geom_boxplot(data = projections_cc_database_allocations.shiftsonly[`Uncert.` != "low"], aes(x = fmp_label, y = Shift, fill = factor(RCP)), lwd = 0.3, outlier.size = 1) +
  facet_grid(~council_label, scales = "free_x", space = "free") +
  labs(x = "Fisheries Management Plan", y = "Shift by the end of 21st century (km)", fill = "RCP") +
  scale_fill_manual(values = c("turquoise3","orange2")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)))

ggsave(projections_federal_stocks, path = "figures",filename = "projections_by_FMP.jpg", width = 10, height = 5, units = "in")















#Federally managed groundfish species with highest projected shifts (2.5 RCP) with: (excluding HMS, carribbean, etc.)
####Some type of spatial allocation
spatial_alloc_RCP26 <- projections_cc_database[RCP == 26 & Uncert.%in% c("medium","low") & (country_allocation == "y" | state_allocation == "y" | other_area_allocation == "y"),]

###TOP FIVE

#Sebastes borealis (shortraker rockfish)
###Projected 292.5 km shift and 15.2% increase in habitat area
###NPFMC
###Groundfish of the GOA
###Western, Central, Eastern GOA spatial allocations

#Sebastes alutus (Pacific ocean perch)
###Projected 203 km shift and 1% increase in habitat area
###NPFMC
###Groundfish of the GOA
###Western, Central, West Yacutat, Southeast Outside District, spatial allocations

#Stenotomus chrysops (Atlantic Coast Scup)
###Projected 183.8 km shift and 43% increase in habitat area
###MAFMC
###Summer Flounder, Scup and Black Sea Bass Scup
###ME (0.12%), MA (21.59%), RI (56.19%), CT (3.15%), NY (15.82%), NJ (2.91%), MD (0.01%), VA (0.17%), NC (0.05%) (state allocations)

#Pleurogrammus monopterygius (Atka mackerel)
###Projected 140.7 km shift and -37% decrease in habitat area
###NPFMC
###Groundfish of the Bering Sea and Aleutian Islands
###541/EBS; 542; 543 (spatial area allocations)

#Scomber scombrus (Atlantic mackerel)
###Projected 66.2 km shift and -0.1% decrease in habitat area
###MAFMC
###Mackerel, Squid and Butterfish Atlantic mackerel
###US (remainder), Canada (10000 mt)  

####Some type of sector allocation
sector_alloc_RCP26 <- projections_cc_database[RCP == 26 & Uncert.%in% c("medium","low") & (sector_allocation == "y"),]

#Stenotomus chrysops (NEUS Scup)
###Projected 183.8 km shift and 43% increase in habitat area
###MAFMC
###Summer Flounder, Scup and Black Sea Bass Scup - Atlantic Coast
###commercial (65%), recreational (35%) 

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
