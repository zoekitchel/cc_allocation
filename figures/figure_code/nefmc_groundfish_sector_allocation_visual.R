#################################
#Zoe Kitchel
#20 Feb 2023
#This code makes visualization of sector allocations for NEFMC groundfish across sectors
##################################
##SETUP
##################################
library(ggplot2)
library(data.table)
##################################
#Pull in table from
#Federal Register :: Magnuson-Stevens Act Provisions; Fisheries of the Northeastern United States;
#Northeast Multispecies Fishery; Approval of 2023 and 2024 Sector Operations Plans and Allocation of
#2023 Northeast Multispecies Annual Catch Entitlements
#https://www.federalregister.gov/documents/2023/05/01/2023-09143/magnuson-stevens-act-provisions-fisheries-of-the-northeastern-united-states-northeast-multispecies

nefmc_multispecies_2023_allocation <- fread(file.path("figures","input_figures","raw","nefmc_multispecies_2023_allocation.csv"))

#wide to long
nefmc_multispecies_2023_allocation.long <- melt(nefmc_multispecies_2023_allocation,
                                                id.vars = c("Sector Name", "MRI Count"),
                                                value.name = "percent",
                                                variable.name = "stock")

#species key
species_stock_key <- data.table(stock = 
                                  c("GB Cod", "GOM Cod","GB Haddock",
                                  "GOM Haddock","GB Yellowtail Flounder", "SNE MA Yellowtail Flounder",
                                  "CC GOM Yellowtail Flounder", "Plaice", "Witch Flounder",
                                  "GB Winter Flounder", "GOM Winter Flounder","SNE MA Winter Flounder",
                                  "Redfish","White Hake", "Pollock" ),
                                species = 
                                 c("Cod", "Cod","Haddock",
                                    "Haddock","Yellowtail\nFlounder", "Yellowtail\nFlounder",
                                    "Yellowtail\nFlounder", "Plaice", "Witch\nFlounder",
                                    "Winter\nFlounder", "Winter\nFlounder","Winter\nFlounder",
                                    "Redfish","White Hake", "Pollock"),
                                location = 
                                  c("GB", "GOM","GB",
                                    "GOM","GB", "SNE\nMA",
                                    "CC\nGOM", "", "",
                                    "GB", "GOM","SNE\nMA",
                                    "","", ""))

#set factor orders
nefmc_multispecies_2023_allocation.long[,`Sector Name` := factor(`Sector Name`, levels = c( 
                                              "Common Pool", "Mooncusser Sector", "New Hampshire Permit Bank", "Maine Coast Community Sector",
                                               "Maine Permit Bank", "Fixed Gear Sector", "NEFS 13" ,
                                               "NEFS 12", "NEFS 11", "NEFS 10" ,
                                                "NEFS 8", "NEFS 6", "NEFS 5" ,"NEFS 4" ,"NEFS 2" ,
                                               "Sustainable Harvest Sector 3", "Sustainable Harvest Sector 2", "Sustainable Harvest Sector 1"))]

#add species column
nefmc_multispecies_2023_allocation.long <- nefmc_multispecies_2023_allocation.long[species_stock_key, on = "stock"]

#set stock as factor
nefmc_multispecies_2023_allocation.long[,stock := factor(stock, levels = c("GB Cod", "GOM Cod","GB Haddock",
                                                                           "GOM Haddock","GB Yellowtail Flounder", "SNE MA Yellowtail Flounder",
                                                                           "CC GOM Yellowtail Flounder", "Plaice", "Witch Flounder",
                                                                           "GB Winter Flounder", "GOM Winter Flounder","SNE MA Winter Flounder",
                                                                           "Redfish","White Hake", "Pollock"))]



#plot
ggplot(nefmc_multispecies_2023_allocation.long, aes(y = `Sector Name`, x = location)) + 
  geom_point(aes(size=percent)) +
  labs(y="Sector", x="Stock", size="Percent\nallocation") +
  facet_wrap(~species, scales = "free_x", nrow = 1) +
  theme_bw() + theme(axis.text.x=element_text(size=9),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11),
                     panel.border = element_blank())

nefmc_multispecies_allocation_matrix_bw <-
ggplot(nefmc_multispecies_2023_allocation.long, aes(y = `Sector Name`, x = location)) + 
  geom_point(aes(size=percent)) +
  labs(y="Sector", x="Stock", size="Percent\nallocation") +
  facet_wrap(~species, scales = "free_x", nrow = 1) +
  theme_bw() + theme(axis.text.x=element_text(size=9),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank())

ggsave(nefmc_multispecies_allocation_matrix_bw, path = file.path("figures","summaries"),
       filename = "nefmc_multispecies_allocation_matrix_bw.jpg",height = 6, width = 12)

#or
nefmc_multispecies_allocation_matrix_color <-
ggplot(nefmc_multispecies_2023_allocation.long, aes(y = `Sector Name`, x = stock, color = species)) + 
  geom_point(aes(size=percent)) +
  labs(y="Sector", x="Stock", size="Percent\nallocation", color = "Species") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=45,hjust=1,vjust=1),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

ggsave(nefmc_multispecies_allocation_matrix_color, path = file.path("figures","summaries"),
       filename = "nefmc_multispecies_allocation_matrix_color.jpg",height = 6, width = 12)
