# CREATION DATE 9 Oct 2024

# AUTHOR: zoe.j.kitchel@gmail.com

# PURPOSE: Visualize catch share characteristics

#############################
##Setup
#############################
library(data.table)
library(stringr)
library(ggplot2)

########################
##Load data
########################

#####Bring in catch shares by year
catch_share_allocation_database_years_real <- fread("data/database/processed/catch_share_allocation_database_years_real.csv")
catch_share_allocation_database_years_real<-catch_share_allocation_database_years_real[complete.cases(catch_share_allocation_database_years_real[,shares_basis_formatted])]
catch_share_allocation_database_years_real<-catch_share_allocation_database_years_real[shares_basis_formatted != ""]
catch_share_allocation_database_years_real[,council_lead := factor(council_lead, levels = c("NEFMC","MAFMC","SAFMC","GMFMC","Atlantic HMS","PFMC","NPFMC"),
                                                                   labels = c("New England","Mid-Atlantic","South Atlantic","Gulf of Mexico","Atlantic HMS","Pacific","North Pacific"))]

#####Bring in catch shares by cap percent
catch_share_allocation_database_quotasharecaps <- fread("data/database/processed/catch_share_allocation_database_quotasharecaps.csv")
catch_share_allocation_database_quotasharecaps[,council_lead := factor(council_lead, levels = c("NEFMC","MAFMC","SAFMC","GMFMC","Atlantic HMS","PFMC","NPFMC"),
                                                                   labels = c("New England","Mid-Atlantic","South Atlantic","Gulf of Mexico","Atlantic HMS","Pacific","North Pacific"))]




########################
##Plot start and end input years, and start year of program
########################

catch_share_start_end_input_years <- ggplot(catch_share_allocation_database_years_real) + 
  geom_errorbar(aes(x = reorder(stock, share_program), ymin = shares_yrs_basis_start, ymax = shares_yrs_basis_end, linetype = shares_basis_formatted)) +
  scale_linetype_manual(values = c("solid","dashed")) +
  geom_point(aes(x = reorder(stock, share_program), y = shares_start_yr), shape = 23, fill = "turquoise3", color = "white") + 
  coord_flip() + 
  facet_wrap(~council_lead, scales = "free", ncol = 1) + 
  theme_classic() + 
  labs(x = "Stock",y = "Year", linetype = "Shares basis") +
  theme(
    axis.text.y = element_blank()
  )

ggsave(catch_share_start_end_input_years, path = "figures",filename = "catch_share_start_end_input_years.jpg", width = 8, height = 12, units = "in")

########################
##Plot share caps
########################

catch_share_caps <- ggplot(catch_share_allocation_database_quotasharecaps) + 
  geom_boxplot(aes(x = share_program_short, y = indiv_quota_share_cap)) +
  facet_wrap(~council_lead, scales = "free_x", ncol = 1) + 
  theme_classic() + 
  labs(x = "Share program",y = "Individiual quota share cap (%)")

ggsave(catch_share_caps, path = "figures",filename = "catch_share_caps.jpg", width = 8, height = 12, units = "in")

