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
catch_share_allocation_database_forloading <- fread("data/database/processed/catch_share_allocation_database_forloading.csv")
catch_share_allocation_database_forloading[,council_lead := factor(council_lead, levels = c("NEFMC","MAFMC","SAFMC","GMFMC","Atlantic HMS","PFMC","NPFMC"),
                                                                   labels = c("New England","Mid-\nAtlantic","South\nAtlantic","Gulf of\nMexico","Atlantic\nHMS","Pacific","North Pacific"))]

########################
##Plot start and end input years, and start year of program
########################

catch_share_allocation_years <- unique(catch_share_allocation_database_forloading[,.(short_program_name,share_program, council_lead, shares_basis,
                                                                                     shares_basis_text, shares_start_yr,
                                                                                    shares_basis_historical_start,
                                                                                     shares_basis_historical_end)])

catch_share_start_end_input_years <- ggplot() + 
  geom_linerange(data = catch_share_allocation_years[complete.cases(catch_share_allocation_years),], aes(x = forcats::fct_rev(short_program_name), ymin = shares_basis_historical_start, ymax = shares_basis_historical_end, color = shares_basis), position = position_dodge(width = 0.5)) +
  geom_point(data = catch_share_allocation_years, aes(x = forcats::fct_rev(short_program_name), y = shares_start_yr, fill = "black"), shape = 23, fill = "black", color = "white") + 
  geom_text(data = catch_share_allocation_years[complete.cases(catch_share_allocation_years[,shares_basis_text]),], aes(x = forcats::fct_rev(short_program_name), y = 1990, label = shares_basis_text), fontface = "italic", size = 2.5, hjust = 1) +
  theme_classic() + 
  facet_grid(council_lead~"", scales = "free", space = "free", switch = "y") + 
  labs(x = "Program", y = "Year", color = "Reference period type") + 
  guides(color = guide_legend(title.position = "top", title.hjust=0.5)) +
  lims(y = c(1970,2025)) +
  theme(
    strip.background = element_blank(),   # Remove the facet label box
    strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
    strip.placement = "outside",           # Place facet labels outside of y-axis
    axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
    legend.position = "top",
    legend.direction = "horizontal"
    
  ) +
  coord_flip()


ggsave(catch_share_start_end_input_years, path = "figures",filename = "catch_share_start_end_input_years.jpg", width = 8, height = 9, units = "in")

########################
##Plot share caps
########################

catch_share_allocation_caps <- unique(catch_share_allocation_database_forloading[,.(short_program_name,share_program, council_lead, 
                                                                                     cap_type_title, cap_text, cap_min, cap_max)])

catch_share_allocation_caps[,cap_type_title := factor(cap_type_title, levels = c(  "Individual holding",  "Individual/vessel use",  "Processor holding","Processor use",     "Crew holding",   
                                                                                   "Cooperative use"      ))]

catch_share_caps <- ggplot() + 
  geom_linerange(data = catch_share_allocation_caps[complete.cases(cap_type_title),], aes(x = forcats::fct_rev(short_program_name), ymin = cap_min, ymax = cap_max, color = cap_type_title), position = position_dodge(width = 0.5), linewidth = 1) +
  geom_point(data = catch_share_allocation_caps[cap_min == cap_max,], aes(x = forcats::fct_rev(short_program_name), y = cap_min, color = cap_type_title), position = position_dodge(width = 0.5), shape = 15, size = 1) +
  scale_color_manual(values = c("#4e79a7", "lightblue", "#e15759", "pink", "#59a14f", "#b07aa1")) +
  geom_text(data = catch_share_allocation_caps[complete.cases(catch_share_allocation_caps[,cap_text]),], aes(x = forcats::fct_rev(short_program_name), y = 20, label = cap_text), fontface = "italic", size = 2.5, hjust = 1) +
  theme_classic() + 
  facet_grid(council_lead~"", scales = "free", space = "free", switch = "y") + 
  labs(x = "Program", y = "Cap (%)", color = "Cap type") + 
  guides(linetype = guide_legend(title.position = "top", title.hjust=0.5)) +
 # lims(y = c(1970,2025)) +
  theme(
    strip.background = element_blank(),   # Remove the facet label box
    strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
    strip.placement = "outside",           # Place facet labels outside of y-axis
    axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
    legend.position = "top",
    legend.direction = "horizontal"
    
  ) +
  coord_flip()


ggsave(catch_share_caps, path = "figures",filename = "catch_share_caps.jpg", width = 8, height = 12, units = "in")

##########
#Merge two figures
##########

catch_share_full <- cowplot::plot_grid(catch_share_start_end_input_years,
                                       catch_share_caps + theme(axis.text.y = element_blank(), strip.text = element_blank()),
                                       ncol = 2, align = "hv")


#to do
#order by year of implementation
#copy to Chris's faceting style, with FMC on right
#add line type for first panel with years for 'fully year based' vs. 'partly year based'
