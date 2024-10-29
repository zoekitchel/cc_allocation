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

#####Bring formatted database where each row = share cap type and program
catch_share_allocation_database_forloading <- fread("data/database/processed/catch_share_allocation_database_forloading.csv")
catch_share_allocation_database_forloading[,council_lead := factor(council_lead, levels = c("NEFMC","MAFMC","SAFMC","GMFMC","Atlantic HMS","PFMC","NPFMC"),
                                                                   labels = c("NE","Mid-\nAtl.","S.\nAtl.","GoM","Atl.\nHMS","Pacific","North\nPacific"))]

####Edit order of reference period type levels
catch_share_allocation_database_forloading[,shares_basis:= factor(shares_basis, levels = c("Historical individual","Recent individual","Historical processor"))]

#####Bring in formatted database where each row = stock
catch_share_allocation_database_bystock_forloading <- fread("data/database/processed/catch_share_allocation_database_bystock_forloading.csv")
catch_share_allocation_database_bystock_forloading[,council_lead := factor(council_lead, levels = c("NEFMC","MAFMC","SAFMC","GMFMC","Atlantic HMS","PFMC","NPFMC"),
                                                                   labels = c("NE","Mid-\nAtl.","S.\nAtl.","GoM","Atl.\nHMS","Pacific","North\nPacific"))]

####Wide to Long
catch_share_allocation_database_bystock_forloading.l <- melt(catch_share_allocation_database_bystock_forloading,
                                                             id.vars = c("council_lead", "fmp", "stock_orig","species","individual_vessel_use_cap_text", "no_cap_text"),
                                                             variable.name = "cap_type",value.name = "cap_value")

########################
##Plot start and end input years, and start year of program
########################

catch_share_allocation_years <- unique(catch_share_allocation_database_forloading[,.(short_program_name,share_program, council_lead, shares_basis,
                                                                                     shares_basis_text, shares_start_yr,years_dependency,
                                                                                    shares_basis_historical_start,
                                                                                     shares_basis_historical_end)])

catch_share_start_end_input_years <- ggplot() + 
  geom_linerange(data = catch_share_allocation_years, aes(x = reorder(short_program_name, -shares_basis_historical_start),
                                                          ymin = shares_basis_historical_start, ymax = shares_basis_historical_end, color = shares_basis,
                                                          linetype = years_dependency),
                                                          position = position_dodge(width = 0.5), linewidth = 0.8) +
  geom_point(data = catch_share_allocation_years, aes(x = reorder(short_program_name, -shares_basis_historical_start), y = shares_start_yr, fill = "black"),
             shape = 23, fill = "black", color = "white", size = 2) + 
  geom_text(data = catch_share_allocation_years[complete.cases(catch_share_allocation_years[,shares_basis_text]),],
            aes(x = reorder(short_program_name, -shares_basis_historical_start), y = 1990, label = shares_basis_text),
            fontface = "italic", size = 2.1, hjust = 1, color = "darkgrey") +
  scale_color_manual(values = c("#264F86","#ADD2EA","#F3B9C0")) +
  theme_classic() + 
  facet_grid(council_lead~"", scales = "free", space = "free") + 
  labs(x = "Program", y = "Year", color = "Reference period type", linetype = "Share based on landings?") + 
  lims(y = c(1955,2025)) +
  theme(
    strip.background = element_rect(fill = "lightgrey",linewidth = 0.5),   # Remove the facet label box
    strip.background.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
   # strip.placement = "outside",           # Place facet labels outside of y-axis
    axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
   legend.position = "null",
   text = element_text(size = 10)
    
  ) +
  coord_flip()

#dummy line graph for legend extraction
color_linetype_legend <- cowplot::get_legend(ggplot() + 
  geom_line(data = catch_share_allocation_years[complete.cases(catch_share_allocation_years[,shares_basis])],
            aes(x = reorder(short_program_name, -shares_start_yr), y = shares_basis_historical_end, color = shares_basis, linetype = years_dependency),
            position = position_dodge(width = 0.5), linewidth = 0.6) +
    scale_color_manual(values = c("#264F86","#ADD2EA","#F3B9C0")) +
  geom_point(data = catch_share_allocation_years, aes(x = reorder(short_program_name, -shares_start_yr), y = shares_start_yr, fill = "black"), shape = 23, fill = "black", color = "white") + 
  theme_classic() + 
  labs(x = "Program", y = "Year", color = "Reference period type", linetype = "Share based on landings?") + 
  guides(color = guide_legend(title.position = "top", title.hjust=0.7, order = 1),
         linetype = guide_legend(title.position = "top", title.hjust=0.7, order = 2)) +
  theme(
    strip.background = element_rect(fill = "lightgrey",linewidth = 0.5),   # Remove the facet label box
    strip.background.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    #strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
    # strip.placement = "outside",           # Place facet labels outside of y-axis
    axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
    legend.direction = "vertical",
    legend.box.margin = unit(c(0.4,0,0,6), "cm"),
    legend.position = "top",
    legend.justification = "center",
    text = element_text(size = 8),
    legend.spacing = unit(0.01,"cm"),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
    
  ))

#Add legend
catch_share_start_end_input_years.l <- cowplot::plot_grid(color_linetype_legend,
                   catch_share_start_end_input_years + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                   rel_heights = c(1,7),
                   ncol = 1)


########################
##Plot distribution of share caps in histogram (by region, but not by program)
########################
#unique values
catch_share_allocation_database_bystock_forloading.l <- unique(catch_share_allocation_database_bystock_forloading.l)

catch_share_allocation_database_bystock_forloading.l[,cap_type := factor(cap_type, levels = c( "individual_holdings_cap"   , "individual_vessel_use_cap"  ,    "catcher_processor_holdings_cap", "vessel_holdings_cap" ,           "processor_holdings_cap" ,       
                                                                                               "crew_holdings_cap"          ,    "catcher_processor_use_cap"    ,  "cooperative_use_cap" ,           "processor_use_cap"))]
catch_share_caps <- ggplot() + 
  geom_histogram(data = catch_share_allocation_database_bystock_forloading.l[cap_type %in% c("individual_holdings_cap","processor_holdings_cap","individual_vessel_use_cap","processor_use_cap")],
                 aes(x = cap_value, fill = cap_type),
                 position =position_dodge(preserve = "single"),
                 binwidth = 3, color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("#4e79a7", "lightblue3", "#e15759", "pink"), labels = c("Individual holdings    ","Individual or vessel use    ","Processor holdings    ","Processor use    ")) +
  scale_y_continuous(breaks = c(0.1,seq(1,9,2)),labels = c("",seq(1,9,2))) +
  scale_x_continuous(breaks = seq(0,65, by = 10)) +
  coord_cartesian(ylim = c(0.45, NA)) +
 # geom_text(data = catch_share_allocation_caps[complete.cases(catch_share_allocation_caps[,cap_text]),], aes(x = forcats::fct_rev(short_program_name), y = 20, label = cap_text), fontface = "italic", size = 2.5, hjust = 1) +
  theme_classic() + 
  facet_grid(council_lead~"") + 
  labs(x = "Cap value (%)", y = "Stock frequency", fill = "Cap type") + 
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5, ncol = 2)) +
  # lims(y = c(1970,2025)) +
 # geom_text(data = catch_share_allocation_database_bystock_forloading.l[cap_type %in% c("individual_vessel_use_cap")],
         #   aes(label = individual_vessel_use_cap_text, x =50, y = 6), color = "lightblue") +
  theme(
     strip.background.x = element_blank(),   # Remove the facet label box
    # strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
    # strip.placement = "outside",           # Place facet labels outside of y-axis
    #axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
    legend.position = "top",
    legend.direction = "horizontal",
    strip.background = element_rect(fill = "lightgrey",linewidth = 0.5),   # Remove the facet label box
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.spacing = unit(0.3,"cm"),
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.box.margin = unit(c(0,0,0,0), "cm"),
    legend.margin = margin(0.29,0,0,0,unit = "cm"),
    text = element_text(size = 10),
    
    
  )

catch_share_caps.annotated <- cowplot::ggdraw(catch_share_caps) +
  annotate(geom ="text",
           label = "New England Multispecies Sectors:\nLimited to holding 5% of permits\nAnnual catch caps",
           color = "darkgrey",
           fontface = "italic",
           x = 0.45, y = 0.8, size = 2.1,
           hjust = 0) +
  annotate(geom ="text",
           label = "Surfclam and Ocean Quahog:\n65/70 cages per year, respectively",
           color = "darkgrey",
           fontface = "italic",
           x = 0.45, y = 0.7, size = 2.1,
           hjust = 0) +
  annotate(geom ="text",
           label = "U.S. Pacific Sablefish Permit Stacking Program:\n3 permits per vessel per year",
           color = "darkgrey",
           fontface = "italic",
           x = 0.45, y = 0.25, size = 2.1,
           hjust = 0)

 ##########
#Merge two figures
##########

Fig7_catch_share <- cowplot::plot_grid(catch_share_start_end_input_years.l + theme(plot.margin = margin(0,0,0.18,0,unit = "cm")),
                                       catch_share_caps.annotated + theme(plot.margin = margin(0,0,0,0,unit = "cm")),
                                       ncol = 2, labels = c("A","B"), label_fontface = "plain", label_size = 12,
                                       rel_widths = c(1.3,1))

ggsave(Fig7_catch_share, path = "figures",filename = "Fig7_catch_share.jpg", width = 10, height = 6.2, units = "in")
ggsave(Fig7_catch_share, path = "figures",filename = "Fig7_catch_share.tiff", width = 10, height = 6.2, units = "in")
ggsave(Fig7_catch_share, path = "figures",filename = "Fig7_catch_share.png", width = 10, height = 6.2, units = "in")
ggsave(Fig7_catch_share, path = "figures",filename = "Fig7_catch_share.eps", width = 10, height = 6.2, units = "in")
ggsave(Fig7_catch_share, path = "figures",filename = "Fig7_catch_share.pdf", width = 10, height = 6.2, units = "in")














########Scratch
########################
##Plot share caps for each catch share program
########################

# catch_share_allocation_caps <- unique(catch_share_allocation_database_forloading[,.(short_program_name,share_program, council_lead, 
#                                                                                     cap_type_title, cap_text, cap_min, cap_max)])
# 
# catch_share_allocation_caps[,cap_type_title := factor(cap_type_title, levels = c(  "Individual holding",  "Individual/vessel use",  "Processor holding","Processor use",     "Crew holding",   
#                                                                                    "Cooperative use"      ))]
# 
# catch_share_caps <- ggplot() + 
#   geom_linerange(data = catch_share_allocation_caps[complete.cases(cap_type_title),], aes(x = forcats::fct_rev(short_program_name), ymin = cap_min, ymax = cap_max, color = cap_type_title), position = position_dodge(width = 0.5), linewidth = 1) +
#   geom_point(data = catch_share_allocation_caps[cap_min == cap_max,], aes(x = forcats::fct_rev(short_program_name), y = cap_min, color = cap_type_title), position = position_dodge(width = 0.5), shape = 15, size = 1) +
#   scale_color_manual(values = c("#4e79a7", "lightblue", "#e15759", "pink", "#59a14f", "#b07aa1")) +
#   geom_text(data = catch_share_allocation_caps[complete.cases(catch_share_allocation_caps[,cap_text]),],
#             aes(x = forcats::fct_rev(short_program_name), y = 20, label = cap_text), fontface = "italic", size = 2.7, hjust = 1) +
#   theme_classic() + 
#   facet_grid(council_lead~"", scales = "free", space = "free") + 
#   labs(x = "Program", y = "Cap (%)", color = "Cap type") + 
#   guides(linetype = guide_legend(title.position = "top", title.hjust=0.5)) +
#   # lims(y = c(1970,2025)) +
#   theme(
#     # strip.background = element_blank(),   # Remove the facet label box
#     # strip.text.y.left = element_text(face = "bold"),  # Make facet labels bold
#     # strip.placement = "outside",           # Place facet labels outside of y-axis
#     axis.title.y = element_blank(),  # Adjust vertical alignment of y-axis title if needed
#     legend.position = "top",
#     legend.direction = "horizontal",
#     
#   ) +
#   coord_flip()
# 
# 
# ggsave(catch_share_caps, path = "figures",filename = "catch_share_caps.jpg", width = 8, height = 12, units = "in")
# 
# 