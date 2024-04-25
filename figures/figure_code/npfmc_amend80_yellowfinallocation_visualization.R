#This code takes Table 34 to part 679 and makes it into a digestible figure
#https://www.law.cornell.edu/cfr/text/50/appendix-Table_34_to_part_679

##########################################################################
##Setup
##########################################################################
library(ggplot2)
library(data.table)
library(cowplot)

##########################################################################
##Manually construct data table of yellowfin sole TAC, and annual apportionment
##between Amendment 80 and BSAI Trawl Limited Access Sectors
##########################################################################

#ITAC rate varies by ITAC bin
ITAC_rates <- data.table(yellowfin_ITAC_min = c(0,87500,95000,102500,110000,117500,125000), #minimum of this rule range
                         yellowfin_ITAC_max = c(87499,94999,102499,109999,117499,124999,NA), #maximum of this rule range
                         ITAC_Rate = c(0.93,0.875,0.82,0.765,0.71,0.655,0.6),
                         ITAC_Rate_char = c("  93%","87.5%","  82%","76.5%","  71%","65.5%","  60%"))

#resulting apportionments from round #s by row
ITAC_rates[,round_apportionment_amend80_singlerow := 
             (yellowfin_ITAC_max-yellowfin_ITAC_min+1) * ITAC_Rate]

#resulting apportionments from round #s cumulative
ITAC_rates[,round_apportionment_amend80_cumulative := 
             cumsum(round_apportionment_amend80_singlerow)]

#build data table to calculate continuous apportionments by ITAC
#add column for ITAC between 0 and 150000mt, as table 34 specifies rules for up to 125000 and greater ITAC
apportionment_table <- data.table(yellowfin_ITAC = seq(0,200000))

#add conditional column for amount of yellowfin sole ITAC allocated to Amendment 80 sector
apportionment_table[,amendment80 := ifelse(yellowfin_ITAC<=ITAC_rates[[1,2]],
                                            yellowfin_ITAC*ITAC_rates[[1,3]],
                                           ifelse(yellowfin_ITAC<=ITAC_rates[[2,2]] & yellowfin_ITAC>=ITAC_rates[[2,1]],
                                            ITAC_rates[[1,6]] + (yellowfin_ITAC-ITAC_rates[[1,2]])*ITAC_rates[[2,3]],
                                           ifelse(yellowfin_ITAC<=ITAC_rates[[3,2]] & yellowfin_ITAC>=ITAC_rates[[3,1]],
                                            ITAC_rates[[2,6]] + (yellowfin_ITAC-ITAC_rates[[2,2]])*ITAC_rates[[3,3]],
                                           ifelse(yellowfin_ITAC<=ITAC_rates[[4,2]] & yellowfin_ITAC>=ITAC_rates[[4,1]],
                                            ITAC_rates[[3,6]] + (yellowfin_ITAC-ITAC_rates[[3,2]])*ITAC_rates[[4,3]],
                                           ifelse(yellowfin_ITAC<=ITAC_rates[[5,2]] & yellowfin_ITAC>=ITAC_rates[[5,1]],
                                            ITAC_rates[[4,6]] + (yellowfin_ITAC-ITAC_rates[[4,2]])*ITAC_rates[[5,3]],
                                           ifelse(yellowfin_ITAC<=ITAC_rates[[6,2]] & yellowfin_ITAC>=ITAC_rates[[6,1]],
                                            ITAC_rates[[5,6]] + (yellowfin_ITAC-ITAC_rates[[5,2]])*ITAC_rates[[6,3]],
                                           ifelse(yellowfin_ITAC>ITAC_rates[[7,1]],
                                            ITAC_rates[[6,6]] + (yellowfin_ITAC-ITAC_rates[[6,2]])*ITAC_rates[[7,3]], NA)))))))]

#convert to percent
apportionment_table[, amendment80_per := amendment80/yellowfin_ITAC]

#add column for amount of yellowfin sole ITAC allocated to BSAI trawl limited access sector
apportionment_table[,bsai_trawl := yellowfin_ITAC-amendment80][,bsai_trawl_per := 1-amendment80_per]

#wide to long for plotting
apportionment_table.l <- melt(apportionment_table, id.vars = c("yellowfin_ITAC"), measure.vars = c("amendment80","amendment80_per","bsai_trawl","bsai_trawl_per"))

##########################################################################
##Visualize relative apportionments by yellowfin ITAC
##########################################################################
#by percent

yellowfin_amend80_percent_allocation <- ggplot() +
  geom_line(data = apportionment_table.l[variable %in% c("amendment80_per", "bsai_trawl_per")],
            aes(x = yellowfin_ITAC/1000, y = value, color = variable)) +
  scale_color_manual(values = c("red","blue"), labels = c("Amendment 80","BSAI Trawl")) +
  scale_x_continuous(expand = c(0,0), breaks = c(seq(0,200,25))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  labs(color = "Sub-sector", y = "Allocation proportion", x = "Yellowfin sole ITAC (1000s of mt)") +
  geom_segment(data = ITAC_rates, aes(x = yellowfin_ITAC_min/1000, xend = yellowfin_ITAC_min/1000),y = 0, yend = 0.94, color = "grey", linetype = "dashed") +
  geom_text(data = ITAC_rates[yellowfin_ITAC_min >0 & yellowfin_ITAC_min <125000,], aes(x = (yellowfin_ITAC_min/1000)+4, label = ITAC_Rate_char),y = 0.98, angle = 90, size = 2) +
  geom_text(data = ITAC_rates[yellowfin_ITAC_min ==0 | yellowfin_ITAC_min ==125000,], aes(x = (yellowfin_ITAC_min/1000)+45, label = ITAC_Rate_char),y = 0.98, angle = 90, size = 2) +
  theme_classic() +
  theme(legend.position = "top", legend.direction = "horizontal", legend.justification = "center", plot.margin = margin(0,10,0,0))

ggsave(yellowfin_amend80_percent_allocation, path = file.path("figures","summaries"), filename = "yellowfin_amend80_percent_allocation.jpg",
       height = 5, width = 5, unit = "in")

 #by biomass

yellowfin_amend80_biomass_allocation <- ggplot() +
  geom_line(data = apportionment_table.l[variable %in% c("amendment80", "bsai_trawl")],
            aes(x = yellowfin_ITAC/1000, y = value/1000, color = variable)) +
  scale_color_manual(values = c("red","blue"), labels = c("Amendment 80","BSAI Trawl")) +
  scale_x_continuous(expand = c(0,0), breaks = c(seq(0,200,25))) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "Sub-sector", y = "Allocation (1000s of mt)", x = "Yellowfin sole ITAC (1000s of mt)") +
  geom_segment(data = ITAC_rates, aes(x = yellowfin_ITAC_min/1000, xend = yellowfin_ITAC_min/1000),y = 0, yend = 140, color = "grey", linetype = "dashed") +
  geom_text(data = ITAC_rates[yellowfin_ITAC_min >0 & yellowfin_ITAC_min <125000,], aes(x = yellowfin_ITAC_min/1000+4, label = ITAC_Rate_char),y = 145, angle = 90, size = 2) +
  geom_text(data = ITAC_rates[yellowfin_ITAC_min ==0 | yellowfin_ITAC_min ==125000,], aes(x = yellowfin_ITAC_min/1000+45, label = ITAC_Rate_char),y = 145, angle = 90, size = 2) +
  theme_classic() +
  theme(legend.position = "top", legend.direction = "horizontal", legend.justification = "center", plot.margin = margin(0,10,0,0))

ggsave(yellowfin_amend80_biomass_allocation, path = file.path("figures","summaries"), filename = "yellowfin_amend80_biomass_allocation.jpg",
       height = 5, width = 5, unit = "in")

#pull out legend separate
yellowfin_allocation_legend <- get_legend(yellowfin_amend80_biomass_allocation)

#merge two plots without legends
yellowfin_amend80_allocation_merge <- plot_grid(yellowfin_amend80_percent_allocation + theme(legend.position = "null"), yellowfin_amend80_biomass_allocation + theme(legend.position = "null"),
                                                ncol = 2, labels = c("a.","b."), label_size = 10, label_x = -0.01)


#add legend
yellowfin_amend80_allocation_merge.l <- plot_grid(yellowfin_allocation_legend, yellowfin_amend80_allocation_merge, ncol = 1, rel_heights = c(1,30))

#save
ggsave(yellowfin_amend80_allocation_merge.l, path = file.path("figures","summaries"), filename = "yellowfin_amend80_allocation_merge.l.jpg",
       height = 4, width = 6.5, unit = "in")

