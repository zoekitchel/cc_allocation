
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/database/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(datadir, "quota_allocations_database.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify columns
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         sector_yn:sector_notes) %>% 
  # Reduce to stocks with sector allocations
  filter(sector_yn=="yes")

# Bluefish - three periods
# South Atlantic spanish mackerel - but updated
# Atlantic bluefin tuna - just says commercial
# South Atlantic complexes - don't provide percents
# Alaska Chinook salmon - don't provide percents

# Build percent data
data_perc <- data %>% 
  # Simplify
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         sector_list) %>% 
  # Seperate sectors
  separate(sector_list, into=c("sector1_info", "sector2_info"), sep=", ", remove=T) %>% 
  # Separate sector 1
  separate(sector1_info, into=c("sector1", "sector1_perc"), sep=" \\(", remove=F) %>% 
  mutate(sector1_perc=gsub("\\%)", "", sector1_perc) %>% as.numeric() / 100) %>% 
  # Separate sector 2
  separate(sector2_info, into=c("sector2", "sector2_perc"), sep=" \\(", remove=F) %>% 
  mutate(sector2_perc=gsub("\\%)", "", sector2_perc) %>% as.numeric() / 100) %>% 
  # Simplify
  rename(comm_perc=sector1_perc, 
         rec_perc=sector2_perc) %>% 
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area, comm_perc, rec_perc) %>% 
  # Check
  mutate(check=comm_perc+rec_perc) %>% 
  select(-check) %>% 
  # Gather
  gather(key="sector", value="percent", 8:9) %>% 
  mutate(sector=recode_factor(sector,
                              "comm_perc"="Commercial",
                              "rec_perc"="Recreational"))

# Build year data
data_yrs <- data %>% 
  # Simplify
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         sector_yrs) %>% 
  # Format years
  mutate(sector_yrs=case_when(sector_yrs=="not specified" ~ NA, 
                              sector_yrs=="based on stakeholder input" ~ NA,
                              T ~ sector_yrs)) %>% 
  mutate(sector_yrs=gsub(", but updated", "", sector_yrs)) %>% 
  # Seperate blocks
  separate(sector_yrs, into=c("block1", "block2"), sep=", ", remove=F) %>% 
  # Separate block 1
  separate(block1, into=c("block1_yr1", "block1_yr2"), sep="-", remove=F, convert = T) %>% 
  # Separate block 2
  separate(block2, into=c("block2_yr1", "block2_yr2"), sep="-", remove=F, convert = T) %>% 
  # Simplify
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         block1_yr1, block1_yr2, block2_yr1, block2_yr2) %>% 
  # Gather
  gather(key="year_type", value="year", 8:ncol(.)) %>% 
  # Format
  mutate(period=ifelse(grepl("block1", year_type), "Period 1", "Period 2"),
         period_yr=ifelse(grepl("yr1", year_type), "year1", "year2")) %>% 
  # Spread
  select(-year_type) %>% 
  spread(key="period_yr", value="year") %>% 
  # Arrange
  arrange(council_lead, stock, period)


# Prepare data
################################################################################

# Prepare data
data_perc_most <- data_perc %>% 
  # Reduce
  filter(!is.na(percent) & council_lead!="CFMC") %>% 
  # Recode
  mutate(council_lead=recode_factor(council_lead, 
                                    "NEFMC"="New\nEngland",
                                   "MAFMC"='Mid-\nAtlantic',
                                   "SAFMC"="South\nAtlantic",
                                   "GMFMC"="Gulf of\nMexico")) %>% 
  # Format stocks
  mutate(stock=recode(stock,
                      "Spanish mackerel - Gulf of Mexico" ="Spanish mackerel (Gulf of Mexico)",
                      "Spanish mackerel - Southern Atlantic Coast" ="Spanish mackerel (South Atlantic)",
                      "Hogfish - Carolinas" = "Hogfish (Carolinas)",
                      "Hogfish - Florida Keys / East Florida" = "Hogfish (Florida)",
                      "King mackerel - Southern Atlantic Coast" = "King mackerel (South Atlantic)", 
                      "King mackerel - Gulf of Mexico" = "King mackerel (Gulf of Mexico)", 
                      "Haddock - Gulf of Maine"="Haddock (Gulf of Maine)",
                      "Atlantic cod - Gulf of Maine"="Atlantic cod (Gulf of Maine)"),
         stock=gsub(" - Southern Atlantic Coast / Gulf of Mexico", "", stock),
         stock=gsub(" - Southern Atlantic Coast", "", stock),
         stock=gsub(" - Atlantic Coast", "", stock),
         stock=gsub(" - Mid-Atlantic Coast", "", stock),
         stock=gsub(" - Gulf of Mexico", "", stock))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.tag = element_text(size=7),
                   plot.title=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "top",
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot percentages
g1 <- ggplot(data=data_perc_most , aes(x=percent, y=stock, fill=sector)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=0.5, linetype="dashed") +
  # Labels
  labs(x="Percent of quota", y="", tag="A", title="Explicit sector allocations") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Sector") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank())
g1

# Plot reference periods
g2 <- ggplot(data=data_yrs %>% filter(!is.na(year1))) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_segment(mapping=aes(x=year1, xend=year2, y=stock, color=period)) +
  # Labels
  labs(x="Year", y="", tag="B", title=" ") +
  # Legend
  scale_color_manual(name="Reference period", values=c("black", "red")) +
  guides(color= guide_legend(title.position="top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Caribbean
###################################################

# Prepare Caribbean data
data_car <- data_perc %>% 
  filter(!is.na(percent) & council=="CFMC") %>% 
  mutate(council_lead=recode(council_lead, 
                        "CFMC"="Caribbean"),
         stock=gsub(" - Puerto Rico", "", stock)) 
car_stats <- data_car %>% 
  filter(sector=="Recreational") %>% 
  arrange(desc(percent))

# Order Caribbean data
data_car_ordered <- data_car %>% 
  mutate(stock=factor(stock, level=car_stats$stock))

# Plot Caribbean percentages
g3 <- ggplot(data=data_car_ordered, aes(x=percent, y=stock, fill=sector)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=0.5, linetype="dashed") +
  # Labels
  labs(x="Percent of quota", y="", tag="C", title="Puerto Rico - implicit sector allocations") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Sector") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        axis.title.y=element_blank())
g3


# Pacific salmon
###################################################


# Coho - South of Cape Falcon
n <- 300000
calc_rec_perc_coho_s <- function(n){
  
  n_rec <- case_when(n < 150000 ~ n,
                     n >= 150000 & n < 350000 ~ 150000 + 0.333*(n-150000),
                     n >= 350000 & n < 800000 ~ 217000 + 0.15*(n-350000),
                     n >= 800000 ~ 280000 + 0.10*(n-800000),
                     T ~ NA)
  
  perc_rec <- n_rec/n
  perc_rec 
  
}


# Coho - North of Cape falcon 
calc_rec_perc_coho_n <- function(n){
  perc_rec <- ifelse(n<=300000, 0.7, 0.4)
}

# Chinook - North of Cape Falcon
calc_rec_perc_chinook_n <- function(n){
  perc_rec=case_when(n <=100000 ~ 0.5,
                     n>100000 & n <=150000 ~ 0.4,
                     n>150000 ~ 0.3,
                     T ~ NA)
  perc_rec
}

# Build data
data_salmon <- tibble(harvest_tot=seq(1, 2e6, 10),
               coho_n=calc_rec_perc_coho_n(harvest_tot),
               coho_s=calc_rec_perc_coho_s(harvest_tot),
               chinook_n=calc_rec_perc_chinook_n(harvest_tot)) %>% 
  gather(key="stock", value="perc_rec", 2:ncol(.)) %>% 
  mutate(stock=recode_factor(stock, 
                             "coho_s"="Coho salmon (South of Cape Falcon)",
                             "coho_n"="Coho salmon (North of Cape Falcon)",
                             "chinook_n"="Chinook salmon (North of Cape Falcon)"))


# Plot data
g4 <- ggplot(data_salmon, aes(x=harvest_tot/1000, y=perc_rec, color=stock)) +
  geom_line() +
  # Labels
  labs(x="Catch limit (1000s of fish)", y="Percent to recreational fishery", tag="D", 
       title="Pacific salmon - dynamic sector allocations") +
  scale_y_continuous(labels=scales::percent_format(), lim=c(0,1)) +
  # Legend
  scale_color_discrete(name="Stock") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g4


# Merge
###################################################

# Merge
layout_matrix <- matrix(c(1, 2,
                          3, 4), nrow=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, 
                             g3, g4, 
                             layout_matrix=layout_matrix, 
                             widths=c(0.6, 0.4), 
                             heights=c(0.6, 0.4))
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig4_sector_allocations_new.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




