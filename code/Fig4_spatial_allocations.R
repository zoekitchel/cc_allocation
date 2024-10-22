
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

# Read spatial database for years
data_yrs <- readxl::read_excel(file.path(datadir, "spatial_database.xlsx")) 


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Simplify columns
  select(council, council_lead, fmp, stock_orig, stock, comm_name, area,
         spatial_yn:area_list) %>% 
  # Recode council
  mutate(council_lead=recode(council_lead,
                             "WPFMC"="Western Pacific",
                             "SAFMC"="South Atlantic",
                             "PFMC"="Pacific",
                             "NPFMC"="North Pacific",
                             "NEFMC"="New England",
                             "MAFMC"="Mid-Atlantic",
                             "GMFMC"="Gulf of Mexico"))


# Type stats
stats_type <- data %>% 
  # Count
  group_by(council_lead, spatial_type) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  # Proportion
  group_by(council_lead) %>% 
  mutate(prop=n/sum(n)) %>% 
  ungroup() %>% 
  # Reduce
  filter(spatial_type!="none") %>% 
  # Format type
  mutate(spatial_type=stringr::str_to_sentence(spatial_type),
         spatial_type=recode(spatial_type, "Country-area"="Country and area"),
         spatial_type=factor(spatial_type, levels=c("Country", "State", "Area", "Country and area")))

# Determine order
stats_type_council_order <- stats_type %>% 
  group_by(council_lead) %>% 
  summarise(prop=sum(prop)) %>% 
  ungroup() %>% 
  arrange(desc(prop)) %>% 
  pull(council_lead) 

table(data$spatial_type)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=7, margin=margin(b = 1, unit = "pt")),
                   plot.tag=element_text(size=7),
                   # Facets
                   panel.margin.y=unit(0.2, "lines"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot types by council
g1 <- ggplot(stats_type, aes(x=prop, 
                             y=factor(council_lead, levels=stats_type_council_order), 
                             fill=spatial_type)) +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="Spatial allocation type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.margin = margin(t=-5, b=-5, r=0, l=0),
        plot.margin=margin(t=1, r=2, b=1, l=4))
g1


# Plot years
################################################################################

# Format
data_yrs_plot <- data_yrs %>% 
  mutate(council=recode_factor(council,
                               "New England"="NE",
                               "Mid-Atlantic"="Mid-Atlantic",
                               "South Atlantic"="South Atlantic",
                               "Gulf of Mexico"="GoM",
                               "Pacific"="Pacific"))

g5 <- ggplot(data_yrs_plot %>% filter(level=="country"), aes(y=comm_name, x=year1, xend=year2)) +
  facet_grid(council~., space="free_y", scales="free_y") +
  geom_segment() +
  # Text
  geom_text(mapping=aes(y=comm_name, x=1965, label=notes), 
            hjust=0, fontface="italic", color="grey60", size=2) +
  # Labels
  labs(x="Year", y="", title=" ", tag=" ") +
  scale_x_continuous(lim=c(1965, 2020), breaks=seq(1960, 2020, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank())
g5

g6 <- ggplot(data_yrs_plot %>% filter(level=="state"), aes(y=comm_name, x=year1, xend=year2)) +
  facet_grid(council~., space="free_y", scales="free_y") +
  geom_segment() +
  # Text
  geom_text(mapping=aes(y=comm_name, x=1965, label=notes), 
            hjust=0, fontface="italic", color="grey60", size=2) +
  # Labels
  labs(x="Year", y="", title=" ", tag=" ") +
  scale_x_continuous(lim=c(1965, 2020), breaks=seq(1960, 2020, 10)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank())
g6

g7 <- ggplot(data_yrs_plot %>% filter(level=="area"), aes(y=comm_name, x=year1, xend=year2)) +
  facet_grid(council~., space="free_y", scales="free_y") +
  geom_segment() +
  # Text
  geom_text(mapping=aes(y=comm_name, x=1965, label=notes), 
            hjust=0, fontface="italic", color="grey60", size=2) +
  # Labels
  labs(x="Year", y="", title=" ", tag=" ") +
  scale_x_continuous(lim=c(1965, 2020), breaks=seq(1960, 2020, 10)) +
  # Theme
  theme_bw() + my_theme + 
  theme(axis.text.y = element_blank())
g7


# Plot percents
################################################################################

# Max char
nchar_max <- nchar("King mackerel (Gulf of Mexico)") + 7

# Build data
pdata <- data %>% 
  # Filter
  filter(spatial_yn=="yes") %>% 
  # Simplify
  select(council:area, country_list, state_list, area_list) %>% 
  # Gather
  gather(key="type", value="values", 8:ncol(.)) %>% 
  # Reduce
  filter(grepl("%", values))

# Build country data
cdata <- pdata %>% 
  filter(type=="country_list") %>% 
  separate(values, sep=", ", into=c("country1", "country2"), remove=T) 
cdata1 <- cdata %>% select(council:area, country1) %>% rename(country=country1)
cdata2 <- cdata %>% select(council:area, country2) %>% rename(country=country2)
cdata3 <- bind_rows(cdata1, cdata2) %>% 
  separate(country, sep=" \\(", into=c("country", "perc")) %>% 
  mutate(perc=gsub("%\\)", "", perc) %>% as.numeric() / 100) %>% 
  mutate(country=recode(country, "US"="United States"),
         country=factor(country, levels=c("Canada", "United States", "Mexico"))) %>% 
  # Pad common name
  mutate(comm_name=stringr::str_pad(comm_name, width=nchar_max, side="left", pad=" ")) %>% 
  # Format council
  mutate(council_lead=recode_factor(council_lead,
                                    "New England"="NE",
                                    "Pacific"="Pacific"))

# Plot country data
g2 <- ggplot(cdata3, aes(x=perc, y=comm_name, fill=country)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of quota", y="", title="Country allocations", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Country                   ", values=c("#3182BD", "#74C476", "#DE2D26")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x = element_blank(),
        plot.margin=margin(t=0, r=2, b=2, l=4),
        legend.title=element_text(size=7, margin=margin(b = 2, unit = "pt")))
g2

# State data
#####################################

# Build state data
sdata <- pdata %>% 
  filter(type=="state_list") %>% 
  separate(values, sep=", ", into=paste0("state", 1:14), remove=T) %>% 
  # Gather
  gather(key="type", value="values", 8:ncol(.)) %>% 
  select(-type) %>% 
  # Reduce
  filter(grepl("%", values)) %>% 
  # Split
  separate(values, sep=" \\(", into=c("state", "perc")) %>% 
  # Format
  mutate(perc=gsub("%\\)", "", perc) %>% as.numeric() / 100) %>% 
  # Format states
  mutate(state=factor(state, levels=c("ME", "NH", "MA", "RI", "CT", "ME-CT", 
                                      "NY", "NJ", "DE", "MD", "VA",
                                      "NC", "SC", "GA",
                                      "FL", "AL", "MS", "LA", "TX"))) %>% 
  # mutate(state=recode_factor(state, 
  #                     "ME"="Maine",
  #                     "NH"="New Hampshire",
  #                     "MA"="Massachusetts",
  #                     "RI"="Rhode Island",
  #                     "CT"="Connecticut",
  #                     "ME-CT"="Conn.-Maine",
  #                     "NY"="New York",
  #                     "NJ"="New Jersey",
  #                     "DE"="Delaware",
  #                     "MD"="Maryland",
  #                     "VA"="Virgina",
  #                     "NC"="North Carolina",
  #                     "SC"="South Carolina",
  #                     "GA"="Georgia",
  #                     "FL"="Florida",
  #                     "AL"="Alabama",
  #                     "MS"="Mississippi",
  #                     "LA"="Louisiana",
  #                     "TX"="Texas"))
  # Pad common name
  mutate(comm_name=stringr::str_pad(comm_name, width=nchar_max, side="left", pad=" ")) %>% 
  # Format councils
  mutate(council_lead=recode_factor(council_lead,
                                    "Mid-Atlantic"="Mid-Atlantic",
                                    "Gulf of Mexico"="GoM"))

# Colors
ne_cols <- RColorBrewer::brewer.pal(6, "Blues") %>% rev()
ma_cols <- RColorBrewer::brewer.pal(6, "Greens") %>% rev()
sa_cols <- RColorBrewer::brewer.pal(3, "Purples")[1:2] %>% rev()
gom_cols <- RColorBrewer::brewer.pal(5, "Reds") %>% rev()
cols <- c(ne_cols, ma_cols, sa_cols, gom_cols)
# "#08519C" "#3182BD" "#6BAED6" "#9ECAE1" "#C6DBEF" "#EFF3FF" 
# "#006D2C" "#31A354" "#74C476" "#A1D99B" "#C7E9C0" "#EDF8E9" 
# "#BCBDDC" "#EFEDF5"
# "#A50F15" "#DE2D26" "#FB6A4A" "#FCAE91" "#FEE5D9"  

# Plot state data
g3 <- ggplot(sdata, aes(x=perc, y=comm_name, fill=state)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of quota", y="", title="State allocations", tag="C") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="State", values=cols) +
  guides(fill = guide_legend(ncol = 2)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x = element_blank(),
        plot.margin=margin(t=0, r=2, b=2, l=4),
        legend.title=element_text(size=7, margin=margin(b = 2, unit = "pt")))
g3


# Area data
#####################################

# Build area data
adata <- pdata %>% 
  filter(type=="area_list") %>% 
  separate(values, sep=", ", into=paste0("state", 1:14), remove=T) %>% 
  # Gather
  gather(key="type", value="values", 8:ncol(.)) %>% 
  select(-type) %>% 
  # Reduce
  filter(grepl("%", values)) %>% 
  # Split
  separate(values, sep=" \\(", into=c("area", "perc")) %>% 
  # Format
  mutate(perc=gsub("%\\)", "", perc) %>% as.numeric() / 100) %>% 
  filter(!is.na(perc)) %>% # FIX Demersal Shelf Rockfish Complex - Gulf of Alaska
  # Format comm name
  mutate(comm_name=case_when(stock=="King mackerel - Southern Atlantic Coast" ~ "King mackerel (South Atlantic)",
                             stock=="King mackerel - Gulf of Mexico" ~ "King mackerel (Gulf of Mexico)",
                             T ~ comm_name)) %>% 
  # Pad common name
  # mutate(comm_name=stringr::str_pad(comm_name, width=nchar_max, side="left", pad=" ")) %>% 
  # Format council
  mutate(council_lead=recode_factor(council_lead,
                                    "New England"="NE",
                                    "South Atlantic"="South Atlantic")) %>% 
  # Format area names
  mutate(area=recode_factor(area,
                            "1A"="Herring 1A",
                            "1B"="Herring 1B",
                            "2"="Herring 2",
                            "3"="Herring 3",
                            "Northern Zone"="SA Northern",
                            "Southern Zone"="SA Southern",
                            "Northern"="GoM Northern",
                            "Southern"="GoM Southern",
                            "Western"="GoM Western",
                            "South Atlantic"="South Atlantic",
                            "Gulf of Mexico"="Gulf of Mexico"))

# Area colors
area_colors <- c(RColorBrewer::brewer.pal(4, "Blues"),
                 RColorBrewer::brewer.pal(3, "Greens")[1:2],
                 RColorBrewer::brewer.pal(4, "Reds")[1:3],
                 RColorBrewer::brewer.pal(3, "Greens")[3],
                 RColorBrewer::brewer.pal(4, "Reds")[4])
                 
  
# Plot area data
g4 <- ggplot(adata, aes(x=perc, y=comm_name, fill=area)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="Percent of quota", y="", title="Area allocations", tag="D") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_manual(name="Area                         ", values=area_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(plot.margin=margin(t=0, r=2, b=2, l=4),
        legend.title=element_text(size=7, margin=margin(b = 2, unit = "pt")))
g4
 

# Merge data
#####################################

# Merge percents
g <- gridExtra::grid.arrange(g2, g3, g4, ncol=1)

# Merge all
top_h <- 0.23
layout_matrix <- matrix(data=c(1,1,
                               2,3,
                               4,5,
                               6,7), ncol=2, byrow=T)
g_out <- gridExtra::grid.arrange(g1,
                                 g2, g5,
                                 g3, g6,
                                 g4, g7, layout_matrix=layout_matrix,
                                 heights=c(top_h, rep((1-top_h)/3, 3)),
                                 widths=c(0.6, 0.4))

# Export
ggsave(g_out, filename=file.path(plotdir, "Fig4_spatial_allocations.png"), 
       width=6.5, height=6.5, units="in", dpi=600)

