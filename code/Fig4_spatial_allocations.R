
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
         spatial_type=factor(spatial_type, levels=c("Country", "State", "Area", "Country-area")))

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
                   plot.title=element_text(size=7),
                   plot.tag=element_text(size=7),
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
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_ordinal(name="Spatial allocation type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g1



# Plot percents
################################################################################

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
         country=factor(country, levels=c("United States", "Canada", "Mexico")))

# Plot country data
g2 <- ggplot(cdata3, aes(x=perc, y=comm_name, fill=country)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Percent of quota", y="", title="Country allocations", tag="B") +
  # Legend
  scale_fill_discrete(name="Country") +
  # Theme
  theme_bw() + my_theme
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
  mutate(state=recode_factor(state, 
                      "ME"="Maine",
                      "NH"="New Hampshire",
                      "MA"="Massachusetts",
                      "RI"="Rhode Island",
                      "CT"="Connecticut",
                      "ME-CT"="Conn.-Maine",
                      "NY"="New York",
                      "NJ"="New Jersey",
                      "DE"="Delaware",
                      "MD"="Maryland",
                      "VA"="Virgina",
                      "NC"="North Carolina",
                      "SC"="South Carolina",
                      "GA"="Georgia",
                      "FL"="Florida",
                      "AL"="Alabama",
                      "MS"="Mississippi",
                      "LA"="Louisiana",
                      "TX"="Texas"))

# Colors
ne_cols <- RColorBrewer::brewer.pal(6, "Blues") %>% rev()
ma_cols <- RColorBrewer::brewer.pal(6, "Greens") %>% rev()
sa_cols <- RColorBrewer::brewer.pal(3, "Purples")[1:2] %>% rev()
gom_cols <- RColorBrewer::brewer.pal(5, "Reds") %>% rev()
cols <- c(ne_cols, ma_cols, sa_cols, gom_cols)
  
# Plot state data
g3 <- ggplot(sdata, aes(x=perc, y=comm_name, fill=state)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Percent of quota", y="", title="State allocations", tag="C") +
  # Legend
  scale_fill_manual(name="Country", values=cols) +
  # Theme
  theme_bw() + my_theme
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
                             T ~ comm_name))

# Plot state data
g4 <- ggplot(adata, aes(x=perc, y=comm_name, fill=area)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", color="grey30") +
  # Labels
  labs(x="Percent of quota", y="", title="Area allocations", tag="D") +
  # Legend
  scale_fill_discrete(name="Area") +
  # Theme
  theme_bw() + my_theme
g4
 
 
# Merge data
#####################################

# Merge percents
g <- gridExtra::grid.arrange(g2, g3, g4, ncol=1)

# Merge all
layout_matrix <- matrix(data=c(1,2,
                               3,2), nrow=2, byrow=T)
g_out <- gridExtra::grid.arrange(g1, g, layout_matrix=layout_matrix)

# g_out <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=2)


# Export
ggsave(g_out, filename=file.path(plotdir, "Fig4_spatial_allocations.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




