
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel("data/fmps/pfmc/data/GMT015-final specifications-2024.xlsx",
                                skip=6, na="-")


# Format data
################################################################################

# Function to replace NAs with zeros
replace_na_with_zero <- function(vec){
  vec <- ifelse(is.na(vec), 0, vec)
}

# Rockfish species
rockfish_spp <- c("Aurora", "Bank", "Blue", "Bocaccio", "Bronzespotted", "Brown", "Calico", "Chameleon", "Chilipepper", "China", "Copper",
                  "Flag", "Freckled", "Gopher", "Grass", "Greenblotched", "Greenspotted", "Greenstriped", 
                  "Halfbanded", "Harlequin", "Honeycomb", "Kelp", "Mexican", "Olive", "Pink", "Pinkrose", "Pygmy", "Puget sound", 
                  "Quillback", "Redbanded", "Redstripe", "Rosethorn", "Rosy", "Rougheye/blackspotted", "Roughtail", 
                  "Sharpchin", "Shortbelly", "Shortraker", "Silvergray", "Speckled", "Splitnose", "Squarespot",
                  "Starry", "Stripetail", "Swordspine", "Tiger", "Vermilion", "Yellowmouth")

# Format data
data <- data_orig %>% 
  # Clean name
  janitor::clean_names("snake") %>% 
  rename(year=spex_year,
         comm_name=stock_or_complex,
         status=stock_status,
         pstar=probability,
         abc_buffer=abc_buffer_fraction,
         ofl_mt=ofl,
         abc_mt=abc,
         acl_mt=annual_catch_limit, 
         hg_mt=fhg,
         act_mt=act, 
         # Set asides
         tribal_mt=tribal,
         efp_mt=efp,
         research_mt=research,
         open_access_mt=oa,
         buffer_mt=buffer,
         incidental_mt=incidental,
         set_aside_total_mt=set_aside_total,
         shorebased_ifq_mt=sb_ifq,
         
         catcher_processor_mt=cp,
         mothership_mt=ms
         ) %>% 
  # Replace NAs
  mutate_at(vars(tribal_mt:set_aside_total_mt), replace_na_with_zero) %>% 
  # Format area
  mutate(area=recode(area, 
                     "3427 - 42"="34°27'-42°", 
                     "4010 - 3427"="40°10'-34°27'", 
                     "4010 - 4616"="40°10'-46°16'",   
                     "42 - 4010"="42°-40°10'",         
                     "CW"="Coastwide",   
                     "N of 3427"="North of 34°27'",   
                     "N of 4010"="North of 40°10'",          
                     "S of 3427"="South of 34°27'",   
                     "S of 4010"="South of 40°10'",         
                     "WA - OR"="WA-OR")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=ifelse(comm_name %in% rockfish_spp, paste(comm_name, "rockfish"), comm_name),
         comm_name=recode(comm_name,
                          "Puget sound rockfish"="Puget Sound rockfish")) %>% 
  # Build stock
  mutate(stock=paste0(comm_name, " (", area, ")")) %>% 
  # Check set aside totals
  mutate(set_aside_total_mt_calc=tribal_mt+efp_mt+research_mt+open_access_mt+buffer_mt+incidental_mt,
         set_aside_total_mt_check=set_aside_total_mt_calc-set_aside_total_mt) %>% 
  # Check allocation percents
  # Remove empty
  select(-c(notes, ofl_document, notes, parent_row, spr)) %>% 
  # Arrange
  select(year, stock, comm_name, area, acl_code,
         assessment_year, assessment_type, assessment_author,
         status, category, pstar, abc_buffer,
         ofl_mt, abc_mt, acl_mt, hg_mt, act_mt, 
         everything()) %>% 
  # Remove the following after checking
  select(-c(set_aside_total_mt_calc, set_aside_total_mt_check))

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$year)
table(data$area)

# Inspect species
spp <- data %>% 
  count(comm_name)


# Plot data
################################################################################

# Prep set aside data
data_sa <- data %>% 
  # Simplify
  select(stock, tribal_mt:incidental_mt) %>% 
  # Gather
  gather(key="sector", value="catch_mt", 2:ncol(.)) %>% 
  # Calculate percent
  group_by(stock) %>% 
  mutate(perc=catch_mt/sum(catch_mt)) %>% 
  ungroup() %>% 
  # Simplify
  filter(perc!=0) %>% 
  # Format sector
  mutate(sector=recode_factor(sector,
                              "tribal_mt"="Tribal",
                              "open_access_mt"="Open access",
                              "research_mt"="Research",
                              "efp_mt"="EFP program"))

# Order data
stats <- data_sa %>% 
  select(-catch_mt) %>% 
  spread(key=sector, value=perc) %>% 
  arrange(desc(Tribal), desc(`Open access`))
data_sa_ordered <- data_sa %>% 
  mutate(stock=factor(stock, levels=stats$stock))
  

# Plot set asides
g1 <- ggplot(data_sa_ordered, aes(y=stock, x=perc, fill=sector)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Percent of 2024 set aside quota", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Sector") +
  # Theme
  theme_bw() +
  theme(legend.position = "top")
g1


# Prep trawl/nontrawl percent
data_trawl_perc <- data %>% 
  # Filter
  filter(!is.na(trawl_percent)) %>% 
  # Simplify
  select(stock, comm_name, allocation_type, trawl_percent, non_trawl_percent) %>% 
  # Gather
  gather(key="subsector", value="percent", 4:ncol(.)) %>% 
  mutate(subsector=recode_factor(subsector, 
                                 "trawl_percent"="Trawl",
                                 "non_trawl_percent"="Non-trawl"),
         percent=percent/100)

# Order data
stats1 <- data_trawl_perc %>% 
  filter(subsector=="Trawl") %>% 
  arrange(desc(percent))
data_trawl_perc_ordered <- data_trawl_perc %>% 
  mutate(stock=factor(stock, levels=stats1$stock))

# Plot data
g2 <- ggplot(data_trawl_perc_ordered, aes(y=stock, x=percent, fill=subsector)) +
  facet_grid(allocation_type~., space="free_y", scales="free_y") +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Percent of 2024 commercial quota", y="", tag="B") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Subsector") +
  # Theme
  theme_bw() + 
  theme(legend.position = "top")
g2
