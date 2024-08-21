
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
data <- readRDS(file.path(datadir, "quota_allocations_database.Rds"))

# Theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=7),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=9),
                    plot.tag = element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Overall
################################################################################

# Overall
stats1 <- data %>% 
  # Simplify
  select(stock, allocation_yn_use, spatial_yn, sector_yn, subsector_yn, season_yn, shares_yn) %>% 
  # Gather
  gather(key="alloc_type", value="alloc_yn", 2:ncol(.)) %>% 
  # Summarize
  group_by(alloc_type) %>% 
  summarize(n=sum(alloc_yn=="yes"),
            n_tot=n(),
            p=n/n_tot) %>% 
  # Format type
  mutate(alloc_type=recode(alloc_type,
                           "allocation_yn_use"="Any", 
                           "spatial_yn"="Spatial", 
                           "sector_yn"="Sector", 
                           "subsector_yn"="Subsector", 
                           "season_yn"="Seasonal", 
                           "shares_yn"="Catch shares"))  %>% 
  # Arrange
  arrange(desc(p))


# Plot overall
g1 <- ggplot(stats1, aes(y=reorder(alloc_type, desc(p)), x=p)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="A") +
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + base_theme
g1


# By council
################################################################################


# Overall
stats2 <- data %>% 
  # Simplify
  select(council_lead, stock, allocation_yn_use, spatial_yn, sector_yn, subsector_yn, season_yn, shares_yn) %>% 
  # Gather
  gather(key="alloc_type", value="alloc_yn", 3:ncol(.)) %>% 
  # Summarize
  group_by(council_lead, alloc_type) %>% 
  summarize(n=sum(alloc_yn=="yes"),
            n_tot=n(),
            p=n/n_tot) %>% 
  # Format type
  mutate(alloc_type=recode(alloc_type,
                           "allocation_yn_use"="Any", 
                           "spatial_yn"="Spatial", 
                           "sector_yn"="Sector", 
                           "subsector_yn"="Subsector", 
                           "season_yn"="Seasonal", 
                           "shares_yn"="Catch shares")) %>% 
  # Format council
  mutate(council_lead=recode(council_lead,
                             "WPFMC"="Western Pacific",
                             "SAFMC"="South Atlantic",
                             "PFMC"="Pacific",
                             "NPFMC"="North Pacific",
                             "MAFMC"="Mid-Atlantic",
                             "SAFMC"="South Atlantic",
                             "GMFMC"="Gulf of Mexico",
                             "NEFMC"="New England",
                             "CFMC"="Caribbean"))

# Council order
council_order <- stats2 %>% 
  filter(alloc_type=="Any") %>% 
  arrange(desc(p))

# Order data
stats2_ordered <- stats2 %>% 
  # Order types
  mutate(alloc_type=factor(alloc_type, levels=stats1$alloc_type)) %>% 
  # Order councils
  mutate(council_lead=factor(council_lead, levels=council_order$council_lead))

# Plot data
g2 <- ggplot(stats2_ordered, aes(y=council_lead, x=p)) +
  facet_wrap(~alloc_type, scales="free_x") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Percent of stocks", y="", tag="B") +
  scale_x_continuous(labels=scales::percent) +
  # Theme
  theme_bw() + base_theme
g2


# Merge data
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.3, 0.7))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_policy_frequency.png"), 
       width=6.5, height=5.5, units="in", dpi=600)



# Sector allocations
################################################################################

extract_numbers <- function(text) {
  numbers <- gsub("[^0-9.-]", "", text)  # Remove all non-numeric characters except minus sign and decimal point
  numbers <- as.numeric(numbers)  # Convert the result to numeric
  return(numbers)
}

remove_percentage_in_parentheses <- function(text) {
  cleaned_text <- gsub("\\(\\s*\\d+(\\.\\d+)?%\\s*\\)", "", text)
  return(cleaned_text)
}

# Build data
data1 <- data %>% 
  # Data of interes
  filter(sector_yn=="yes") %>% 
  # Simplify
  select(council_lead, stock, comm_name, area, sector_yn:sector_notes) %>% 
  # Seperate
  separate(col=sector_list, into=c("sector1", "sector2"), sep=", ", remove=F) %>% 
  # Extract percentages
  mutate(sector1_perc=extract_numbers(sector1),
         sector2_perc=extract_numbers(sector2)) %>% 
  # Remove percentages
  mutate(sector1=remove_percentage_in_parentheses(sector1),
         sector2=remove_percentage_in_parentheses(sector2))

table(data1$sector1)
table(data1$sector2)



