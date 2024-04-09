
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/fmps/gfmc"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "gulf_of_mexico_acls.xlsx"), na="NA")

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  select(species, year, comm_acl_lbs, total_acl_lbs) %>% 
  mutate(comm=comm_acl_lbs/total_acl_lbs,
         rec=1-comm) %>% 
  select(species, year, comm, rec) %>% 
  gather(key="sector", value="perc", 3:4) %>% 
  filter(grepl("groupers", species) & !is.na(perc)) %>% 
  mutate(sector=recode_factor(sector,
                              "rec"="Recreational",
                              "comm"="Commercial")) %>% 
  mutate(perc_label=paste0(round(perc*100, 1), "%"),
         perc_label=ifelse(is.na(perc), NA, perc_label))

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=perc, fill=sector)) +
  facet_wrap(~species) +
  geom_bar(stat="identity") +
  geom_text(data=data %>% filter(sector=="Commercial"),
            mapping=aes(x=year, y=0, label=perc_label),
            angle=90, size=1.5,  hjust=0) +
  # Labels
  labs(x="Year", y="Percent of catch limit") +
  scale_x_continuous()
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.key.size = unit(0.5, "cm"))
g

# Export plot
ggsave(g, filename=file.path(datadir, "FigX_gmfc_comm_rec_percs.png"), 
       width=4.5, height=2.5, units="in", dpi=600)


