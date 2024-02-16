
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)

# Directories
datadir <- "data/morrison_scott"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Morrison_Scott_Appendix1.xlsx"), sheet=1)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(perc_notes=comm2rec_notes) %>% 
  # Format fishery
  # mutate(fishery=stringr::str_to_sentence(fishery)) %>% 
  # Format percents
  separate(col=comm2rec, into=c("perc_comm", "perc_rec"), sep=":", remove = F) %>% 
  mutate(perc_comm=gsub("%| ", "", perc_comm) %>% as.numeric(),
         perc_rec=gsub("%| ", "", perc_rec) %>% as.numeric(),
         perc_check=(perc_comm+perc_rec)==100) %>% 
  # Format category/note
  separate(col=category, into=c("hist_catch_yn", "basis_period"), sep="/", remove = F) %>%
  mutate(hist_catch_yn=recode(hist_catch_yn,
                              "N"="no",
                              "Y"="yes",
                              "RE"="")) %>% 
  mutate(basis_period=recode(basis_period,
                      "B"="before regulations impacted catch",
                      "L"="longest",
                      "NE"="unexplained",
                      "R"="most recent", 
                      "L&R"="longest and most recent",
                      "SQ"="current (status quo)")) %>% 
  # Break basis years
  separate(col=basis_years1, into=c("year1", "year2"), sep="-", convert=T, remove=F) %>% 
  separate(col=basis_years2, into=c("year3", "year4"), sep="-", convert=T, remove=F) %>% 
  # Simplify
  select(council, fmp, reg, reg_year, fishery, 
         perc_comm, perc_rec, perc_notes, 
         hist_catch_yn, basis_period, basis_years1, year1, year2, basis_years2, year3, year4) #everything())


# Plot data
################################################################################

# Prep data for plotting
############################################

# Recent data
data_recent <- data %>% 
  # Remove Pacific b/c no data
  filter(council!="PFMC") %>% 
  # Reduce to most recent rules
  group_by(council, fishery) %>% 
  arrange(council, fishery, desc(reg_year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Order councils
  mutate(council=recode_factor(council,
                               "NEFMC"="New\nEngland",
                               "MAFMC"="Mid-\nAtlantic",
                               "SAFMC"="South\nAtlantic",
                               "GMFMC"="Gulf of\nMexico"))

# Make data long
data_percs <- data_recent %>% 
  # Gather
  select(council:perc_rec) %>% 
  gather(key="sector", value="percent", 6:7) %>% 
  # Recode
  mutate(sector=recode_factor(sector,
                              "perc_comm"="Commercial",
                              "perc_rec"="Recreational")) %>% 
  # Format percent
  mutate(percent=percent/100) %>% 
  # Record rec percent
  group_by(council, fishery) %>% 
  mutate(perc_rec=percent[sector=="Recreational"]) %>% 
  ungroup()

# Period data
data_period <- data_recent %>% 
  # Simplify
  select(council:fishery, perc_rec, year1, year2, year3, year4) %>% 
  # Gather
  gather(key="year_type", value="year", 7:ncol(.)) %>% 
  # Add endpoint and period
  mutate(period=ifelse(year_type %in% c("year1", "year2"), "Reference", "Recent"),
         period=factor(period, levels=c( "Reference", "Recent")), 
         endpoint=ifelse(year_type %in% c("year1", "year3"), "start", "end")) %>% 
  select(-year_type) %>% 
  # Spread
  spread(key="endpoint",value="year")


# Plot data
############################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data_percs, aes(x=percent, 
                            y=tidytext::reorder_within(fishery, desc(perc_rec), council), 
                            fill=sector)) +
  facet_grid(council~., scales="free_y", space="free_y") +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept = 0.5, linetype="dashed", color="grey30") +
  # Labels
  labs(x="Percent of quota", y="") +
  tidytext::scale_y_reordered() +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "top",
        legend.key.size = unit(0.3, "cm"),
        legend.box.margin = margin(-3,0,-7,0))
g1

# Plot data
g2 <- ggplot(data_period) +
  facet_grid(council~., scales="free_y", space="free_y") +
  geom_segment(mapping=aes(x=start, 
                           xend=end, 
                           color=period,
                           y=tidytext::reorder_within(fishery, desc(perc_rec), council), 
                           yend=tidytext::reorder_within(fishery, desc(perc_rec), council)),
               linewidth=0.8) +
  geom_point(mapping=aes(x=reg_year, y=tidytext::reorder_within(fishery, desc(perc_rec), council))) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(lim=c(NA, 2020), breaks=seq(1975,2020,5)) +
  tidytext::scale_y_reordered() +
  # Legend
  scale_color_manual(name="", values=c("grey60", "red")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank()) +
  theme(legend.position = "top",
        legend.key.size = unit(0.3, "cm"),
        legend.box.margin = margin(-3,0,-7,0))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.6, 0.4))
g

# Export
ggsave(g, filename=file.path(datadir, "FigX_sector_based_rules.png"), 
       width=6.5, height=5, units="in", dpi=600)



