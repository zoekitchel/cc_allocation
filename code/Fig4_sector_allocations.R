
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
# Atlantic bluefun tuna - just says commercial
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



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=6),
                   plot.tag = element_text(size=7),
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
g1 <- ggplot(data=data_perc %>% filter(!is.na(percent)), aes(x=percent, y=stock, fill=sector)) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_bar(stat="identity") +
  # Reference line
  geom_vline(xintercept=0.5, linetype="dashed") +
  # Labels
  labs(x="Percent of quota", y="", tag="A") +
  scale_x_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Sector") +
  guides(fill = guide_legend(title.position="top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot reference periods
g2 <- ggplot(data=data_yrs %>% filter(!is.na(year1))) +
  facet_grid(council_lead~., space="free_y", scales="free_y") +
  geom_segment(mapping=aes(x=year1, xend=year2, y=stock, color=period)) +
  # Labels
  labs(x="Year", y="", tag="B") +
  # Legend
  scale_color_manual(name="Reference period", values=c("black", "red")) +
  guides(color= guide_legend(title.position="top", title.hjust = 0.5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.6, 0.4))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_sector_allocations.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




