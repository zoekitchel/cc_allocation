
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/fmps/pfmc/data"
plotdir <- "data/fmps/pfmc/figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Salmon_FMP_Table5-4.xlsx"))

# Build data
data1 <- data_orig %>% 
  select(total, rec_perc, comm_perc) %>% 
  gather(key="sector", value="perc", 2:ncol(.)) %>% 
  mutate(sector=recode(sector,
                       "comm_perc"="Commercial",
                       "rec_perc"="Recreational"))

# Build data
data2 <- data_orig %>% 
  select(total, rec_n, comm_n) %>% 
  gather(key="sector", value="n", 2:ncol(.)) %>% 
  mutate(sector=recode(sector,
                       "comm_n"="Commercial",
                       "rec_n"="Recreational"))


# Plot check
################################################################################

# Plot data
g1 <- ggplot(data1, aes(x=total, y=perc, fill=sector)) +
  geom_bar(stat="identity") +
  labs(x="Total harvest (1000s of fish)", y="Percent of harvest", tag="A") +
  theme_bw()
g1

# Plot data
g2 <- ggplot(data2, aes(x=total, y=n, fill=sector)) +
  geom_bar(stat="identity") +
  labs(x="Total harvest (1000s of fish)", y="Number of fish", tag="B") +
  theme_bw()
g2


# Plot figure
################################################################################

# Function
n <- 300000
calc_rec_perc <- function(n){
  
  n_rec <- case_when(n < 150000 ~ n,
                     n >= 150000 & n < 350000 ~ 150000 + 0.333*(n-150000),
                     n >= 350000 & n < 800000 ~ 217000 + 0.15*(n-350000),
                     n >= 800000 ~ 280000 + 0.10*(n-800000),
                     T ~ NA)
  
  perc_rec <- n_rec/n
  perc_rec 
  
}

# Build data
data <- tibble(harvest_tot=seq(1, 3e6, 10),
               rec_perc=calc_rec_perc(harvest_tot),
               comm_perc=1-rec_perc,
               rec_n=harvest_tot*rec_perc,
               comm_n=harvest_tot*comm_perc)


# Plot data
g1 <- ggplot(data1 %>% filter(sector=="Recreational"), aes(x=total*1000, y=perc/100)) +
  geom_point() +
  geom_line(data=data, mapping=aes(x=harvest_tot, y=rec_perc), inherit.aes = F) +
  labs(x="Total harvest", y="Percent of harvest", tag="A") +
  theme_bw()
g1

# Plot figure
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g1 <- ggplot(data, aes(x=harvest_tot/1000, y=rec_perc)) +
  # Reference lines
  geom_vline(xintercept=c(150, 350, 800), linetype="dashed", color="grey70") +
  # Data
  geom_line(stat="identity") +
  # Labels
  labs(x="Total harvest (1000s of fish)", y="Recreational percent", tag="A") +
  scale_y_continuous(lim=c(0,1), labels=scales::percent) +
  # Theme
  theme_bw() + my_theme
g1

# Build data
data_n <- data %>% 
  select(harvest_tot, rec_n, comm_n) %>% 
  gather(key="sector", value="n", 2:3) %>% 
  mutate(sector=recode(sector,
                       "comm_n"="Commercial",
                       "rec_n"="Recreational"))

# Plot data
g2 <- ggplot(data_n, aes(x=harvest_tot/1000, y=n/1000, group=sector, color=sector)) +
  # Reference lines
  geom_vline(xintercept=c(150, 350, 800), linetype="dashed", color="grey70") +
  # Data
  geom_line() +
  # Labels
  labs(x="Total harvest (1000s of fish)", y="Harvest (1000s of fish)", tag="B") +
  scale_color_discrete(name="Sector") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.5,0.8))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export figure
ggsave(g, filename=file.path(plotdir, "coho_south_falcon_rec_percent.png"), 
       width=6.5, height=3, units="in", dpi=600)





