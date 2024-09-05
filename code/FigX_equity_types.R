# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data"
plotdir <- "figures"


# Build data
################################################################################

# Function
vonb <- function(linf, k,x){linf * (1 - exp(-k*x))}

# Simulate
x <- seq(0,10,0.1)
comm <- vonb(linf=140, k=0.2, x=x)
rec <- vonb(linf=110, k=0.15, x=x)

# Prepare data for plot one
data1 <- tibble(quota_comm=x,
               value_comm=comm,
               value_rec=rev(rec)) %>% 
  mutate(value_tot=value_comm+value_rec) %>% 
  # Gather
  gather(key="type", value="value", 2:ncol(.)) %>% 
  # Recode
  mutate(type=recode_factor(type,
                            "value_tot"="Total",
                            "value_rec"="Recretational",
                            "value_comm"="Commercial"))

# Optimal commerical quota
comm_q_opt <- data1 %>% 
  filter(type=="Total") %>% 
  arrange(desc(value)) %>% 
  slice(1) %>% 
  pull(quota_comm)

# Optimal points
data1_opt <- data1 %>% 
  filter(quota_comm==comm_q_opt)

# Read data
data2 <- readxl::read_excel(file.path(datadir, "Fig4_data.xlsx")) %>%
  filter(category=="Horizontal") %>% 
  # Format sector
  mutate(sector=factor(sector, levels=c("Subsistence", "Recreational", "Commercial"))) %>% 
  # Format type
  mutate(type=recode_factor(type,
                            "Initial allocation"="Initial allocation",
                            "Re-allocation with better rec. catch data"="Re-allocation with\nbetter rec. catch data",
                            "Re-allocation with addition of new sector"="Re-allocation with\naddition of new sector"))

# Read data
data3 <- readxl::read_excel(file.path(datadir, "Fig4_data.xlsx")) %>% 
  filter(category=="Vertical") %>% 
  # Format sector
  mutate(sector=factor(sector, levels=c("Recreational", "Commercial"))) %>% 
  # Format type
  mutate(type=recode_factor(type,
                            "Initial allocation"="Initial allocation",
                            "Re-allocation to offset commercial climate impacts"="Re-allocation to offset\ncommercial climate impacts",
                            "Re-allocation to offset historical exclusion of rec. sector"="Re-allocation to offset\nhistorical exclusion of rec. sector"))

# Read data
data4 <- readxl::read_excel(file.path(datadir, "Fig4_data.xlsx")) %>% 
  filter(category=="Other") %>% 
  # Format sector
  mutate(sector=factor(sector, levels=c("Recreational", "Commercial"))) %>% 
  # Format type
  mutate(type=recode_factor(type,
                            "Initial allocation"="Initial allocation",
                            "Re-allocation to promote food production/sovereignty"="Re-allocation to promote\nfood production/sovereignty",
                            "Re-allocation to reduce protected species bycatch"="Re-allocation to reduce\nprotected species bycatch"))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=6),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   plot.title=element_text(size=7, face="bold"),
                   plot.subtitle = element_text(size=6, face="italic", color="grey50"),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data1, aes(x=quota_comm, y=value, color=type)) +
  # Ref line
  geom_vline(xintercept=comm_q_opt, color="grey50", linetype="dotted") +
  # Lines
  geom_line() +
  # Points
  geom_point(data=data1_opt, mapping=aes(fill=type), color="black", pch=21) +
  # Scales
  labs(x="Commercial quota\n\n\n\n\n\n\n\n\n", y="Net economic value", 
       title="A. Economic efficiency", 
       subtitle="Distribution of welfare\nmaximizes net societal benefit") +
  # X-axis
  scale_x_continuous(breaks=0:10,
                     sec.axis = sec_axis(~ 10 - ., name = "Recreational quota", breaks=0:10)) +
  # Legend
  scale_color_manual(name="", values=c("#f58b1f", "#a60434", "#00b9ba")) +
  scale_fill_manual(name="", values=c("#f58b1f", "#a60434", "#00b9ba")) +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position = "none")
g1

# Plot data
g2 <- ggplot(data2, aes(x=type, y=prop, fill=sector)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="\n\n\n", y="% of quota", 
       title="B. Horizontal equity", 
       subtitle="Distribution of welfare\nmaintained post-intervention") +
  # Legend
  scale_fill_manual(name="", values=c("#f58b1f", "#a60434", "#00b9ba")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g2

# Plot data
g3 <- ggplot(data3, aes(x=type, y=prop, fill=sector)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="% of quota", 
       title="C. Vertical equity", 
       subtitle="Distribution of welfare\nchanges post-intervention") +
  # Legend
  scale_fill_manual(name="", values=c("#a60434", "#00b9ba")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g3

# Plot data
g4 <- ggplot(data4, aes(x=type, y=prop, fill=sector)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="\n", y="% of quota", 
       title="D. Other considerations", 
       subtitle="Distribution of welfare\nchanges post-intervention") +
  # Legend
  scale_fill_manual(name="", values=c("#a60434", "#00b9ba")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g4

# Merge
width1 <- 0.35
g <- gridExtra::grid.arrange(g1, g2, g3, g4, widths=c(width1, rep((1-width1)/3, 3)), nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigX_equity_schematic.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
 

