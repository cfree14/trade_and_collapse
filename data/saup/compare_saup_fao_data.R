
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir  <-"/Users/cfree/Dropbox/Chris/UCSB/data/saup/processed"
plotdir <- "data/saup"

# Read SAUP data
data_orig <- readRDS(file.path(datadir, "SAUP_1950_2018_catch_data_by_eez.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Summarize reported/unreported by area, species, year
  group_by(area_name, area_type, scientific_name, common_name, year, reporting_status) %>% 
  summarize(catch_mt=sum(tonnes)) %>% 
  ungroup() %>% 
  # Spread
  spread(key="reporting_status", value="catch_mt", fill=0) %>% 
  # Calculate total
  mutate(saup_mt=Reported+Unreported) %>% 
  rename(fao_mt=Reported) %>% 
  select(-Unreported) %>% 
  # Ration
  mutate(ratio=saup_mt/fao_mt)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=ratio, group=year)) +
  geom_boxplot(outlier.size=0.5, size=0.2) +
  # Labels
  labs(x="Year", y="SAUP / FAO catch ratio") +
  # Axis
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_y_continuous(trans="log10", breaks=10^(0:12), labels=parse(text=paste0("10^", 0:12))) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_saup_fao_catch_ratio_by_stock.png"), 
       width=6.5, height=3, units="in", dpi=600)



