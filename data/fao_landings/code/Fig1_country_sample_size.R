
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(freeR)

# Directories
datadir <- "data/fao_landings/data"
tabledir <- "data/fao_landings/tables"
plotdir <- "data/fao_landings/figures"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Setup
################################################################################

# Country sample size
n_iso <- stocks %>% 
  group_by(iso3) %>% 
  summarize(n=n())
hist(n_iso$n, breaks=seq(0,120,5))

# Build spatial sample size data
data(wrld_simpl) # from maptools
data_orig <- wrld_simpl@data
n_iso1 <- data_orig %>% 
  left_join(n_iso, by=c("ISO3"="iso3")) %>% 
  mutate(n_bin=cut(n, breaks=c(seq(0,60,5),120)),
         n_color=colorpal(brewer.pal(9,"YlOrRd"), n=nlevels(n_bin))[n_bin],
         n_color=ifelse(is.na(n_color), "grey90", n_color))
wrld_simpl@data <- n_iso1

# Which FAO ISO3s aren't in this map?
fao_isos <- sort(unique(stocks$iso3))
map_isos <- sort(unique(data_orig$ISO3))
fao_isos[!fao_isos%in%map_isos]

# Plot data
################################################################################

# Setup figure
figname <- "Fig1_country_sample_size.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=4, units="in", res=600)
par(mar=c(0.1,0.1,0.1,0.1), xpd=NA)

# Plot data
plot(wrld_simpl, col=wrld_simpl$n_color, border="grey30", lwd=0.3)

# Add legend
breaks <- c(seq(0,60,5),120)
labels <- paste0(breaks[1:(length(breaks)-1)], "-", breaks[2:length(breaks)])
colors <- colorpal(brewer.pal(9,"YlOrRd"), n=nlevels(wrld_simpl$n_bin))
legend(x=-180, y=30, fill=colors, legend=labels, bty="o", cex=0.7, 
       title=expression(bold("# of stocks")), bg="white")

# Off
dev.off()
graphics.off()
