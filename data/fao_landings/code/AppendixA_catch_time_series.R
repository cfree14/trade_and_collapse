
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



# Plot data
################################################################################

# Setup figure
figname <- "AppendixA_catch_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(8,6), mar=c(0.5,0.1,0.1,0.1), oma=c(4,4,4,4), xpd=NA)

# Loop through stocks
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  if(i %in% seq(10, nrow(stocks), 10)){print(i)}
  
  # Plot data
  plot(tl_mt ~ year, sdata, type="l", bty="n", lwd=0.8, col="grey30",
       xlim=c(1950, 2020), xlab="", ylab="", xaxt="n", yaxt="n")
  # axis(1, at=seq(1950,2020,10), labels=F)
  title(stock, cex.main=1, xpd=NA)
  
}

# Off
dev.off()
graphics.off()
