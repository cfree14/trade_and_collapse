
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
datadir <- "data/fao_landings/predictions"
tabledir <- "data/fao_landings/tables"
plotdir <- "data/fao_landings/figures"

# Read data
data <- read.csv(paste(datadir, "1950_2017_FAO_bbmsy_timeseries_merge.csv", sep="/"), as.is=T)



# Plot data
################################################################################

# Stocks
stocks <- sort(unique(data$stockid))

# COM params
display.brewer.pal(4, "Set1")
com_colors <- brewer.pal(4, "Set1")
com_cols <- c("cmsy13", "ocom", "comsir", "mprm")

# Setup figure
figname <- "AppendixB_bbmsy_time_series.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(8,6), mar=c(0.5,1.5,0.1,0.1), oma=c(4,4,4,4), mgp=c(2,0.8,0))

# Loop through stocks
# for(i in 1:48){
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(data, stockid==stock)
  if(i %in% seq(10, length(stocks), 10)){print(i)}
  
  # Setup empty plot
  ymax <- ceiling(max(sdata[,com_cols], na.rm=T) / 2) * 2
  plot(1:10, 1:10, type="n", bty="n", xaxt="n",
       xlim=c(1950, 2020), xlab="", ylab="", ylim=c(0,ymax), las=1)
  title(stock, cex.main=1, xpd=NA)
  # axis(1, at=seq(1950,2020,10), labels=F)
  
  # Loop through COMs
  for(j in 1:length(com_cols)){
    lines(x=sdata$year, y=sdata[,com_cols[j]], col=com_colors[j])
  }
  
  # Add superensemble model
  lines(x=sdata$year, y=sdata$super, col="black", lwd=1.1)

}

# Off
dev.off()
graphics.off()
