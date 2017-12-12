

# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# READ DATA
################################################################################

# Packages
library(plyr)
library(dplyr)

# Define directories
datadir <- "~/Dropbox/Chris/Rutgers/projects/productivity/data/ramldb/ramldb_v3.8"
plotdir <- "~/Dropbox/Chris/Rutgers/projects/trade_and_collapse/figures"
outputdir <- "~/Dropbox/Chris/Rutgers/projects/trade_and_collapse/data"
boundarydir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/stock_boundaries/data"

# Read keys
# lme_key <- read.csv(paste(datadir, "ramldb_v38_lme_key.csv", sep="/"), as.is=T)
# taxa_key <- read.csv(paste(datadir, "ramldb_v38_taxonomy.csv", sep="/"), as.is=T)
stock_key <- read.csv(paste(datadir, "ramldb_v38_stock.csv", sep="/"), as.is=T)
# assessment_key <- read.csv(paste(datadir, "ramldb_v38_assessment.csv", sep="/"), as.is=T)
# assessor_key <- read.csv(paste(datadir, "ramldb_v38_assessor.csv", sep="/"), as.is=T)
area_key <- read.csv(paste(datadir, "ramldb_v38_area.csv", sep="/"), as.is=T)

# Read data
values <- read.csv(paste(datadir, "ramldb_v38_timeseries_values_views.csv", sep="/"), as.is=T)
units <- read.csv(paste(datadir, "ramldb_v38_timeseries_units_views.csv", sep="/"), as.is=T)
# bioparams_vals <- read.csv(paste(datadir, "ramldb_v38_bioparams_values_views.csv", sep="/"), as.is=T)
# bioparams_units <- read.csv(paste(datadir, "ramldb_v38_bioparams_units_views.csv", sep="/"), as.is=T)

# Read stock boundary centroid and area info
centroids <- read.csv(paste(boundarydir, "ramldb_v3.8_stock_boundary_centroids_areas.csv", sep="/"), as.is=T)
centroids <- select(centroids, -flag)


# READ DATA
################################################################################

# B/BMSY collapse threshold
bbmsy.collapse <- 0.2

# Build dataset
data <- as.data.frame(
  values %>%
    filter(!is.na(B.Bmsytouse)) %>%
    left_join(select(stock_key, stockid, scientificname, commonname, region, areaid), by="stockid") %>%
    left_join(select(area_key, areaid, areaname, country), by="areaid") %>%
    select(assessid, stockid, stocklong, country, region, areaname, 
           commonname, scientificname, year, B.Bmsytouse)  %>%
    rename(area=areaname, common_name=commonname, sci_name=scientificname, bbmsy=B.Bmsytouse)
)

# Build stock info
stock.info <- as.data.frame(
  data %>%
    group_by(assessid, stockid, stocklong, country, region, area, common_name, sci_name) %>%
    summarize(yr1=min(year),
              yr2=max(year),
              nyr=n(),
              collapse=ifelse(sum(bbmsy<=bbmsy.collapse)>0, "yes", "no"),
              nyrs.collapsed=sum(bbmsy<=bbmsy.collapse),
              yr.1st.collapse=min(year[bbmsy<=bbmsy.collapse]))  %>%
    left_join(centroids, by="assessid") %>%
    select(assessid, stockid, stocklong,
           country, region, area, lat_dd, long_dd, area_sqkm, everything()) %>%
    arrange(desc(nyr))
)
nstocks <- nrow(stock.info)
ncollapsed <- sum(stock.info$collapse=="yes")
nstocks; ncollapsed

# Export data
write.csv(data, paste(outputdir, "ramldb_v38_bbmsy_time_series.csv", sep="/"), row.names=F)
write.csv(stock.info, paste(outputdir, "ramldb_v38_stocks_with_bbmsy.csv", sep="/"), row.names=F)


# PLOT DATA
################################################################################

# Years
min(stock.info$yr1)
max(stock.info$yr2)

# Setup plot
figname <- paste0("ramldb_collapse_frequency_", bbmsy.collapse, ".png") 
png(paste(plotdir, figname, sep="/"), width=6, height=8, units="in", res=600)
par(mar=c(3.5, 2.5, 0.5, 0.5), mgp=c(2.4,1,0))

# Plot data
plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n",
     xlim=c(1860,2020), ylim=c(1,nrow(stock.info)), xlab="", ylab="")
axis(1, at=seq(1860,2020,20), las=2)
for(i in 1:nrow(stock.info)){
  stock <- stock.info$assessid[i]
  yr1 <- stock.info$yr1[i]
  yr2 <- stock.info$yr2[i]
  lines(x=c(yr1, yr2), y=c(i,i), col="grey70")
  collapse.yrs <- data$year[data$assessid==stock & data$bbmsy<=bbmsy.collapse]
  points(collapse.yrs, rep(i, length(collapse.yrs)), col="red", pch=15, cex=0.4)
}

# Add text
text1 <- paste0("Collapsed = ", bbmsy.collapse, "*BMSY")
text2 <- paste0(ncollapsed, " of ", nstocks, " collapsed")
text(x=1860, y=nstocks, labels=text1, pos=4)
text(x=1860, y=nstocks-8, labels=text2, pos=4)

# Off
dev.off()
graphics.off()

# PLOT COLLAPSED STOCKS
################################################################################

# Collapsed stocks
stocks.collapsed <- subset(stock.info, collapse=="yes")
stocks.collapsed <- arrange(stocks.collapsed, region, yr.1st.collapse)
stocks.collapsed$line.draw <- 1:ncollapsed + as.numeric(as.factor(stocks.collapsed$region)) - 1

# Years
min(stocks.collapsed$yr1)
max(stocks.collapsed$yr2)

# Unique regions
regions <- unique(stocks.collapsed$region)
nregions <- length(regions)

# Setup plot
figname <- paste0("ramldb_collapsed_stocks_", bbmsy.collapse, ".png") 
png(paste(plotdir, figname, sep="/"), width=6, height=5, units="in", res=600)
par(mar=c(3, 0.5, 0.5, 12.5), mgp=c(2.4,1,0))

# Plot data
plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n",
     xlim=c(1860,2020), ylim=c(1,ncollapsed+nregions), xlab="", ylab="")
axis(1, at=seq(1860,2020,20), las=2, cex.axis=0.8)
axis(4, at=stocks.collapsed$line.draw, labels=stocks.collapsed$stocklong, tick=F, las=1, cex.axis=0.55, line=-1)
for(i in 1:nrow( stocks.collapsed)){
  stock <- stocks.collapsed$assessid[i]
  yr1 <- stocks.collapsed$yr1[i]
  yr2 <- stocks.collapsed$yr2[i]
  line.draw <- stocks.collapsed$line.draw[i]
  lines(x=c(yr1, yr2), y=c(line.draw,line.draw), col="grey70")
  collapse.yrs <- data$year[data$assessid==stock & data$bbmsy<=bbmsy.collapse]
  points(collapse.yrs, rep(line.draw, length(collapse.yrs)), col="red", pch=15, cex=0.4)
}

# Add region text
region.lines <- (1:(ncollapsed+nregions))[!(1:(ncollapsed+nregions)%in%stocks.collapsed$line.draw)]
text(x=2015, y=region.lines-0.1, labels=regions, pos=4, cex=0.5, font=2, xpd=T)

# Off
dev.off()
graphics.off()





