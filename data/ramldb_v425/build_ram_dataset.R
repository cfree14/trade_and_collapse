
# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(ggplot2)

# Directories
datadir <- "data/ramldb_v425/data"
plotdir <- "data/ramldb_v425/figures"

# Load RAMLDB v4.25
ramdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ramldb/ramldb_v4.25/DB Files With Assessment Data"
load(paste(ramdir, "DBdata.RData", sep="/"))


# Build time series data
################################################################################

# Format B/BMSY time series
ts <- timeseries_values_views %>% 
  select(stockid, stocklong, year, BdivBmsypref) %>% 
  rename(bbmsy=BdivBmsypref) %>% 
  filter(!is.na(bbmsy))

# B/BMSY sample size
ts_stocks <- ts %>% 
  group_by(stockid, stocklong) %>% 
  summarize(n=n(),
            yr1=min(year),
            yr2=max(year),
            overfished=sum(bbmsy<0.5)>0,
            collapsed=sum(bbmsy<0.2)>0)
hist(ts_stocks$n, breaks=seq(0,220,10), las=1, col="grey50", border=F,
     xlab="Time series length (yr)", main="")
abline(v=20, lty=3)

# Reduce TS sample to stocks with >= 20 years of data
ts_stocks_use <- ts_stocks %>%
  filter(n>=20)

# Reduce time series to sample stocks
ts_use <- ts %>% 
  filter(stockid %in% ts_stocks_use$stockid)

# Export data
write.csv(ts_use, paste(datadir, "ramldb_v425_bbmsy_time_series.csv", sep="/"))


# Build stock info
################################################################################

# Format stock info
# Area info, taxanomic info, management/assessment info
stocks <- stock %>% 
  # Filter to stocks of interest
  filter(stockid %in% ts_stocks_use$stockid) %>% 
  # Select and rename columns
  select(stockid, stocklong, tsn, scientificname, commonname, region, areaid) %>% 
  rename(species=scientificname, comm_name=commonname) %>% 
  # Add area info
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  rename(area=areaname) %>% 
  # Add taxanomic info
  left_join(select(taxonomy, kingdom, phylum, classname, ordername, family, genus, 
                   scientificname, FisheryType, taxGroup), by=c("species"="scientificname")) %>% 
  rename(class=classname, order=ordername, fishery_type=FisheryType, taxa_group=taxGroup) %>% 
  # Add B/BMSY sample size
  left_join(select(ts_stocks_use, -stocklong), by="stockid") %>% 
  # Rearrange columns
  select(stockid, stocklong, 
         country, region, area, fishery_type, taxa_group, 
         kingdom, phylum, class, order, family, genus, species, comm_name, yr1, yr2, n, overfished, collapsed)

# Inspect completeness 
apply(stocks, 2, function(x) sum(is.na(x)))

# Export data
write.csv(stocks, paste(datadir, "ramldb_v425_bbmsy_time_series_stocks.csv", sep="/"))


# Plot stock demographics
################################################################################

# Setup figure
figname <- "Fig1_stock_demographics.png"
png(paste(plotdir, figname, sep="/"), width=7.5, height=3.5, units="in", res=600)
par(mfrow=c(1,3), mar=c(10,2,0.5,0.5), oma=c(0,4,0,0))

# Taxanomic group
taxa_n <- rev(sort(table(stocks$taxa_group)))
barplot(taxa_n, las=2, col="grey50", border=F, cex.names=0.8)
nspp <- n_distinct(stocks$species)
nfam <- n_distinct(stocks$family)
nstocks <- nrow(stocks)
ntext <- paste0(nstocks, " stocks\n", nspp, " species\n", nfam, " families\n")
mtext(ntext, cex=0.7, side=3, adj=0.9, line=-4)

# Region
region_n <- rev(sort(table(stocks$region)))
barplot(region_n, las=2, col="grey50", border=F, cex.names=0.8, ylim=c(0,60))

# Time series length
hist(stocks$n, breaks=seq(0,220,10), xlim=c(0,150), las=1, right=F, 
     col="grey50", border=F, xlab="Time series length (yr)", main="")

# Add y-axis label
mtext("# of stocks", outer=T, side=2, line=1.5, cex=0.8, adj=0.7)

# Off
dev.off()
graphics.off()


# Plot time series (overfishing/collapse frequency)
################################################################################

# Sort stocks by start year
stocks <- arrange(stocks, yr1)
n_over <- sum(stocks$overfished)
n_collapse <- sum(stocks$collapsed)

# Setup figure
figname <- "Fig2_overfishing_frequency.png"
png(paste(plotdir, figname, sep="/"), width=8, height=6, units="in", res=600)
par(mfrow=c(1,2), mar=c(4,0.5, 0.5, 0.5))

# Overfished
bbmsy_thresh <- 0.5
plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n",
     xlim=c(1860,2020), ylim=c(1,nrow(stocks)), xlab="", ylab="")
axis(1, at=seq(1860,2020,20), las=2)
for(i in 1:nrow(stocks)){
  stock <- stocks$stockid[i]
  yr1 <- stocks$yr1[i]
  yr2 <- stocks$yr2[i]
  lines(x=c(yr1, yr2), y=c(i,i), col="grey70")
  collapse.yrs <- ts$year[ts$stockid==stock & ts$bbmsy<=bbmsy_thresh]
  points(collapse.yrs, rep(i, length(collapse.yrs)), col="red", pch=15, cex=0.4)
}
mtext("Overfished (B/BMSY < 0.50)", side=3, adj=0.05, line=-1.5, cex=0.8, font=2)
mtext(paste(n_over, "stocks"), side=3, adj=0.05, line=-2.5, cex=0.8)

# Collapesed
bbmsy_thresh <- 0.2
plot(1:10, 1:10, type="n", bty="n", xaxt="n", yaxt="n",
     xlim=c(1860,2020), ylim=c(1,nrow(stocks)), xlab="", ylab="")
axis(1, at=seq(1860,2020,20), las=2)
for(i in 1:nrow(stocks)){
  stock <- stocks$stockid[i]
  yr1 <- stocks$yr1[i]
  yr2 <- stocks$yr2[i]
  lines(x=c(yr1, yr2), y=c(i,i), col="grey70")
  collapse.yrs <- ts$year[ts$stockid==stock & ts$bbmsy<=bbmsy_thresh]
  points(collapse.yrs, rep(i, length(collapse.yrs)), col="red", pch=15, cex=0.4)
}
mtext("Collapsed (B/BMSY < 0.20)", side=3, adj=0.05, line=-1.5, cex=0.8, font=2)
mtext(paste(n_collapse, "stocks"), side=3, adj=0.05, line=-2.5, cex=0.8)

# Off
dev.off()
graphics.off()


# Plot example time series
################################################################################

# Setup figure
figname <- "Fig0_bbmsy_v_trade_example.png"
png(paste(plotdir, figname, sep="/"), width=8, height=4, units="in", res=600)
par(mfrow=c(1,2), mar=c(3,4, 0.5, 0.5), mgp=c(2.7,0.7,0))

# Setup plot
ts_example <- filter(ts, stockid=="HERRNS-IIIa-VIId")
plot(bbmsy~year, ts_example, type="n", bty="n", las=1, cex.axis=0.9,
     xlim=c(1940,2020), ylim=c(0,3.5), xlab="", ylab=expression("B / B"["MSY"]))
# Add shading
rect(xleft=1940, xright=2020, ybottom=0, ytop=0.5, col=rgb(t(col2rgb("red"))/255, alpha=0.6), border=F)
rect(xleft=1940, xright=2020, ybottom=0.5, ytop=1.5, col=rgb(t(col2rgb("orange"))/255, alpha=0.6), border=F)
rect(xleft=1940, xright=2020, ybottom=1.5, ytop=3.5, col=rgb(t(col2rgb("forestgreen"))/255, alpha=0.6), border=F)
# Add B/BMSY lines
lines(ts_example$year, ts_example$bbmsy, lwd=1.8)
lines(x=c(1940, 2020), y=c(1,1), lwd=1.2, lty=3)

# Trade plot (made up data)
ts_example <- filter(ts, stockid=="HERRNS-IIIa-VIId")
plot((0-bbmsy+3.5)*100~year, ts_example, type="l", bty="n", las=1, lwd=1.8,
     xlim=c(1940,2020), ylim=c(0,400), xlab="", ylab="Trade", cex.axis=0.9)

# Off
dev.off()
graphics.off()

