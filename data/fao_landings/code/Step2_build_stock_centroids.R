
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(mapdata)
library(freeR)

# Directories
gisdir <- "data/gis_data"
faodir <- "data/gis_data/fao_stat_areas/gis_data"
eezdir <- "data/gis_data/World_EEZ_v9_20161021_LR"
datadir <- "data/fao_landings/data"
plotdir <- "data/fao_landings/figures"

# Read FAO/EEZ data
fao_orig <- readOGR(dsn=faodir, layer="FAO_AREAS")
eez <- readOGR(dsn=eezdir, layer="eez_lr")

# Read FAO no lines
fao_lines <- readOGR(dsn=faodir, layer="FAO_AREAS_NOCOASTLINE", verbose=F)
fao_lines <- subset(fao_lines, F_LEVEL=="MAJOR")

# Read stock info
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Build data
################################################################################

# Format FAO data
fao <- subset(fao_orig, F_LEVEL=="MAJOR")
fao_data <- fao@data %>% 
  select(F_CODE) %>% 
  rename(fao_code=F_CODE)
fao@data <- fao_data

# Which EEZ ISO3 to use?
isos_in_catch <- sort(unique(stocks$iso3))
isos_in_eez1 <- sort(unique(eez@data$ISO_Ter1)) # best one
isos_in_eez2 <- sort(unique(eez@data$ISO_Ter2))
isos_in_eez3 <- sort(unique(eez@data$ISO_Ter3))
isos_in_catch[!isos_in_catch%in%isos_in_eez1] # best one
isos_in_catch[!isos_in_catch%in%isos_in_eez2]
isos_in_catch[!isos_in_catch%in%isos_in_eez3]

# Format EEZ data
eez_data <- eez@data %>% 
  select(ISO_Ter1, Sovereign1) %>% 
  rename(iso3=ISO_Ter1, country=Sovereign1)
eez@data <- eez_data

# Dissolve EEZ data
isos <- as.character(sort(unique(eez@data$iso3)))
eez_dissolve <- unionSpatialPolygons(eez, IDs=eez@data$iso3)
# eez_dissolve <- gUnaryUnion(eez, id = eez@data$iso3)
eez_dissolve_df <- SpatialPolygonsDataFrame(Sr=eez_dissolve, 
                                            data=data.frame(iso3=isos, stringsAsFactors=F, 
                                                            row.names=getSpPPolygonsIDSlots(eez_dissolve)))

# Intesect FAO regions and EEZs
stocks <- raster::intersect(fao, eez_dissolve_df)
stock_centroids <- coordinates(stocks)
colnames(stock_centroids) <- c("long_dd", "lat_dd")
stocks_data <- stocks@data %>% 
  mutate(areaid=paste(fao_code, iso3, sep="-"),
         fao_code=as.character(fao_code)) %>% 
  select(areaid, fao_code, iso3) %>% 
  cbind(stock_centroids)
stocks@data <- stocks_data

# Export data
writeOGR(stocks, dsn=gisdir, layer="FAO_ISO3_stocks", driver="ESRI Shapefile")
write.csv(stocks_data, paste(datadir, "FAO_ISO3_stock_centroids.csv", sep="/"), row.names=F)


# Plot data
################################################################################

# Setup figure
figname <- "Fig2_stock_centroids.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=4, units="in", res=600)
par(mfrow=c(1,1), mar=c(0.1,0.1,0.1,0.1), xpd=NA)

# Plot FAO areas
plot(fao_lines, border="grey70", lty=3, xlim=xlim, ylim=ylim)

# Plot centroids
xlim <- c(-180, 180)
ylim <- c(-90, 90)
set.seed(4)
fao_cols <- sample(rainbow(length(unique(stocks_data$fao_code))))
map("world", col="grey85", fill=T, border="white", lwd=0.3, 
    xlim=xlim, ylim=ylim, add=T)
points(x=stocks_data$long_dd, y=stocks_data$lat_dd, cex=0.9,
       pch=16, col=fao_cols[as.factor(stocks_data$fao_code)], xpd=NA)
text(x=stocks_data$long_dd, y=stocks_data$lat_dd, 
     labels=stocks_data$iso3, cex=0.5, col=tcolor("black", 0.7))

# Off
dev.off()
graphics.off()
