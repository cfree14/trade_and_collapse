
# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(plyr)
library(dplyr)

# Directories
datadir <- "data/fao_landings/data"
preddir <- "data/fao_landings/predictions"

# Read landings data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))
catch <- data; rm(data)

# Read COM predictions
mprm <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_mprm.csv", sep="/"), as.is=T)
cmsy13 <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_cmsy13.csv", sep="/"), as.is=T)
comsir <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_comsir.csv", sep="/"), as.is=T)
ocom <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_ocom.csv", sep="/"), as.is=T)

# Read spectral density data
dens <- read.csv(paste(preddir, "1950_2017_FAO_spectral_densities.csv", sep="/"), as.is=T)

# What's wrong with OCOM
length(1950:2015)
ocom_info <- ocom %>%
  group_by(stockid) %>% 
  summarize(n=n()) %>% 
  filter(n>66)

# Merge data
################################################################################

# Merge data
data <- dens %>%
  # Add mPRM predictions
  left_join(select(mprm, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>% 
  rename(mprm=bbmsy_q50) %>% 
  # Add Catch-MSY predictions
  left_join(select(cmsy13, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>%
  rename(cmsy13=bbmsy_q50) %>%
  # Add COMSIR predictions
  left_join(select(comsir, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>%
  rename(comsir=bbmsy_q50) %>% 
  # Add OCOM predictions
  left_join(select(ocom, stockid, year, q0.5), by=c("stockid", "year")) %>%
  rename(ocom=q0.5)

# Inspect completeness
apply(data, 2, function(x) sum(is.na(x)))


# Export data
################################################################################

# Export data
write.csv(data, paste(preddir, "1950_2017_FAO_bbmsy_timeseries_merge.csv", sep="/"), row.names=F)

  


  
  