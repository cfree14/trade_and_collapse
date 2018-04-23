
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
# cmsy13 <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_cmsy13.csv", sep="/"), as.is=T)
# comsir <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_comsir.csv", sep="/"), as.is=T)
# sscom <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_sscom.csv", sep="/"), as.is=T)

# Read spectral density data
dens <- read.csv(paste(preddir, "1950_2017_FAO_spectral_densities.csv", sep="/"), as.is=T)


# Merge data
################################################################################

# Merge data
data <- dens %>%
  # Add mPRM predictions
  left_join(select(mprm, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>% 
  rename(mprm=bbmsy_q50) 

# Inspect completeness
apply(data, 2, function(x) sum(is.na(x)))

  # # Add Catch-MSY predictions
  # left_join(select(cmsy13, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>% 
  # rename(cmsy13=bbmsy_q50) %>% 
  # # Add COMSIR predictions
  # left_join(select(comsir, stockid, year, bbmsy_q50), by=c("stockid", "year")) %>% 
  # rename(comsir=bbmsy_q50)
  
  


  
  