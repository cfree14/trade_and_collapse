
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
modeldir <- "data/fao_landings/superensemble/data"

# Read superensemble model
load(paste(modeldir, "brt_model.Rdata", sep="/"))

# Read landings data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))
catch <- data; rm(data)

# Read COM predictions
mprm <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_mprm.csv", sep="/"), as.is=T)
cmsy13 <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_cmsy13.csv", sep="/"), as.is=T)
comsir <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_comsir.csv", sep="/"), as.is=T)
ocom_orig <- read.csv(paste(preddir, "1950_2017_FAO_bbmsy_timeseries_ocom.csv", sep="/"), as.is=T)

# Read spectral density data
dens <- read.csv(paste(preddir, "1950_2017_FAO_spectral_densities.csv", sep="/"), as.is=T)


# Fix OCOM status predictions
################################################################################

# Many stocks get their results duplicated by OCOM
# This must be a bug in the datalimited2 package - rerunning did not help.
# Here, I pull out the duplicate results

# Look
length(1950:2015)
ocom_probs <- ocom_orig %>%
  group_by(stockid) %>% 
  summarize(n=n()) %>% 
  filter(n>66)

# Add a temporory id, STOCKID-YEAR, and make it unique
# Then pull out the duplicates
ocom <- ocom_orig %>% 
  mutate(id=make.unique(paste(stockid, year, sep="-"), sep="!!!")) %>% 
  select(id, everything()) %>% 
  filter(!grepl("!!!", id)) %>% 
  select(-id)


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

# Make sure there aren't any negative B/BMSY values
sum(data$ocom<0, na.rm=T)
sum(data$comsir<0, na.rm=T)
sum(data$cmsy13<0, na.rm=T)
sum(data$mprm<0, na.rm=T)

# Which OCOM stocks have B/BMSY < 0? Overwrite all OCOM results for them.
prob_stocks <- sort(unique(data$stockid[data$ocom < 0]))
prob_data <- subset(data, stockid%in%prob_stocks)
data$ocom[data$stockid%in%prob_stocks] <- NA

# Inspect completeness
freeR::complete(data)


# Make superensemble predictions
################################################################################

# Add superensemble predictions
data$super <- exp(predict(brtfit, newdata=data, na.action=NULL))


# Export data
################################################################################

# Export data
write.csv(data, paste(preddir, "1950_2017_FAO_bbmsy_timeseries_merge.csv", sep="/"), row.names=F)

  


  
  