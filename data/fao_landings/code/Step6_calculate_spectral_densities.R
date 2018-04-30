
# Calculate spectral densities for FAO catch time series
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# If datalimited is not installed:
# devtools::install_github("datalimited/datalimited")

# Packages
library(tools)
library(plyr)
library(dplyr)
library(tidyr)
library(datalimited)

# Directories
datadir <- "data/fao_landings/data"
preddir <- "data/fao_landings/predictions"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Calculate spectral densities
################################################################################

# Calculate spectral frequencies
# Don't calcuate if time series has no variance or is shorter than 10 years
train_spec_mat <- function(x, freq_vec = 1/c(5, 20)) {
  # using AR as smoother, empirical didn't seem to confer much more benefit
  if(length(x) >= 10 & sd(tl)>0) {
    sp <- spec.ar(x/max(x), plot = FALSE)
    # approximate at fixed frequencies - necessary as series of different length
    df <- as.data.frame(approx(x = sp$freq, y = sp$spec, xout = freq_vec))
  } else {
    df <- data.frame(x=freq_vec, y=NA)
  }
}

# Add spectral density columns to data
data$spec_freq_0.05 <- NA
data$spec_freq_0.20 <- NA

# Loop through stocks: i <- 1
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  print(paste(i, stock))
  
  # Loop through years, calculate spec dens, and record
  yr_min <- min(sdata$year)
  yr_10 <- min(sdata$year) + 9
  yr_max <- max(sdata$year)
  for(j in yr_10:yr_max){
    tl <- sdata$tl_mt[sdata$year<=j]
    spec <- train_spec_mat(tl)
    data$spec_freq_0.05[data$stockid==stock & data$year==j] <- spec$y[spec$x==0.05]
    data$spec_freq_0.20[data$stockid==stock & data$year==j] <- spec$y[spec$x==0.20]
  }
  
}

# Sort data
data <- data %>% 
  arrange(stockid, year)


# Export spectral densities
################################################################################

# Export predictions
write.csv(data, paste(preddir, "1950_2017_FAO_spectral_densities.csv", sep="/"), row.names=F)





