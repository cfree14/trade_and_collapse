
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(tools)
library(plyr)
library(dplyr)
library(datalimited)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/superensemble/data"

# Read data
load(paste(datadir, "simstock_data_orig.Rdata", sep="/"))


# Format data
################################################################################

# Add M and resilience to simulated stocks
stocks <- stocks %>% 
  mutate(m=as.numeric(as.character(revalue(lh, c("DE"=0.315, "SP"=0.729, "LP"=0.315)))),
         resilience=as.character(revalue(lh, c("DE"="Low", "SP"="Medium", "LP"="Low"))))

# Completeness
str(stocks)
complete(stocks)


# Fit COMs
################################################################################

# The following code will create a data frame with the following:
# stockid, method, year, catch,
# bbmsy_q2.5, bbmsy_q25, bbmsy_q50, bbmsy_q75, bbmsy_q97.5 (bbmsy_avg, bbmsy_sd, convergence)

# Which model?
com_to_fit <- "ocom"

# Loop through stocks and fit model: i <- 1
# for(i in 1:3){
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  m <- stocks$m[i]
  res <- stocks$resilience[i]
  lh_catg <- stocks$lh[i]
  print(paste(i, stock))
  
  # Fit Zhou-OCOM
  if(com_to_fit=="ocom"){
    ocom_ts <- try({
      ocom_output <- ocom(year=sdata$year, catch=sdata$catch, m=m)
      bbmsy_ts <- ocom_output$bbmsy_ts %>%
        mutate(stockid=stock, method="OCOM") %>% 
        select(stockid, method, year, everything())
    })
  }
  
  # Save B/BMSY time series
  if(!inherits(bbmsy_ts, "try-error")){
    if(!exists("bbmsy_ts_all")){bbmsy_ts_all <- bbmsy_ts}
    if(i==1){bbmsy_ts_all <- bbmsy_ts}else{bbmsy_ts_all <- rbind(bbmsy_ts_all, bbmsy_ts)}

  }
  
}


# Export model predictions
################################################################################

# Export predictions
outfile <- paste0("simstock_preds_", com_to_fit, ".csv")
write.csv(bbmsy_ts_all, paste(datadir, outfile, sep="/"), row.names=F)

