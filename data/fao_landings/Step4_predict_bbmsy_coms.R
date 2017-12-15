
# Predict status of FAO stocks using catch-only models
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
library(datalimited)

# Directories
datadir <- "data/fao_landings/data"
preddir <- "data/fao_landings/predictions"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Fit example models
################################################################################

# Fit example models?
fit_examples <- F
if(fit_examples==F){
  # load(paste(datadir, "example_com_outputs.Rdata", sep="/"))
}else{
  # Example stock
  i <- 52
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  lh_catg <- "Tunas, bonitos, billfishes"
  res <- stocks$resilience[i]
  
  # Fit mPRM
  # Output = 1 dataframe
  # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) - no mean/sd
  mprm_data <- format_prm(year=sdata$year, catch=sdata$tl, bbmsy=NA, species_cat=lh_catg)
  mprm_output <- predict_prm(mprm_data, model=datalimited::ram_prm_model, ci=T)
  
  # Fit COMSIR
  # Output = 1 5-element list (6.3 GB) where 1 element is a
  # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) and mean/sd
  comsir_nposterior <- 2000000
  comsir_nburnin <- nposterior*0.1
  comsir_output <- comsir(yr=sdata$year, ct=sdata$tl, start_r=resilience(res),
                          nsim=comsir_nburnin, n_posterior=comsir_nposterior)
  comsir_bbmsy <- comsir_output$bbmsy
  
  # Fit cMSY-13
  # Output = 1 6-element list (6.8 MB) where 1 element is a 
  # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) and mean/sd
  cmsy13_nreps <- 500000
  cmsy13_output <- cmsy(yr=sdata$year, ct=sdata$tl, start_r=resilience(res), reps=cmsy13_nreps)
  cmsy13_bbmsy <- cmsy13_output$bbmsy
  
  # Fit SSCOM (really slow)
  # Output = 1 dataframe
  # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) - no mean/sd
  sscom_output <- sscom(yr=sdata$year, ct=sdata$tl, start_r=resilience(res))
  
  # Save example model runs
  save(sscom_output, comsir_output, cmsy13_output, mprm_output,
       file=paste(datadir, "example_com_outputs.Rdata", sep="/"))
}


# Fit real models
################################################################################

# Which model?
com_to_fit <- "mprm"

# Loop through stocks and fit model: i <- 1
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  res <- stocks$resilience[i]
  lh_catg <- stocks$lh_catg[i]
  print(paste(i, stock))
  
  # Fit mPRM
  if(com_to_fit=="mprm"){
    mprm_data <- format_prm(year=sdata$year, catch=sdata$tl, bbmsy=NA, species_cat=lh_catg)
    mprm_output <- predict_prm(mprm_data, model=datalimited::ram_prm_model, ci=T)
    bbmsy_ts <- mprm_output %>% 
      mutate(stockid=stock) %>% 
      select(stockid, everything())
  }
  
  # Save B/BMSY time series
  if(i==1){bbmsy_ts_all <- bbmsy_ts}else{bbmsy_ts_all <- rbind(bbmsy_ts_all, bbmsy_ts)}
  
}

# Export model predictions
################################################################################

# Export predictions
outfile <- paste0("1950_2017_FAO_bbmsy_timeseries_", com_to_fit, ".csv")
write.csv(bbmsy_ts_all, paste(preddir, outfile, sep="/"), row.names=F)





