
# Predict status of FAO stocks using catch-only models
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# If datalimited is not installed:
# devtools::install_github("datalimited/datalimited")
# devtools::install_github("cfree14/datalimited2")

# Packages
library(tools)
library(plyr)
library(dplyr)
library(datalimited)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/data"
preddir <- "data/fao_landings/predictions"
sscomdir <- "data/fao_landings/predictions/sscom"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Fit example models
################################################################################

# # Fit example models?
# fit_examples <- F
# if(fit_examples==F){
#   load(paste(preddir, "example_com_outputs.Rdata", sep="/"))
# }else{
#   # Example stock
#   i <- 52
#   stock <- stocks$stockid[i]
#   sdata <- subset(data, stockid==stock)
#   lh_catg <- "Tunas, bonitos, billfishes"
#   res <- stocks$resilience[i]
#   
#   # Fit mPRM
#   # Output = 1 dataframe
#   # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) - no mean/sd
#   mprm_data <- format_prm(year=sdata$year, catch=sdata$tl, bbmsy=NA, species_cat=lh_catg)
#   mprm_output <- predict_prm(mprm_data, model=datalimited::ram_prm_model, ci=T)
#   
#   # Fit cMSY-13
#   # Output = 1 6-element list (6.8 MB) where 1 element is a 
#   # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) and mean/sd
#   cmsy13_nreps <- 500000
#   cmsy13_output <- cmsy(yr=sdata$year, ct=sdata$tl, start_r=resilience(res), reps=cmsy13_nreps)
#   cmsy13_bbmsy <- cmsy13_output$bbmsy
#   
#   # Fit COMSIR
#   # Output = 1 5-element list (6.3 GB) where 1 element is a
#   # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) and mean/sd
#   comsir_nposterior <- 2000000
#   comsir_nburnin <- comsir_nposterior*0.1
#   comsir_output <- comsir(yr=sdata$year, ct=sdata$tl, start_r=resilience(res),
#                           nsim=comsir_nburnin, n_posterior=comsir_nposterior)
#   comsir_bbmsy <- comsir_output$bbmsy
#   
#   # Fit SSCOM (really slow)
#   # Output = 1 dataframe
#   # Time series of B/BMSY quantiles (2.5, 25, 50, 75, 97.5) - no mean/sd
#   sscom_output <- sscom(yr=sdata$year, ct=sdata$tl, start_r=resilience(res))
#   
#   # Fit zBRT
#   # Output = 1 2 element list where 1 element is a 
#   # Time series of saturation and B/BMSY quantities
#   zbrt_output <- zbrt(year=sdata$year, catch=sdata$tl)
#   zbrt_bbmsy <- zbrt_output[["ts"]]
#   
#   # Save example model runs
#   save(sscom_output, comsir_output, cmsy13_output, mprm_output, zbrt_output,
#        file=paste(preddir, "example_com_outputs.Rdata", sep="/"))
#   
# }


# Fit real models
################################################################################

# The following code will create a data frame with the following:
# stockid, method, year, catch,
# bbmsy_q2.5, bbmsy_q25, bbmsy_q50, bbmsy_q75, bbmsy_q97.5 (bbmsy_avg, bbmsy_sd, convergence)

# Which model?
<<<<<<< HEAD:data/fao_landings/Step4_predict_bbmsy_coms.R
com_to_fit <- "sscom"
=======
com_to_fit <- "ocom"
>>>>>>> 3f86c605052b65fb1a850b3adec70e42fb6fb238:data/fao_landings/code/Step5_predict_bbmsy_coms.R

# Loop through stocks and fit model: i <- 1
# for(i in 1:3){
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- subset(data, stockid==stock)
  m <- stocks$m[i]
  res <- stocks$resilience[i]
  lh_catg <- stocks$lh_catg[i]
  print(paste(i, stock))
  
  # Fit mPRM
  if(com_to_fit=="mprm"){
    bbmsy_ts <- try({
      mprm_data <- format_prm(year=sdata$year, catch=sdata$tl_mt, bbmsy=NA, species_cat=lh_catg)
      mprm_output <- predict_prm(mprm_data, model=datalimited::ram_prm_model, ci=T)
      bbmsy_ts <- mprm_output %>% 
        mutate(stockid=stock, method="mPRM") %>% 
        select(stockid, method, everything())
    })
  }
  
  # Fit cMSY-13
  if(com_to_fit=="cmsy13"){
    bbmsy_ts <- try({
      cmsy13_nreps <- 500000
      cmsy13_output <- cmsy(yr=sdata$year, ct=sdata$tl_mt, start_r=resilience(res), reps=cmsy13_nreps)
      bbmsy_ts <- cmsy13_output$bbmsy %>% 
        mutate(stockid=stock, method="cMSY-13") %>% 
        rename(bbmsy_avg=bbmsy_mean) %>% 
        select(stockid, method, year, catch, bbmsy_q2.5:bbmsy_q97.5, bbmsy_avg, bbmsy_sd)
    })
  }
  
  # Fit SSCOM (really slow)
  if(com_to_fit=="sscom"){
    bbmsy_ts <- try({
      sscom_output <- sscom(yr=sdata$year, ct=sdata$tl_mt, start_r=resilience(res))
      bbmsy_ts <- sscom_output %>%
        mutate(stockid=stock, method="SSCOM") %>% 
        rename(bbmsy_q50=b_bmsy, bbmsy_q97.5=b_bmsyUpper, bbmsy_q2.5=b_bmsyLower, 
               bbmsy_q25=b_bmsy_iq25, bbmsy_q75=b_bmsy_iq75) %>% 
        select(stockid, method, year, bbmsy_q2.5, bbmsy_q25, bbmsy_q50, bbmsy_q75, bbmsy_q97.5, convergence)
    })
  }
  
  # Fit COMSIR
  if(com_to_fit=="comsir"){
    bbmsy_ts <- try({
      comsir_nposterior <- 2000000
      comsir_nburnin <- comsir_nposterior*0.05
      comsir_output <- comsir(yr=sdata$year, ct=sdata$tl_mt, start_r=resilience(res),
                              nsim=comsir_nburnin, n_posterior=comsir_nposterior)
      bbmsy_ts <- comsir_output$bbmsy %>%
        mutate(stockid=stock, method="COMSIR") %>% 
        rename(bbmsy_avg=bbmsy_mean) %>%
        select(stockid, method, year, catch, bbmsy_q2.5:bbmsy_q97.5, bbmsy_avg, bbmsy_sd)
    })
  }
  
  # Fit Zhou-BRT
  if(com_to_fit=="zbrt"){
    zbrt_ts <- try({
      zbrt_output <- zbrt(year=sdata$year, catch=sdata$tl_mt)
      bbmsy_ts <- zbrt_output$ts %>%
        mutate(stockid=stock, method="zBRT") %>% 
        select(stockid, method, year, everything())
    })
  }
  
  # Fit Zhou-OCOM
  if(com_to_fit=="ocom"){
    ocom_ts <- try({
      ocom_output <- ocom(year=sdata$year, catch=sdata$tl_mt, m=m)
      bbmsy_ts <- ocom_output$bbmsy_ts %>%
        mutate(stockid=stock, method="OCOM") %>% 
        select(stockid, method, year, everything())
    })
  }
  
  # Save B/BMSY time series
  if(!inherits(bbmsy_ts, "try-error")){
    if(!exists("bbmsy_ts_all")){bbmsy_ts_all <- bbmsy_ts}
    if(i==1){bbmsy_ts_all <- bbmsy_ts}else{bbmsy_ts_all <- rbind(bbmsy_ts_all, bbmsy_ts)}
    write.csv(bbmsy_ts, paste(sscomdir, paste0(stock, ".csv"), sep="/"), row.names=F)
    
  }
  
}


# Export model predictions
################################################################################

# Export predictions
outfile <- paste0("1950_2017_FAO_bbmsy_timeseries_", com_to_fit, ".csv")
write.csv(bbmsy_ts_all, paste(preddir, outfile, sep="/"), row.names=F)

