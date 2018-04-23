
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
library(rslurm)
library(datalimited)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/data"
preddir <- "data/fao_landings/predictions"
stockpreddir <- "data/fao_landings/predictions/stock_preds"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Functions
################################################################################

# Fit catch-only model
# For testing: com <- "mprm"; stock <- stocks$stockid[1]
fit_com <- function(stock, com){
  
  # Subset data
  sdata <- subset(data, stockid==stock)
  res <- stocks$resilience[stocks$stockid==stock]
  lh_catg <- stocks$lh_catg[stocks$stockid==stock]

  # Fit mPRM
  if(com=="mprm"){
    bbmsy_ts <- try({
      mprm_data <- format_prm(year=sdata$year, catch=sdata$tl_mt, bbmsy=NA, species_cat=lh_catg)
      mprm_output <- predict_prm(mprm_data, model=datalimited::ram_prm_model, ci=T)
      bbmsy_ts <- mprm_output %>% 
        mutate(stockid=stock, method="mPRM") %>% 
        select(stockid, method, everything())
    })
  }
  
  # Fit cMSY-13
  if(com=="cmsy13"){
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
  if(com=="sscom"){
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
  if(com=="comsir"){
    bbmsy_ts <- try({
      comsir_nposterior <- 1000000
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
  if(com=="zbrt"){
    zbrt_ts <- try({
      zbrt_output <- zbrt(year=sdata$year, catch=sdata$tl_mt)
      bbmsy_ts <- zbrt_output$ts %>%
        mutate(stockid=stock, method="zBRT") %>% 
        select(stockid, method, year, everything())
    })
  }
  
  # 
  # if(!inherits(bbmsy_ts, "try-error")){
  #   if(!exists("bbmsy_ts_all")){bbmsy_ts_all <- bbmsy_ts}
  #   if(i==1){bbmsy_ts_all <- bbmsy_ts}else{bbmsy_ts_all <- rbind(bbmsy_ts_all, bbmsy_ts)}
  # }
  
  # Return
  return(bbmsy_ts)
}

# Test function
fit_com(com="mprm", stock=stocks$stockid[1])


# Fit catch-only models
################################################################################

# The following code will create a data frame with the following:
# stockid, method, year, catch,
# bbmsy_q2.5, bbmsy_q25, bbmsy_q50, bbmsy_q75, bbmsy_q97.5 (bbmsy_avg, bbmsy_sd, convergence)

# Which model?
com_to_fit <- "comsir"

# Create parameters to apply SLURM job over
pars <- data.frame(stock=stocks$stockid, com=com_to_fit)

# Create SLURM job
# 21 nodes available (for empty queue), start with 10, each node has 8 CPUs
# For testing: slurm_options=list(partition="sesynctest", time="1:00:00"), 2 nodes, 4 cores, limits jobs to 1 hour
# For real: slurm_options=list(partition="sesynctest")
sjob <- slurm_apply(fit_com, pars, jobname = 'fao_com_fits',
                    nodes = 2, cpus_per_node = 8, submit = T, 
                    add_objects=c("data", "stocks"), 
                    slurm_options=list(partition="sesynctest", time="1:00:00"))

# Read in output from SLURM job
# Could export to "/nfs/FISHMAR-data"
results <- get_slurm_out(sjob, outtype="raw", wait=T)



