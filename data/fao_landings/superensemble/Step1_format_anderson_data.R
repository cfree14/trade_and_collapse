
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(fishensembles)
# devtools::install_github("datalimited/fishensembles")

# Directories
datadir <- "data/fao_landings/superensemble/data"

# Build data
################################################################################

# 5760 scenarios from Rosenberg et al. (2014)
# ----------------------------------------------
# Life history (LH): large pelagic (LP), small pelagic (SP), demersal (DE)
# Biomass deplation (ID): 100% (1.0), 70% (0.7), 40% (0.4) of carrying capacity
# Exploitation dynamics (ED): constant (ED0), biomass-coupled (ED0.6), increasing (OW), roller-coaster (RC)
# Process error (sigmaR): 0.2 or 0.6 variance in normal distribution
# Autoregressive error (AR): uncorrelated (0.0) or 0.6 autoregressive correlation
# Observation error (sigmaC): No error (0.0) or 0.2 variance in normal distribution
# Time series length (TS): 20 or 60 years
# Iterations (iter): 10 stochastic iterations for each scenario

# Get data
data_orig <- fishensembles::dsim

# Format data
colnames(data_orig) <- tolower(colnames(data_orig))
data_long <- data_orig %>%
  # Add stockid
  mutate(stockid=paste(lh, "_", id*100, "%_", ed, "_", 
                       sigmar, "sr_", ar, "ar_", sigmac, "sc_", ts, "yr_", iter, sep="")) %>% 
  # Rearrange columns
  select(stockid, stock_id, lh, id, ed, sigmar, ar, sigmac, ts, iter, 
         year, bmsy_true, stock, harvest, catch, b_bmsy_true, b_bmsy_est, method_id) %>% 
  # Rename columns
  rename(stockid_orig=stock_id, method=method_id, bmsy=bmsy_true, bbmsy=b_bmsy_true, bbmsy_est=b_bmsy_est, biomass=stock) %>% 
  # Revalue method names
  mutate(method=revalue(method, c("Costello"="mPRM", "CMSY"="cMSY", "COM.SIR"="COMSIR"))) %>%
  # Remove duplicates (Sean duplicated things somehow)
  unique()


# Prediction data
# There should be 230400 predictions: 
# Half of time series are 20 yr long and half are 60 yr long
# 5760/2 * 20 + 5760/2 * 60 = 230400
data_wide <- dcast(data_long, stockid + lh + id + ed + sigmar + ar + sigmac + ts + iter +
                   year + bmsy + biomass + harvest + catch + bbmsy ~ method, value.var="bbmsy_est")
data <- data_wide %>% 
  rename(cmsy13=cMSY, mprm=mPRM, comsir=COMSIR, sscom=SSCOM) %>% 
  arrange(stockid, year)

# Check completeness
apply(data, 2, function(x) sum(is.na(x)))

# Build stock key
stocks <- data %>% 
  select(stockid, lh, id, ed, sigmar, ar, sigmac, ts, iter, bmsy) %>% 
  unique()


# Calculate spectral frequencies
################################################################################

# Calculate spectral frequencies using Sean's code:
# https://github.com/datalimited/fishensembles/blob/master/R/make.R

# Calculate spectral frequencies
train_spec_mat <- function(x, freq_vec = 1/c(5, 20)) {
  # using AR as smoother, empirical didn't seem to confer much more benefit
  if(length(x) >= 10) {
    sp <- spec.ar(x/max(x), plot = FALSE)
    # approximate at fixed frequencies - necessary as series of different length
    as.data.frame(approx(x = sp$freq, y = sp$spec, xout = freq_vec))
  } else {
    data.frame(x=NA, y=NA, xout=NA)
  }
}

# Calculcate spectral frequencies for each stock's catch time series (full time series)
spec_freq <- data_wide %>% 
  group_by(stockid) %>% 
  do(train_spec_mat(.$catch)) %>%
  rename_(spec_freq = ~x, spec_dens = ~y)

# Reshape spectral frequencies
speq_freq_wide <- dcast(spec_freq, stockid ~ spec_freq, value.var="spec_dens") 
colnames(speq_freq_wide) <- c("stockid", "spec_freq_0.05", "spec_freq_0.20")

# Add spectral frequencies to stock data
stocks <- stocks %>%
  left_join(speq_freq_wide, by="stockid")


# Export data
################################################################################

# Export data
save(data, stocks, file=paste(datadir, "simstock_data_orig.Rdata", sep="/"))

