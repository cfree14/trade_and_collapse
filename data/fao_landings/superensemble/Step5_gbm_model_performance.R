

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(gbm)
library(caret)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/superensemble/data"
plotdir <- "data/fao_landings/superensemble/figures"

# Load data with SSCOM, cMSY-17, zBRT predictions
preds_orig <- read.csv("/Users/cfree/Dropbox/Chris/Rutgers/projects/fao_dlm/paper/data/com_status_predictions_simstocks.csv", as.is=T)

# Read data
load(paste(datadir, "brt_model.Rdata", sep="/"))


# Build data
################################################################################

# Build data
preds <- preds_orig %>% 
  select(stockid, iter, spec_freq_0.05, spec_freq_0.2, true_bbmsy, 
         mprm_bbmsy, zbrt_bbmsy, ocom_bbmsy, 
         cmsy13_bbmsy, cmsy17_bbmsy, comsir_bbmsy, sscom_bbmsy, super_bbmsy) %>% 
  rename(bbmsy=true_bbmsy, super1=super_bbmsy, spec_freq_0.20=spec_freq_0.2)
colnames(preds) <- gsub("_bbmsy", "", colnames(preds))

# Add super predictions
preds$super2 <- exp(predict(brtfit, newdata=preds, na.action=NULL))

# Reduce to testing dataset
preds_test <- filter(preds, iter==10)

# Plot quickly
performance(select(preds_test, bbmsy, cmsy13, cmsy17, comsir, sscom, mprm,
                   zbrt, ocom, super1, super2), 
            methods=c("cMSY-13", "cMSY-17", "COMSIR", "SSCOM", "mPRM", "
                      zBRT", "OCOM", "Super1", "Super2"))


# Plot data
################################################################################

# Setup figure
figname <- "SFig2_gbm_model_performance.png" 
png(paste(plotdir, figname, sep="/"), width=4, height=4, units="in", res=600)
par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.5), mgp=c(2.2,0.8,0), xpd=NA)


# Plot
performance(select(preds_test, bbmsy, cmsy13, cmsy17, comsir, sscom, mprm,
                   zbrt, ocom, super1, super2),
            methods=c("cMSY-13", "cMSY-17", "COMSIR", "SSCOM", "mPRM", "
                      zBRT", "OCOM", "Super1", "Super2"))

# Off
dev.off()
graphics.off()






