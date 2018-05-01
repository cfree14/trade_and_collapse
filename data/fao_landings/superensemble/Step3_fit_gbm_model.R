

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

# Directories
datadir <- "data/fao_landings/superensemble/data"

# Read data
load(paste(datadir, "simstock_data_orig.Rdata", sep="/"))
coms_orig <- data; rm(data)
ocom_orig <- read.csv(paste(datadir, "simstock_preds_ocom.csv", sep="/"))


# Build data
################################################################################

# Format OCOM data
ocom <- ocom_orig %>% 
  filter(year==60) %>% 
  select(stockid, q0.5) %>% 
  rename(ocom=q0.5)

# Format other predictions
coms <- coms_orig %>% 
  filter(year==60) %>% 
  select(stockid, bbmsy:mprm)
  
# Build data
data <- stocks %>% 
  select(-bmsy) %>% 
  left_join(coms, by="stockid") %>% 
  left_join(ocom, by="stockid")

# Completeness
freeR::complete(data)

# Fit models
################################################################################

# Divide into training and testing datasets
data_test <- subset(data, iter==10)
data_train <- subset(data, iter!=10)

# Define tuning parameter grid
fitGrid <- expand.grid(interaction.depth=c(1,2,4,6,8,10),
                       n.trees=c(100, seq(500, 10000, 500)),
                       shrinkage=c(0.01, 0.005, 0.001),
                       n.minobsinnode=10)

# Define tuning and training method
fitControl <- trainControl(method="repeatedcv", number=10, repeats=10)

# Train BRT model
brtfit <- train(log(bbmsy) ~ cmsy13 + comsir + mprm + ocom + spec_freq_0.05 + spec_freq_0.20,
                data=data_train,
                method="gbm", bag.fraction=0.5,
                distribution="gaussian", metric="RMSE",
                tuneGrid=fitGrid, trControl=fitControl, na.action=na.pass, verbose=T)

# Export model
save(brtfit, data, data_train, data_test,
     file=paste(datadir, "brt_model.Rdata", sep="/"))




