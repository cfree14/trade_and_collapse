
# SETUP
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(rfishbase)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/trade_and_collapse/data/comtrade/data"

# Read data
data_orig <- read.csv(paste(datadir, "UN_Comtrade_Commodity_Classifications.csv", sep="/"), as.is=T)
colnames(data_orig) <- tolower(colnames(data_orig))

# BUILD DATA
################################################################################

# Format data
data <- as.data.frame(
  data_orig %>% 
    filter()
    
)


