
# Get LH category and resilience for marine fin/shellfish stocks identified to species-level
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(rfishbase)
library(FishLife)

# Directories
datadir <- "data/fao_landings/data"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_all.Rdata", sep="/"))


# Setup
################################################################################

# Species
species <- sort(unique(stocks$sci_name_fb))





