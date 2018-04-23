
# Get LH category and resilience for marine fin/shellfish stocks identified to species-level
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tools)
library(plyr)
library(dplyr)
library(ggplot2)
library(rfishbase)
library(datalimited)

# Instal FishLife package
devtools::install_github("james-thorson/FishLife")
library(FishLife)

# Directories
datadir <- "data/fao_landings/data"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))


# Setup
################################################################################





