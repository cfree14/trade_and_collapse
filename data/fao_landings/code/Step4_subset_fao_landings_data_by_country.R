
# Subset FAO landings data to "stocks" of interest
# By Chris Free, Rutgers University

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tools)
library(plyr)
library(dplyr)

# Directories
datadir <- "data/fao_landings/data"
tabledir <- "tables"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_all.Rdata", sep="/"))
load(paste(datadir, "FAO_marine_species_resilience_plus.Rdata", sep="/"))

# Read stock centroids


# Build data
################################################################################

# Filters to apply
# 1. Marine
# 2. Wild capture
# 3. Finfish and invertebrates only (no marine mammals, birds, turtles, etc.)
# 4. Species-level taxa
# 5. >20 years and >1,000 t of catch (after trimming initial 0s!)
# 6. Stocks that can't be placed in mPRM species categories

# Define a stock as:
# FAO Area-Country-Species
# Using FAO Area & Country accounts for biogeographical breaks (e.g., US East vs. West Coast)

# Life history categories
lh_catgs <- sort(unique(spp_lh_data$lh_catg))
lh_catgs_bad <- c("Barnacles", "Corals", "Horseshoe crabs", "Jellyfish",
                  "Sea cucumbers", "Sea squirts", "Sea urchins", "Sponges", "Starfish")
lh_catgs_good <- lh_catgs[!lh_catgs%in%lh_catgs_bad]

# Subset landings to species of interest
# Note: stockid is based on IS03 and ALPHA3 b/c these are only unique fields
data_raw <- landings %>% 
  filter(area_type=="marine") %>% 
  filter(prod_type=="Capture production") %>% 
  filter(class %in% c("Pisces", "Mollusca", "Crustacea", "Invertebrata Aquatica")) %>% 
  filter(taxa_level=="species") %>% 
  mutate(stockid=paste(area_code, iso3, spp_code, sep="-")) %>% 
  select(stockid, area_code, iso3, spp_code, everything())

# Identify stocks with 0 catch (necessary for trimming)
stocks0 <- data_raw %>% 
  group_by(stockid) %>% 
  summarize(tl_mt=sum(tl_mt)) %>% 
  filter(tl_mt==0)

# Trim leading 0s
# Remove stocks with 0 catch then trim leading zeros
data_trim <- data_raw %>%
  filter(!(stockid %in% stocks0$stockid)) %>% 
  group_by(stockid) %>%
  filter(year%in%c(min(year[which(tl_mt!=0)]):max(year)))

# Create stocks data frame
stocks <- data_trim %>% 
  group_by(stockid, area_code, fao_area, iso3, country, spp_code, sci_name, comm_name) %>% 
  # Calculate catch statistics
  summarize(nyr=n(),
            tl_mt_median=median(tl_mt),
            year1=min(year),
            year2=max(year)) %>% 
  # Filter by catch statistics
  filter(nyr>=20 & tl_mt_median>=1000) %>% 
  # Add key life history information
  left_join(unique(select(spp_lh_data, sci_name_orig, sci_name_fb, lh_catg, resilience)), by=c("sci_name"="sci_name_orig")) %>% 
  # Remove stocks with "bad" life history categories
  filter(lh_catg%in%lh_catgs_good) %>%
  # Rename columns and reorder
  rename(sci_name_orig=sci_name, fao_code=area_code) %>% 
  select(stockid:sci_name_orig, sci_name_fb, comm_name, year1, year2, nyr, tl_mt_median, lh_catg, resilience)
apply(stocks, 2, function(x) sum(is.na(x)))
anyDuplicated(stocks$stockid)

# Reduce trimmed data to only stocks used in the analysis
data <- data_trim %>%
  filter(stockid %in% stocks$stockid) %>% 
  rename(fao_code=area_code) %>% 
  select(stockid, fao_code, fao_area, iso3, country,
         class, sci_name, comm_name, year, tl_mt, tl_type)
  

# Inspect sample size
################################################################################

# Number of stocks
nrow(stocks)

# Number of stocks by FAO area
n_by_area <- stocks %>%
  group_by(fao_code, fao_area) %>% 
  summarize(n=n())

# Number of stocks by country
n_by_country <- stocks %>%
  group_by(iso3, country) %>% 
  summarize(n=n())


# Export data
################################################################################

# Export data
save(data, stocks,
     file=paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))
  
