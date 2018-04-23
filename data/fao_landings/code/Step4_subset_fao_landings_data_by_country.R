
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
library(freeR)

# Directories
datadir <- "data/fao_landings/data"
tabledir <- "tables"

# Read data
load(paste(datadir, "1950_2017_FAO_landings_data_all.Rdata", sep="/"))
load(paste(datadir, "FAO_marine_species_resilience_plus.Rdata", sep="/"))

# Read stock area centroids
areas <- read.csv(paste(datadir, "FAO_ISO3_stock_centroids.csv", sep="/"), as.is=T)


# Build data
################################################################################

# Filters to apply
# 1. Marine
# 2. Wild capture
# 3. Finfish and invertebrates only (no marine mammals, birds, turtles, etc.)
# 4. Species-level taxa
# 5. Occurs in FAO-IS03 with centroid (eliminates distant water fleets and old countries)
# 6. >20 years and >1,000 t of catch (after trimming initial 0s!)
# 7. Stocks that can't be placed in mPRM species categories
# 8. Highly migratory species

# Define a stock as:
# FAO Area-Country-Species
# Using FAO Area & Country accounts for biogeographical breaks (e.g., US East vs. West Coast)

# Life history categories
lh_catgs <- sort(unique(lhdata$lh_catg))
lh_catgs_bad <- c("Barnacles", "Corals", "Horseshoe crabs", "Jellyfish",
                  "Sea cucumbers", "Sea squirts", "Sea urchins", "Sponges", "Starfish")
lh_catgs_good <- lh_catgs[!lh_catgs%in%lh_catgs_bad]

# Highly migratory species
tunas <- subset(lhdata, lh_catg=="Tunas, bonitos, billfishes")
hms_genera <- c("Kajikia", "Istiompax", "Makaira", "Thunnus", "Katsuwonus",
                "Xiphias", "Tetrapturus")
tunas_reduced <- subset(tunas, !genus%in%hms_genera)

# Subset landings to species of interest
# Note: stockid is based on IS03 and ALPHA3 b/c these are only unique fields
data_raw <- landings %>% 
  mutate(areaid=paste(area_code, iso3, sep="-"),
         stockid=paste(area_code, iso3, spp_code, sep="-")) %>% 
  filter(area_type=="marine") %>% 
  filter(prod_type=="Capture production") %>% 
  filter(class %in% c("Pisces", "Mollusca", "Crustacea", "Invertebrata Aquatica")) %>% 
  filter(taxa_level=="species") %>% 
  filter(areaid%in%areas$areaid) %>% 
  select(stockid, areaid, area_code, iso3, spp_code, everything())

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
  filter(nyr>=20 & tl_mt_median>=250) %>% 
  # Add key life history information
  left_join(unique(select(lhdata, sci_name_orig, sci_name_fb, genus, lh_catg, resilience, m)), by=c("sci_name"="sci_name_orig")) %>% 
  # Remove stocks with "bad" life history categories
  filter(lh_catg%in%lh_catgs_good) %>%
  # Remove highly migratory species %>% 
  filter(!genus%in%hms_genera) %>% 
  # Rename columns and reorder
  rename(sci_name_orig=sci_name, fao_code=area_code) %>% 
  select(stockid:sci_name_orig, sci_name_fb, comm_name, year1, year2, nyr, tl_mt_median, lh_catg, resilience, m)

# Inspect completeness/uniqueness
complete(stocks)
anyDuplicated(stocks$stockid)

# Reduce trimmed data to only stocks used in the analysis
data <- data_trim %>%
  filter(stockid %in% stocks$stockid) %>% 
  rename(fao_code=area_code) %>% 
  select(stockid, areaid, fao_code, fao_area, iso3, country,
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

# Number of stocks by taxa
n_by_taxa <- stocks %>% 
  group_by(lh_catg) %>% 
  summarize(n=n())

# Number of stocks by taxa
n_by_species <- stocks %>% 
  group_by(sci_name_fb, comm_name) %>% 
  summarize(n=n())
sum(n_by_species$n>=10)
sum(n_by_species$n>=5)


# Export data
################################################################################

# Export data
save(data, stocks,
     file=paste(datadir, "1950_2017_FAO_landings_data_use.Rdata", sep="/"))
  
