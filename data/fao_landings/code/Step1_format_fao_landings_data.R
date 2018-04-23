
# Format FAO landings data
# By Chris Free, Rutgers University

# FAO (2017) Fishery and Aquaculture Statistics. Global production by production 
# source 1950-2015 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. 
# Rome, Italy. Updated 2017. www.fao.org/fishery/statistics/software/fishstatj/en
# Downloaded here (12/11/2017): http://www.fao.org/fishery/statistics/global-production/en

# TO DO LIST
# Fix missing species common names
# Could correct scientific names, improve taxa level
# Species designations are imperect - () and commas are indicators

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(freeR)
library(plyr)
library(dplyr)
library(tools)
library(stringr)
library(taxize)
library(rfishbase)

# Directories
inputdir <- "data/fao_landings/data/GlobalProuction_2017.1.1"
outputdir <- "data/fao_landings/data"

# Read data
area_key_orig <- read.csv(paste(inputdir, "CL_FI_AREA_GROUPS.csv", sep="/"), as.is=T, na.strings=c(""))
country_key_orig  <- read.csv(paste(inputdir, "CL_FI_COUNTRY_GROUPS.csv", sep="/"), as.is=T, na.strings=c(""))
prod_key_orig  <- read.csv(paste(inputdir, "CL_FI_PRODUCTION_SOURCE.csv", sep="/"), as.is=T, na.strings=c(""))
species_key_orig  <- read.csv(paste(inputdir, "CL_FI_SPECIES_GROUPS.csv", sep="/"), as.is=T, na.strings=c(""))
symbol_key_orig  <- read.csv(paste(inputdir, "CL_FI_SYMBOL.csv", sep="/"), as.is=T)
landings_orig  <- read.csv(paste(inputdir, "TS_FI_PRODUCTION.csv", sep="/"), as.is=T) # blanks in landings type mean "original"


# Build taxa key
################################################################################

# FB/SLB taxa keys
taxa_key_fb <- load_taxa()
taxa_key_slb <- sealifebase

# Combine taxa keys
taxa_key <-  taxa_key_fb %>%
  bind_rows(taxa_key_slb) %>% 
  setNames(tolower(names(.))) %>% 
  mutate(sciname=paste(genus, species)) %>% 
  select(class, order, family, genus, species, sciname) %>% 
  unique()

# Check for duplicated scientific names
sum(duplicated(taxa_key$sciname))


# Format data
################################################################################

# Format production source key
colnames(prod_key_orig) <- tolower(colnames(prod_key_orig))
prod_key <- prod_key_orig %>% 
  rename(prod_code=code, name_en=name_e, name_fr=name_f, name_es=name_s) %>% 
  mutate(name_en=revalue(name_en, c("Aquaculture (brackishwater)"="Aquaculture (brackish water)")),
         prod_short=revalue(name_en, c("Aquaculture"="A", 
                                       "Aquaculture (marine)"="ASW",
                                       "Aquaculture (freshwater)"="AFW",
                                       "Aquaculture (brackish water)"="ABW",
                                       "Capture production"="WC"))) %>% 
  select(prod_code, prod_short, everything())
complete(prod_key)

# Format area key
colnames(area_key_orig) <- tolower(colnames(area_key_orig))
area_key <- area_key_orig %>% 
  rename(area_code=code, ocean=ocean_group, region=major_group, subregion=faregion_group) %>% 
  mutate(area_type=ifelse(grepl("Inland", name_en), "inland", "marine")) %>% 
  select(area_code, area_type, everything())
complete(area_key)

# Format country key
# The column names are in a wierd order
colnames(country_key_orig) <- tolower(colnames(country_key_orig))
colnames.wrong <- colnames(country_key_orig)
colnames.fixed <- c(colnames.wrong[length(colnames.wrong)], colnames.wrong[1:(length(colnames.wrong)-1)])
colnames(country_key_orig) <- colnames.fixed
country_key <- country_key_orig %>% 
  rename(gis_code=figis_id, iso2=iso2_code, iso3=iso3_code,
         contininent=continent_group, ecoclass=ecoclass_group, region=region_group)
country_key$iso2[is.na(country_key$iso2)] <- paste0("UN", 1:sum(is.na(country_key$iso2)))
country_key$iso3[is.na(country_key$iso3)] <- paste0("UNK", 1:sum(is.na(country_key$iso3)))
complete(country_key)

# Format species key
# For more on taxa codes: http://www.fao.org/fishery/collection/asfis/en
colnames(species_key_orig) <- tolower(colnames(species_key_orig))
species_key <- species_key_orig %>% 
  # Rename columns
  rename(alpha3=x3alpha_code, taxa_code=taxonomic_code, sci_name=scientific_name, 
         family=family_group, order=order_group, class=major_group) %>% 
  # Convert taxa groups to title case (Note: these might not be taxanomically correct)
  mutate(family=toTitleCase(tolower(family)),
         order=toTitleCase(tolower(order)),
         class=toTitleCase(tolower(class)),
         # Remove parentheses from author designation
         author=gsub("\\(", "", author),
         author=gsub("\\)", "", author), 
         # Identify level (rank) of taxanomic resolution
         taxa_level=ifelse(!grepl("X", taxa_code), "species",
                           ifelse(grepl("spp", sci_name), "genus",
                                  ifelse(sci_name%in%taxa_key$family, "family",
                                         ifelse(sci_name%in%taxa_key$order, "order",
                                                ifelse(sci_name%in%taxa_key$class, "class", "other")))))) %>% 
  select(alpha3, taxa_code, taxa_level, sci_name, everything())
table(species_key$taxa_level)
other_spp <- subset(species_key, taxa_level=="other")
complete(species_key)

# Inspect species key
spp_check <- unique(select(species_key, alpha3, sci_name, name_en)) # simplify to scan through
anyDuplicated(species_key$alpha3) # no duplicated ALPHA3s
spp_check$sci_name[duplicated(spp_check$sci_name)] # but there are duplicated SCINAMES

# Format symbol key
# Make blank symbols "O" for "original"
colnames(symbol_key_orig) <- tolower(colnames(symbol_key_orig))
symbol_key <- symbol_key_orig %>% 
  mutate(name=tolower(name), 
         symbol=ifelse(symbol==" ", "O", symbol))

# Format landings data
colnames(landings_orig) <- tolower(colnames(landings_orig)) 
landings <- landings_orig %>% 
  # Rename some columns
  rename(gis_code=country, area_code=area, prod_code=source, alpha3=species, 
         tl_mt=quantity, tl_symbol=quantity_symbol) %>% 
  # Add country info
  left_join(select(country_key, gis_code, iso3, name_en), by="gis_code") %>%
  rename(country=name_en) %>% 
  # Add area info
  left_join(select(area_key, area_code, area_type, name_en), by="area_code") %>% 
  rename(fao_area=name_en) %>% 
  # Add production info
  left_join(select(prod_key, prod_code, prod_short, name_en), by="prod_code") %>% 
  rename(prod_type=name_en) %>% 
  # Add species info
  left_join(select(species_key, class, alpha3, taxa_level, sci_name, name_en), by="alpha3") %>% 
  rename(comm_name=name_en) %>% 
  # Add landings units
  # Make blank symbols "O" for "original"
  mutate(tl_symbol=ifelse(tl_symbol=="", "O", tl_symbol)) %>% 
  left_join(select(symbol_key, symbol, name), by=c("tl_symbol"="symbol")) %>% 
  rename(tl_type=name) %>% 
  # Rearrange columns and rows
  select(area_code, fao_area, area_type, iso3, country, prod_short, prod_type, 
         class, alpha3, taxa_level, sci_name, comm_name, year, tl_mt, tl_type) %>% 
  rename(spp_code=alpha3) %>% 
  arrange(country, year)

# Inspect
complete(landings)


# Export data
################################################################################

# Export data
write.csv(landings, paste(outputdir, "1950_2017_FAO_landings_data.csv", sep="/"), row.names=F)
save(landings, species_key, country_key, prod_key, area_key, symbol_key, taxa_key,
     file=paste(outputdir, "1950_2017_FAO_landings_data_all.Rdata", sep="/"))

