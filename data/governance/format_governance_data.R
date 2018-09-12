

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(plyr)
library(dplyr)
library(freeR)
library(maptools)
library(RColorBrewer)

# Directories
datadir <- "data/governance/data"

# Read data
data_orig <- import(file.path(datadir, "wgidataset.xlsx"), sheet="RuleofLaw")


# Format data
################################################################################

# Remove header
# (but retain 2 column name rows)
data <- data_orig %>% 
  slice(13:n())

# Build and add columns names
# Use 2 columns name rows to build column names then remove
cols <- paste(data[1,], data[2,], sep="_")
colnames(data) <- cols
data <- slice(data, 3:n())

# Grab country info
countries <- data$`NA_Country/Territory`
isos <- data$NA_WBCode

# Isolate and format estimates
# Replace NA values, convert to numeric, remove lingering column, rename columns
ests <- data[,grepl("Estimate", colnames(data))]
ests[ests=="#N/A"] <- NA
ests <- sapply(ests, as.numeric)
ests <- ests[,!grepl("#N/A", colnames(ests))]
colnames(ests) <- gsub("_Estimate", "", colnames(ests))

# Add countries
ests_wide <- data.frame(iso3=isos, country=countries, ests, stringsAsFactors=F)

# Reshape wide-to-long
ests_long <- reshape2::melt(ests_wide, id.vars=c("iso3", "country"), 
                            variable.name="year", value.name="est") %>% 
  mutate(year=as.numeric(gsub("X", "", year)))

# Calculate average
results <- ests_long %>% 
  group_by(iso3, country) %>% 
  summarize(index=mean(est, na.rm=T)) %>% 
  arrange(index) %>% 
  mutate(strength=cut(index, breaks=c(-2.5, 0, 1.25, 2.5), labels=c("weak", "moderate", "strong")))


# Visualize results
################################################################################

# Build spatial sample size data
data(wrld_simpl) # from maptools
world_orig <- wrld_simpl@data
world <- world_orig %>% 
  left_join(results, by=c("ISO3"="iso3")) %>% 
  mutate(index_bin=cut(index, breaks=seq(-2.5, 2.5, 0.25)),
         index_color=colorpal(brewer.pal(9,"RdYlBu"), n=nlevels(index_bin))[index_bin],
         index_color=ifelse(is.na(index_color), "grey90", index_color),
         catg_color=revalue(strength, c("strong"=brewer.pal(9,"RdYlBu")[8], 
                                        "moderate"=brewer.pal(9,"RdYlBu")[4],
                                        "weak"=brewer.pal(9,"RdYlBu")[2])),
         catg_color=ifelse(is.na(catg_color), "grey90", as.character(catg_color)))
wrld_simpl@data <- world


# Plot data
par(mfrow=c(2,1), mar=c(0,0,0,0))
plot(wrld_simpl, col=wrld_simpl$index_color, border="grey30", lwd=0.3)
plot(wrld_simpl, col=wrld_simpl$catg_color, border="grey30", lwd=0.3)


# Export
################################################################################

write.csv(results, file.path(datadir, "WB_rule_of_law_avg_by_iso3.csv"), row.names=F)

