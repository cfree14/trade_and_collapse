

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/superensemble/data"
plotdir <- "data/fao_landings/superensemble/figures"

# Read data
load(paste(datadir, "simstock_data_orig.Rdata", sep="/"))
coms_orig <- data; rm(data)
ocom_orig <- read.csv(paste(datadir, "simstock_preds_ocom.csv", sep="/"))


# Build data
################################################################################

# Merge data
data <- coms_orig %>% 
  select(stockid:year, catch, bbmsy:sscom) %>% 
  left_join(select(ocom_orig, stockid, year, q0.5), by=c("stockid", "year")) %>% 
  rename(ocom=q0.5) %>% 
  filter(ts==60)

# Add superensemble predictions
################################################################################

# Calculate spectral densities
##########################################

# Calculate spectral frequencies
# Don't calcuate if time series has no variance or is shorter than 10 years
train_spec_mat <- function(x, freq_vec = 1/c(5, 20)) {
  # using AR as smoother, empirical didn't seem to confer much more benefit
  if(length(x) >= 10 & sd(tl)>0) {
    sp <- spec.ar(x/max(x), plot = FALSE)
    # approximate at fixed frequencies - necessary as series of different length
    df <- as.data.frame(approx(x = sp$freq, y = sp$spec, xout = freq_vec))
  } else {
    df <- data.frame(x=freq_vec, y=NA)
  }
}

# Add spectral density columns to data
data$spec_freq_0.05 <- NA
data$spec_freq_0.20 <- NA

# Loop through stocks: i <- 1
stocks <- unique(data$stockid)
for(i in 1:length(stocks)){
  
  # Subset data
  stock <- stocks[i]
  sdata <- subset(data, stockid==stock)
  print(paste(i, stock))
  
  # Loop through years, calculate spec dens, and record
  yr_min <- min(sdata$year)
  yr_10 <- min(sdata$year) + 9
  yr_max <- max(sdata$year)
  for(j in yr_10:yr_max){
    tl <- sdata$catch[sdata$year<=j]
    spec <- train_spec_mat(tl)
    data$spec_freq_0.05[data$stockid==stock & data$year==j] <- spec$y[spec$x==0.05]
    data$spec_freq_0.20[data$stockid==stock & data$year==j] <- spec$y[spec$x==0.20]
  }
  
}

# Make superensemble predictions
##########################################

# Library
library(randomForest)

# Load RF-superensemble model object
faodlmdir2 <- "~/Dropbox/Chris/Rutgers/projects/fao_dlm/data/predictions/simstocks"
load(paste(faodlmdir2, "fishensembles_object.Rdata", sep="/"))

# Format data for superensemble predictions
data.s <- data %>% 
  rename(stock_id=stockid,
         spec_freq_0.2=spec_freq_0.20,
         CMSY=cmsy13, 
         COMSIR=comsir, 
         Costello=mprm,
         SSCOM=sscom,
         bbmsy_true_mean=bbmsy)

# Make, record, inspect predictions
se.preds <- exp(predict(x$model, newdata=data.s))
data$super <- se.preds
boxplot(data$super, frame=F, las=1, ylim=c(0,2), ylab="B/Bmsy")


# Calculate performance
################################################################################

# Methods
colnames(data)
methods <- c("cMSY-13", "COM-SIR", "mPRM", "SSCOM", "OCOM", "Super") 

# Loop through years and calculate performance
i <- 20
yrs <- unique(data$year)
for(i in 20:length(yrs)){
  
  # Subset data for year i
  yr <- yrs[i]
  sdata <- data %>% 
    filter(year==yr) %>% 
    select(bbmsy, cmsy13, comsir, mprm, sscom, ocom, super)
  
  # Calculate performance stats
  stats <- performance(sdata, methods)
  
  # Merge performance stats
  stats$yr <- yr
  if(i==20){astats <- stats}else{astats <- rbind(astats, stats)}
  
} 

# Export data
write.csv(data, file.path(datadir, "simstock_com_time_series_preds.csv"), row.names=F)
write.csv(astats, file.path(datadir, "simstock_com_time_series_preds_performance.csv"), row.names=F)


# Visualize performance
################################################################################

# Colors
colors <- brewer.pal(length(methods)+1, "Set1")
colors <- colors[c(1:5, 7)] # to omit yellow

# Setup figure
figname <- "SFig3_com_time_series_performance.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=2.25, units="in", res=600)
par(mfrow=c(1,3), mar=c(3.5, 4, 0.5, 0.5), mgp=c(2.5,0.8,0))

# A. Accuracy
plot(astats$yr, astats$inaccuracy, type="n", bty="n", las=1,
     xlab="", ylab="Inaccuracy (MAPE)", ylim=c(0.2,0.6))
for(i in 1:length(methods)){
  sdata <- filter(astats, method==methods[i])
  lines(sdata$yr, sdata$inaccuracy, col=colors[i])
}

legend("bottomright", bty="n", legend=methods, col=colors, lwd=1, cex=0.8)

# B. Rank
plot(astats$yr, astats$rank, type="n", bty="n", las=1,
     xlab="Year", ylab="Rank-order correlation", ylim=c(-0.2, 0.8))
for(i in 1:length(methods)){
  sdata <- filter(astats, method==methods[i])
  lines(sdata$yr, sdata$rank, col=colors[i])
}

# C. Bias
plot(astats$yr, astats$bias, type="n", bty="n", las=1,
     xlab="", ylab="Bias (MPE)", ylim=c(-0.3, 0.2))
for(i in 1:length(methods)){
  sdata <- filter(astats, method==methods[i])
  lines(sdata$yr, sdata$bias, col=colors[i])
}
lines(x=c(20,60), y=c(0,0), lty=2)

# Off
dev.off()


