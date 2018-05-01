

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
library(RColorBrewer)

# Directories
datadir <- "data/fao_landings/superensemble/data"
plotdir <- "data/fao_landings/superensemble/figures"

# Read data
load(paste(datadir, "brt_model.Rdata", sep="/"))


# Best tune info
################################################################################

# Best tune info
best_tune <- brtfit$bestTune
id <- brtfit$bestTune$interaction.depth
lr <- brtfit$bestTune$shrinkage
ntrees <- brtfit$bestTune$n.trees
rmse <- subset(brtfit$results, interaction.depth==id & 
                                         shrinkage==lr  & n.trees==ntrees)$RMSE
r2 <- subset(brtfit$results, interaction.depth==id & 
               shrinkage==lr  & n.trees==ntrees)$Rsquared
print(c(id, lr, ntrees, rmse, r2))


# Plot model tuning
################################################################################

# Setup figure
figname <- "SFig1_brt_model_tuning_overall.png" 
png(paste(plotdir, figname, sep="/"), width=6, height=2.5, units="in", res=600)
par(mfrow=c(1,3), mar=c(5,1,1,0.5), mgp=c(2,1,0), xpd=NA, oma=c(0,3,0,0))

# Get tuning results
brt_tune <- brtfit$results

# Best tune for model
best_tune <- brtfit$bestTune
id_best <- brtfit$bestTune$interaction.depth
lr_best <- brtfit$bestTune$shrinkage
ntrees_best <- brtfit$bestTune$n.trees
rmse_best <- subset(brt_tune, interaction.depth==id_best & shrinkage==lr_best & n.trees==ntrees_best)$RMSE

# Training parameters
params <- brtfit$modelInfo$parameters$parameter
paramvals <- apply(brtfit$results[,params, drop = FALSE], 2, function(x) unique(x))
ids <- paramvals$interaction.depth
lrs <- rev(paramvals$shrinkage)
ntrees <- paramvals$n.trees

# Parameters
id_colors <- brewer.pal(length(ids), "Dark2")
  
# Loop through learning rates
for(i in 1:length(lrs)){
  # Params
  lr <- lrs[i]
  lr_tune <- subset(brt_tune, shrinkage==lr)
  ymin <- floor(min(lr_tune$RMSE) / 0.001) * 0.001
  ymax <- ceiling(max(lr_tune$RMSE) / 0.001) * 0.001
  ymax_nice <- ymax-(ymax-ymin)*0.05
  ymax_nice2 <- ymax-(ymax-ymin)*0.18
  # Setup empty plot
  plot(RMSE ~ n.trees, brt_tune, las=2, bty="n", type="n", xaxt="n", yaxt="n",
       xlim=c(0, max(ntrees)), ylim=c(ymin, ymax), xlab="", ylab="", main=paste0("LR = ", lr))
  # Plot axis
  axis(1, at=seq(0,max(ntrees),1000), labels=T, las=2)
  if(i==1){axis(2, at=c(ymin,ymax), labels=T, las=1)}else{axis(2, at=c(ymin,ymax), labels=F)}
  # Plot one line per interaction depth
  for(j in 1:length(ids)){
    id <- ids[j]
    id_tune <- subset(brt_tune, shrinkage==lr & interaction.depth==id)
    lines(id_tune$n.trees, id_tune$RMSE, col=id_colors[j])
  }
  # If this is the interaction depth with the best tune, add some stuff
  if(lr==lr_best){
    # Mark the best tune point and label kappa value
    color <- id_colors[which(ids==id_best)]
    points(ntrees_best, rmse_best, pch=16, cex=1.2, col=color)
    rmse_best_format <- format(round(rmse_best, 3), nsmall=3)
    text(ntrees_best, rmse_best, rmse_best_format, pos=3, col=color, cex=1, font=2)
    
    # Print the best tune info
    paramtext <- paste(paste("Learning rate = ", lr_best, sep=""), "\n",
                       paste("Interaction depth = ", id_best, sep=""), "\n",
                       paste("Number of trees = ", ntrees_best, sep=""), sep="")
    text(x=max(ntrees), y=ymax_nice2, pos=2, labels=paramtext, cex=0.95)
  }
  
  # Add learning rate legend
  if(i==1){
    legend("topright", legend=ids, col=id_colors, lwd=2, bty="n", 
           y.intersp=0.8,
           title=expression(bold("Interaction depth")), title.adj=0.2)
  }
  
}

# Add axis labels
mtext("# of trees", outer=T, side=1, adj=0.5, line=-1.3, cex=0.9)
mtext("RMSE", outer=T, side=2, adj=0.6, line=1.7, cex=0.9)

# Off
dev.off()
graphics.off()


