
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(maptools)
library(RColorBrewer)
library(freeR)
library(datalimited2)

# Directories
datadir <- "data/fao_landings/predictions"
tabledir <- "data/fao_landings/tables"
plotdir <- "data/fao_landings/figures"

# Read data
data_orig <- read.csv(paste(datadir, "1950_2017_FAO_bbmsy_timeseries_merge.csv", sep="/"), as.is=T)


# Build data
################################################################################

# Add status
data <- data_orig %>% 
  filter(!is.na(super)) %>% 
  mutate(status=bbmsy2catg(super, breaks=c(0.5,1.25), catgs=c("over", "fully", "under")))

# Count by catg over time
count1 <- table(data$year, data$status)
count2 <- data.frame(year=as.numeric(rownames(count1)),
                     n=apply(count1, 1, sum), 
                     n_under=count1[,3],
                     n_fully=count1[,1],
                     n_over=count1[,2])

# Percentage by catg over time
count3 <- count2 %>% 
  mutate(p_under=n_under/n,
         p_fully=n_fully/n,
         p_over=n_over/n)

# Quickly plot data
count4 <- t(as.matrix(count3[, c("p_under", "p_fully", "p_over")]))
barplot(count4, col=c("green", "orange", "red"), names=count3$year, las=2)

# Mean B/BMSY over time
bbmsy <- data %>% 
  group_by(year) %>% 
  summarize(n=n(),
            bbmsy=mean(super),
            bbmsy_se=plotrix::std.error(super),
            bbmsy_lo=bbmsy-bbmsy_se*1.96,
            bbmsy_hi=bbmsy+bbmsy_se*1.96)
plot(bbmsy ~ year, bbmsy, type="l")


# Plot data
################################################################################

# Setup figure
figname <- "Fig2_fao_status_over_time.png"
png(paste(plotdir, figname, sep="/"), width=6.5, height=4, units="in", res=600)
par(mar=c(3.5,3.5,0.5,4), mgp=c(2.5,0.8,0))

# Props for plotting
p1 <- count3$p_under
p2 <- p1 + count3$p_fully

# Empty plot
yrs <- count3$year
plot(x=1:10, y=1:10, bty="n", las=1, xaxt="n",
     xlim=c(1950,2015), ylim=c(0,1), xlab="", ylab="Proportion of stocks")
axis(1, at=seq(1950,2015,5), las=2)

# Add shading
polygon(x=c(yrs, rev(yrs)), 
        y=c(rep(0, length(yrs)), rev(p1)), col="darkgreen", border=F)
polygon(x=c(yrs, rev(yrs)), 
        y=c(p1, rev(p2)), col="orange", border=F)
polygon(x=c(yrs, rev(yrs)), 
        y=c(p2, rep(1,length(yrs))), col="red", border=F)

# Add mean B/BMSY over time
par(new = T)
plot(bbmsy ~ year, bbmsy, bty="n", type="l", lwd=1.2, col="black",
     xlim=c(1950,2015), ylim=c(0,2), xaxt="n", yaxt="n", xlab="", ylab="")
# lines(x=bbmsy$year, y=bbmsy$bbmsy_lo, lty=2, lwd=1.1)
# lines(x=bbmsy$year, y=bbmsy$bbmsy_hi, lty=2, lwd=1.1)
axis(4, at=seq(0,2,0.5), labels=T, las=1)
text(x=2025, y=1, xpd=NA, labels=expression("Mean B/B"["MSY"]), srt=270)

# # Add legend
# legend(x=2016, y=0.95, bty="n", xpd=NA, cex=0.9,
#        fill=c("darkgreen", "orange", "red"),
#        title.adj=0, title=expression(bold("Stock status")),
#        legend=c("Underexploited", "Fully exploited", "Overexploited"))

# Off
dev.off()
graphics.off()

