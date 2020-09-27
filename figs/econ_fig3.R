#========================================================================
#0.0 Load data
#========================================================================
rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")
dff <- na.omit(dff)

#========================================================================
#1.0 Best index Insurance contract (Segmented regression + log Z-MODIS NDVI)
#========================================================================
#Predict mortality using segmented regression
x <- dff$zlmodis
y <- dff$mortality_rate
ml <- lm(y~x, data=dff)
library(segmented)
sm <- segmented(ml, seg.Z = ~x, psi=0)
#plot(y~x, pch=16, cex=0.9, main="Segmented regression", cex.main=1.1, cex.lab=1.1, xlab="z-score(lMD)", ylab="Mortality rate")
#plot.segmented(sm, add=T, lwd=2, lty=1, col="red") 
dff$zlmodis_mortality <- predict(sm)

#make a plot of predictive skill for segmented regression and log transformed MODIS
x11()
par(mfrow=c(1, 2), mar=c(15, 4, 1.8, 0.5)) #c(bottom, left, top, right)
x <- dff$mortality_rate
y <- dff$zlmodis_mortality
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(a)", cex.axis=0.85, cex.lab=0.9, cex=0.65, ylim = c(0,50))
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")
#aa <- rep(23,24)
#bb <- 0:23
#lines(aa, bb, col="red", lwd=1.5)
#text(30,5, "Trigger", cex=0.85, col="red")
#text(40,20, "Under predicted", cex=0.75)
#text(10,45, "Over predicted", cex=0.75)

#========================================================================
#2.0 Poor index Insurance contract (linear regression based on rainfall predictors)
#========================================================================
#Predict mortality using linear regression and z-scored rainfall as predictors
x <- dff$zlrain
y <- dff$mortality_rate
lm <- lm(y~x, data=dff)
dff$zrain_mortality <- predict(lm)

#make a plot of predictive skill for segmented regression and log transformed MODIS
#x11()
x <- dff$mortality_rate
y <- dff$zrain_mortality
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(b)", cex.axis=0.85, cex.lab=0.9, cex=0.65, ylim = c(0,50))
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")
#aa <- rep(23,29)
#bb <- -5:23
#lines(aa, bb, col="red", lwd=1.5)
#text(30,0, "Trigger", cex=0.85, col="red")
#text(40,10, "Under predicted")
#text(10,25, "Over predicted")
