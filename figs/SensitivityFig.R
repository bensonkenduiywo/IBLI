#========================================================================
#0.0 Load data
#========================================================================
rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")

dff <- na.omit(dff)

#========================================================================
#1.0 Perfect Insurance contract
#========================================================================
trigger <- 0.18 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$mortality_rate - trigger) * 1000
#Compute Actuarily fair premiumL Premium with markup 25%
markup <- 0.1
premium <- mean(dff$payouts, na.rm=TRUE) * (1 + markup)
#Compute capital with perfect insurance
dff$capital     <- (1 - dff$mortality) * 1000
dff$perfect_ins <- (dff$capital + dff$payouts) - premium

#========================================================================
#2.0 Best index Insurance contract (Segmented regression + log Z-MODIS NDVI)
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
#Compute MODIS index payouts; 1TLU=1000$
dff$zlmodis_payouts <- pmax(0, dff$zlmodis_mortality - trigger) * 1000
premium <- mean(dff$zlmodis_payouts, na.rm=TRUE) * (1 + markup)
dff$Modis_ins <- (dff$capital + dff$zlmodis_payouts) - premium

#========================================================================
#3 Perfect Insurance contract 
#========================================================================
# 
# df_p <- readRDS("m_data.rds")
# df_p <- na.omit(df_p)
# #Compute payouts; 1TLU=1000$
# tlu <- 1
# df_p$payouts <- pmax(0, df_p$mortality_rate - trigger) * 1000 * tlu
# #Compute Actuarily fair premiumL Premium with markup 25%
# premium <- mean(df_p$payouts, na.rm=TRUE) * 1.1
# #Compute capital with insurance
# df_p$capital     <- (1 - df_p$mortality) * 1000 * tlu
# df_p$capital_ins <- (df_p$capital + df_p$payouts) - premium

#========================================================================
#4 Plots Perfect Insurance & sm+logMODIS contract vs rho
#========================================================================

library(agro)
Rho <- seq(0,3,0.5)
df <- data.frame(Rho=Rho)
for(i in 1:length(Rho)){
  df$Modis_ins[i] <- ce_income(dff$Modis_ins, rho=Rho[i])
  df$Perfect_ins[i] <- ce_income(dff$perfect_ins, rho=Rho[i])
  df$capital[i] <- ce_income(dff$capital, rho=Rho[i])
}

##Graph rho~RIB (x-axis rho, y1-axis value of insurance with a 
##perfect index and another one with RIB) 

tiff("figs/S1 Fig.tif", units="px", width=2250, height=2250, res=300, pointsize=16)

par(mai=c(1.1,1,0.5,0.5))
plot(Modis_ins~Rho, data=df, ylab="Insurance value ($)", xlab=expression(paste("Risk aversion (", symbol(rho),')')), las=1, yaxs="i", 
     xaxs="i", main="", cex.axis=0.8, cex.lab=0.9, type='l', 
     ylim=c(min(df$Modis_ins)-10,max(df$Perfect_ins))+5, lwd=2, col='blue')
box()
#lines(Modis_ins~Rho, data=df, lty=2) #dashed line
lines(Perfect_ins~Rho, data=df, lty=4,lwd=2, col='green')
lines(capital~Rho, data=df, lty=5, lwd=2, col='red')

legend("bottomleft", legend=c('Index insurance (sm and LMD)', 'Perfect insurance', "No insurance"), 
       lty=c(1, 4, 5), col=c('blue', 'green', 'red'), lwd=c(2,2,2), bty="n", cex=0.9)

#png("figs/figure1.png", units="in", width=12, height=12, res=300, pointsize=24)
dev.off()
