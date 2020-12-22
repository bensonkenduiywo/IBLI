#========================================================================
#0.0 Load data
#========================================================================
rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")

dff <- na.omit(dff)

#========================================================================
#1.0 Perfect Insurance contract
#========================================================================
trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$mortality_rate - trigger) * 1000
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.25
#Compute capital with insurance
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
dff$Modis_ins <- (dff$capital + dff$zlmodis_payouts) - premium

#========================================================================
#3.0 Plot perfect insurance superimposed on MODIS (best) index Insurance contract
#========================================================================
#Make plots of perfect insurance contract
library(ggplot2)
x11()
cols <- c("blue", "red", "green")
ggplot(dff, aes(x=capital)) +
  geom_smooth(aes(y=Modis_ins, colour="lMD"), se = FALSE, size=0.7, method = 'gam')+
  geom_point(aes(y=Modis_ins), colour="black", size=1.5)+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.7) +
  geom_line(aes(y=perfect_ins, colour="Perfect insurance"),size=0.7)+
  labs(y="Assets (USD)", x="Assets (USD)") +
  scale_colour_manual(name="Key", values=cols) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), 
        axis.line.x=element_line(), axis.line.y=element_line(),
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12),
        legend.text=element_text(size=12))  
#p1
#========================================================================
#4.0 lm+RN contract (poor contract) (R2=0.418 & RIB=0.49886841)
#========================================================================
#Predict mortality using linear regression and z-scored rainfall as predictors
x <- dff$zlrain
y <- dff$mortality_rate
lm <- lm(y~x, data=dff)
dff$zrain_mortality <- predict(lm)
dff$zrain_payouts <- pmax(0, dff$zrain_mortality - trigger) * 1000
dff$zrain_ins <- (dff$capital + dff$zrain_payouts) - premium

#========================================================================
#3.0 Plot perfect insurance superimposed on poorest index Insurance contract
#========================================================================
#Make plots of perfect insurance contract
x11()
cola <- c("blue", "green", "red")
p2 <- ggplot(dff, aes(x=capital)) +
  geom_smooth(aes(y=zrain_ins,  colour="RN"), se = FALSE, size=0.7, method = 'gam')+
  geom_point(aes(y=zrain_ins), colour="red", size=1.5)+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.7) +
  geom_line(aes(y=perfect_ins, colour="Perfect insurance"), size=0.7)+
  labs(y="Assets (USD)", x="Assets (USD)") +
  scale_colour_manual(name="Type", values=cola) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), axis.line.x=element_line(), axis.line.y=element_line(),
        axis.text.x = element_text(color="black",size=12),
        axis.text.y = element_text(color="black",size=12),
        legend.text=element_text(size=12))

p2
#Combine the figures
#library("ggpubr")
#fig <- ggarrange(p1, p2,
                    #labels = c("(a)", "(b)"),
                    #ncol = 1, nrow = 2)
#fig

