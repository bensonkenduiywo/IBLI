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
#2.0 lRN+sm contract (R2=0.413 & RIB =0.41572367)
#========================================================================
#Predict mortality using segmented regression
x <- dff$zlrain
y <- dff$mortality_rate
ml <- lm(y~x, data=dff)
library(segmented)
sm <- segmented(ml, seg.Z = ~x, psi=0)
#plot(y~x, pch=16, cex=0.9, main="Segmented regression", cex.main=1.1, cex.lab=1.1, xlab="z-score(lMD)", ylab="Mortality rate")
#plot.segmented(sm, add=T, lwd=2, lty=1, col="red") 
dff$zlrain_mortality <- predict(sm)
#Compute MODIS index payouts; 1TLU=1000$
dff$zlrain_payouts <- pmax(0, dff$zlrain_mortality - trigger) * 1000
dff$zlrain_ins <- (dff$capital + dff$zlrain_payouts) - premium

#make a plot of predictive skill for sm using log transformed Z-scored rainfall
x11()
par(mfrow=c(1, 2), mar=c(15, 4, 1.8, 0.5)) #c(bottom, left, top, right)
x <- dff$mortality_rate
y <- dff$zlrain_mortality
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(a)", cex.axis=0.85, cex.lab=0.9, cex=0.65, ylim = c(0,50))
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")

#========================================================================
#3.0 Plot contract in 2 above over perfect model
#========================================================================
#Make plots of perfect insurance contract
library(ggplot2)
x11()
cols <- c("blue", "red", "green")
ggplot(dff, aes(x=capital)) +
  geom_smooth(aes(y=zlrain_ins, colour="lRN"), se = FALSE, size=0.7, method = 'gam')+
  geom_point(aes(y=zlrain_ins), colour="red", size=1.5)+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.7) +
  geom_line(aes(y=perfect_ins, colour="Perfect insurance"), size=0.7)+
  labs(y="Assets (USD)", x="Assets (USD)", title = "(a)", size=12) +
  scale_colour_manual(name="Key", values=cols) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), 
        axis.line.x=element_line(), axis.line.y=element_line(),
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5,face = "bold")) 


#========================================================================
#4.0 lMD+lm5 contract (R2=0.418, RIB=0.49886841)
#========================================================================
#Predict mortality using piecewise regression
df0 <- dff[dff$zlmodis< -0.5,]
x <- df0$zlmodis
y <- df0$mortality_rate
lm5 <- lm(y~x)
df0$lm5lMD_mortality <- predict(lm5)
#Compute MODIS index payouts; 1TLU=1000$
df0$lm5lMD_payouts <- pmax(0, df0$lm5lMD_mortality - trigger) * 1000
df0$lm5lMD_ins <- (df0$capital + df0$lm5lMD_payouts) - premium

#make a plot of predictive skill for lm5 model based on log transformed MODIS (lMD)
#==============================
x <- df0$mortality_rate
y <- df0$lm5lMD_mortality
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(b)", cex.axis=0.85, cex.lab=0.9, cex=0.65, ylim = c(0,50))
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")
#========================================================================
#5.0 Plot the model in 4 above with perfect insurance.
#========================================================================
#Make plots of perfect insurance contract
library(ggplot2)
x11()
cols <- c("blue", "red", "green")
ggplot(df0, aes(x=capital)) +
  geom_smooth(aes(y=lm5lMD_ins,  colour="lMD"), se = FALSE, size=0.7, method = 'gam')+
  geom_point(aes(y=lm5lMD_ins), colour="red", size=1.5)+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.7) +
  geom_line(aes(y=perfect_ins, colour="Perfect insurance"), size=0.7)+
  labs(y="Assets (USD)", x="Assets (USD)", title = "(b)", size=12) +
  scale_colour_manual(name="Key", values=cols) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), axis.line.x=element_line(), axis.line.y=element_line(),
        axis.text.x = element_text(color="black",size=12),
        axis.text.y = element_text(color="black",size=12),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5,face = "bold"))

#fig <- ggarrange(p1, p2,
                 #labels = c("(a)", "(b)"),
                 #ncol = 1, nrow = 2)
#fig