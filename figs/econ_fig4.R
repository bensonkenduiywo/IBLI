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
#2.0 Contracc with high R2 but low RIB: lm5+lNO
#========================================================================
#Predict mortality using piecewise regression
df0 <- dff[dff$zlnoaa< -0.5,]
x <- df0$zlnoaa
y <- df0$mortality_rate
lm5 <- lm(y~x)
df0$lm5lNO_mortality <- predict(lm5)
#Compute MODIS index payouts; 1TLU=1000$
df0$lm5lNO_payouts <- pmax(0, df0$lm5lNO_mortality - trigger) * 1000
df0$lm5lNO_ins <- (df0$capital + df0$lm5lNO_payouts) - premium

#========================================================================
#3.0 Plot perfect insurance superimposed lm5+lNO index Insurance contract
#========================================================================
#Make plots of perfect insurance contract
library(ggplot2)
x11()
cols <- c("blue", "red", "green")
ggplot(df0, aes(x=capital)) +
  geom_smooth(aes(y=lm5lNO_ins,  colour="log NO index"), se = FALSE, size=0.7, method = 'gam')+
  geom_point(aes(y=lm5lNO_ins), colour="red", size=1.5)+
  geom_line(aes(y=capital, colour="Income without insurance"), size=0.7) +
  geom_line(aes(y=perfect_ins, colour="Perfect insurance"), size=0.7)+
  labs(y="Assets (USD)", x="Assets (USD)") +
  scale_colour_manual(name="Type", values=cols) + 
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), axis.line.x=element_line(), axis.line.y=element_line(),
        axis.text.x = element_text(color="black",size=10),
        axis.text.y = element_text(color="black",size=10))  
#https://stackoverflow.com/questions/26587940/ggplot2-different-legend-symbols-for-points-and-lines

