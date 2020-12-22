rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")

library(segmented)
dff <- na.omit(dff)

df <- dff
 
dff <- df[df$SUBLOCATION=="KARARE",]
#Predict mortality using segmented regression
x <- dff$zlmodis
y <- dff$mortality_rate
ml <- lm(y~x, data=dff)
sm <- segmented(ml, seg.Z = ~x, psi=0)
#plot(y~x, pch=16, cex=0.9, main="Segmented regression", cex.main=1.1, cex.lab=1.1, xlab="z-score(lMD)", ylab="Mortality rate")
#plot.segmented(sm, add=T, lwd=2, lty=1, col="red") 
dff$predicted_mortality <- predict(sm)

trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$predicted_mortality - trigger) * 1000
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.1
#Compute capital with insurance
dff$capital     <- (1 - dff$mortality) * 1000
dff$capital_ins <- (dff$capital + dff$payouts) - premium
saveRDS(dff$capital_ins, "ins_model.rds")
library(lava)

png("figs/fig5.png", 800, 800, pointsize = 24)
x11()
income   <- dff$capital
c_i <- dff$capital_ins

plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",xlab="Assets (USD)", ylab="Assets (USD)")
lines(income[order(income)], c_i[order(income)], col="green")
curly(x=650, y=700, len= 46, lty=1, lwd=1, theta=pi)
text(x = 600, y = 700, 'Insurance')
curly(x=900, y=890, len= 10, lty=1, lwd=1, wid=5,theta=pi*2)
text(x = 950, y = 890, 'Premium')
#text(x = 900, y = 885, '{', srt = 180, cex = 1.5, family = 'mono')
#text(x = 600, y = 700, '}', srt = 180, cex = 5, family = 'mono')
legend("bottomright", lty = 1, col = c("red", "green"), legend=c('Asset without insurance', 'Asset with insurance'))
dev.off()

library(ggplot2)
x11()
ggplot(data=dff, aes(x=capital, y=capital)) +
  geom_line(aes(x=capital, y=capital), color="blue", size=1.1)+
  geom_line(aes(x=capital, y=c_i), color="red", size=1.1)
