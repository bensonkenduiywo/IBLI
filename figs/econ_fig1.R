rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")

library(lava)
dff <- na.omit(dff)
#Perfect Insurance contract
trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$mortality_rate - trigger) * 1000
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.25
#Compute capital with insurance
dff$capital     <- (1 - dff$mortality) * 1000
dff$capital_ins <- (dff$capital + dff$payouts) - premium

#Make plots of perfect insurance contract
x11()
income   <- dff$capital
c_i <- dff$capital_ins
png("figs/econ_fig1.png", 800, 800, pointsize = 24)
plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",xlab="Assets (USD)", ylab="Assets (USD)", lwd=1.2)
lines(income[order(income)], c_i[order(income)], col="green", lwd=1.2)
points(income, c_i, col="green", pch=1, cex=0.9)
curly(x=650, y=698, len= 46, lty=1, lwd=1.2, theta=pi)
text(x = 540, y = 700, 'Insurance relief')
curly(x=900, y=890, len= 13, lty=1, lwd=1.2, wid=5,theta=pi*2)
text(x = 960, y = 890, 'Premium')
legend("bottomright", lwd=1.2, lty = c(1,1), pch = c(NA,1), col = c("red", "green"), legend=c('Asset without insurance', 'Asset with insurance'))
dev.off()


