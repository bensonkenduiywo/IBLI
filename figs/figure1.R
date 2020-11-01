rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")
dff <- na.omit(dff)
#Perfect Insurance contract
trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$mortality_rate - trigger) * 5000
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.25
#Compute capital with insurance
dff$capital     <- (1 - dff$mortality) * 5000
dff$capital_ins <- (dff$capital + dff$payouts) - premium

#Compute CE with and without insurance
library(agro)

ce_income(dff$capital, rho=2)
ce_income(dff$capital_ins, rho=2)

#Make plots of perfect insurance contract
x11()
income   <- dff$capital
c_i <- dff$capital_ins
#png("figs/econ_fig1.png", 800, 800, pointsize = 24)
plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",xlab="Assets (USD)", ylab="Assets (USD)", lwd=1.2)
lines(income[order(income)], c_i[order(income)], col="green", lwd=1.2)
points(income, c_i, col="green", pch=1, cex=0.9)
curly(x=650, y=698, len= 46, lty=1, lwd=1.2, theta=pi)
text(x = 550, y = 700, expression(paste(symbol("\xd1"),'=I(',theta,')-p>0')), cex=1.2)
curly(x=900, y=890, len= 20, lty=1, lwd=1.2, wid=6,theta=pi*2)
text(x = 960, y = 890, expression(paste(symbol("\xd1"),'=<p<0')), cex=1.2)
legend("bottomright", lwd=1.2, lty = c(1,1), pch = c(NA,1), col = c("red", "green"), legend=c('Asset without insurance', 'Asset with insurance'))
#dev.off()

x11()
ggplot(data = dff,  aes(x = capital, y= capital)) + 
  geom_line(aes(y= capital_ins)) + 
  geom_line(aes(y= capital))

