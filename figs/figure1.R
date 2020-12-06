rm(list=ls(all=TRUE))
unlink(".RData") 
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
library(lava)
library(scales)
ce_income(dff$capital, rho=2)
ce_income(dff$capital_ins, rho=2)

#Make plots of perfect insurance contract
x11()
income   <- dff$capital
c_i <- dff$capital_ins
#png("figs/econ_fig1.png", 800, 800, pointsize = 24)
plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",xlab="Assets (USD)", 
     ylab="Assets (USD)", lwd=2)
lines(income[order(income)], c_i[order(income)], col="blue", lwd=2.5, lty=4)
#points(income, c_i, col="green", pch=1, cex=0.9)
curly(x=3320, y=3500, len= 200, lty=1, lwd=1.5, theta=pi)
text(x = 2850, y = 3500, expression(paste(symbol("\xd1"),'=I(',theta,')-p>0')), cex=1.2)
curly(x=4000, y=3950, len= 100, wid=50,lty=1, lwd=1.5,theta=pi*2)
text(x = 4360, y = 3940, expression(paste(symbol("\xd1"),'=-p<0')), cex=1.2)
pdf <- data.frame(x=density(dff$mortality_rate)$x, y=density(dff$mortality_rate)$y)
pdf$x <- rescale(pdf$x, to = c(min(income), max(income)))
pdf$y <- rescale(pdf$y, to = c(min(income), max(income)))
lines(pdf, col = "black", lty=5, lwd=2) 
legend("top", lty = c(1,4,5), lwd=c(2,2,2), col = c("red", "blue",'black'), 
       legend=c('Asset without insurance', 'Asset with insurance','Mortality PDF'), bty="n")

#legend("top", lty = c(1,4,5), lwd=c(2,2,2), pch = c(NA,1,NA), col = c("red", "green",'blue'), 
       #legend=c('Asset without insurance', 'Asset with insurance','Mortality PDF'))
#dev.off()


# #Perfect Insurance Histogram
# x11()
# hist(c_i, breaks=20, xlab="Mortality", ylab="", main="", probability = T)
# lines(density(c_i), col = "red")                   # Overlay density curve
# hist(x, prob=T, br=50, col="skyblue2", main=lbl)
# lines(density(x), col="red")
# 
# 
# 
# library(ggplot2)
# x11()
# ggplot(data = dff,  aes(x = capital, y= capital)) +
#   geom_line(aes(y= capital_ins)) +
#   geom_line(aes(y= capital))+
#   geom_density(aes(x=capital_ins))
# 
# ggplot(dff) +
#   geom_density(aes(x=capital_ins), col='red')


