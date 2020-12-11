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
#
dff$hist <- (1-dff$mortality_rate)*5000

#Make plots of perfect insurance contract

x11()
income   <- dff$capital
c_i <- dff$capital_ins
png("figs/figure1.png", units="in", width=12, height=12, res=300, pointsize=24)
plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",xlab="Assets (USD)", 
     ylab="Assets (USD)", lwd=1.5)
lines(income[order(income)], c_i[order(income)], col="blue", lwd=2, lty=5)
points(income, c_i, col="blue", pch=1, cex=1)
curly(x=3320, y=3500, len= 200, lty=1, lwd=1.5, theta=pi)
text(x = 2850, y = 3500, expression(paste(symbol(Delta),'=I(',theta,')-p>0')), cex=1.2)
curly(x=4000, y=3950, len= 100, wid=50,lty=1, lwd=1.5,theta=pi*2)
text(x = 4360, y = 3940, expression(paste(symbol(Delta),'=-p<0')), cex=1.2)
pdf <- data.frame(x=density(dff$hist)$x, y=density(dff$hist)$y)
pdf$x <- rescale(pdf$x, to = c(min(income), max(income)))
pdf$y <- rescale(pdf$y, to = c(min(income), max(income)))
lines(pdf, col = "black", lty=4, lwd=2) 
legend("top", lty = c(1,5,4), pch=c(NA, 1, NA), lwd=c(2,2,2), col = c("red", "blue",'black'), 
       legend=c('Asset without insurance', 'Asset with insurance','Asset PDF'), bty="n")

#legend("top", lty = c(1,4,5), lwd=c(2,2,2), pch = c(NA,1,NA), col = c("red", "green",'blue'), 
       #legend=c('Asset without insurance', 'Asset with insurance','Mortality PDF'))
dev.off()


# h <- hist(dff$hist, breaks = 30, plot=FALSE)
# h$counts=h$counts/sum(h$counts)
# plot(h)
# x1 = income
# y1 = rescale(income, to = c(0, 0.15))
# lines(x1[order(x1)], y1[order(x1)], col="red", lwd=2, lty=5)
# y2 = rescale(c_i, to = c(0, 0.15))
# lines(x1[order(x1)], y2[order(x1)], col="blue", lwd=2, lty=5)
# points(x1, y2, col="blue", pch=1, cex=1)

# #Perfect Insurance Histogram
# x11()
# hist(c_i, breaks=20, xlab="Mortality", ylab="", main="", probability = T)
# lines(density(c_i), col = "red")                   # Overlay density curve
# hist(x, prob=T, br=50, col="skyblue2", main=lbl)
# lines(density(x), col="red")
# 
# 
# 
dd <- data.frame(pdf= pdf, income=dff$capital, insurance=dff$capital_ins)
library(ggplot2)
x11()
ggplot(data = dff,  aes(x = capital, y= capital)) +
  geom_line(aes(y= capital_ins), size=0.9, color='blue') +
  geom_point(aes(y = capital_ins), color='blue', shape=1,size=2)+
  geom_line(aes(y= capital), size=0.9, linetype='dotdash')+
  geom_line(data=pdf, aes(x=x,y= y), size=0.9, linetype='longdash')
library(pBrackets)
#grid.locator(unit="native") 
grid.brackets(292, 363, 292, 244, lwd=2, col="red")

# ggplot(dff) +
#   geom_density(aes(x=capital_ins), col='red')


