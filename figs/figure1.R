
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
} else { setwd('') }
rm(list=ls(all=TRUE)) #https://www.r-bloggers.com/

dff <- readRDS("m_data.rds")
dff <- na.omit(dff)
#Perfect Insurance contract
trigger <- 0.23 
#Compute payouts; 1TLU=1000$
tlu <- 1
dff$payouts <- pmax(0, dff$mortality_rate - trigger) * 1000 * tlu
#Compute Actuarily fair premiumL Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.25
#Compute capital with insurance
dff$capital     <- (1 - dff$mortality) * 1000 * tlu
dff$capital_ins <- (dff$capital + dff$payouts) - premium

#Compute CE with and without insurance
library(agro)
library(lava)
library(scales)
ce_income(dff$capital, rho=2)
ce_income(dff$capital_ins, rho=2)
#
dff$hist <- (1-dff$mortality_rate)*1000 * tlu

income   <- dff$capital
c_i <- dff$capital_ins

#Make plots of perfect insurance contract



# plot(income, income, col="red", xlim=range(income), ylim=range(income), type="l",
#      xlab="Assets ($)", ylab="Assets ($)", lwd=1.5, cex.lab=0.9, cex.axis=0.8)
# lines(income[order(income)], c_i[order(income)], col="blue", lwd=2, lty=5)
# points(income, c_i, col="blue", pch=1, cex=1)
# 
# #curly(x=3320, y=3500, len= 200, lty=1, lwd=1.5, theta=pi)
# #text(x = 2800, y = 3500, expression(paste(symbol(Delta),' = I(',theta,')-p>0')), cex=1)
# curly(x=550, y=648, len= 93, wid=25,lty=1, lwd=1.5, theta=pi)
# text(x = 430, y = 648, expression(paste(symbol(Delta),' = I(',theta,')-p>0')), cex=1)
# 
# #curly(x=4000, y=3950, len= 80, wid=50,lty=1, lwd=4, col="white", theta=pi)
# curly(x=800, y=780, len= 25, wid=15,lty=1, lwd=1.5,theta=pi)
# text(x = 720, y = 780, expression(paste(symbol(Delta),' = -p<0')), cex=1)
# 
# pdf <- data.frame(x=density(dff$hist)$x, y=density(dff$hist)$y)
# pdf$x <- rescale(pdf$x, to = c(min(income), max(income)))
# pdf$y <- rescale(pdf$y, to = c(min(income), max(income)))
# lines(pdf, col = "black", lty=4, lwd=2) 
# legend("topleft", lty = c(1,5,4), pch=c(NA, 1, NA), lwd=c(2,2,2), col = c("red", "blue",'black'), 
#        legend=c('Asset without insurance', 'Asset with insurance','Asset PDF'), bty="n", cex=0.8)



#legend("top", lty = c(1,4,5), lwd=c(2,2,2), pch = c(NA,1,NA), col = c("red", "green",'blue'), 
       #legend=c('Asset without insurance', 'Asset with insurance','Mortality PDF'))


#png("figs/figure1.png", units="in", width=12, height=12, res=300, pointsize=24)
#setEPS()
#postscript("figs/figure1.pdf", width=12, height=12, pointsize=24)
#tiff("figs/figure1.tif", units="in", width=12, height=12, res=300, pointsize=24)
tiff("figs/figure1.tif", units="px", width=2250, height=2250, res=300, pointsize=16)

h <- hist(dff$hist, breaks = 25, main = "", border='black',
          col='white', xlim=c(300,1000))

par(mar=c(4, 4, 4, 4))#c(bottom, left, top, right)
plot(income, income, col="red", xlim=range(income), ylim=range(income), 
     xlab='', ylab='', type="l", lwd=1.5, cex.lab=0.9, cex.axis=0.8)
mtext("Assets ($)", side = 1, line = 2, cex = 0.9)
mtext("Assets ($)", side = 2, line = 2, cex = 0.9)
lines(income[order(income)], c_i[order(income)], col="blue", lwd=2, lty=5)
points(income, c_i, col="blue", pch=1, cex=1)
curly(x=550, y=648, len= 95, wid=25,lty=1, lwd=1.5, theta=pi)
text(x = 430, y = 648, expression(paste(symbol(Delta),' = I(',theta,')-p>0')), cex=1)
curly(x=805, y=790, len= 20, wid=15,lty=1, lwd=1.5,theta=pi)
text(x = 720, y = 790, expression(paste(symbol(Delta),' = -p<0')), cex=1)

par(new=TRUE) #c(bottom, left, top, right)
plot(h, xlab='', ylab='', main="", axes = "False")
axis(4, col='black', cex.axis=0.8) 

mtext("Probability", side = 4, line = 2, cex = 0.9)
legend("topleft", lty = c(1,5,NA), pch=c(NA, 1, 0), lwd=c(2,2,1), col = c("red", "blue",'black'), 
       legend=c('Asset without insurance', 'Asset with insurance','Asset histogram'), bty="n", cex=0.8)

dev.off()