#========================================================================
#0.0 Load data
#========================================================================
rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")
dff <- na.omit(dff)
library(ggplot2)
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
set.seed(100)
library(segmented)
sm <- segmented(ml, seg.Z = ~x, psi=0)
#plot(y~x, pch=16, cex=0.9, main="Segmented regression", cex.main=1.1, cex.lab=1.1, xlab="z-score(lMD)", ylab="Mortality rate")
#plot.segmented(sm, add=T, lwd=2, lty=1, col="red") 
dff$zlrain_mortality <- predict(sm)
#Compute MODIS index payouts; 1TLU=1000$
dff$zlrain_payouts <- pmax(0, dff$zlrain_mortality - trigger) * 1000
dff$zlrain_ins <- (dff$capital + dff$zlrain_payouts) - premium

#Filter data fo equal comparison with lm5 model
#dff <- dff[dff$zlmodis< -0.5,]
dff$zlrain_ins <- round(dff$zlrain_ins,3)

#========================================================================
#3.0 CLASSIFY POINTS IN contract in 2 
#========================================================================
#Make plots of perfect insurance contract

dff$class <- "Type0"
v <- c(467.904, 471.060,495.503,502.861,653.984,677.168,686.861,687.529,691.013,704.738,715.484,715.572,722.851,734.156)
dff$class[dff$zlrain_ins %in% v]  <- "Type1"
v <- c(438.948,607.496,596.581)
dff$class[dff$zlrain_ins %in% v]  <- "Type2"
v <- c(454.310, 579.617, 657.694, 747.760)#667.798, 739.201, 744.914
dff$class[dff$zlrain_ins %in% v]  <- "Type3"
v <- c(796.271, 777.878,797.769,905.353,937.981,978.579,949.231, 800.974, 815.323, 908.733, 946.703)#454.311, 579.618,607.496, 657.695
dff$class[dff$zlrain_ins %in% v]  <- "Type4"
#v <- c(905.353,937.981,978.579,949.231, 800.974, 815.323, 908.733, 946.703) #905.35,945.23,937.98,978.58
#dff$class[dff$zlrain_ins %in% v]  <- "Type5"

#========================================================================
#4.0 Plot contract in 2 above over perfect model
#========================================================================
cols <- c("black","red", "blue", "green", "purple","cyan")
# p1 <- ggplot(dff, aes(x=capital)) +
#   xlim(300, 1000) +
#   ylim(300, 1000) +
#   geom_point(aes(y=zlrain_ins, colour=class, shape=class), size=1.5)+
#   scale_colour_manual(values=cols)+
#   geom_line(aes(y=capital, linetype ="No insurance"), size=0.7) +
#   geom_line(aes(y=perfect_ins, linetype ="Perfect insurance"), size=0.7)+
#   labs(y="Assets (USD)", x="Assets (USD)",title = "(a)") +
#   scale_linetype_manual(name="", values=c("No insurance"=1,"Perfect insurance"=2))+
#   theme(panel.background = element_rect(fill = "white"), 
#         legend.title = element_blank(),
#         axis.line.x=element_line(), axis.line.y=element_line(),
#         axis.text.x = element_text(color="black", size=12),
#         axis.text.y = element_text(color="black", size=12),
#         legend.text=element_text(size=11), 
#         plot.title = element_text(hjust = 0.5,face = "bold"))

#========================================================================
#5.0 lMD+lm5 contract (R2=0.418, RIB=0.49886841)
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

df0$lm5lMD_ins <- round(df0$lm5lMD_ins, 3)

#========================================================================
#6.0 cLASSIFY POINTS IN 5 ABOVE
#========================================================================

df0$class.lMD <- "Type0"
v <- c(370.694, 677.168, 715.484, 715.572, 725.987)
df0$class.lMD[df0$lm5lMD_ins %in% v]  <- "Type1"
v <- c(536.664, 557.468, 559.680, 563.607, 610.711, 670.899, 688.960)
df0$class.lMD[df0$lm5lMD_ins %in% v]  <- "Type2"
v <- c(497.951,620.497, 721.962, 734.236, 743.293)
df0$class.lMD[df0$lm5lMD_ins %in% v]  <- "Type3"
v <- c(791.273, 827.501,841.954, 936.100,946.541, 946.650, 855.321, 783.652)
df0$class.lMD[df0$lm5lMD_ins %in% v]  <- "Type4"
#v <- c(936.100,946.541, 946.650, 855.321, 783.652)
#df0$class.lMD[df0$lm5lMD_ins %in% v]  <- "Type5"

#========================================================================
#8.0 Plot contract in 5 above over perfect model
#========================================================================
# p2 <- ggplot(df0, aes(x=capital)) +
#   xlim(300, 1000) +
#   ylim(300, 1000) +
#   geom_point(aes(y=df0$lm5lMD_ins, colour=class.lMD, shape=class.lMD), size=1.5)+
#   scale_colour_manual(values=cols)+
#   geom_line(aes(y=capital, linetype ="No insurance"), size=0.7) +
#   geom_line(aes(y=perfect_ins, linetype ="Perfect insurance"), size=0.7)+
#   labs(y="Assets (USD)", x="Assets (USD)",title = "(b)") +
#   scale_linetype_manual(name="", values=c("No insurance"=1,"Perfect insurance"=2))+
#   theme(panel.background = element_rect(fill = "white"), 
#         legend.title = element_blank(),
#         axis.line.x=element_line(), axis.line.y=element_line(),
#         axis.text.x = element_text(color="black", size=12),
#         axis.text.y = element_text(color="black", size=12),
#         legend.text=element_text(size=11), 
#         plot.title = element_text(hjust = 0.5,face = "bold"))
# 
# #Form figure 6
# library(gridExtra)
# x11()
# grid.arrange(p1, p2, nrow=2)


x11()
png("figs/figure7.png", units="in", width=12, height=12, res=300, pointsize=24)
par(mfrow=c(2, 2), mar=c(4.5, 4.2, 1.0, 1)) #c(bottom, left, top, right)
classes <- c("Type0", "Type1", "Type2", "Type3", "Type4")
pcols <- cols[match(dff$class, classes)]
sy <- c(18, 17, 15, 9, 7)
sch1 <- sy[match(dff$class, classes)]
plot(zlrain_ins~capital, data = dff, col=pcols, pch=sch1, xlab='Asset (USD)', 
     ylab='Asset (USD)', cex.axis=1.0, cex.lab=1.1, xlim =c(300,1000), 
     ylim=c(300,1000), main='(a)', cex.main=1.0)
x=dff$capital
y=dff$perfect_ins
lines(x,x,lwd=1.2)
lines(x[order(x)],y[order(x)],lwd=2,lty=2)

#==== p2
pcols <- cols[match(df0$class.lMD, classes)]
sch1 <- sy[match(df0$class.lMD, classes)]
plot(lm5lMD_ins~capital, data = df0, col=pcols, pch=sch1, xlab='Asset (USD)', 
     ylab='Asset (USD)', cex.axis=1.0, cex.lab=1.1, xlim =c(300,1000), 
     ylim=c(300,1000), main='(b)', cex.main=1.0)
x=df0$capital
y=df0$perfect_ins
lines(x,x,lwd=1.2)
lines(x[order(x)],y[order(x)],lwd=2,lty=2)


par(fig = c(0, 1, 0, 1), mar = c(11, 6.5, 0, 6.5), new = TRUE)
plot.new()
#plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottomright", pch=c(18, 17, 15, 9, 7, 10), col = cols, 
       legend=c('True Negatives', 'Severe False Negatives','Intermediate False Negatives',
                'Small False Negative', 'False Positives'), bty="n", 
       xpd = T, pt.cex = 1, cex=1.0)
legend("bottomleft", lwd=1.5, lty = c(1,2), 
       legend=c('No insurance', 'Perfect insurance'), bty="n",xpd = T,
       pt.cex = 1, cex=1.0)
dev.off()

#========================================================================
#Figure 7; predictive skill of the two models
#========================================================================
#make a plot of predictive skill for sm using log transformed Z-scored rainfall
x11()
png("figs/figure8.png", units="in", width=12, height=12, res=300, pointsize=24)
par(mfrow=c(1, 2), mar=c(18.5, 4, 1.8, 0.5)) #c(bottom, left, top, right)
dff <- dff[dff$zlmodis< -0.5,]
x <- dff$mortality_rate
y <- dff$zlrain_mortality
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(a)", cex.axis=0.85, cex.lab=0.9, cex.main=0.9, cex=0.65)
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")
x <- df0$mortality_rate
y <- df0$lm5lMD_mortality
#make a plot of predictive skill for lm5 model based on log transformed MODIS (lMD)

plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)",
     main="(b)", cex.axis=0.85, cex.lab=0.9, cex.main=0.9, cex=0.65)
abline(0, 1, lwd=1.5, col="blue")
abline(h=23, col="red")
abline(v=23, col="red")
dev.off()
#ALTERNATIVE PLOT
# x11()
# plot(dff$capital,dff$capital,xlab="Assets (USD)", ylab="Assets (USD)", type="n")
# cola <- c("black","red", "blue", "green", "purple","yellow")[dff$class]
# points(dff$capital,dff$zlrain_ins,col=cola, pch=16,cex=0.7)
# lines(dff$capital,dff$capital,lty=1,col="black")
# z <- dff$zlrain_ins
# lines(dff$capital,dff$zlrain_ins,lty=2,col="black")