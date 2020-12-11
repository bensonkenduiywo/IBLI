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
dff$Modis_ins <- round(dff$Modis_ins,3)
library(ggplot2)
#x11()
dff$class <- "Type0"
v <- c(370.694,467.904,677.168,686.861,691.013,704.738,715.484,715.572,725.987)
dff$class[dff$Modis_ins %in% v]  <- "Type1"
#dff$class[dff$Modis_ins > 500 & dff$capita < 650]  <- "Type2"
v <- c(542.263,567.338,564.862, 563.000,608.079,669.691,680.997)
dff$class[dff$Modis_ins %in% v]  <- "Type2"
v <- c(539.463, 667.798, 739.201, 744.914)
dff$class[dff$Modis_ins %in% v]  <- "Type3"
v <- c(784.893, 795.194,780.849,943.809, 872.652,856.845, 964.304, 966.183) 
dff$class[dff$Modis_ins %in% v]  <- "Type4"
#v <- c(872.652,856.845, 964.304, 966.183)
#dff$class[dff$Modis_ins %in% v]  <- "Type5"
#cols <- c("black","red", "blue", "green", "purple","cyan")
cols <- c("black","red", "blue", "green", "purple")
#p < - c(1, 2, 3, 4, 5, 6)
# ggplot(dff, aes(x=capital)) +
#   xlim(300, 1000) +
#   ylim(300, 1000) +
#   geom_point(aes(y=Modis_ins, colour=class, shape=class), size=1.8)+
#   scale_colour_manual(values=cols)+
#   #scale_shape_manual(values=p)+
#   #guides("Type1", "Type2","Type3","Type4","Type5")
#   geom_line(aes(y=capital, linetype ="No insurance"), size=0.7) +
#   geom_line(aes(y=perfect_ins, linetype ="Perfect insurance"), size=0.7)+
#   labs(y="Assets (USD)", x="Assets (USD)") +
#   scale_linetype_manual(name="", values=c("No insurance"=1,"Perfect insurance"=2))+
#   theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), 
#         legend.title = element_blank(),
#         axis.line.x=element_line(), axis.line.y=element_line(),
#         axis.text.x = element_text(color="black", size=12),
#         axis.text.y = element_text(color="black", size=12),
#         legend.text=element_text(size=11))  

classes <- c("Type0", "Type1", "Type2", "Type3", "Type4")
#classes <- c("Type0", "Type1", "Type2", "Type3", "Type4", "Type5")
pcols <- cols[match(dff$class, classes)]
#sy <- c(18, 17, 15, 9, 7, 10)
sy <- c(18, 17, 15, 9, 7)
sch1 <- sy[match(dff$class, classes)]
x11()
png("figs/figure6.png", units="in", width=12, height=12, res=300, pointsize=24)
plot(Modis_ins~capital, data = dff, col=pcols, pch=sch1, xlab='Asset (USD)', 
     ylab='Asset (USD)', cex.axis=1.0, cex.lab=1.2, xlim =c(300,1000), ylim=c(300,1000))
x=dff$capital
y=dff$perfect_ins
lines(x,x,lwd=1.2)
lines(x[order(x)],y[order(x)],lwd=2,lty=2)
legend("bottomleft", lwd=1.5, lty = c(1,2), 
       legend=c('No insurance', 'Perfect insurance'), bty="n", merge = T)
legend("bottomright", pch=c(18, 17, 15, 9, 7, 10), col = cols, 
       legend=c('True Negatives', 'Severe False Negatives','Intermediate False Negatives',
                'Small False Negative', 'False Positives'), bty="n")

dev.off()
