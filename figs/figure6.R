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
classes <- c("Type0", "Type1", "Type2", "Type3", "Type4")
#classes <- c("Type0", "Type1", "Type2", "Type3", "Type4", "Type5")
pcols <- cols[match(dff$class, classes)]
#sy <- c(18, 17, 15, 9, 7, 10)
sy <- c(18, 17, 15, 9, 7)
sch1 <- sy[match(dff$class, classes)]

##:LayMam statistic Summary
stat <- dff[,c("mortality_rate", "capital","perfect_ins","zlmodis_payouts","Modis_ins",'class')]
boxplot(mortality_rate~class,data=stat)
#tapply(stat$mortality_rate,stat$class,mean)
d <- aggregate(.~class, stat, mean)
leg <- c('True Negatives', 'Severe False Negatives','Intermediate False Negatives','Small False Negative', 'False Positives')
d$class[match(d$class,classes)] <- leg
write.csv(d,'Figure6_Statistics.csv')

#x11()
#png("figs/figure6.png", units="in", width=12, height=12, res=300, pointsize=24)
#tiff("figs/figure6.tif", units="in", width=12, height=12, res=300, pointsize=24)
tiff("figs/figure6.tif", units="px", width=2250, height=2625, res=300, pointsize=18)

plot(Modis_ins~capital, data = dff, col=pcols, pch=sch1, xlab='Asset ($)', 
     ylab='Asset ($)', cex.axis=0.8, cex.lab=0.9, xlim =c(300,1000), ylim=c(300,1000))
x=dff$capital
y=dff$perfect_ins
lines(x,x,lwd=1.2)
lines(x[order(x)],y[order(x)],lwd=2,lty=2)
legend(700, 550, lwd=1.5, lty = c(1,2), cex=.8,
       legend=c('No insurance', 'Perfect insurance'), bty="n", merge = T)

legend("bottomright", pch=c(18, 17, 15, 9, 7, 10), col = cols, cex=0.8,
       legend=c('True Negatives', 'Severe False Negatives','Intermediate False Negatives','Small False Negative', 'False Positives'), bty="n", 
       title="Example insurance contract", title.adj=0.1)

dev.off()
