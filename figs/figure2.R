rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")
dff <- na.omit(dff)
trigger <- 0.23 
#Compute payouts; 1TLU=1000$
dff$payouts <- pmax(0, dff$mortality_rate - trigger)  * 5000
#Compute Actuarialy fair  Premium with markup 25%
premium <- mean(dff$payouts, na.rm=TRUE) * 1.25
#Compute capital with insurance
dff$capital     <- (1 - dff$mortality)*5000
dff$capital_ins <- (dff$capital + dff$payouts) - premium

dff$hist <- (1-dff$mortality_rate)*5000
dff$lambda <- dff$capital^-2
dff$delta <- dff$capital_ins - dff$capital 

#Working independently but needs to be combined
#=====
#Mortality histogram
x11()
hist(dff$hist, breaks=20, xlab="Mortality", ylab="", main="", probability = T)
#Lambda plot
x11()
plot(dff$hist[order(dff$hist)],dff$lambda[order(dff$hist)], type = "l")
#delta plot
x11()
plot(dff$hist[order(dff$hist)],dff$delta[order(dff$hist)], type = "l")

#Now combine all plots to one with similar y-axis
library(ggplot2)
# ddf is from your script
ddo <- dff[order(dff$hist),]  
# Now combine all plots to transform to a rescaled y-axis, between 0 and 1?
fscale <- function(x){
  (x - min(x))/(max(x)-min(x))
}

#Mean normalization formula to -1 and 1
scale2mean <- function(x){
  (x - mean(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}


ddo$lambda <- fscale(ddo$lambda)
ddo$delta <- fscale(ddo$delta)

x11()
ggplot(data = ddo, aes(x = hist)) +
  geom_histogram(aes(y=..ncount..),
                 col="black",
                 bins = 20,
                 alpha=.2) +
  geom_line(data = ddo, aes(x = hist, y = lambda, colour='blue')) + 
  geom_line(data = ddo, aes(x = hist, y = delta, colour='green')) + 
  scale_colour_manual(values = c('blue' = 'blue', 'green' = 'green'),name = '', labels = expression(lambda,  Delta)) +
  labs(y="", x="Assets") +
  theme(legend.position= "bottom", panel.background = element_rect(fill = "white"), 
        axis.line.x = element_line(), axis.line.y=element_line(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.text = element_text(size=14))  

  

