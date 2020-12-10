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


ddo$lambda <- fscale(ddo$lambda)+0.15
ddo$delta <- scale2mean(ddo$delta)-0.1


#
x11()
png("figs/figure2.png", units="px", width=1600, height=1600, res=300, pointsize=24)
ggplot(data = ddo, aes(x = hist)) +
  geom_histogram(aes(y=..ncount..),
                 col="black",
                 bins = 25,
                 alpha=.2,
                 fill="white") +
  geom_line(data = ddo, aes(x = hist, y = lambda, linetype = "lambda")) + 
  geom_line(data = ddo, aes(x = hist, y = delta, linetype = "delta")) + 
  scale_linetype_manual(values=c("solid", "longdash"),name = '', labels = expression(lambda,  Delta))+
  labs(y="", x="Assets") +
  scale_x_continuous(expand = expansion(mult = c(0, 0)))+ 
  #scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  scale_y_continuous(expand = c(0, 0), limits = c(-0.17, NA))+
  theme(legend.position= "right",
        panel.background = element_rect(fill = 'white',linetype = 1, colour='black'),
        #panel.background = element_blank(),
        axis.text.x=element_text(size=12,colour="black"),
        axis.text.y=element_text(size=12, colour="black"),
        axis.title=element_text(size=13),
        legend.text = element_text(size=16),
        legend.key=element_blank()
        )
dev.off()

#ggsave("figs/figure2.png",dpi=300)


  # 
  # theme(legend.position= "right", panel.background = element_rect(fill = "white"), 
  #       axis.line.x = element_line(), axis.line.y=element_line(),
  #       axis.text.x = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       legend.text = element_text(size=14))  

  

