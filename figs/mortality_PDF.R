rm(list=ls(all=TRUE))
dff <- readRDS("m_data.rds")
x11()
mortality <- dff$mortality_rate*100

h <- hist(mortality, breaks = 50, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h, xlab="Mortality rate(%)")
lines(density(energy),col="red")
#Check if area under curve sum to 1
dens <- density(mortality)
sum(dens$y)*diff(dens$x[1:2])

#Another way of plotting
#https://stackoverflow.com/questions/6973579/plotting-probability-density-mass-function-of-dataset-in-r
h <- hist(mortality, breaks = 50, plot=FALSE)
dens1 <-  h$counts/sum(h$counts)
dens2 <- dnorm(mortality,0,0.5)

hist(mortality,probability=TRUE,breaks="fd", ylim=c(0,0.5))
lines(h$mids,dens1,col="red")
lines(mortality[order(dens2)],dens2[order(dens2)],col="darkgreen")
