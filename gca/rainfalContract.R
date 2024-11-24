rm(list=ls(all=TRUE))
g <- gc(reset = T);
library(dplyr)
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies = T)
lapply(list.of.packages, require, character.only = TRUE)
#Index Insurance
rain <- readRDS("gca/Trans-Nzoia_Chirps_Rainfall.rds")
head(rain)
startDate <- "2024.03.01"
endDate <- "2024.03.14"
plant <- rain[rain$Date >= startDate & rain$Date <= endDate, ]
#temp <- ungroup(mutate(group_by(id), znoaa=zscore(noaa), zrain=zscore(rainfall), zmodis=zscore(modis),  zlnoaa=zscore(noaa, TRUE), zlrain=zscore(rainfall, TRUE), zlmodis=zscore(modis, TRUE)))
germ <- aggregate(Rainfall~id, plant, sum, na.rm=T, drop=T)
plot(Rainfall~id, data=germ, ylab='Total rainfall', xlab='Grid ids', main="Germination")

#Contract
#Assuming an rainfall index that nakes pay-outs if rainfall is below a 10 mmm in  a given 10 by 10 km grid id.
#trigger is the rainfall threshold below which the insurance pays
#exit is the z-score level corresponding to maximum payment (100%).
indeminity <- function(r, trigger=10, exit=0, suminsured=100){
  result <- ifelse(r < trigger,
                   100*((trigger-r)/(trigger-exit)),
                   0)
  return(result)
}
germ$indeminity <- 0
germ$indeminity <- indeminity(germ$Rainfall, trigger=10, exit=0, suminsured=100)
print(germ)
