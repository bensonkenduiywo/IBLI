
# if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
# } else { setwd('C:/Users/camila/Google Drive/AfSF') }

library(dplyr)
library(raster)
library(agro)

n <- agrodata::data_ibli("marsabit_avhrr_ndvi") #NOAA
colnames(n)[1:5]
m <- agrodata::data_ibli("marsabit_modis_ndvi_agg") #MODIS
colnames(m)[1] <- toupper(colnames(m)[1])
colnames(m)[1:5]
p <- agrodata::data_ibli("marsabit_chirps_precipitation") #rainfall
colnames(p)[1:5]
p <- reshape2::melt(p, variable.name = "date", value.name = "rainfall",id.vars = "SUBLOCATION")
p$date <- as.Date(p$date, format = "X%Y%m%d")
n <- reshape2::melt(n, variable.name = "date", value.name = "noaa", id.vars = "SUBLOCATION")
n$date <- as.Date(n$date, format = "X%Y%m%d")
m <- reshape2::melt(m, variable.name = "date", value.name = "modis", id.vars = "SUBLOCATION")
m$date <- as.Date(m$date, format = "X%Y_%m_%d")

seasonMean <- function(year, df, seasons=1:2) {
	#df$date <- as.Date(df$date, format = "X%Y%m%d")
  res <- list()
  for (i in seasons) {
    season <- ifelse(i==1, "long", "short")
    if (season =="long") {
      sdate <- paste0(year, "-03-01")
      edate <- paste0(year, "-09-30")
      season <- "LRLD"
    } else if (season =="short") {
      sdate <- paste0(year-1, "-10-01")
      edate <- paste0(year, "-02-28")
      season <- "SRSD"
    } else {
      stop("Define season")
    }
    ydf <- df[df$date >= sdate & df$date <= edate, ]
    ym <- aggregate(ydf[,3], ydf[,1, drop=FALSE], mean, na.rm=T)
    ym$year <- year
    ym$season <- season
    res[[i]] <- ym    
  }  
  do.call(rbind, res)  
}

years <- 1982:2019
#CHIRPS
temp <- lapply(years, seasonMean, p)
p <- do.call(rbind, temp)
names(p)[2] <- "rainfall"

#NOAA NDVI
temp <- lapply(years, seasonMean, n)
n <- do.call(rbind, temp)
names(n)[2] <- "noaa"

#MODIS NDVI
years <- 2001:2019
temp <- lapply(years, seasonMean, m)
m <- do.call(rbind, temp)
names(m)[2] <- "modis"

mort <- agrodata::data_ibli("marsabit_mortality.rds")
mort2 <- mort[mort$type == "TLU", ]  # remote TLU for merging

amort <- aggregate(mort2[,c("stock_beginning", "loss"), drop=FALSE], mort2[, c("season","year","sublocation"), drop=FALSE], mean, na.rm=TRUE)
amort$mortality_rate <- (amort$loss / amort$stock_beginning) # * 100


## Normal vs lognormal data treament 
zscore <- function(y, log=FALSE){
    # +.05 to avoid NA from log(x) where x <= 0
    if (log) y <- log(y+.05)
    (y - mean(y, na.rm=TRUE) ) / (sd(y, na.rm=TRUE))
}

#Create data frame
df1 <- merge(p, n, by = c("SUBLOCATION","year", "season")) #NOAA and CHIRPSs
df1 <- merge(df1, m, by = c("SUBLOCATION","year", "season"), all.x=TRUE)#Add modis data
#Compute Z-scores per season per sublocation
dff <- ungroup(mutate(group_by(df1, SUBLOCATION, season), znoaa=zscore(noaa), zrain=zscore(rainfall), zmodis=zscore(modis),  zlnoaa=zscore(noaa, TRUE), zlrain=zscore(rainfall, TRUE), zlmodis=zscore(modis, TRUE)))

colnames(amort)[3] <- "SUBLOCATION"
dff <- merge(dff, amort, by=c("year", "season", "SUBLOCATION"))


regfun <- function(x, y, main="", label="", ylab="") {


  df <- na.omit(data.frame(x=x, y=y))
  df0 <- df[df$x<0,]
  df5 <- df[df$x < -0.5,]
 
	xlab <- paste0("z-score (", main, ")")
 
	plot(y~x, pch=16, main=label, cex=0.9, cex.main=1.2, cex.axis=0.8, 
	cex.lab=1.2, xlab="", ylab="", las=1)

	title(xlab=xlab, line=2)
	title(ylab=ylab, line=3)

  ml <- lm(y~x, data=df)
  abline(ml, col="magenta", lwd=3, lty=2)  
  ml2 <- lm(y~x, data=df0)
  abline(ml2, col="blue", lwd=3, lty=3)  
  ml5 <- lm(y~x, data=df5)
  abline(ml5, col="green", lwd=3, lty=4)  

  sm <- segmented(ml, seg.Z = ~x, psi=0)
  plot.segmented(sm, add=T, lwd=2, lty=1, col="red") 
  
	if (label == "(c)") {  
	  legend("topright", c("lm", "lm0", "lm5", "sm"), col=c("magenta", "blue", "green", "red"), lwd=c(3,3,3,2), lty=c(2,3,4,1)) #, bty="n")
	}
}


png("figs/figure3.png", units="in", width=12, height=12, res=300, pointsize=24)
#png("figs/fig2.png", 800, 800, pointsize = 24)
par(mfrow=c(2, 3), mar=c(4.5, 4, 1.8, 0.2)) #c(bottom, left, top, right)
regfun(dff$znoaa, dff$mortality_rate, "NO", "(a)", ylab="mortality rate")
regfun(dff$zrain, dff$mortality_rate, "RN", "(b)")
regfun(dff$zmodis, dff$mortality_rate, "MD", "(c)")

regfun(dff$zlnoaa, dff$mortality_rate, "lNO", "(d)", ylab="mortality rate")
regfun(dff$zlrain, dff$mortality_rate, "lRN", "(e)")
regfun(dff$zlmodis, dff$mortality_rate, "lMD", "(f)")
dev.off()



