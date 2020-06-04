#Create a function to compute means for Long Rain Long Dry (March -- September) and Short Rain Short Dry (October -- February) seasons.
seasonMean <- function(year, df, season){
  if(season =="long"){
    sdate <- paste0(year, "-03-01")
    edate <- paste0(year, "-09-30")
    season <- "LRLD"
  }else if (season =="short"){
    sdate <- paste0(year-1, "-10-01")
    edate <- paste0(year, "-02-28")
    season <- "SRSD"
  }else{
    print("Define season")
  }
  ydf <- df[df$date >= sdate & df$date <= edate, ]
  ym <- aggregate(ydf[,3], ydf[,1, drop=FALSE], mean, na.rm=T)
  ym$year <- year
  ym$season <- season
  return(ym)
}