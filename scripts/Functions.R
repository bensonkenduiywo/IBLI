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

rmse <- function(error){
  return(sqrt(mean(error^2)))
}


MAPE <- function (y_pred, y_true){
  MAPE <- mean(abs((y_true - y_pred)/y_true))
  return(MAPE*100)
}

R_square <- function(actual, predicted) {
  val <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
  return(val)
} 

zscore <- function(y){
  (y - mean(y, na.rm=TRUE) ) / (sd(y, na.rm=TRUE))
}

test <- function(rhos, df1){
  ce_base <- ce_ins <- ce_ins_nomarkup<- mqs <- mqs_no <- rep(NA, length(rhos))
  for(i in 1:length(rhos)){
    ce_base[i] <- ce_income(df1$y_noins, rhos[i])
    ce_ins[i]  <- ce_income(df1$y_25markup, rhos[i])
    ce_ins_nomarkup[i] <- ce_income(df1$y_nomarkup, rhos[i])
    mqs[i]    <- ce_ins[i] - ce_base[i]
    mqs_no[i] <- ce_ins_nomarkup[i] - ce_base[i]
  }
  return(list(mqs=mqs,mqs_no=mqs_no))
}