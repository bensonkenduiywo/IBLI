#Create a function to compute means for Long Rain Long Dry (March -- September) and Short Rain Short Dry (October -- February) seasons.
seasonMean <- function(year, df, season){
	df$date <- as.Date(df$date, format = "X%Y%m%d")
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
  mean(abs((y_true - y_pred)/y_true)) * 100
}

R_square <- function(actual, predicted) {
  1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
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

#Below you will find an R function that draws a label in one of the three regions — figure (default), plot or device. You specify the position of the label using the labels also used by legend: “topleft”, “bottomright” etc.
#https://www.r-bloggers.com/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/


fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}
