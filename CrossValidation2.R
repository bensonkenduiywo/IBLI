
## Packages
rm(list=ls(all=TRUE))
library(dplyr)
library(raster)
library(sp)

## Data preparation

### Administrative data

subloc <- agrodata::data_ibli("marsabit")
names(subloc)[6] <- "sublocation"
### NDVI and rainfall data.

n <- agrodata::data_ibli("marsabit_avhrr_ndvi") #NOAA
colnames(n)[1:5]
m <- agrodata::data_ibli("marsabit_modis_ndvi_agg") #MODIS
colnames(m)[1] <- toupper(colnames(m)[1])
colnames(m)[1:5]
p <- agrodata::data_ibli("marsabit_chirps_precipitation") #rainfall
colnames(p)[1:5]


#Reshape the data from wide to long with date as a variable. 

p <- reshape2::melt(p, variable.name = "date", value.name = "rainfall",id.vars = "SUBLOCATION")
p[1,]
n <- reshape2::melt(n, variable.name = "date", value.name = "noaa", id.vars = "SUBLOCATION")
n[1,]
m <- reshape2::melt(m, variable.name = "date", value.name = "modis", id.vars = "SUBLOCATION")
m[1,]

#Format the date variable by removing unnecessary variables.
#Chirps
p$date <- as.Date(p$date, format = "X%Y%m%d")
p[1,]
#NOAA
n$date <- as.Date(n$date, format = "X%Y%m%d")
n[1,]
#MODIS
m$date <- as.Date(m$date, format = "X%Y_%m_%d")
m[1,]

#Compute daily average rainfall and NDVI per season in each year. First Create a function to compute means for Long Rain Long Dry (March -- September) and Short Rain Short Dry (October -- February) seasons.

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

# Now compute the averages using the function.

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

### Livestock data

#Mortality and causes of mortality data

mort <- agrodata::data_ibli("marsabit_mortality.rds")
colnames(mort)[colnames(mort)=="type"] <- "animal"
mort <- mort[mort$animal %in% c("Camel", "Cattle", "Shoat"), ]  # remote TLU for merging
mort <- mort[!is.na(mort$mortality_rate), ]                     # remove zero stock

#Check distribution of households
temp <- aggregate(hhid~sublocation+year, mort, length)
temp <- reshape(temp, idvar = "sublocation", timevar = "year", direction = "wide")
names(temp) <- c("Sublocation", "2008","2009","2010","2011","2012","2013","2014","2015")
knitr::kable(temp)

cause <- agrodata::data_ibli("marsabit_losses")[,c("hhid", "sublocation", "animal", "cause","month", "year")]
cause$animal[cause$animal == "Goat/Sheep"] <- "Shoat"
cause$season <- "SRSD"
cause$season[cause$month >=3 & cause$month <=9] <-"LRLD"
head(cause, n=2)
cause <- na.omit(unique(cause))
table(cause$cause)


#Merge causes table with mortality data.

#We cannot really merge these. For now, to make progress, let's just drop that

nrow(mort)
nrow(cause)
mortx  <- merge(mort, cause, by=c("hhid","sublocation", "animal", "season", "year"), all.x=TRUE)
nrow(mortx)

#Barplot of losses per TLU
temp <- mortx
temp$tluloss <- NA
temp$tluloss[temp$animal=='Cattle'] <- temp$loss[temp$animal=='Cattle']
temp$tluloss[temp$animal=='Camel'] <- temp$loss[temp$animal=='Camel']/0.7
temp$tluloss[temp$animal=='Shoat'] <- temp$loss[temp$animal=='Shoat']/10
x <- tapply(temp$tluloss, temp$cause, sum, na.rm=TRUE)
x <- sort(round(x))
x11()
par(mai=c(1,2,1.5,0.1)) #c(bottom, left, top, right)
barplot(x, horiz=T, las=1)

#Compute mortality

#It might be of interest to do a separate model for Starvation/Drought

mort <- agrodata::data_ibli("marsabit_mortality.rds")
mort2 <- mort[mort$type == "TLU", ]  # remote TLU for merging

amort <- aggregate(mort2[,c("stock_beginning", "loss"), drop=FALSE], mort2[, c("season","year","sublocation"), drop=FALSE], mean, na.rm=TRUE)
amort$mortality_rate <- (amort$loss / amort$stock_beginning) 

## Normal vs lognormal data treament 

#Compute z-Score based data time series lengths. Z-score function.

zscore <- function(y, log=FALSE){
    # +.05 to avoid NA from log(x) where x <= 0
    if (log) y <- log(y+.05)
    (y - mean(y, na.rm=TRUE) ) / (sd(y, na.rm=TRUE))
}

#Compute rainfall and NDVI Z-scores per season per sublocation.

df1 <- merge(p, n, by = c("SUBLOCATION","year", "season")) #NOAA and CHIRPSs
df1 <- merge(df1, m, by = c("SUBLOCATION","year", "season"), all.x=TRUE)#Add modis data
#Compute Z-scores per season per sublocation
dff <- ungroup(mutate(group_by(df1, SUBLOCATION, season), znoaa=zscore(noaa), zrain=zscore(rainfall), zmodis=zscore(modis),  zlnoaa=zscore(noaa, TRUE), zlrain=zscore(rainfall, TRUE), zlmodis=zscore(modis, TRUE)))

#Merge mortality data with NDVI and rainfall data.

colnames(amort)[3] <- "SUBLOCATION"
dff <- merge(dff, amort, by=c("year", "season", "SUBLOCATION"))
saveRDS(dff,  "m_data.rds")

## Mortality modelling and prediction

# We will determine the effect rainfall data transformation on mortality predictions based on R$^2$ and Root Mean Square Error (RMSE). 
# RMSE and R$^2$ functions are given as: This is the relative RMSE 
# (relative to a null model. that is more informative).

rrmse <- function(observed, predicted){ 
  error <- observed - predicted
  rmse <- sqrt(mean(error^2, na.rm=TRUE))
  nullerror <- observed - mean(observed)
  nullrmse <- sqrt(mean(nullerror^2, na.rm=TRUE))
  (nullrmse - rmse) / nullrmse
}


Rsq <- function(actual, predicted) { 
  d <- na.omit(data.frame(actual, predicted))
  1 - (sum((d$actual-d$predicted)^2)/ sum((d$actual - mean(d$actual))^2)) 
} 


### Regime switching linear regression and segmented regression

# Here we test the performance of piecewise linear regression model 
# and segmented regression in predicting livestock mortality. 
# Piecewise will consider two  regimes; those when conditions are 
# below zero and -0.5. Basically, we care more on the extreme cases 
# because that is when farmers should receive compensation. 
# We also evaluate farmers welfare benefit based on MQS with the 
# assumption that 1TLU=1000$ and that each farmer has 1TLU before any shocks. 

library(segmented)
#MQS function
library(agro)
mqs_test <- function(rho, d){
  const <- 1000 # 1TLU is 1000$
  ce_base <- ce_income(d$capital * const, rho)
  ce_ins  <- ce_income(d$capital_ins * const, rho)
  (ce_ins - ce_base) / const
}

get_benefit <- function(df, trigger, rho=1.5) {
  #Define trigger
  d <- na.omit(df)
  #Compute payouts
  d$payouts <- pmax(0, d$predicted_mortality - trigger) 
  #Compute Actuarially fair premiumL Premium with markup 25%
  premium <- mean(d$payouts, na.rm=TRUE) * 1.1
  #Compute capital with insurance
  d$capital     <- 1 - d$mortality
  d$capital_ins <- (d$capital + d$payouts) - premium
  #Determine insurance benefit
  c <- mqs_test(rho, d)
  #MQS test for a perfect insurance
  #Compute payouts
  d$payouts <- pmax(0, d$mortality - trigger) 
  #Compute Actuarially fair premiumL Premium with markup 25%
  premium <- mean(d$payouts, na.rm=TRUE) * 1.1
  #Compute capital with insurance
  d$capital     <- 1 - d$mortality
  d$capital_ins <- (d$capital + d$payouts) - premium
  #Determine insurance benefit
  p <- mqs_test(rho, d)
  rib <- c/p
  return(rib)
}


#Possible pay-out triggers

mort <- na.omit(dff$mortality_rate)
triggers <- round(quantile(mort, seq(0.75, 0.9, 0.05)), 2)
triggers

aggregate(dff[, "mortality_rate", drop=FALSE], dff[,"SUBLOCATION", drop=FALSE], function(i) quantile(i, c(0.75, 0.8, 0.85), na.rm=T))

##CROSS-VALIDATION
##REGRESSION MODELS
regfun <- function(train, main="znoaa", valid, label, triggers) {
  lm5 <- -0.5
  lm0 <- 0
  df0 <- train[train[,main]<lm0,]
  df5 <- train[train[,main]<lm5,]
  test <- data.frame(matrix(nrow=4, ncol=4+length(triggers)))
  colnames(test) <- c("group", "model", "R2", "RMSE", paste0("mqs", triggers))
  test$group <- label
  test$model <- c("lm", "lm0", "lm5", "seg")
  #MODELS
  ml  <- lm(paste0("mortality_rate","~",main), data=train)
  ml2 <- lm(paste0("mortality_rate","~",main), data=df0)
  ml5 <- lm(paste0("mortality_rate","~",main), data=df5)
  dd  <- data.frame(x=train[,main],y=train$mortality_rate)
  sm  <- segmented(lm(y~x, data=dd),seg.Z = ~x, psi=0)
  
  e <- predict(ml, valid)
  valid$mortality <- valid$mortality_rate
  valid$predicted_mortality <- e
  for (i in 1:length(triggers)) test[1,i+4] <- get_benefit(valid, triggers[i])
  test[1,3:4] <- c(Rsq(valid$mortality_rate, e), rrmse(valid$mortality_rate, e))
  yy <- valid[valid[,main]<lm0,]
  e <- predict(ml2, yy)
  yy$mortality <- yy$mortality_rate
  yy$predicted_mortality <- e
  for (i in 1:length(triggers)) test[2,i+4] <- get_benefit(yy, triggers[i])
  test[2,3:4] <- c(Rsq(yy$mortality_rate, e), rrmse(yy$mortality_rate, e))
  yy <- valid[valid[,main]<lm5,]
  e <- predict(ml5, yy)
  yy$mortality <- yy$mortality_rate
  yy$predicted_mortality <- e
  for (i in 1:length(triggers)) test[3,i+4] <- get_benefit(yy, triggers[i])
  test[3,3:4] <- c(Rsq(yy$mortality_rate, e), rrmse(yy$mortality_rate, e))
  dd <- data.frame(x=valid[,main],y=valid$mortality_rate)
  e <- predict(sm, dd)
  valid$mortality <- valid$mortality_rate
  valid$predicted_mortality <- e
  for (i in 1:length(triggers)) test[4,i+4] <- get_benefit(valid, triggers[i])
  test[4,3:4] <- c(Rsq(valid$mortality_rate, e), rrmse(valid$mortality_rate, e))
#  print(test)
  return(test)
}

library(dismo)
nfolds <- 5
dd <- na.omit(dff)

docv <- function(...) {

	k <- kfold(dd, k = nfolds)
	no  <- lno <- rn  <- lrn <- mo  <- lmo <- list()

	for(jj in 1:nfolds){
#	  cat("Fold", jj, "of", nfolds, "\n")
	  train <- dd[k != jj, ]
	  valid <- dd[k == jj, ]
	  #NOAA
	  no[[jj]]  <- regfun(train, "znoaa", valid, "NO", triggers)
	  lno[[jj]] <- regfun(train, "zlnoaa", valid, "lNO", triggers)
	  #RAINFALL
	  rn[[jj]]  <- regfun(train, "zrain", valid, "RN", triggers)
	  lrn[[jj]] <- regfun(train, "zlrain", valid, "lRN", triggers)
	  #MODIS
	  mo[[jj]]  <- regfun(train, "zmodis", valid, "MD", triggers)
	  lmo[[jj]] <- regfun(train, "zlmodis", valid, "lMD", triggers)
	}


	no <- do.call(rbind, no)
	lno <- do.call(rbind, lno)
	mo <- do.call(rbind, mo)
	lmo <- do.call(rbind, lmo)
	rn <- do.call(rbind, rn)
	lrn <- do.call(rbind, lrn)

	all <- rbind(no, lno, mo,lmo,rn,lrn)
	# take the average to get CV scores
	aggregate(all[, -c(1:2)], all[, 1:2], mean)
}


set.seed(530)
cv <- docv()



#apply(all[,5:ncol(all)], 2, max)
#apply(all[,5:ncol(all)], 2, min)

tiff("figs/S2 Fig 1.tif", units="px", width=2250, height=2625, res=300, pointsize=15)
	par(mfrow=c(2, 2), mar=c(4.5, 4.2, 1.8, 1)) #c(bottom, left, top, right)
	boxplot(R2~model, data=cv, ylab = expression(R^2), xlab="", las=1, main="(a)", cex.axis=.9)
	title(xlab="Model", line=2)
	boxplot(mqs0.18~model, data=cv, xlab="", ylab="", las=1, main="(b)", cex.axis=.9)
	title(xlab="Model", ylab="RIB", line=2)

	boxplot(R2~group, data=cv, ylab = expression(R^2), xlab="", las=1, main="(c)", cex.axis=.81)
	title(xlab="Predictor", line=2)
	boxplot(mqs0.18~group, data=cv, xlab="", ylab="", las=1, main="(d)", cex.axis=.81)
	title(xlab="Predictor", ylab="RIB", line=2)
dev.off()

#SAVE DATA
write.csv(cv, '5fold_CrossValidation.csv')



#Save data for use in next steps.

#saveRDS(x,  "regression_models_XX.rds")

x <- readRDS("reg_RIB.rds")
m <- merge(x, cv, by=1:2)

tab <- m[, c("group", "model", "r2", "mqs0.23.x", "R2",  "mqs0.23.y")]
write.csv(tab, 'S2table1.csv')

tiff("figs/S2 Fig 2.tif", units="px", width=2250, height=2250, res=300, pointsize=15)
par(mfrow=c(1,2), mar=c(4.5, 4.3, 16, 1)) #c(bottom, left, top, right)
plot(tab$r2, tab$R2, xlab=expression(paste("Internal ",R^2)), 
     ylab=expression(paste("Cross-validated ",R^2)), xlim=c(0.1, 0.55), 
     ylim=c(0.1, 0.55), las=1, pch=20, cex=0.95, cex.axis=0.85, cex.lab=0.85)
abline(0,1)
plot(tab[,4], tab[,6], xlab="Internal RIB", ylab="Cross-validated RIB", 
     xlim=c(0, 0.5), ylim=c(0, 0.5), las=1, pch=20, cex=0.95, cex.axis=0.85, cex.lab=0.85)
abline(0,1)
dev.off()


