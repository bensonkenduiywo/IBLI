We can further compute RMSE, R$^2$ and MAPE using Leave one year out cross-validation to evaluate the models accuracies.

RMSE is given as:

$$
\text{RMSE} = \sqrt{\frac{1}{n} \sum_{i=1}^n \widehat{y}-y},
$$
where $\widehat{y}$  and $y$ are predicted yields and observed yields respectively while *n* is the number of fitted points.

```{r rmse}
rmse <- function(error){
  sqrt(mean(error^2))
}
```

MAPE is given as:

$$
\text{MAPE} = \frac{100\%}{n} \sum_{i=1}^n |\frac{y-\widehat{y}}{y}|.
$$

In R we can write it as:

```{r mape}
MAPE <- function (y_pred, y_true){
    MAPE <- mean(abs((y_true - y_pred)/y_true))
    return(MAPE*100)
}
```

We can also compute $R^2$ measure as follows:

```{r r_Squared}
R_square <- function(actual, predicted) {
  val <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))
  val
} 
```

5-fold Cross validation.

```{r y12, message=FALSE}
library(dismo)
set.seed(530)
nfolds <- 5
data <- na.omit(d[, c("zndvi", "zrain", "mortality_rate", "year")])
k <- kfold(data, k = nfolds, by=data$year)
nrow(data) / nfolds
table(k)

rmse_r <- rmse_n <- mape_r <- mape_n <- NULL
start <- min(unique(data$year))
end   <- max(unique(data$year))
for(i in 1:nfolds){
  print(paste0(i, " Cross-validation"))
  train <- data[k != i, ]
  valid <- data[k == i, ]
  observed_y <- 0
  p_y <- 0
  observed_y <- valid$mortality_rate
  #Loess
  m <- loess(mortality_rate ~ zrain , data=train, control=loess.control(surface="direct"))
  p_y <- predict(m, valid, na.action=na.omit)
  rmse_r[i] <- rmse(observed_y - p_y)
  cat("RMSE: ", rmse_r[i], "\n")
  mape_r[i] <- MAPE(observed_y, p_y)
  cat("MAPE: ", mape_r[i], "\n")
  m <- loess(mortality_rate ~ zndvi , data=train, control=loess.control(surface="direct"))
  p_y <- predict(m, valid, na.action=na.omit)
  rmse_n[i] <- rmse(observed_y - p_y)
  cat("RMSE: ", rmse_n[i], "\n")
  mape_n[i] <- MAPE(observed_y, p_y)
  cat("MAPE: ", mape_n[i], "\n")
  #lo_r[i] <- R_square(observed_y, p_y)
  #cat("Coefficient of determination R^2: ", lo_r[i], "\n")
}
cat("NDVI RMSE is ", mean(rmse_n), "\n")
cat("NDVI MAPE is ", mean(mape_n), "\n")
cat("Rain RMSE is ", mean(rmse_r), "\n")
cat("Rain MAPE is ", mean(mape_r), "\n")

iterations <- 1:nfolds

#Combined
#aa= read.csv(paste0("D:\\JKUAT\\PUBLICATIONS\\ISPRS_2020\\Results\\5-Fold_Cross_Validation_Plots.csv"), stringsAsFactors =  FALSE)
windows()
par(mfrow=c(1,2),mar=c(13,4.5,1,1))
plot(iterations, rf_a, type="l", ylim= c(0,1), col="red", ylab="RMSE (tons/ha)", xlab= "n-fold", cex.axis =1.2, cex.lab = 1.2)
lines(iterations, rf_a, col="blue")
legend("topright", c("SVM", "RF"), col=c("red","blue"), lty = 1)

plot(iterations, svm_b, type="l", ylim= c(0,100), col="red", ylab="MAPE (%)", xlab= "n-fold", cex.axis =1.2, cex.lab = 1.2)
lines(iterations, rf_b, col="blue")
legend("topright", c("SVM", "RF"), col=c("red","blue"), lty = 1)

windows()
par(mar=c(4.5,4.5,1,1))
plot(iterations, r_svm, type="l", ylim= c(0,1), col="red", ylab=expression(R^2), xlab= "n-fold", cex.axis =1.2, cex.lab = 1.2)
lines(iterations, r_rf, col="blue")
legend("topright", c("SVM", "RF"), col=c("red","blue"), lty = 1)


```
