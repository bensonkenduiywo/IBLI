rm(list=ls(all=TRUE))
df <- readRDS("sm_predictions.rds")
#Convert predicted and observed mortalities into assets
#NB: 1TLU=1000$
df$Predicted_assets <- (1 - df$predicted) * 1000
df$observed_assets  <- (1 - df$observed) * 1000 

x11()
y <- df$predicted
x <- df$observed
plot(x*100, y*100, pch=16, xlab="Observed loss (%)", ylab="Predicted loss (%)")
abline(0, 1, lwd=2, col="blue")
aa <- rep(23,24)
bb <- 0:23
lines(aa, bb, col="red", lwd=2)
text(28,5, "Trigger")
text(40,20, "Under predicted")
text(10,45, "Over predicted")
