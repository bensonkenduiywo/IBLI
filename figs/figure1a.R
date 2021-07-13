d <- agrodata::data_ibli("marsabit_losses")
d[1:5, c(1:6, 9:10)]

tlu <- c(camel=1.4, cattle=1, shoat=0.1)
xtlu <- x * tlu
barplot(xtlu, col=rainbow(3), las=1)

animals <- match(trimws(d$animal), c("Goat/Sheep", "Camel", "Cattle"))
anitlu <- tlu[animals]
youngloss <- d$totalloss - d$adultloss
d$tluloss <- (d$adultloss + youngloss/2) * anitlu

a <- aggregate(d[, "tluloss", drop=FALSE], d[, c("year", "month")], sum, na.rm=TRUE)
a <- a[order(a$year, a$month), ]
time <- as.Date(paste(a$year, a$month, 15, sep="-"))
plot(time, a$tluloss, ylab="Loss (TLU)")

xtabs(tluloss ~ month+year, d)

cause <- tapply(d$tluloss, d$cause, sum, na.rm=TRUE)
x <- sort(round(cause))
x11()
par(mai=c(1,2,1,1))
barplot(x, horiz=T, las=1)