
library(reshape2)

## read data

x <- readRDS("reg_RIB.rds")
z <- readRDS("perfect.rds")
tt <- x[, c("group", "model","mqs10.3", "mqs12.6", "mqs13.9", "mqs15.8", "mqs17.6", "mqs23", "mqs27.6", "mqs41.5" )]
tt <- melt(tt, id=c("group", "model")) 
tt$variable = as.double(gsub("mqs","",tt$variable))
trigs <- unique(tt$variable)


tv <- "23"
tvar <- paste0("mqs", tv)
vars <- c("group", "model", "r2", tvar)
d <- x[, vars]
colnames(d) <- gsub(tvar, "RIB", colnames(d))
head(d)


#Plot RIB against R$^2$ under mortality trigger of 23%.

plot(d[,3:4], pch=20, col="red", xlab = expression(R^2)) #, xlim=c(0,1), ylim=c(0,1))

`

