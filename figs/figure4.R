
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
} else { }

rm(list=ls(all=TRUE))

x <- readRDS("reg_RIB.rds")
x$group[x$group=='lMD'] <- 'LMD'
x$group[x$group=='lNO'] <- 'LNO'
x$group[x$group=='lRN'] <- 'LRN'

#z <- readRDS("rmd/perfect.rds")


tv <- "0.18"
tvar <- paste0("mqs", tv)
vars <- c("group", "model", "r2", tvar)
d <- x[, vars]
colnames(d) <- gsub(tvar, "RIB", colnames(d))
d$model[d$model=='seg'] <- 'sm'


png("figs/figure4.png", units="in", width=12, height=12, res=300, pointsize=24)

par(mfrow=c(2, 2), mar=c(4.5, 4.2, 1.8, 1)) #c(bottom, left, top, right)
boxplot(r2~model, data=d, ylab = expression(R^2), xlab="", las=1, main="(a)", cex.axis=.9)
title(xlab="Model", line=2)
boxplot(RIB~model, data=d, xlab="", las=1, main="(b)", cex.axis=.9)
title(xlab="Model", line=2)

boxplot(r2~group, data=d, ylab = expression(R^2), xlab="", las=1, main="(c)", cex.axis=.9)
title(xlab="Predictor", line=2)
boxplot(RIB~group, data=d, xlab="", las=1, main="(d)", cex.axis=.9)
title(xlab="Predictor", line=2)

dev.off()

