
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
} else { }

rm(list=ls(all=TRUE))

x <- readRDS("reg_RIB.rds")
z <- readRDS("perfect.rds")

tv <- "0.18"
tvar <- paste0("mqs", tv)
vars <- c("group", "model", "r2", tvar)
d <- x[, vars]
colnames(d) <- gsub(tvar, "RIB", colnames(d))
d$model[d$model=="seg"] <- "sm"
head(d)


#Plot RIB against R$^2$ under mortality trigger of 23%.

ug <- unique(d$group)
um <- unique(d$model)
pal <- c("red", "blue", "magenta", "black")
cols <- pal[match(d$model, um)]
char <- (1:6)[match(d$group, ug)]


png("figs/figure5.png", units="in", width=6, height=6, res=600, pointsize=18)

par(mar=c(4, 4, 1, 1))
plot(d[,3:4], type="n", xlab = expression(R^2),cex=.8,
	xlim=c(0,0.7), ylim=c(0,0.7), las=1, asp=1, xaxs="i", yaxs="i")
#abline(lm(RIB~r2, data=d), lty=1, lwd=2)
abline(0,1, col="light gray", lwd=4, lty=3)
points(d[,3:4], pch=char, col=cols, cex=1.1)
legend("topleft", legend=ug, pt.cex=1.1, pch=1:6, bty="n", cex=.8, inset=c(.02, 0))
legend("topleft", legend=um, pch=20, pt.cex=1.5, col=pal, bty="n", cex=.8, inset=c(.2, 0))
text(0.65, 0.6, "y=x", col="light gray")

dev.off()

