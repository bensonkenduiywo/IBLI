
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
} else { setwd('C:/Users/camila/Google Drive/AfSF') }

x <- readRDS("rmd/reg_RIB.rds")
z <- readRDS("rmd/perfect.rds")

tv <- "23"
tvar <- paste0("mqs", tv)
vars <- c("group", "model", "r2", tvar)
d <- x[, vars]
colnames(d) <- gsub(tvar, "RIB", colnames(d))
d$model[d$model=="seg"] <- "sm"
head(d)


#Plot RIB against R$^2$ under mortality trigger of 23%.

ug <- unique(d$group)
um <- unique(d$model)
pal <- c("red", "blue", "orange", "black")
cols <- pal[match(d$model, um)]
char <- (1:6)[match(d$group, ug)]

png("figs/fig4.png", 800, 800, pointsize = 24)

x11()
par(mar=c(4, 4, 1, 1))
plot(d[,3:4], type="n", xlab = expression(R^2),cex=.8,
	xlim=c(0,0.7), ylim=c(0,0.7), las=1, asp=1)
abline(lm(RIB~r2, data=d), col="gray", lty=2, lwd=3)
points(d[,3:4], pch=char, col=cols, cex=1.1)
legend("topleft", legend=ug, pt.cex=1.1, pch=1:6, bty="n", cex=.8, inset=c(.02, 0))
legend("topleft", legend=um, pch=20, pt.cex=1.5, col=pal, bty="n", cex=.8, inset=c(.15, 0))

dev.off()

