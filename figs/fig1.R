
if (system('hostname', TRUE) == "LAPTOP-IVSPBGCA") { setwd('C:/github/IBLI')
} else { setwd('C:/Users/camila/Google Drive/AfSF') }

library(geodata)
ken0 <- geodata::gadm("KEN", level=0, "data")
ken1 <- geodata::gadm("KEN", level=1, "data")

mi <- ken1$NAME_1 == "Marsabit"
cols <- c(gray(0.95), "blue")[mi+1]

png("figs/fig1.png", 650, 800, pointsize = 24)

par(mar=c(2,2,1,1))
plot(ken1, col=cols, border="white", lwd=4, las=1)
lines(ken1)
lines(ken0, lwd=2)
text(35.1, -3.5, "Kenya", cex=1.8)
text(37.6, 3.1, "Marsabit", col="white", cex=1.3)
raster::scalebar(250, type="bar", cex=.8, below="km")

dev.off()
