Hi Robert,

Please find attached the values you requested. I kept daily estimates that can be converted to weekly, monthly and seasonal averages.
path <- "C:/Users/kenson/Desktop/TEMP/"
# read daily avhrr mean values for marsabit sublocations (58 rows); used ibli_data("marsabit") 
# date range: ("1981-06-24","2019-05-16")
dd <- read.csv("avhrr_ndvi_cdr_marsabit.csv", stringsAsFactors = FALSE)

# fix columns, any column without NDVI is not needed
d <- dd[, grep("NDVI", names(d))]
# multiply mean NDVI by AVHRR scale factor
d <- d*0.0001

# dates of collections
dt <- gsub("X|_NDVI", "", names(d))
names(d) <- dt
dt <- as.Date(dt, format = "%Y%m%d")

# earth engine is doing something with the order of slname
# I think this is the correct order, checking again
slname <- c("MAJENGO", "NYAYOROAD","HULAHULA","OGUCHO","KARARE","TOWNSHIP","WABERA","JIRIME","MATAARBA","DAKABARICHA",
            "SONGA","KITURUNI","FUROLE","HURRIHILLS","BUBISA","SHURA","MAIKONA","ELGADE","KALACHA","TURBI","DIRIBGOMBO","QILTA",
            "RUKESAQARSA","SAGANTE","JALDESA","BADASA","MERILLE","HAFARE","KORR","LAISAMIS","LONTOLIO","KOYA","IRIR","KAMBOYE","GUDAS/SORIADI",
            "LOGOLOGO","ILLAUT","LONYORIPICHAU","NGURUNIT","EL-HARDI","BALESA","SABARET", "DUKANA","NORTHHORR","DARADE","GALAS","ILLERET","ARAPAL",
            "LARACHI","MTKULAL","OLTUROT","KARUNGU","SOUTHHORR","MOITE","GAS","LOIYANGALANI","KURUGUM","KARGI")

d <- data.frame(SLNAME = slname, d, stringsAsFactors = FALSE)

# test
plot(dt[1:365], d[1,][1:365], pch = 16, cex = 0.3)
