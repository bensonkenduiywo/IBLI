set.seed(99)
# We want to create 10 clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
kmncluster <- kmeans(na.omit(nr), centers = 10, iter.max = 500, nstart = 5, algorithm="Lloyd")
# kmeans returns an object of class "kmeans"
str(kmncluster)

#Convert long to Wide
#==================================
lr <- rain[rain$season=="LRLD", 1:3]
lr_w <- dcast(lr, SUBLOCATION ~ year, value.var="rainfall")
set.seed(99)
# We want to create 10 clusters, allow 500 iterations, start with 5 random sets using "Lloyd" method
r_kmn <- kmeans(lr_w[,-1], centers = 10, iter.max = 500, nstart = 5, algorithm="Lloyd")
# kmeans returns an object of class "kmeans"
subloc$Zones <- r_kmn$cluster
sort(unique(subloc$SUB_LOCATION[subloc$Zones==1]))
