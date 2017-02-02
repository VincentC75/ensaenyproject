load ("data/alldata.Rda")

head (alldata)

summary(alldata)

data <- na.omit(alldata)
ids <- as.data.frame(data$station.id)
data <- data[5:62]

# 
data.cr <- scale(data,center=T,scale=T)
D <- dist(data.cr)
cah_wardD2 <- hclust(D,method="ward.D2") # meilleure méthode pour notre population
plot(cah_wardD2)
plot(sort(cah_wardD2$height,dec=T)[1:100],type="h")
res <- cutree(cah_wardD2,k=6)
table(res)
data$id <- unlist(ids)
data$cluster <- res
alldatacluster <- data
save(alldatacluster,file="data/alldatacluster.Rda")
# Affichage du profil des clusters
boxplot(trips_out ~ cluster, data= alldatacluster)
boxplot(meanspeed_out ~ cluster, data= alldatacluster)
boxplot(mean_age_out ~ cluster, data= alldatacluster)
boxplot(percent_male_out ~ cluster, data= alldatacluster)




library(FactoMineR)
# 
myPCA <- PCA(data[1:58], ncp =6, scale.unit = TRUE)
head(myPCA$eig, 15)

# répartition en clusters
myclusters <- HCPC(myPCA, nb.clust = -1)
myclusters$data.clust$clust

data2 <- myclusters$data.clust
data2$station.id <- unlist(ids)

alldatacluster <- data2
save(alldatacluster,file="data/alldatacluster2.Rda")

# Affichage du profil des clusters
boxplot(trips_out ~ clust, data= alldatacluster)
boxplot(meanspeed_out ~ clust, data= alldatacluster)
boxplot(mean_age_out ~ clust, data= alldatacluster)
boxplot(percent_male_out ~ clust, data= alldatacluster)
