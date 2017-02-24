# Clusters

load ("data/201609-alldata.Rda")
load ("data/201609-citibike-tripdata-dist.Rda")
head (alldata)
summary(alldata)


data <- na.omit(alldata)
ids <- as.data.frame(data$station.id)
data <- na.omit(alldata)[,5:64]
# Suppression des colonnes relatives à la station
#data <- na.omit(alldata)[,-c(1,2,3,4,5,7,34,36)]


library(FactoMineR)
# 
myPCA <- PCA(data, ncp =7, scale.unit = TRUE)
head(myPCA$eig, 15)

# répartition en clusters
myclusters <- HCPC(myPCA, nb.clust = 3)
myclusters$data.clust$clust

data2 <- myclusters$data.clust
data2$station.id <- unlist(ids)

alldatacluster <- myclusters$data.clust
alldatacluster$station.id <- unlist(ids)
rm(alldata, data, ids, myclusters, myPCA)
save(alldatacluster,file="data/201609-alldatacluster.Rda")
#load(file="data/201609-alldatacluster.Rda")


# Affichage du profil des clusters

#par(mfrow = c(1,1))
boxplot(trips_out ~ clust, data= alldatacluster)
boxplot(meanspeed_out ~ clust, data= alldatacluster)
boxplot(mean_age_out ~ clust, data= alldatacluster)
boxplot(mean_age_in ~ clust, data= alldatacluster)
boxplot(percent_male_out ~ clust, data= alldatacluster)

library(rAmCharts)
mycolors <- c("red", "blue", "green", "magenta", "orange")
amBoxplot(mean_age_out ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les départs")
amBoxplot(mean_age_in ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les arrivées")
amBoxplot(mean_age_in ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les arrivées")


# aggregation par cluster
library(dplyr)
idclust <- alldatacluster %>% select(station.id, clust) %>% rename(start.station.id = station.id)
nydata <- left_join(nydata,idclust) 

#save(nydata,file="data/201609-nydata.Rda")

statcluster <- nydata %>%
               select(birth.year, clust, Distance, HourStart, HourStop, gender) %>%
               group_by(clust) %>%
               summarise(mean_age = round(mean(2017 - birth.year,na.rm = TRUE),digits=2),
                         mean_distance = round(mean(Distance,na.rm = TRUE),digits=2),
                         trips = n(),
                         h0_out = sum(HourStart ==0),
                         h1_out = sum(HourStart ==1),
                         h2_out = sum(HourStart ==2),
                         h3_out = sum(HourStart ==3),
                         h4_out = sum(HourStart ==4),
                         h5_out = sum(HourStart ==5),
                         h6_out = sum(HourStart ==6),
                         h7_out = sum(HourStart ==7),
                         h8_out = sum(HourStart ==8),
                         h9_out = sum(HourStart ==9),
                         h10_out = sum(HourStart ==10),
                         h11_out = sum(HourStart ==11),
                         h12_out = sum(HourStart ==12),
                         h13_out = sum(HourStart ==13),
                         h14_out = sum(HourStart ==14),
                         h15_out = sum(HourStart ==15),
                         h16_out = sum(HourStart ==16),
                         h17_out = sum(HourStart ==17),
                         h18_out = sum(HourStart ==18),
                         h19_out = sum(HourStart ==19),
                         h20_out = sum(HourStart ==20),
                         h21_out = sum(HourStart ==21),
                         h22_out = sum(HourStart ==22),
                         h23_out = sum(HourStart ==23),
                         trips_men = sum(gender == 1),
                         trips_women = sum(gender == 2),
                         percent_men = round(100 * trips_men / (trips_men + trips_women), digits = 2),
                         percent_women = round(100 * trips_women / (trips_men + trips_women), digits = 2)
               )


statcluster <- statcluster[statcluster$clust %in% 1:3,]
mycolors <- c("red", "blue", "green")
statcluster$color <- mycolors

save(statcluster,file="data/201609-statcluster.Rda")

amBarplot(x = "clust", y = "mean_age", data = statcluster, labelRotation = -45)

# Répartition par sexe.
amBarplot(x = "clust", y = c("trips_men", "trips_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))
amBarplot(x = "clust", y = c("percent_men", "percent_women"), data = statcluster, stack_type = "regular", groups_color = c("#87cefa", "pink"))



### Avec variables par heure
load ("data/201609-alldataPerHour.Rda")
alldataPerHour$trips_out_0[is.na(alldataPerHour$trips_out_0)] <- 0

dataPerHour <- na.omit(alldataPerHour)
str(dataPerHour)
ids <- as.data.frame(dataPerHour$station.id)
ids
dataPerHour <- dataPerHour[5:244]

# 
dataPerHour.cr <- scale(dataPerHour,center=T,scale=T)
D_PerHour <- dist(dataPerHour.cr)
#test de la meilleure methode de clusterisation pour notre population
cah_PerHour_wardD2 <- hclust(D_PerHour,method="ward.D2") # meilleure méthode pour notre population
plot(cah_PerHour_wardD2)
cah_PerHour_complete <- hclust(D_PerHour,method="complete") # 
plot(cah_PerHour_complete)


plot(sort(cah_PerHour_wardD2$height,dec=T)[1:20],type="h")
res_PerHour <- cutree(cah_PerHour_wardD2,k=5)
table(res_PerHour)
dataPerHour$id <- unlist(ids)
dataPerHour$cluster <- res_PerHour
alldataPerHourcluster <- dataPerHour
#save(alldataPerHourcluster,file="data/alldataPerHourcluster.Rda")
# il faut calculer les moyennes par cluster et tracer les courbes par moyenne pour les 6 clusters
dim(alldataPerHourcluster)
str(alldataPerHourcluster)

# une liste de data.frame des colonnes numeriques, par cluster            
data_split_cluster <- split(alldataPerHourcluster[, 1:240], alldataPerHourcluster$cluster)
str(data_split_cluster)

# recuperation de la moyenne
mean_cluster <- t(sapply(data_split_cluster, function(data_cluster){
  colMeans(data_cluster)
}))

dim(mean_cluster)

head(mean_cluster)

colnames(mean_cluster[, 217:240])


#affichage de la dur?e moyenne d?part
matplot(t(mean_cluster[, 1:24]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="dur?e moyenne d?part")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage de la vitesse moyenne d?part
matplot(t(mean_cluster[, 25:48]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="vitesse moyenne d?part")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage du nombre de voyages sortants 
matplot(t(mean_cluster[, 49:72]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="nombre de voyages sortants ")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage du pourcentage d'hommes sortants
matplot(t(mean_cluster[, 73:96]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="pourcentage d'hommes sortants")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))


#affichage de l'?ge moyen sortante
matplot(t(mean_cluster[, 97:120]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="?ge moyen sortante")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage de la dur?e moyenne entrante
matplot(t(mean_cluster[, 121:144]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="dur?e moyenne entrante")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage de la vitesse moyenne entrante
matplot(t(mean_cluster[, 145:168]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="vitesse moyenne entrante")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage du nombre de voyages sortants
matplot(t(mean_cluster[, 169:192]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="nombre de voyages sortants")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage du pourcentage d'hommes entrants
matplot(t(mean_cluster[, 193:216]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="Pourcentage d'hommes entrant")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))

#affichage de l'?ge moyen entrant
matplot(t(mean_cluster[, 217:240]), type = "l", col = c("red", "blue", "green", "black", "purple", "pink"),main="Age moyen entrant")
legend("topright", col = c("red", "blue", "green", "black", "purple", "pink"), 
       lty= 1, legend = paste("Cluster", 1:6))



