# Clusters

load ("data/201609-alldata.Rda")
load ("data/201609-citibike-tripdata-dist.Rda")
head (alldata)
summary(alldata)

data <- na.omit(alldata)
ids <- as.data.frame(data$station.id)
data <- data[5:62]

library(FactoMineR)
# 
myPCA <- PCA(data, ncp =6, scale.unit = TRUE)
head(myPCA$eig, 15)

# répartition en clusters
myclusters <- HCPC(myPCA, nb.clust = 5)
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
               select(birth.year, clust, Distance, HourStart, HourStop) %>%
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
                         h23_out = sum(HourStart ==23)
               )


statcluster <- statcluster[statcluster$clust %in% 1:5,]
statcluster$color <- mycolors

save(statcluster,file="data/201609-statcluster.Rda")

amBarplot(x = "clust", y = "mean_age", data = statcluster, labelRotation = -45)

