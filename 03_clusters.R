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
load(file="data/alldatacluster2.Rda")


# Affichage du profil des clusters
boxplot(trips_out ~ clust, data= alldatacluster)
boxplot(meanspeed_out ~ clust, data= alldatacluster)
par(mfrow = c(1,1))
boxplot(mean_age_out ~ clust, data= alldatacluster)
boxplot(mean_age_in ~ clust, data= alldatacluster)
boxplot(percent_male_out ~ clust, data= alldatacluster)

# test plot
#ggplot() +
#  geom_bar(data = alldatacluster, aes(x = clust, y = percent_male_out))

#qplot(x=clust, y=mean_age_in, 
      #fill=variable,
#      data=alldatacluster, geom="bar")

library(rAmCharts)
mycolors <- c("red", "blue", "green", "magenta", "orange")
amBoxplot(mean_age_out ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les départs")
amBoxplot(mean_age_in ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les arrivées")
amBoxplot(mean_age_in ~ clust, data= alldatacluster, col = mycolors, main = "Age moyen pour les arrivées")

#amBarplot(x = "year", y = c("income", "expenses"), data = alldatacluster, stack_type = "regular")


#statsex <- alldatacluster %>%
#              select(percent_male_out)
#amBarplot(x = "clust", y = "percent_male_out", data = alldatacluster, labelRotation = -45) 


# aggregation par cluster
library(dplyr)
idclust <- data2 %>% select(station.id, clust) %>% rename(start.station.id = station.id)
nydata2 <- left_join(nydata,idclust) 
nydata2$starttime <- as.character(nydata$starttime)
nydata2$HourStart <- as.numeric(substr(t(as.data.frame(strsplit(nydata2$starttime,' ')))[,2],1,2))
table(nydata2$HourStart)
nydata2$stoptime <- as.character(nydata2$stoptime)
nydata2$HourStop <- as.numeric(substr(t(as.data.frame(strsplit(nydata2$stoptime,' ')))[,2],1,2))
table(nydata2$HourStop)

save(nydata2,file="data/nydtat2.Rda")

statcluster <- nydata2 %>%
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


#amHist(x = as.numeric(unlist(statcluster[1,5:28])), xlim=c(0,23))

statcluster <- statcluster[statcluster$clust %in% 1:5,]
statcluster
mycolors <- c("red", "blue", "green", "magenta", "orange")
statcluster$color <- mycolors

save(statcluster,file="data/statcluster.Rda")


amBarplot(x = "clust", y = "mean_age", data = statcluster, labelRotation = -45)
#%>% amOptions(color = mycolors)

                                                                                              
