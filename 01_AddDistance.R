#
# Ajout d'une colonne distance aux donnees Velib NY
#

# Ajout de la distance entre les stations
library(osrm)

testsrc <- c("Allen St & Stanton St", -73.98911, 40.72205)
testdst <- c("Washington Ave & Park Ave", -73.96751, 40.69610)
routetest <- osrmRoute(src=testsrc, dst = testdst, sp = TRUE)
routetest2 <- osrmRoute(src=testdst, dst = testsrc, sp = TRUE)

# La distance aller et retour est différent en raison des sens uniques
routetest$distance
routetest2$distance
rm(routetest, routetest2)

# Affichage de la route aller
route <- osrmRoute(src = testsrc, dst = testdst)
route2 <- osrmRoute(src = testdst, dst = testsrc)

library(ggmap)
maptest <- get_map(location = c(mean(c(max(route$lon),min(route$lon))), mean(c(max(route$lat),min(route$lat)))), zoom=14)
ggmap(maptest) + 
  geom_point(data = route[1,], aes(x=lon, y=lat), col = "red", size = 5) +
  geom_point(data = route[nrow(route),], aes(x=lon, y=lat), col = "magenta", size = 5) +
  geom_point(data = route, aes(x=lon, y = lat), col = "blue", size = 3, alpha = 0.5) + 
  geom_path(data = route, aes(x=lon, y = lat), col = "blue", size = 1, alpha=0.5) + 
  geom_point(data = route2, aes(x=lon, y = lat), col = "green", size = 3, alpha = 0.5) + 
  geom_path(data = route2, aes(x=lon, y = lat), col = "green", size = 1, alpha=0.5) 

rm(maptest, route, route2, testsrc, testdst)


nydata <- read.csv("data/201609-citibike-tripdata.csv")

# Test sur peu de donnees
testdata <- head(nydata, 15)
testdata$Distance <- NA

# Approche procedurale avec une boucle for
for (recnum in 1:nrow(testdata)) {
  route <- osrmRoute(src = testdata[recnum,c("start.station.name", "start.station.longitude", "start.station.latitude")],
                     dst = testdata[recnum,c("end.station.name", "end.station.longitude", "end.station.latitude")],
                     sp = TRUE)
  print(paste(recnum,route$distance))
  testdata[recnum,"Distance"] <- route$distance
}
rm(testdata, recnum, route)

# Execution sur l'ensemble des donnees
nydata$Distance <- NA
nydata$Loop <- 0

# Si arrivee = depart alors distance = 0 et loop = 1
nydata[nydata$start.station.latitude == nydata$end.station.latitude & nydata$end.station.longitude == nydata$end.station.longitude,"Distance"] <- 0
nydata[nydata$start.station.latitude == nydata$end.station.latitude & nydata$end.station.longitude == nydata$end.station.longitude,"Loop"] <- 1
table(nydata$Distance, useNA="always")
table(nydata$Loop)

# Parcours de l'ensemble des donnees

for (recnum in 1:nrow(nydata)) {
  if (is.na(nydata[recnum,"Distance"])) {
    route <- osrmRoute(src = nydata[recnum,c("start.station.name", "start.station.longitude", "start.station.latitude")],
                       dst = nydata[recnum,c("end.station.name", "end.station.longitude", "end.station.latitude")],
                       sp = TRUE)
    if (is.null(route)) {
      print("An error occured in osrmRoute call, skipping")
    }
    else {
      print(paste(recnum,route$distance))
      # On met a jour tous les enregistrements concernant ces deux stations avec la distance calculee
      nydata[(nydata$start.station.latitude == nydata[recnum,"start.station.latitude"] &
              nydata$end.station.latitude == nydata[recnum,"end.station.latitude"] &              
              nydata$start.station.longitude == nydata[recnum,"start.station.longitude"] &
              nydata$end.station.longitude == nydata[recnum,"end.station.longitude"]), "Distance" ] <- route$distance
    }
  }
}

# stats
table(nydata$Distance >0, useNA="always")

# Calcul des heures de depart
nydata$starttime <- as.character(nydata$starttime)
nydata$HourStart <- as.numeric(substr(t(as.data.frame(strsplit(nydata$starttime,' ')))[,2],1,2))
table(nydata$HourStart)
nydata$stoptime <- as.character(nydata$stoptime)
nydata$HourStop <- as.numeric(substr(t(as.data.frame(strsplit(nydata$stoptime,' ')))[,2],1,2))
table(nydata$HourStop)
hist(nydata$HourStart)
hist(nydata$HourStop)

save(nydata,file="data/201609-citibike-tripdata-dist.Rda")
#load("data/201609-citibike-tripdata-dist.Rda")

# Dictionnaire des distances connues
library(dplyr)
dist_dict <- nydata %>%
  select(start.station.id, end.station.id, Distance) %>%
  filter(start.station.id != end.station.id) %>%
  group_by(start.station.id, end.station.id) %>%
  unique()

# Calcul sur le mois d'octobre 2016
nydata_201610 <- read.csv("data/201610-citibike-tripdata.csv")
names(nydata_201610) <- tolower(names(nydata_201610)) # Il y a des majuscules dans le fichier d'octobre
nydata_201610 <- left_join(nydata_201610, dist_dict) # on charge les distance deja connues
table(nydata_201610$Distance>0, useNA="always")
# Si arrivee = depart alors distance = 0 et loop = 1
nydata_201610[nydata_201610$start.station.latitude == nydata_201610$end.station.latitude & nydata_201610$end.station.longitude == nydata_201610$end.station.longitude,"Distance"] <- 0
nydata_201610[nydata_201610$start.station.latitude == nydata_201610$end.station.latitude & nydata_201610$end.station.longitude == nydata_201610$end.station.longitude,"Loop"] <- 1
table(nydata_201610$Distance>0, useNA="always")
table(nydata_201610$Loop)

nydata <- nydata_201610
# Parcours de l'ensemble des donnees

for (recnum in 1:nrow(nydata)) {
  if (is.na(nydata[recnum,"Distance"])) {
    route <- osrmRoute(src = nydata[recnum,c("start.station.name", "start.station.longitude", "start.station.latitude")],
                       dst = nydata[recnum,c("end.station.name", "end.station.longitude", "end.station.latitude")],
                       sp = TRUE)
    if (is.null(route)) {
      print("An error occured in osrmRoute call, skipping")
    }
    else {
      print(paste(recnum,route$distance))
      # On met a jour tous les enregistrements concernant ces deux stations avec la distance calculee
      nydata[(nydata$start.station.latitude == nydata[recnum,"start.station.latitude"] &
                nydata$end.station.latitude == nydata[recnum,"end.station.latitude"] &              
                nydata$start.station.longitude == nydata[recnum,"start.station.longitude"] &
                nydata$end.station.longitude == nydata[recnum,"end.station.longitude"]), "Distance" ] <- route$distance
    }
  }
}

# stats octobre
table(nydata$Distance >0, useNA="always") # 30 NA : osrmRoute renvoie "Error : Impossible route between points"
nydata_201610 <- nydata[!is.na(nydata$Distance),]

# Calcul des heures de depart
nydata$start.time <- as.character(nydata$start.time)
nydata$HourStart <- as.numeric(substr(t(as.data.frame(strsplit(nydata$start.time,' ')))[,2],1,2))
table(nydata$HourStart)
nydata$stop.time <- as.character(nydata$stop.time)
nydata$HourStop <- as.numeric(substr(t(as.data.frame(strsplit(nydata$stop.time,' ')))[,2],1,2))
table(nydata$HourStop)
hist(nydata$HourStart)
hist(nydata$HourStop)


save(nydata,file="data/201610-citibike-tripdata-dist.Rda")
#load("data/201610-citibike-tripdata-dist.Rda")

# TODO 
# Idem avec apply ?
# Idem avec data.table ?
# R?utilisation des distances d?j? calculees sur un nouveau fichier
# Investigation des quelques distances que osrmRoute ne veut pas calculer

