#
# Ajour d'une colonne distance aux donnees Velib NY
#

nydata <- read.csv("data/201609-citibike-tripdata.csv")

#
# Ajout de la distance entre les stations
#

#
# Step 1 : Test sur peu de donnees
# 
testdata <- head(nydata, 15)
testdata$Distance <- NA

library(osrm)

routetest <- osrmRoute(src=c("Allen St & Stanton St", -73.98911, 40.72205),
                      dst = c("Washington Ave & Park Ave", -73.96751, 40.69610),
                      sp = TRUE)
routetest$distance

# Approche procedurale avec une boucle for

for (recnum in 1:nrow(testdata)) {
  route <- osrmRoute(src = testdata[recnum,c("start.station.name", "start.station.longitude", "start.station.latitude")],
                     dst = testdata[recnum,c("end.station.name", "end.station.longitude", "end.station.latitude")],
                     sp = TRUE)
  print(paste(recnum,route$distance))
  testdata[recnum,"Distance"] <- route$distance
}

# Sauvegarde du resultat
save(testdata,file="testdata.Rda")

# Test de rechargement
load("testdata.Rda")

# Execution sur l'ensemble des donnees
nydata$Distance <- NA
nydata$Loop <- 0

# Si arrivee = depart alors distance = 0 et loop = 1
nydata[nydata$start.station.latitude == nydata$end.station.latitude & nydata$end.station.longitude == nydata$end.station.longitude,"Distance"] <- 0
nydata[nydata$start.station.latitude == nydata$end.station.latitude & nydata$end.station.longitude == nydata$end.station.longitude,"Loop"] <- 1
table(nydata$Distance, useNA="always")
table(nydata$Loop)

# Parcours de l'ensemble des données

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
      nydata[(nydata$end.station.latitude == nydata[recnum,"start.station.latitude"] &
                nydata$start.station.latitude == nydata[recnum,"end.station.latitude"] &              
                nydata$end.station.longitude == nydata[recnum,"start.station.longitude"] &
                nydata$start.station.longitude == nydata[recnum,"end.station.longitude"]), "Distance" ] <- route$distance
    }
  }
}

# stats
table(nydata$Distance >0, useNA="always")

#save(nydata,file="nydata2.Rda")
#load("nydata2.Rda")

#save(nydata,file="201609-citibike-tripdata-dist.Rda")
load("data/201609-citibike-tripdata-dist.Rda")

# TODO 
# Idem avec apply ?
# Idem avec data.table ?
# Réutilisation des distances déjà calculees sur un nouveau fichier
# Investigation des quelques distances que osrmRoute ne veut pas calculer

