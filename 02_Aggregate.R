#
# Aggregation des donnees Velib NY
#

library(dplyr)

# Chargement des donnees traitees a l'etape 1

load("data/201609-citibike-tripdata-dist.Rda")
#load("data/201610-citibike-tripdata-dist.Rda")

#nydata <- tbl_df(nydata)

startdata <- nydata %>% 
  rename(station.id = start.station.id, station.name = start.station.name, station.latitude = start.station.latitude, station.longitude = start.station.longitude) %>%
  mutate(Speed = Distance * 3600 / trip.duration) %>%
  filter(Speed <= 50) %>%
  group_by(station.id, station.name, station.latitude, station.longitude) %>%
  summarise(meanduration_out = mean(trip.duration),
            meanspeed_out = mean(Speed, na.rm = TRUE),
            meandist_out = mean(Distance, na.rm = TRUE),
            trips_out = n(),
            percent_male_out = 100 * sum(gender == 1) / sum(gender > 0),
            mean_age_out = mean(2017 - birth.year, na.rm = TRUE),
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
#glimpse(startdata)
hist(startdata$meanspeed_out)
hist(startdata$meandist_out)

enddata <- nydata %>% 
  rename(station.id = end.station.id, station.name = end.station.name, station.latitude = end.station.latitude, station.longitude = end.station.longitude) %>%
  mutate(Speed = Distance * 3600 / trip.duration) %>%
  filter(Speed <= 50) %>%
  group_by(station.id, station.name, station.latitude, station.longitude) %>%
  summarise(meanduration_in = mean(trip.duration),
            meanspeed_in = mean(Speed, na.rm = TRUE),
            meandist_in = mean(Distance, na.rm = TRUE),
            trips_in = n(),
            percent_male_in = 100 * sum(gender == 1) / sum(gender > 0),
            mean_age_in = mean(2017 - birth.year, na.rm = TRUE),
            h0_in = sum(HourStop ==0),
            h1_in = sum(HourStop ==1),
            h2_in = sum(HourStop ==2),
            h3_in = sum(HourStop ==3),
            h4_in = sum(HourStop ==4),
            h5_in = sum(HourStop ==5),
            h6_in = sum(HourStop ==6),
            h7_in = sum(HourStop ==7),
            h8_in = sum(HourStop ==8),
            h9_in = sum(HourStop ==9),
            h10_in = sum(HourStop ==10),
            h11_in = sum(HourStop ==11),
            h12_in = sum(HourStop ==12),
            h13_in = sum(HourStop ==13),
            h14_in = sum(HourStop ==14),
            h15_in = sum(HourStop ==15),
            h16_in = sum(HourStop ==16),
            h17_in = sum(HourStop ==17),
            h18_in = sum(HourStop ==18),
            h19_in = sum(HourStop ==19),
            h20_in = sum(HourStop ==20),
            h21_in = sum(HourStop ==21),
            h22_in = sum(HourStop ==22),
            h23_in = sum(HourStop ==23)
  )
#glimpse(enddata)
hist(enddata$meanspeed_in)
hist(enddata$meandist_in)

# Join everything
alldata <- full_join(startdata, enddata)
rm(startdata, enddata)
alldata$station.name <- as.factor(alldata$station.name)

save(alldata,file="data/201609-alldata.Rda")
#save(alldata,file="data/201610-alldata.Rda")
rm(alldata)


# Aggregations par heure
library(data.table)

startdataPerHour <- nydata %>% 
  rename(station.id = start.station.id, station.name = start.station.name, station.latitude = start.station.latitude, station.longitude = start.station.longitude) %>%
  mutate(Speed = Distance * 3600 / trip.duration) %>%
  filter(Speed <= 50) %>%
  group_by(station.id, station.name, station.latitude, station.longitude, HourStart) %>%
  summarise(meanduration_out = mean(trip.duration),
            meanspeed_out = mean(Speed, na.rm = TRUE),
            trips_out = n(),
            percent_male_out = 100 * sum(gender == 1) / sum(gender > 0),
            mean_age_out = mean(2017 - birth.year, na.rm = TRUE)
  )

startdataPerHourPivot <- dcast(data.table(startdataPerHour),station.id + station.name + station.latitude + station.longitude ~ HourStart, 
                               value.var=c("meanduration_out","meanspeed_out", "trips_out","percent_male_out", "mean_age_out"))

#colnames(startdataPerHourPivot)
#summary(startdataPerHourPivot)

enddataPerHour <- nydata %>% 
  rename(station.id = end.station.id, station.name = end.station.name, station.latitude = end.station.latitude, station.longitude = end.station.longitude) %>%
  mutate(Speed = Distance * 3600 / trip.duration) %>%
  filter(Speed <= 50) %>%
  group_by(station.id, station.name, station.latitude, station.longitude,HourStop) %>%
  summarise(meanduration_in = mean(trip.duration),
            meanspeed_in = mean(Speed, na.rm = TRUE),
            trips_in = n(),
            percent_male_in = 100 * sum(gender == 1) / sum(gender > 0),
            mean_age_in = mean(2017 - birth.year, na.rm = TRUE)
  )

enddataPerHourPivot <- dcast(data.table(enddataPerHour),station.id + station.name + station.latitude + station.longitude ~ HourStop, 
                             value.var=c("meanduration_in","meanspeed_in", "trips_in","percent_male_in", "mean_age_in"))

# Join everything
alldataPerHour <- full_join(startdataPerHourPivot, enddataPerHourPivot)
alldataPerHour$station.name <- as.factor(alldataPerHour$station.name)

# Gestion des valeurs manquantes
which(is.na(alldataPerHour$trips_in_0))
alldataPerHour$trips_in_0[which(is.na(alldataPerHour$trips_in_0))] <- 0
which(is.na(alldataPerHour$trips_in_1))
alldataPerHour$trips_in_1[which(is.na(alldataPerHour$trips_in_1))] <- 0
which(is.na(alldataPerHour$trips_in_2))
alldataPerHour$trips_in_2[which(is.na(alldataPerHour$trips_in_2))] <- 0
which(is.na(alldataPerHour$trips_in_3))
alldataPerHour$trips_in_3[which(is.na(alldataPerHour$trips_in_3))] <- 0
which(is.na(alldataPerHour$trips_in_4))
alldataPerHour$trips_in_4[which(is.na(alldataPerHour$trips_in_4))] <- 0
which(is.na(alldataPerHour$trips_in_5))
alldataPerHour$trips_in_5[which(is.na(alldataPerHour$trips_in_5))] <- 0
which(is.na(alldataPerHour$trips_in_6))
alldataPerHour$trips_in_6[which(is.na(alldataPerHour$trips_in_6))] <- 0
which(is.na(alldataPerHour$trips_in_7))
alldataPerHour$trips_in_7[which(is.na(alldataPerHour$trips_in_7))] <- 0
which(is.na(alldataPerHour$trips_in_8))
alldataPerHour$trips_in_8[which(is.na(alldataPerHour$trips_in_8))] <- 0
which(is.na(alldataPerHour$trips_in_9))
alldataPerHour$trips_in_9[which(is.na(alldataPerHour$trips_in_9))] <- 0
which(is.na(alldataPerHour$trips_in_10))
alldataPerHour$trips_in_10[which(is.na(alldataPerHour$trips_in_10))] <- 0
which(is.na(alldataPerHour$trips_in_11))
alldataPerHour$trips_in_11[which(is.na(alldataPerHour$trips_in_11))] <- 0
which(is.na(alldataPerHour$trips_in_12))
alldataPerHour$trips_in_12[which(is.na(alldataPerHour$trips_in_12))] <- 0
which(is.na(alldataPerHour$trips_in_13))
alldataPerHour$trips_in_13[which(is.na(alldataPerHour$trips_in_13))] <- 0
which(is.na(alldataPerHour$trips_in_14))
alldataPerHour$trips_in_14[which(is.na(alldataPerHour$trips_in_14))] <- 0


for(i in 0:23){
  #print(i)
  print(which(is.na(paste("alldataPerHour$meanspeed_in", i, sep = "_"))))
  #alldataPerHour$Paste
  #alldataPerHour[which(alldataPerHour[is.na(paste("meanspeed_in", i, sep = "_"))]), paste("meanspeed_in", i, sep = "_")] <- 0
}


save(alldataPerHour,file="data/201609-alldataPerHour.Rda")

rm(startdataPerHour, enddataPerHour, startdataPerHourPivot, enddataPerHourPivot)
rm(nydata)
