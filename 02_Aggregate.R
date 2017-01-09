#
# Aggregation des donnees Velib NY
#

# Chargement des donnees traitees a l'etape 1

load("data/201609-citibike-tripdata-dist.Rda")

# Calcul des heures de départ
nydata$starttime <- as.character(nydata$starttime)
nydata$HourStart <- as.numeric(substr(t(as.data.frame(strsplit(nydata$starttime,' ')))[,2],1,2))
table(nydata$HourStart)
nydata$stoptime <- as.character(nydata$stoptime)
nydata$HourStop <- as.numeric(substr(t(as.data.frame(strsplit(nydata$stoptime,' ')))[,2],1,2))
table(nydata$HourStop)

hist(nydata$HourStart)
hist(nydata$HourStop)

save(nydata, file="data/201609-citibike-tripdata-dist-hour.Rda")


library(dplyr)
nydata <- tbl_df(nydata)
#nydata
#class(nydata)

# Add speed
# test <- nydata %>% mutate(Speed = Distance * 3600 / tripduration)
# summary(test$Speed)
# investigate max speed = 1167 km/h !!!

startdata <- nydata %>% 
  rename(station.id = start.station.id, station.name = start.station.name, station.latitude = start.station.latitude, station.longitude = start.station.longitude) %>%
  mutate(Speed = Distance * 3600 / tripduration) %>%
  group_by(station.id, station.name, start.station.latitude, start.station.longitude) %>%
  summarise(meanduration_out = mean(tripduration),
            meanspeed_out = mean(Speed, na.rm = TRUE),
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
glimpse(startdata)

save(startdata,file="startdata.Rda")



enddata <- nydata %>% 
  rename(station.id = end.station.id, station.name = end.station.name, station.latitude = end.station.latitude, station.longitude = end.station.longitude) %>%
  mutate(Speed = Distance * 3600 / tripduration) %>%
  group_by(station.id, station.name, station.latitude, station.longitude) %>%
  summarise(meanduration_in = mean(tripduration),
            meanspeed_in = mean(Speed, na.rm = TRUE),
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
glimpse(enddata)

save(enddata,file="enddata.Rda")

# Join everything

alldata <- full_join(startdata, enddata)

save(alldata,file="alldata.Rda")

