
library(dplyr)
library(lubridate)
library(plyr)
library(rgdal)
library(stringr)

rm(list=ls())



## Load data
#-------------------

## STz admin shapefiles
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))
STzWard <- readOGR(paste("data/GIS/STzWard_NBS2012",sep=""), paste("STzWard_NBS2012",sep=""))


## Transect data
transects <- read.csv("Output/TransectData/transects_cleaned.csv")


## Vaccination data
vax <- read.csv(paste("Output/VaxData/AllDogVaccinatedGA_cleaned.csv",sep=""))




## Add round information
#--------------------

## dates
vax$dateVaccination <- as.Date(vax$dateVaccination)
vax$month <- month(vax$dateVaccination) + (year(vax$dateVaccination)-2010)*12
vax$year <- year(vax$dateVaccination)
transects$Date <- as.Date(transects$Date)

## Add rounds to vax data
vax$round <- NA
vax$round[which(vax$month<=19)] <- 1
vax$round[which(vax$month>=21 & vax$month<=34)] <- 2
vax$round[which(vax$month>=38 & vax$month<=57)] <- 3
vax$round[which(vax$month>=60 & vax$month<=70)] <- 4
vax$round[which(vax$month>=78)] <- 5
which(is.na(vax$round))


## Add rounds to vax data
transects$round <- NA
transects$round[which(transects$month<=19)] <- 1
transects$round[which(transects$month>=21 & transects$month<=34)] <- 2
transects$round[which(transects$month>=38 & transects$month<=57)] <- 3
transects$round[which(transects$month>=60 & transects$month<=70)] <- 4
transects$round[which(transects$month>=78)] <- 5
which(is.na(transects$round))




## Match Vaccination to transects
#-------------------

##now match with vaccination data
matchVax <- match(unique(tolower(transects$DWV)),tolower(vax$Correct_Village))
unique(tolower(transects$DWV))[which(is.na(matchVax))] 
length(unique(tolower(transects$DWV))[which(is.na(matchVax))] )
# quite a lot not matching up - probably correspond to villages (mostly streets) that were
# vaccinated through other central points


##join prep
vax <- vax[order(vax$Correct_Village,vax$round),]
vax$DW <- paste(word(vax$Correct_Village,1,sep="_"),word(vax$Correct_Village,2,sep="_"),sep="_")
vaxDogsMonth<-aggregate(Dogs~Correct_Village+DW+month+year+round,vax[which(vax$Dogs<1000),],sum)
vaxDogsMonthWard<-aggregate(Dogs~DW+month+year+round,vax[which(vax$Dogs<1000),],sum) # records with >1000 dogs are for cases where data was not available at village-level but aggregated over the district - remove these for now
which(duplicated(vaxDogsMonth[,c("Correct_Village","round")])) # no multiple vaccinations in the same round
which(duplicated(vaxDogsMonthWard[,c("DW","round")]))
write.csv(vaxDogsMonth,paste("Output/VaxData/vaxDogsMonthVillage.csv",sep=""),row.names = F)
write.csv(vaxDogsMonthWard,paste("Output/VaxData/vaxDogsMonthWard.csv",sep=""),row.names = F)


## Group transects carried out in same village in same round
transects_round_with_collars <- aggregate(Dogs.with.collars~DWV+DW+Category+round,transects,sum)
transects_round <- cbind(transects_round_with_collars,aggregate(Dogs.without.collars~DWV+DW+Category+round,transects,sum)[,5])
names(transects_round)[6] <- "Dogs.without.collars"


## Match vaccination data to transects at the village level
transects_vax <- merge(transects_round, vaxDogsMonth, by.x=c("DWV","DW","round"), by.y=c("Correct_Village","DW","round"), all.x=T, all.y=F)
names(transects_vax)[7:9] <- c("month_vax","year_vax","Dogs_vax")
write.csv(transects_vax,paste("Output/TransectData/transects_vax_village.csv",sep=""),row.names = F)


## Group transects by ward
transects_round_with_collars_ward <- aggregate(Dogs.with.collars~DW+Category+round,transects,sum)
transects_round_ward <- cbind(transects_round_with_collars_ward,aggregate(Dogs.without.collars~DW+Category+round,transects,sum)[,4])
names(transects_round_ward)[5] <- "Dogs.without.collars"
matchVaxWard <- match(unique(tolower(transects_round_ward$DW)),tolower(vax$DW))
unique(tolower(transects_round_ward$DW))[which(is.na(matchVaxWard))] 
# 9 transect wards not matching up to vax wards - better than for villages!


## Match vaccination data to transects at the ward level
transectsWard_vax <- merge(transects_round_ward, vaxDogsMonthWard, by.x=c("DW","round"), by.y=c("DW","round"), all.x=T, all.y=F)
names(transectsWard_vax)[6:8] <- c("month_vax","year_vax","Dogs_vax")
STzWard$DW <- paste(tolower(gsub("[^[:alpha:]]", "", STzWard$District_N)),
                    tolower(gsub("[^[:alpha:]]", "", STzWard$matchVill)),
                    sep="_")

transectsWard_vax <- transectsWard_vax[which(!is.na(transectsWard_vax$Dogs_vax)),]
write.csv(transectsWard_vax,paste("Output/TransectData/transectsWard_matchVax.csv",sep=""),row.names = F)

