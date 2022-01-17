
library(dplyr)
library(lubridate)
library(plyr)
library(rgdal)
library(stringr)

rm(list=ls())


## STz admin shapefiles
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))
STzWard <- readOGR(paste("data/GIS/STzWard_NBS2012",sep=""), paste("STzWard_NBS2012",sep=""))


## Transect data
transects <- read.csv("data/transectsall_corrected_locs.csv")


## Find and delete duplicated records
duplicates<-transects[which(duplicated(transects) | duplicated(transects[nrow(transects):1,])[nrow(transects):1]),]
if(nrow(duplicates)>0){transects <- transects[-which(duplicated(transects)),]}


## Get rid of data from regions not currently of interest
transects <- transects[-which(is.element(transects$Correct_Region,c("Mara","Kaskazini Pemba","Kusini Pemba"))),]


## Some villages have multiple records for different transects on the same day - aggregate these
which(duplicated(transects[,c(5:6,14:ncol(transects))]))
transects <- aggregate(.~Date+Correct_Village+Correct_Ward+Correct_District+Correct_Region,transects[,c(5,10:11,14:ncol(transects))],sum)


## Match district names between datasets
studyDist <- match(unique(paste(transects$Correct_Region,transects$Correct_District)),unique(paste(STzVill$Region_Nam,STzVill$District_N)))
unique(paste(transects$Correct_Region,transects$Correct_District))[which(is.na(studyDist))] ## Districts all match


## Match ward names between datasets
transects$Correct_Ward[which(transects$Correct_District=="Tandahimba" & transects$Correct_Ward=="Mihenjele")] <- "Michenjele"
transects$Correct_District[which(transects$Correct_District=="Masasi" & transects$Correct_Ward=="Nyasa")] <- "Masasi Township Authority"
transects$Correct_District[which(transects$Correct_District=="Mtwara Urban" & transects$Correct_Ward=="Namatutwe")] <- "Masasi"
studyWard <- match(unique(paste(transects$Correct_Region,transects$Correct_District,transects$Correct_Ward)),unique(paste(STzVill$Region_Nam,STzVill$District_N,(STzVill$Ward_Name))))
unique(paste(transects$Correct_Region,transects$Correct_District,transects$Correct_Ward))[which(is.na(studyWard))] 


## Make corrections to village names
transects$Correct_Village[which(transects$Correct_Ward=="Migongo" & transects$Correct_Village=="Misufini")] <- "Mtaa wa Misufini"
transects$Correct_Village[which(transects$Correct_Ward=="Lundi" & transects$Correct_Village=="Tambuui")] <- "Tambuu"
transects$Correct_Village[which(transects$Correct_Ward=="Visiga" & transects$Correct_Village=="Kichangani")] <- "Visiga Kati"


## Dates
transects$Date <- as.Date(transects$Date,format = "%d/%m/%Y")
transects$month <- month(transects$Date) + (year(transects$Date)-2010)*12
transects$year <- year(transects$Date)


## Get correct amalgamated district_ward and district_ward_village name
transects$DWV <-
  apply(transects[, c("Correct_District", "Correct_Ward", "Correct_Village")], 1,
        function(x) paste(tolower(gsub("[^[:alpha:]]", "", x)), collapse = "_"))
transects$DW<-
  apply(transects[, c("Correct_District", "Correct_Ward")], 1,
        function(x) paste(tolower(gsub("[^[:alpha:]]", "", x)), collapse = "_"))


## Match STzVillages
STzVill$matchVill<-paste(
  tolower(gsub("[^[:alpha:]]", "", STzVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Ward_Name)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Vil_Mtaa_N)),
  sep = "_")
studyVill <- match(unique(transects$DWV),unique(STzVill$matchVill))
unique(transects$DWV)[which(is.na(studyVill))] # all matching


## Add urban/rural indicator to transect data
# transects$Category <- STzVill@data[match(transects$DWV,STzVill$matchVill),"Category"]
transects$Category <- ifelse(is.element(transects$Correct_District,
                                        c("Ilala", "Kinondoni", "Lindi Urban", 
                                          "Masasi Township Authority", "Morogoro Urban", 
                                          "Mtwara Urban", "Temeke","Kibaha Urban")),
                             "Urban","Rural")


## Save cleaned transect data
if(!dir.exists("Output/TransectData")){dir.create("Output/TransectData")}
write.csv(transects,paste("Output/TransectData/transects_cleaned.csv",sep=""),row.names = F)



