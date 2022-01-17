#
## Get dog vaccination coverage in each ward/cell in each month
# paired with CreatePop5.R
#_______________________________


rm(list=ls())

library(maptools)
library(raster)
library(rgeos)
library(rgdal)
library(stringr)
library(RColorBrewer)
library(lubridate)

options(stringsAsFactors=F) 


cell_size <- 2 #km
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2017-12-31")

PAR <- 0.2562058 #pup/adult ratio
propPups <- 0.2039521 #proportion of dogs that are pups
maxVax <- 1-propPups # maximum vaccination coverage can reach 

set.seed(0)


## Read in data 
#--------------

## Cell Data
STzUTM <- read.csv(paste0("Output/STzCellData_",cell_size^2,"kmsq.csv"))

## Village shapefile
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))

## Ward shapefile
STzWard <- readOGR(paste("data/GIS/STzWard_NBS2012",sep=""), paste("STzWard_NBS2012",sep=""))
STzWard$DW <- paste(STzWard$District_N,STzWard$matchVill,sep="_")

## District shapefile
STzDist <- readOGR(paste("data/GIS/STzDist_NBS2012",sep=""), paste("STzDist_NBS2012",sep=""))

## Protected areas Shapefile
crs37S <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
PAs <- readShapePoly("data/GIS/ProtectedAreas/TZprotected_areas.shp", proj4string = crs37S) # IN CORRECT PROJECTION
PAs <- gIntersection(PAs,STzDist)

## Dog populations in cells and villages by month
dogPopMat_transects <- as.matrix(read.csv(paste("Output/STzDogPopMat_CellByMonth_",cell_size^2,"kmsq_",startDate,"_to_",endDate,".csv",sep=""), header=F))
dogPopVillMat_transects <- as.matrix(read.csv(paste("Output/STzDogPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""), header=F))

## Study area grid
STzGrid <- raster(paste("Output/GIS/",cell_size^2,"kmsqGrids/STzGrid",cell_size^2,"kmsq.grd",sep=""))

## Vaccination data
vaxDogsMonthVill <- read.csv("Output/VaxData/vaxDogsMonthVillage.csv")
vaxDogsMonthWard <- read.csv("Output/VaxData/vaxDogsMonthWard.csv")
vaxDogsMonthVill$District <- word(vaxDogsMonthVill$Correct_Village,1,sep="_")
vaxDogsMonthWard$District <- word(vaxDogsMonthWard$DW,1,sep="_")
vax <- read.csv(paste("Output/VaxData/AllDogVaccinatedGA_cleaned.csv",sep=""))
vax_agg <- vax[which(vax$Dogs>1000),]
vax_agg$dateVaccination <- as.Date(vax_agg$dateVaccination)

##Vaccination rounds
vacc_rounds <- read.csv("data/Vaccinationrounds.csv")




## Match vaccination data to village and ward shapefiles
#--------------

## shapefile names to match to vax and transect data
STzVill$matchVill<-paste(
  tolower(gsub("[^[:alpha:]]", "", STzVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Ward_Name)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Vil_Mtaa_N)),
  sep = "_")
STzVill$matchWard<-paste(
  tolower(gsub("[^[:alpha:]]", "", STzVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", STzVill$Ward_Name)),
  sep = "_")
STzWard$matchWard<-paste(
  tolower(gsub("[^[:alpha:]]", "", STzWard$District_N)),
  tolower(gsub("[^[:alpha:]]", "", STzWard$matchVill)),
  sep = "_")

vaxDogsMonthVill$matchVill <- match(vaxDogsMonthVill$Correct_Village,STzVill$matchVill)
which(is.na(vaxDogsMonthVill$matchVill)) 

vaxDogsMonthVill$matchWard <- match(vaxDogsMonthVill$DW,STzWard$matchWard)
which(is.na(vaxDogsMonthVill$matchWard))

vaxDogsMonthWard$matchWard <- match(vaxDogsMonthWard$DW,STzWard$matchWard)
which(is.na(vaxDogsMonthWard$matchWard)) # all good!



## Create matrix of dogs vaccinated in each village in each month
#--------------

## Create vaccination matrix and add data
dogVaxVillMat <- matrix(0,nrow(dogPopVillMat_transects),ncol=ncol(dogPopVillMat_transects))
for(i in 1:nrow(dogVaxVillMat)){
  if(STzVill$Category[i]=="Rural"){
    vax_i <- vaxDogsMonthVill[which(vaxDogsMonthVill$matchVill==i),]
    if(nrow(vax_i)>0){
      dogVaxVillMat[i,vax_i$month] <- vax_i$Dogs
    }
  }
}
UrbanWards <- unique(STzVill$matchWard[which(STzVill$Category=="Urban")])
for(i in 1:length(UrbanWards)){
  
  ## find vaccination events and villages in ward
  vax_i <- vaxDogsMonthWard[which(vaxDogsMonthWard$DW==UrbanWards[i]),]
  if(nrow(vax_i)>0){
    vills <- which(STzVill$matchWard==UrbanWards[i])
    
    ## For each vaccination event...
    for(j in 1:nrow(vax_i)){
      
      ##...Select dogs to vaccinate from each village
      Dogs_ij <- vax_i$Dogs[j]
      dogs_ij <- rep(0,length(vills))
      dogs_ij_table <- table(sample(rep(1:length(vills),times=dogPopVillMat_transects[vills,vax_i$month[j]]),Dogs_ij))
      dogs_ij[as.numeric(names(dogs_ij_table))] <- as.numeric(dogs_ij_table)
      
      ## Add to matrix
      dogVaxVillMat[vills,vax_i$month[j]]<-dogs_ij
      
    }
  }
}


## For cases where we don't have village/ward-level resolution, distribute dogs between villages
unique(year(vax_agg$dateVaccination)) # use rounds 4 & 5 to draw vaccination weights
vacc_rounds <- vacc_rounds[which(vacc_rounds$Region!="Pemba"),]
vcVillRound <- matrix(0,nrow=nrow(STzVill),ncol=5)
for(i in 1:ncol(vcVillRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    vcVillRound[which(STzVill$District_N==vacc_rounds$District[j]),i]<-rowSums((dogVaxVillMat/dogPopVillMat_transects)[which(STzVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-2010)])
  }
}
vcVillRound[which(is.na(vcVillRound))]<-0 # a few issues where dog pop was 0
vax_draw <- c(c(vcVillRound[which(STzVill$Category=="Rural"),4:5]),c(vcVillRound[match(unique(STzVill$matchWard[which(STzVill$Category=="Urban")]),STzVill$matchWard),4:5]))
for(i in 1:nrow(vax_agg)){

  ## Villages in the district that we don't already have vaccination info for this year
  year <- year(vax_agg$dateVaccination[i])
  month <- month(vax_agg$dateVaccination[i]) + (year(vax_agg$dateVaccination[i])-2010)*12
  vaxVillYear <- rowSums(dogVaxVillMat[,(1:12)+12*(year-2010)])
  vills <- which(STzVill$District_N==vax_agg$Correct_District[i] & vaxVillYear==0)

  ## Is the district urban or rural
  category_i <- unique(STzVill$Category[vills])

  ## Total number of vaccination units
  if(is.element("Rural",category_i)){
    units_i <- length(vills)
  }else{
    units_i <- length(unique(STzVill$matchWard[vills]))
  }

  ## Draw vaccination weight for each unit
  weights <- sample(vax_draw,units_i,replace=T)
  weights[which(weights==0)]<-1e-12 # means dogs in these pops can still be selected if we run out of dogs elsewhere

  ## Distribute dogs among vaccination units based on weights (and dog populations)
  if(category_i=="Rural"){dogPops_i <- dogPopVillMat_transects[vills,month]
    }else{dogPops_i <- rowsum(dogPopVillMat_transects[vills,month], STzVill$matchWard[vills])}
  dogs_i <- rep(1:units_i,round(dogPops_i*maxVax))
  probs <- rep(weights,round(dogPops_i*maxVax))
  draw_dogs_i <- sample(dogs_i, vax_agg$Dogs[i],prob = probs)
  if(category_i=="Rural"){
    dogs_vill_i <- table(draw_dogs_i)
    dogVaxVillMat[vills[as.numeric(names(dogs_vill_i))],month]<-dogs_vill_i

  }else{
    dogs_ward_i <- table(draw_dogs_i)
    for(ward in 1:length(dogs_ward_i)){
      vills_j <- which(STzVill$matchWard==sort(unique(STzVill$matchWard[vills]))[as.numeric(names(dogs_ward_i))[ward]])
      dogs_vills_j <- table(sample(rep(1:length(vills_j),times=dogPopVillMat_transects[vills_j,month]),dogs_ward_i[ward]))
      dogVaxVillMat[vills_j[as.numeric(names(dogs_vills_j))],month] <- as.numeric(dogs_vills_j)

    }
  }
}

## Get vaccination coverages
vcVill <- dogVaxVillMat/dogPopVillMat_transects
vcVill[which(is.nan(vcVill))] <- 0 #a few nans caused by dividing by 0 dogs

## save coverages (round to reduce file size)
write.table(round(vcVill,4),paste("Output/vcMat_Vill_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")



## Get vaccination coverage by village by round
vacc_rounds <- vacc_rounds[which(vacc_rounds$Region!="Pemba"),]
vcVillRound <- matrix(0,nrow=nrow(STzVill),ncol=5)
for(i in 1:ncol(vcVillRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    vcVillRound[which(STzVill$District_N==vacc_rounds$District[j]),i]<-rowSums(vcVill[which(STzVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-2010)])
  }
}
vcMatRound <- vcVillRound[STzUTM$VillageID,]
write.table(round(vcVillRound,4),file=paste("Output/vcMat_Vill_Round_",startDate,"_to_",endDate,".csv",sep=""),row.names = STzVill$matchVill,col.names = F, sep=",")

## Get vaccination coverage by district by round
vcDistRound <- matrix(0,nrow=nrow(STzDist),ncol=5)
for(i in 1:ncol(vcDistRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    months_round <- ((1:(12*length(years_round)))+12*(years_round[1]-2010))[which(vcVill[which(STzVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-2010)]!=0,arr.ind = T)[,2]]
    vcDistRound[j,i]<-sum(vcVill[cbind(which(STzVill$District_N==vacc_rounds$District[j]),months_round)]*dogPopVillMat_transects[cbind(which(STzVill$District_N==vacc_rounds$District[j]),months_round)])
    vcDistRound[j,i]<-vcDistRound[j,i]/sum(dogPopVillMat_transects[cbind(which(STzVill$District_N==vacc_rounds$District[j]),months_round)])
  }
}
write.table(round(vcDistRound,4),file=paste("Output/vcMat_Dist_Round_",startDate,"_to_",endDate,".csv",sep=""),row.names = vacc_rounds$District,col.names = F, sep=",")



## Get Waning coverage matrices
#--------------

## Waning rate
vaccine_duration <- 3
annual_death_rate <- 0.448375
lambda <- exp(-(1/vaccine_duration - log(1-annual_death_rate))/12)

## Waning number of vaccinated dogs in each village
vax_waning <- cbind(dogVaxVillMat,matrix(0,ncol=12*(year(endDate)-year(endDate)),nrow=nrow(dogVaxVillMat)))
for(i in 2:ncol(vax_waning)){
  popChange <- (dogPopVillMat_transects[,i-1] - dogPopVillMat_transects[,i])/dogPopVillMat_transects[,i-1]
  popChange[which(popChange<0)] <- 0
  vax_waning[,i] <- pmax(vax_waning[,i],lambda*(vax_waning[,i-1]*(1-popChange)))
}
vax_waning[which(is.na(vax_waning))] <- 0

## Waning village coverage
vc_waningVill<-vax_waning[,1:ncol(vax_waning)]/dogPopVillMat_transects
vc_waningVill[which(is.na(vc_waningVill))] <- 0
write.table(round(vc_waningVill,4),file=paste("Output/MonthlyCoverageVillage_",startDate,"_to_",endDate,".csv",sep=""),col.names = F,row.names = STzVill$matchVill,sep=",")

## Waning cell coverage
vcMatWaning <- vc_waningVill[STzUTM$VillageID,]
write.table(round(vcMatWaning,4),file=paste("Output/MonthlyCoverageCell_",cell_size^2,"kmsq_",startDate,"_to_",endDate,".csv",sep=""),col.names = F, row.names = F,sep=",")

## Get waning coverage time series for each district
vc_waning_dist <- rowsum(vax_waning,STzVill$District_N)/rowsum(dogPopVillMat_transects,STzVill$District_N)
write.table(round(vc_waning_dist,4),file=paste("Output/MonthlyCoverageDistrict_",startDate,"_to_",endDate,".csv",sep=""),col.names = F,sep=",")

## Get waning coverage time series for whole Gates area
vc_waning_all <- colSums(vax_waning)/colSums(dogPopVillMat_transects)
write.table(matrix(round(vc_waning_all,4),nrow=1),file=paste("Output/MonthlyCoverageSTz_",startDate,"_to_",endDate,".csv",sep=""),col.names = F,row.names = F,sep=",")



##Plots 
#_________________

## Create colours for map of coverage
breaks=seq(0,1,length.out=100)
colours=colorRampPalette(c("white",brewer.pal(8,"YlGn")[2:8],"black"))(length(breaks)-1)


## Whole region coverage through time (monthly)
#--------------

par(mfrow=c(1,1))
par(mar=c(5,4,2,1))
plot(vc_waning_all,type="l",lwd=2,ylim=c(0,1),ylab="Coverage",xlab="Month",bty="l",col="navy")


## Plot vaccination rounds
#--------------

source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")

colours <- colorRampPalette(c("white",brewer.pal(8,"YlGn")[2:8],"black"))(100)
tiff(filename = paste("Figs/Figure3.tiff",sep=""),
     width = 140, height = 180, units = "mm", pointsize = 12,
     res=350)
par(mfrow=c(3,2),mar=c(0,0,0,0))
for(i in 1:ncol(vcMatRound)){

  vaxGrid<-STzGrid
  vaxGrid[which(vaxGrid[]==0)]<-NA
  vaxGrid[which(!is.na(vaxGrid[]))]<-vcMatRound[,i]
  plot(STzDist,xlim=c(0,658184.7 ))
  plot(vaxGrid,add=T,col=colours,breaks=seq(0,1,length.out=100),legend=F)
  if(i==1){
    plot(STzDist[which(STzDist$District_N %in% c("Ilala","Morogoro Urban","Kinondoni","Morogoro","Mkuranga","Masasi")),],
         col="grey45", add=T)
  }else if(i==2){
    plot(STzDist[which(STzDist$District_N %in% c("Morogoro Urban","Kinondoni","Morogoro","Rufiji")),],
         col="grey45", add=T)
  }else if(i==3){
    plot(STzDist[which(STzDist$District_N %in% c("Kinondoni")),],
         col="grey45", add=T)
  }
  plot(PAs,col="lightgrey",add=T,border=F)
  plot(PAs,col="grey30",density=30,add=T,border=F)
  plot(STzDist,add=T)
  text(70000,9300000,paste("(",letters[i],") Round ",i,sep=""),font=1,cex=1.1)

  if(i==1){
    legend(-30000,9290000, legend=c("Protected areas","Data not spatially\nresolved"), fill=c("lightgrey", "grey45"),
           density=c(NA, NA), bty="n",box.cex = c(1.2,0.8)) 
    legend(-30000,9290000, legend=c("Protected areas","Data not spatially\nresolved"), fill=c("grey30", "grey45"),
           density=c(30, NA), bty="n",box.cex = c(1.2,0.8)) 
    
  }

  if(i==ncol(vcMatRound)){
    plot.new()
    plot(vaxGrid, breaks=seq(0,1,length.out=100),
         legend.only=T, add=T,col=colours,
         legend.args=list(text="Vaccination Coverage", side=4, line=3.8, cex=0.9),
         axis.args=list(at=seq(0,1,0.2)),
         smallplot=c(0.45,0.48, .25,.75))
  }

}
dev.off()


