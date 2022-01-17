#
## Get dog population in each village/cell in each month
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



## Read in data 
#--------------

## cell data
STzUTM <- read.csv(paste0("Output/STzCellData_",cell_size^2,"kmsq.csv"))

## Village shapefile
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))

## District shapefile
STzDist <- readOGR(paste("data/GIS/STzDist_NBS2012",sep=""), paste("STzDist_NBS2012",sep=""))

## NBS 2012 human population census
NBSpop <- read.csv("Output/NBS2012_STzVillagePop_matched.csv")
NBSpop_unmatched <- read.csv("data/unmatchedShapefileVillCorrected.csv")

## Study area grid
STzGrid <- raster(paste("Output/GIS/",cell_size^2,"kmsqGrids/STzGrid",cell_size^2,"kmsq.grd",sep=""))

## cell grid
cell_grid <- raster(paste("Output/GIS/",cell_size^2,"kmsqGrids/STzCellGrid",cell_size^2,"kmsq.grd",sep=""))
cell_grid[which(cell_grid[]==0)]<-NA


## WorldPop data
WorldPop15 <- raster(paste("data/GIS/WorldPop2015/WorldPop_reproject_",cell_size^2,"kmsq_2015.grd",sep=""))
WorldPop10 <- raster(paste("data/GIS/WorldPop2010/WorldPop_reproject_",cell_size^2,"kmsq_2010.grd",sep=""))

## Human population growth rate
growth_rates <- read.csv("data/pop_census_2012_G_rate.csv")
growth_rates <- growth_rates[match(unique(STzVill$District_N),growth_rates$District),]



## Exponential human pop growth rate in each grid cell based on Worldpop
#--------------

## first set population in protected areas to areas (even if human pop isn't quite 0, dog pop should be)
WorldPop10[which(!is.na(WorldPop10[]))[which(STzUTM$PA==1)]] <-0
WorldPop15[which(!is.na(WorldPop15[]))[which(STzUTM$PA==1)]] <-0

# growth raster
STzPopGrowth <- (WorldPop15/WorldPop10)^(1/5)-1
STzPopGrowth[which(is.na(STzPopGrowth[])&!is.na(STzGrid[]))]<-0



##Project a population distribution for 2012 using Worldpop growth raster
#--------------

WorldPop15 <- WorldPop10*(1+STzPopGrowth)^2
plot(WorldPop15)



## Match human census data to village shapefile
#--------------

STzVill$matchDWV <- gsub("'|\"|`|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(NBSpop$matchDWV,STzVill$matchDWV)
NBSpop$matchDWV[which(is.na(matchVill))]  # no census villages still unmatched
STzVill$matchDWV[which(is.na(match(STzVill$matchDWV,NBSpop$matchDWV)))] # 12 unmatched

STzVill$villPops<-0
STzVill$villPops[sort(unique(matchVill))] <- rowsum(NBSpop$Population,matchVill)


##Estimate the number of people in unmatched villages from the worldPop data
NBSpop_unmatched$DWV<-gsub("'|\"|`|/|,", "",paste(NBSpop_unmatched$District_N,tolower(NBSpop_unmatched$Ward_Name),gsub(" ","",tolower(NBSpop_unmatched$Vil_Mtaa_N))))
NBSpop_unmatched$DWVmatch<-gsub("'|\"|`|/|,", "",paste(NBSpop_unmatched$District_N,tolower(NBSpop_unmatched$Ward_Name),gsub(" ","",tolower(NBSpop_unmatched$Vill_combine))))
for(i in 1:nrow(NBSpop_unmatched)){
  
  ## Find cells containing each village
  vill_a<-which(STzVill$matchDWV==NBSpop_unmatched$DWV[i])
  vill_b<-which(STzVill$matchDWV==NBSpop_unmatched$DWVmatch[i])
  cells_a<-which(STzUTM$VillageID==vill_a) # which cells are assigned to the village
  cells_b<-which(STzUTM$VillageID==vill_b) # which cells are assigned to the village
  if(length(cells_a)==0){ # if no cells (village too small), find the cell containing the village centroid
    cells_a <- raster::extract(cell_grid,SpatialPoints(coordinates(STzVill[vill_a,]),proj4string=cell_grid@crs))}
  if(length(cells_b)==0){
    cells_b <- raster::extract(cell_grid,SpatialPoints(coordinates(STzVill[vill_b,]),proj4string=cell_grid@crs))}
  
  ## mean density in cells containing the village
  mean_dens_a <- mean(WorldPop15[which(!is.na(WorldPop15[]))[cells_a]])  
  mean_dens_b <- mean(WorldPop15[which(!is.na(WorldPop15[]))[cells_b]])
  
  ## Proportion of people to go to each village
  if(mean_dens_a==0|mean_dens_b==0){
    prop_a <- STzVill$Area_kmsq[vill_a]/(STzVill$Area_kmsq[vill_b] + STzVill$Area_kmsq[vill_a])
  }else{
    prop_a <- (STzVill$Area_kmsq[vill_a]*mean_dens_a)/(STzVill$Area_kmsq[vill_b]*mean_dens_b + STzVill$Area_kmsq[vill_a]*mean_dens_a)
  }
  
  ## People going to each village
  STzVill$villPops[vill_a] <- round(prop_a*STzVill$villPops[vill_b])
  STzVill$villPops[vill_b] <- STzVill$villPops[vill_b] - STzVill$villPops[vill_a]
}



## Get human population in each cell in 2012
##--------------

##Find villages that don't have an assigned cell
missedVill <- which(!is.element(1:nrow(STzVill),unique(STzUTM$VillageID)))

##Find which grid cell each of these has the greatest degree of overlap with
centroids <- coordinates(STzVill[missedVill,])
missedVillCells <- raster::extract(cell_grid,centroids)

## If some missed villages haven't got a cell as they're on the coast, assign these to the closest cell
if(length(which(is.na(missedVillCells)))>0){
  for (i in which(is.na(missedVillCells))){
    missedVillCells[which(is.na(missedVillCells))] <- 
      apply(X = centroids[which(is.na(missedVillCells)),], MARGIN = 1, 
            FUN = function(xy) cell_grid[which.min(replace(distanceFromPoints(cell_grid, xy), is.na(STzGrid), NA))])
  } 
}

## Save for later
write.table(cbind(missedVill,missedVillCells),paste("Output/VillagesMissedByGrid_",cell_size,"kmRes.csv",sep=""),row.names = F,col.names = c("villID","cellID"),sep=",")

## Add humans to each cell
source("Rcode/CodeForFigure3/popMap.R")
villPops<-STzVill$villPops
set.seed(0)
STzUTM$popMap<- PopMap(STzUTM, STzVill, init=0, probMap=WorldPop15, villPops=villPops,
                       missedVill=missedVill, missedVillCells=missedVillCells,villageIDs=STzUTM$VillageID)

## Plot
colours <- colorRampPalette(c(brewer.pal(8,"YlOrRd")[1:8]))(100)
par(mfrow=c(1,1),mar=c(0,0,0,6))
popGrid<-STzGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-STzUTM$popMap
plot(STzDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(STzDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=bquote("Humans/"~.(cell_size^2) * "km"^2), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.80,0.81, .25,.75))



sum(popGrid[],na.rm=T)
sum(NBSpop$Population,na.rm=T) 



## Use district-level human population growth rates to project human population forwards and back  
##--------------

## Time period of interest
years <- 2010:2017
months <- 12*length(years)
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2017-12-31")


## Set up population matrices (both cell and village)
popMat <- matrix(nrow=nrow(STzUTM),ncol=months)
popVillMat <- matrix(nrow=nrow(STzVill),ncol=months)
census_month <- 12*2 + 8 # census happened in August 2012
popMat[,census_month] <- STzUTM$popMap
popVillMat[,census_month] <- villPops

## exponential annual growth rate for each cell/village
growth_cells <- growth_rates$AnnualGrowthrate[match(STzUTM$District,growth_rates$District)]/100
growth_villages <- growth_rates$AnnualGrowthrate[match(STzVill$District_N,growth_rates$District)]/100

## Project forward
forward <- (census_month+1):months
popMat[,forward] <- rep(popMat[,census_month],length(forward))* 
  (1+rep(growth_cells,length(forward)))^rep((1:length(forward))/12,each=nrow(STzUTM))
popVillMat[,forward] <- rep(popVillMat[,census_month],length(forward))* 
  (1+rep(growth_villages,length(forward)))^rep((1:length(forward))/12,each=nrow(STzVill))

## Project backwards
backward <- (census_month-1):1
popMat[,backward] <- rep(popMat[,census_month],length(backward))* 
  (1+rep(growth_cells,length(backward)))^rep(-(1:length(backward))/12,each=nrow(STzUTM))
popVillMat[,backward] <- rep(popVillMat[,census_month],length(backward))* 
  (1+rep(growth_villages,length(backward)))^rep(-(1:length(backward))/12,each=nrow(STzVill))

## Round to the nearest human
popMat <- round(popMat)
popVillMat <- round(popVillMat)
sum(popMat[,1])
sum(popMat[,months])


## Save human population matrices for simulation 
write.table(popMat,paste("Output/STzHumanPopMat_CellByMonth_",cell_size^2,"kmsq_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")
write.table(popVillMat,paste("Output/STzHumanPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")

