#
## Create grids of study region
#__________________________

rm(list=ls())

library(maptools)
library(raster)
library(rgeos)
library(rgdal)
library(stringr)
library(dplyr)

options(stringsAsFactors=F) 

cell_size<-2 #km


## Read in data 
#--------------

## Projections
crs37S <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
crs36S <- CRS("+proj=utm +zone=36 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
crsLL <- CRS("+proj=longlat")

## Load in administrative shapefiles 
STzVill <- readOGR("data/GIS/STzVill_NBS2012","STzVill_NBS2012")
STzWard <- readOGR("data/GIS/STzWard_NBS2012","STzWard_NBS2012")
STzDist <- readOGR("data/GIS/STzDist_NBS2012","STzDist_NBS2012")

## Protected areas
PAs <- readShapePoly("data/GIS/ProtectedAreas/TZprotected_areas.shp", proj4string = crs37S) # IN CORRECT PROJECTION
spTransform(PAs,STzVill@proj4string)



## Create STz grids
#--------------

## Create grid
grid <- raster(extent(STzVill),crs=STzVill@proj4string)
res(grid) <- cell_size*1000

##Create STz Grid
STzVill$studyArea<-1
gridPoints <- SpatialPoints(rasterToPoints(grid), proj4string = STzVill@proj4string)
values <- over(gridPoints,STzVill)[,"studyArea"]; values[which(values==0)]<-1
STzGrid <- grid
STzGrid[] <- as.numeric(values)
plot(STzGrid)

## Create 2012 village grid
STzVill@data$VillID <- 1:nrow(STzVill@data)
values <- over(gridPoints,STzVill)$VillID
villGrid <- grid
villGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(STzGrid[]))
plot(villGrid)

## Create 2012 district grid
STzDist$DistID<-1:nrow(STzDist)
values <- over(gridPoints,STzDist)$DistID
distGrid <- grid
distGrid[] <- values
cellsToEmpty <- which(is.na(villGrid[])&!is.na(distGrid[]))
cellsToFill <- which(!is.na(villGrid[])&is.na(distGrid[]))
if(length(cellsToEmpty>0)){
  distGrid[cellsToEmpty]<-NA}
if(length(cellsToFill)>0){
  distGrid[cellsToFill]<- STzDist$DistID[match(STzVill$District_N[villGrid[cellsToFill]],STzDist$District_N)]
}
plot(distGrid)

## Create 2012 ward grid
STzWard$WardID<-1:nrow(STzWard)
values <- over(gridPoints,STzWard)$WardID
wardGrid <- grid
wardGrid[] <- values
cellsToEmpty <- which(is.na(villGrid[])&!is.na(wardGrid[]))
cellsToFill <- which(!is.na(villGrid[])&is.na(wardGrid[]))
if(length(cellsToEmpty>0)){
  wardGrid[cellsToEmpty]<-NA}
if(length(cellsToFill)>0){
  wardGrid[cellsToFill]<- STzWard$WardID[match(STzVill$Ward_Name[villGrid[cellsToFill]],STzWard$Ward_Name)]
}
plot(wardGrid)


##Protected areas grid
PAs@proj4string <- STzVill@proj4string
PAs$layer<-1
values <- over(gridPoints,PAs)$layer
PAGrid <- grid
PAGrid[] <- values
PAGrid[which(is.na(STzGrid[])&!is.na(PAGrid[]))] <- NA
PAGrid[which(is.na(PAGrid[])&!is.na(STzGrid[]))] <- 0
plot(PAGrid)


##Save grids
if(!dir.exists("Output/GIS/")){dir.create("Output/GIS/")}
if(!dir.exists(paste("Output/GIS/",cell_size^2,"kmsqGrids",sep=""))){dir.create(paste("Output/GIS/",cell_size^2,"kmsqGrids",sep=""))}
writeRaster(STzGrid,file=paste("Output/GIS/",cell_size^2,"kmsqGrids/STzGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(distGrid,file=paste("Output/GIS/",cell_size^2,"kmsqGrids/distGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(villGrid,file=paste("Output/GIS/",cell_size^2,"kmsqGrids/villGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(wardGrid,file=paste("Output/GIS/",cell_size^2,"kmsqGrids/wardGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)


## Give ID to each cell
STzUTM<-data.frame(cellID=1:length(which(!is.na(STzGrid[]))))
cellGrid<-STzGrid
cellGrid[which(!is.na(cellGrid[]))]<-STzUTM$cellID
cellGrid[which(is.na(cellGrid@data@values))]<-0
plot(cellGrid)


## Add additional information on cells to dataframes
STzUTM$District <- STzDist$District_N[(distGrid[which(!is.na(distGrid@data@values))])]
STzUTM$DistrictID <- distGrid[which(!is.na(distGrid@data@values))]
STzUTM$Ward <- STzWard$matchVill[(wardGrid[which(!is.na(wardGrid@data@values))])]
STzUTM$WardID <- wardGrid[which(!is.na(wardGrid@data@values))]
STzUTM$Village <- STzVill$Vil_Mtaa_N[(villGrid[which(!is.na(villGrid@data@values))])]
STzUTM$VillageID <- villGrid[which(!is.na(villGrid@data@values))]
STzUTM$PA <- PAGrid[which(!is.na(PAGrid@data@values))]
STzUTM$Urban <- STzVill$Category[STzUTM$VillageID]; STzUTM$Urban <- ifelse(STzUTM$Urban=="Urban",1,0)


# Save cell grids and data 
write.table(as.matrix(cellGrid),paste("Output/STz_matrix_",cell_size^2,"kmsq_cellID.csv",sep=""),row.names=F,col.names=F,sep=",")
writeRaster(cellGrid,file=paste("Output/GIS/",cell_size^2,"kmsqGrids/STzCellGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
write.table(STzUTM,paste("Output/STzCellData_",cell_size^2,"kmsq.csv",sep=""),row.names=F,sep=",")

