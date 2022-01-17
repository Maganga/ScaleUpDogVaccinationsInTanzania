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

## Cell Data
STzUTM <- read.csv(paste0("Output/STzCellData_",cell_size^2,"kmsq.csv"))

## Village shapefile
STzVill <- readOGR(paste("data/GIS/STzVill_NBS2012",sep=""), paste("STzVill_NBS2012",sep=""))

## Ward shapefile
STzWard <- readOGR(paste("data/GIS/STzWard_NBS2012",sep=""), paste("STzWard_NBS2012",sep=""))
STzWard$DW <- paste(STzWard$District_N,STzWard$matchVill,sep="_")

## District shapefile
STzDist <- readOGR(paste("data/GIS/STzDist_NBS2012",sep=""), paste("STzDist_NBS2012",sep=""))

## Study area grid
STzGrid <- raster(paste("Output/GIS/",cell_size^2,"kmsqGrids/STzGrid",cell_size^2,"kmsq.grd",sep=""))

## HDR
HDR <- read.csv("data/EstimatedDogs2015.csv") # estimates are from Sambo et al. 2018, "Estimating the Size of Dog Populations in Tanzania to Inform Rabies Control", Veterinary Sciences, doi:10.3390/vetsci5030077
HDR$District[which(HDR$District=="masasi urban")]<-"masasi township authority"
HDR$District[which(HDR$District=="morogoro rural")]<-"morogoro"
HDR$District[which(HDR$District=="kibaha rural")]<-"kibaha"
HDR <- HDR[match(tolower(unique(STzVill$District_N)),HDR$District),]

##Transect data matched to vax data
transectsVill_vax <- read.csv("Output/transectData/transects_vax_village.csv") #village-level
transectsWard_vax <- read.csv("Output/TransectData/transectsWard_matchVax.csv") #ward-level

## Vaccination data (has had the aggregated data removed)
vaxDogsMonthVill <- read.csv("Output/VaxData/vaxDogsMonthVillage.csv")
vaxDogsMonthWard <- read.csv("Output/VaxData/vaxDogsMonthWard.csv")

## maximum VC
## Assuming that pups are not vaccinated, and relative pup/adult numbers as in Serengeti (described again in Sambo et al. 2018)
PAR <- 12850/50155 #pup/adult ratio
propPups <- 12850/(12850+50155) #proportion of dogs that are pups
maxVax <- 1-propPups

## minimum number of dogs required for us not to throw out transect as unreliable
minN <- 5

## Time period of interest
years <- 2010:2017
months <- 12*length(years)
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2017-12-31")

## Human population growth rate
growth_rates <- read.csv("data/pop_census_2012_G_rate.csv")
growth_rates <- growth_rates[match(unique(STzVill$District_N),growth_rates$District),]

## human population by cell/village and month
popMat <- as.matrix(read.csv(paste("Output/STzHumanPopMat_CellByMonth_",cell_size^2,"kmsq_",startDate,"_to_",endDate,".csv",sep=""),header=F))
popVillMat <- as.matrix(read.csv(paste("Output/STzHumanPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""),header=F))

## census happened in August 2012
census_month <- 12*2 + 8 

## Function for distributing individuals in cells for each village 
source("Rcode/CodeForFigure3/popMap.R")

## Villages missed by the grid and cellIDs they should be assigned
missedVill <- read.csv(paste("Output/VillagesMissedByGrid_",cell_size,"kmRes.csv",sep=""))
missedVillCells <- missedVill[,2]
missedVill <- missedVill[,1]



## Calculate HDRs and obtain dog population from human
#--------------

## Human:dog ratios for each district
humanDistPop12 <- rowsum(popVillMat[,census_month],(STzVill$District_N))
humanDistPop15 <- as.numeric(humanDistPop12)*(1+growth_rates$AnnualGrowthrate[match(rownames(humanDistPop12),growth_rates$District)]/100)^3
HDR$HDR <- humanDistPop15[match(HDR$District,tolower(rownames(humanDistPop12)))]/HDR$est_dogs15
HDR_cells <- HDR$HDR[match(tolower(STzUTM$District),HDR$District)] # for each cell
HDR_villages <- HDR$HDR[match(tolower(STzVill$District_N),HDR$District)] # for each village

## Dog Population matrix
dogPopMat <- round(popMat/HDR_cells)
dogPopVillMat <- round(popVillMat/HDR_villages)
sum(dogPopMat[,months])
sum(dogPopVillMat[,months]) 
# Using the same approach to get cell-level estimates (i.e. by dogPopMat <-
# round(popMat/HDR_cells)) doesn't work well, since in low-density areas dog
# numbers often come out at <0.5, leading to lots of rounding down to zero, and
# underestimation of total numbers.  Instead use the following approach to
# distribute village dogs.

## Use village matrix to distribute dogs at the cell level for the first month
popGrid1<-STzGrid
popGrid1[which(popGrid1[]==0)]<-NA
popGrid1[which(!is.na(popGrid1[]))]<-popMat[,1]
dogPopMat[]<-0
set.seed(0)
dogPopMat[,1]<- PopMap(STzUTM, STzVill, probMap=popGrid1, villPops=dogPopVillMat[,1],
                       missedVill=missedVill, missedVillCells=missedVillCells,villageIDs = STzUTM$VillageID)

## Fill in for rest of months
diff <- t(diff(t(dogPopVillMat)))
for(i in 2:ncol(dogPopMat)){
  popGrid<-STzGrid
  popGrid[which(popGrid[]==0)]<-NA
  popGrid[which(!is.na(popGrid[]))]<-popMat[,i]
  dogPopMat[,i] <- PopMap(STzUTM, STzVill, init=dogPopMat[,i-1], probMap=popGrid, villPops=diff[,i-1],
                          missedVill=missedVill, missedVillCells=missedVillCells,villageIDs = STzUTM$VillageID)
}

sum(dogPopMat[,months])
sum(dogPopVillMat[,months]) 





##Get dog population estimates from transect data
#--------------

# Above estimates are based purely on district-level human-dog ratios.  This
# doesn't allow for differences in HDR within districts and can lead to
# vaccination coverages of more than 100% at the village-level.  Transect data
# at the village-level can allow us to incorporate these differences in HDR and
# when combined with data on total dogs vaccinated, we can prevent coverages of
# >100%


## Village-level
#------

##estimate vaccination coverage from transects
transectsVill_vax <- transectsVill_vax[which(!is.na(transectsVill_vax$Dogs_vax) & transectsVill_vax$Dogs.with.collars>0),]
transectsVill_vax$cov <- transectsVill_vax$Dogs.with.collars/(transectsVill_vax$Dogs.with.collars+transectsVill_vax$Dogs.without.collars)
hist(transectsVill_vax$cov,breaks="fd")
length(which(transectsVill_vax$cov==1))/nrow(transectsVill_vax)
## 18.5% of coverages are 100%...

totalDogs <- transectsVill_vax$Dogs.with.collars+transectsVill_vax$Dogs.without.collars
hist(totalDogs,breaks="fd")
length(which(totalDogs<5))/length(totalDogs) # 24.4% of transects involved <5 dogs
length(which(totalDogs>10))/length(totalDogs) # only 38.0% of transects counted more than 10 dogs
## probably can't trust transects that reached small numbers of dogs

## Throw out transects with less than the minimum number of dogs (minN)
transectsVill_vax <- transectsVill_vax[which(totalDogs>=minN),]
length(which(transectsVill_vax$cov==1))/nrow(transectsVill_vax) # now 8.2% of coverages ==1
hist(transectsVill_vax$cov,breaks="fd")

## Get population size from transect data
transectsVill_vax$pop <- round((transectsVill_vax$Dogs_vax/transectsVill_vax$cov)*(1+PAR)) # accounting for pups too with pup:adult ratio
hist(transectsVill_vax$pop,breaks="fd")

## Get coverage that accounts for pups
transectsVill_vax$cov_pupAdjust <- transectsVill_vax$Dogs_vax/transectsVill_vax$pop
hist(transectsVill_vax$cov_pupAdjust)



## Ward-level
#------

## estimate vaccination coverage at the ward level
transectsWard_vax <- transectsWard_vax[which(!is.na(transectsWard_vax$Dogs_vax) & transectsWard_vax$Dogs.with.collars>0),]
transectsWard_vax$cov <- transectsWard_vax$Dogs.with.collars/(transectsWard_vax$Dogs.with.collars+transectsWard_vax$Dogs.without.collars)
hist(transectsWard_vax$cov,breaks="fd")
length(which(transectsWard_vax$cov==1))/nrow(transectsWard_vax) #7.1% of transects 100% coverage
totalDogs <- transectsWard_vax$Dogs.with.collars+transectsWard_vax$Dogs.without.collars
hist(totalDogs,breaks="fd")
hist(totalDogs[which(transectsWard_vax$cov==1)])
length(which(totalDogs<5))/length(totalDogs) # 7.3% of transects involved <5 dogs
length(which(totalDogs>10))/length(totalDogs) # 77.6% of transects counted more than 10 dogs
## a little more trustworthy...

## Throw out transects with less than the minimum number of dogs (minN)
transectsWard_vax <- transectsWard_vax[which(totalDogs>=minN),]
length(which(transectsWard_vax$cov==1))/nrow(transectsWard_vax) # now 3.8% of coverages ==1
hist(transectsWard_vax$cov,breaks="fd")

## Get population size from transect data
transectsWard_vax$pop <- round((transectsWard_vax$Dogs_vax/transectsWard_vax$cov)*(1+PAR)) # accounting for pups too with pup:adult ratio
hist(transectsWard_vax$pop,breaks="fd")

## Get coverage that accounts for pups
transectsWard_vax$cov_pupAdjust <- transectsWard_vax$Dogs_vax/transectsWard_vax$pop
hist(transectsWard_vax$cov_pupAdjust)



## Prep vaccination matrix
#--------------

## Remove data from rounds and districts where data is not properly resolved
vaxDogsMonthVill$District <- word(vaxDogsMonthVill$Correct_Village,1,sep="_")
vaxDogsMonthWard$District <- word(vaxDogsMonthWard$DW,1,sep="_")


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

## Match vax data to vills and wards
vaxDogsMonthVill$matchVill <- match(vaxDogsMonthVill$Correct_Village,STzVill$matchVill)
which(is.na(vaxDogsMonthVill$matchVill)) 
vaxDogsMonthVill$matchWard <- match(vaxDogsMonthVill$DW,STzVill$matchWard)
which(is.na(vaxDogsMonthVill$matchWard)) 
vaxDogsMonthWard$matchWard <- match(tolower(vaxDogsMonthWard$DW),tolower(STzWard$matchWard))
which(is.na(vaxDogsMonthWard$matchWard)) # all good!


## Create vaccination matrix for rural villages and add data
dogVaxVillMat <- matrix(0,nrow(dogPopVillMat),ncol=ncol(dogPopVillMat))
for(i in 1:nrow(dogVaxVillMat)){
  if(STzVill$Category[i]=="Rural"){
    vax_i <- vaxDogsMonthVill[which(vaxDogsMonthVill$matchVill==i),]
    if(nrow(vax_i)>0){
      dogVaxVillMat[i,vax_i$month] <- vax_i$Dogs
    }
  }
}

## Create vaccination matrix for urban wards and add data
dogVaxWardMat <- matrix(0,nrow(STzWard),ncol=ncol(dogPopVillMat))
UrbanWards <- unique(STzVill$matchWard[which(STzVill$Category=="Urban")])
for(i in 1:length(UrbanWards)){
   vax_i <- vaxDogsMonthWard[which(vaxDogsMonthWard$DW==UrbanWards[i]),]
  if(nrow(vax_i)>0){
    dogVaxWardMat[which(STzWard$matchWard==UrbanWards[i]),vax_i$month]<-vax_i$Dogs
  }
}

## dog population matrix for wards
dogPopWardMat <- matrix(0,nrow(STzWard),ncol=ncol(dogPopVillMat))
for(i in 1:nrow(dogPopWardMat)){
  dogPopWardMat[i, ] <- colSums(matrix(dogPopVillMat[which(tolower(STzVill$matchWard)==tolower(STzWard$matchWard[i])),],ncol=ncol(dogPopWardMat)))
}



## Create Population matrices from combo of HDR, transect and vaccination data
#--------------

# Vaccination units are taken to be villages in rural areas and wards in urban
# areas (i.e. in urban areas all the vaccination and transect data will be
# combined over the villages in a ward when estimating populations).  This is
# because not every village has a vaccination point in urban areas due to the
# relatively small size of urban villages

RuralVills <- which(STzVill$Category=="Rural")
UrbanVills <- which(STzVill$Category=="Urban")

# Only keep dog population estimates where we don't have transect data
dogPopVillMat[RuralVills[which(is.element(STzVill$matchVill[RuralVills],transectsVill_vax$DWV))],] <- 0
dogPopVillMat[UrbanVills[which(is.element(STzVill$matchWard[UrbanVills],transectsWard_vax$DW))],] <- 0
length(which(rowSums(dogPopVillMat)==0))/nrow(dogPopVillMat) # have transect info in 69% of cases

set.seed(0)
count_no_transects <- 0
count_units <- 0
for(i in 1:nrow(STzVill@data)){
  
  ## Find any transects relating to village i (at ward level if urban, village
  ## level if rural), and extract population estimates
  if(STzVill$Category[i]=="Rural"){
    transects_i <- transectsVill_vax[which(transectsVill_vax$DWV==STzVill$matchVill[i]),]
    dogPop_i <- dogPopVillMat[i,]; dogPop_i[transects_i$month_vax] <- transects_i$pop
    vills <- i

  }else if(STzVill$Category[i]=="Urban"){
    transects_i <- transectsWard_vax[which(transectsWard_vax$DW==STzVill$matchWard[i]),]
    vills <- which(STzVill$matchWard==STzVill$matchWard[i]) # Find other villages in the ward
    dogPop_i <- dogPopVillMat[i,]; dogPop_i[transects_i$month] <- transects_i$pop
    
  }
  
  ## If there were no transects in this village & it is rural or it is urban and
  ## the first village in the ward to appear
  if(sum(dogPopVillMat[i,])>0 & (STzVill$Category[i]=="Rural"|(STzVill$Category[i]=="Urban" & i==vills[1]))){
    
    count_no_transects<-count_no_transects+1
    count_units<-count_units+1
    
    ## Get coverage values
    if(STzVill$Category[i]=="Rural"){vax_i <- dogVaxVillMat[i,]
    }else if(STzVill$Category[i]=="Urban"){vax_i <- dogVaxWardMat[which(STzWard$matchWard==STzVill$matchWard[i]),]}
    vc_i <- vax_i/dogPop_i
    
    ## If coverages>maxVax
    if(length(which(vax_i>round(dogPop_i*maxVax)))!=0){
  
      ## What should the population be to get highest coverage <=maxVax
      pop_correct <- round(vax_i[which.max(vc_i)]/maxVax)
      HDR_correct <- sum(popVillMat[vills,which.max(vc_i)])/pop_correct
      
      if(STzVill$Category[i]=="Rural"){
        dogPopVillMat[i,] <- round(popVillMat[i,]/HDR_correct)
      
      }else{
        
        ## Divide up dog population between villages in ward in proportion to human population
        dogPop_proj_i <- round(colSums(popVillMat[vills,])/HDR_correct)
        for(j in 1:months){
          if(j==1){
            done<-0
            while(done==0){
              dogPopVillMat[vills,j] <- c(rmultinom(1, dogPop_proj_i[j], prob=popVillMat[vills,j]))
              if(length(which(dogPopVillMat[vills,j]==0))==0){done<-1}
            }
          }else{
            popChanges <- rep(0,length(vills))
            if(sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])==-1){
              popChangesTable <- table(sample(rep(1:length(vills),times=dogPopVillMat[vills,j-1]),abs(dogPop_proj_i[j]-dogPop_proj_i[j-1])))
              popChanges[as.numeric(names(popChangesTable))] <- as.numeric(popChangesTable)
              
            }else if(sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])==1){
              popChanges <- rmultinom(1,abs(dogPop_proj_i[j]-dogPop_proj_i[j-1]),prob=popVillMat[vills,j])
            }    
            dogPopVillMat[vills,j] <- dogPopVillMat[vills,j-1] + sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])*popChanges
          }
        }
        
        
      }
    }    
    
      
    
  ## If transect data is available
  }else if(sum(dogPopVillMat[i,])==0){
    
    count_units<-count_units+1
  
    
    ## Project forwards and backwards from known transect population estimates to fill in rows
    vc_i_done <- 0
    while(vc_i_done==0){
      
      ## For villages with just one population estimate available
      if(length(which(dogPop_i>0))==1){
        
        ## Get HDR from the known estimate
        month_i <- transects_i$month_vax
        humans_i <- sum(popVillMat[vills,month_i])
        HDR_i <- humans_i/dogPop_i[month_i]
        
        ## In empty months use HDR to convert humans to dogs
        dogPop_proj_i <- dogPop_i
        dogPop_proj_i[-month_i] <- round(colSums(matrix(popVillMat[vills,-month_i],ncol=months-1))/HDR_i)
        
        
        
      ## For villages with more than one population estimate available
      }else if(length(which(dogPop_i>0))>1){
        
        ## months where estimates available and estimates themselves
        month_i <- which(dogPop_i>0)
        dogs_i <- dogPop_i[month_i]
        
        ## calculate growth rates between estimates
        dogPop_proj_i <- dogPop_i
        for(j in 1:(length(month_i))){
          
          ## Project between estimates
          if(j!=(length(month_i))){
            growth_vills <- log(dogs_i[j+1]/dogs_i[j])/(month_i[j+1]-month_i[j])
            forward <- (month_i[j]+1):(month_i[j+1]-1)
            dogPop_proj_i[forward] <- round(dogs_i[j]*exp(growth_vills*c(1:length(forward))))
            
          }
          
          ## Project beyond estimates
          if(j==length(month_i)){
            
            forward <- (month_i[j]+1):months
            
            ## Get HDR from the last estimate
            humans_i <- colSums(matrix(popVillMat[vills,],nrow=length(vills)))
            HDR_ij <- humans_i[month_i[j]]/dogs_i[j]
            
            ## In empty months use HDR to convert humans to dogs
            dogPop_proj_i[forward] <- round(humans_i[forward]/HDR_ij)
            
          }
          
          ## Project back before first estimate
          if(j==1){
            backward <- (month_i[1]-1):1
            
            ## Get HDR from the first estimate
            humans_i <- colSums(matrix(popVillMat[vills,],nrow=length(vills)))
            HDR_ij <- humans_i[month_i[1]]/dogs_i[1]
            
            ## In empty months use HDR to convert humans to dogs
            dogPop_proj_i[backward] <- round(humans_i[backward]/HDR_ij)
          }
        }
        
      }  
      
      
      ## Check once vax data incorporated won't get coverages >maxVax
      if(STzVill$Category[i]=="Rural"){vax_i <- dogVaxVillMat[i,]
      }else if(STzVill$Category[i]=="Urban"){vax_i <- dogVaxWardMat[which(STzWard$matchWard==STzVill$matchWard[i]),]}
      vc_i <- vax_i/dogPop_proj_i
      if(length(which(vax_i>round(dogPop_proj_i*maxVax)))==0){
        
        ##If we don't, break out of the while loop
        vc_i_done<-1
      
      ## If we do, add a new known population point based on the highest coverage month and stay in the loop 
      }else{
        dogPop_i[which.max(vc_i)] <- round(vax_i[which.max(vc_i)]/maxVax) # new estimate of population size at month when vc is greatest
      }
      
    }
    
    
    ## If we're looking at a rural village...   
    if(STzVill$Category[i]=="Rural"){
      dogPopVillMat[i,] <- dogPop_proj_i 
      
    
    ## If we're looking at an urban ward...   
    }else if(STzVill$Category[i]=="Urban"){
      
      ## Divide up dog population between villages in ward in proportion to human population
      for(j in 1:months){
        if(j==1){
          done<-0
          while(done==0){
            dogPopVillMat[vills,j] <- c(rmultinom(1, dogPop_proj_i[j], prob=popVillMat[vills,j]))
            if(length(which(dogPopVillMat[vills,j]==0))==0){done<-1}
          }
        }else{
          popChanges <- rep(0,length(vills))
          if(sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])==-1){
            popChangesTable <- table(sample(rep(1:length(vills),times=dogPopVillMat[vills,j-1]),abs(dogPop_proj_i[j]-dogPop_proj_i[j-1])))
            popChanges[as.numeric(names(popChangesTable))] <- as.numeric(popChangesTable)
            
          }else if(sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])==1){
            popChanges <- rmultinom(1,abs(dogPop_proj_i[j]-dogPop_proj_i[j-1]),prob=popVillMat[vills,j])
          }    
          dogPopVillMat[vills,j] <- dogPopVillMat[vills,j-1] + sign(dogPop_proj_i[j]-dogPop_proj_i[j-1])*popChanges
        }
      }
      
    }
  }
}


## What prop of vaccination units do we have transect info for?
(length(unique(RuralVills[which(is.element(STzVill$matchVill[RuralVills],transectsVill_vax$DWV))])) +
    length(unique(STzVill$matchWard[UrbanVills[which(is.element(STzVill$matchWard[UrbanVills],transectsWard_vax$DW))]])))/
  (length(RuralVills) + length(unique(STzVill$matchWard[UrbanVills])))


## Use village matrix to distribute dogs at the cell level for the first month
popGrid1<-STzGrid
popGrid1[which(popGrid1[]==0)]<-NA
popGrid1[which(!is.na(popGrid1[]))]<-popMat[,1]
dogPopMat[]<-0
set.seed(0)
dogPopMat[,1]<- PopMap(STzUTM, STzVill, probMap=popGrid1, villPops=dogPopVillMat[,1],
                       missedVill=missedVill, missedVillCells=missedVillCells,villageIDs = STzUTM$VillageID)

## Fill in for rest of months
diff <- t(diff(t(dogPopVillMat)))
for(i in 2:months){
  popGrid<-STzGrid
  popGrid[which(popGrid[]==0)]<-NA
  popGrid[which(!is.na(popGrid[]))]<-popMat[,i]
  dogPopMat[,i] <- PopMap(STzUTM, STzVill, init=dogPopMat[,i-1], probMap=popGrid, villPops=diff[,i-1],
                          missedVill=missedVill, missedVillCells=missedVillCells,villageIDs = STzUTM$VillageID)
}


## Plot
colours <- colorRampPalette(c(brewer.pal(8,"YlOrRd")[1:8]))(100)
par(mfrow=c(1,1),mar=c(0,0,0,6))
popGrid<-STzGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-dogPopMat[,census_month]
plot(STzDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(STzDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=bquote("Dogs/"~.(cell_size^2) * "km"^2), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.75,0.76, .25,.75))



## Save dog population matrices 
write.table(dogPopMat,paste("Output/STzdogPopMat_CellByMonth_",cell_size^2,"kmsq_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")
write.table(dogPopVillMat,paste("Output/STzdogPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")

## Also save version of village-level dog population matrix with attached village/ward names for sharing
STzVill$matchDWV <- gsub("'|\"|`|/|,", "",paste(STzVill$District_N,tolower(STzVill$Ward_Name),gsub(" ","",tolower(STzVill$Vil_Mtaa_N)),sep="_"))
write.table(cbind(STzVill$matchDWV,dogPopVillMat),paste("Output/STzdogPopMat_VillageByMonth_withNames_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")




