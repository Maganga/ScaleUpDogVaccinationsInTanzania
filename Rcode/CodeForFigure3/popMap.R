#
## Function to distribute individuals to cells based on a probability map
#________________________________________


PopMap<- function(STzUTM,STzVill,init=0,probMap,villPops,missedVill=NA,missedVillCells=NA,villageIDs){
  
  ## Set up column to hold initial population map
  STzUTM$popMap <- init
  
  if(!is.null(missedVill)){
    if(is.na(missedVill)|is.na(missedVillCells)){
      
      ##Find villages that don't have an assigned cell
      missedVill <- which(!is.element(1:length(villPops),unique(STzUTM$villageIDs)))
      
      ##Find which grid cell each of these has the greatest degree of overlap with
      cell_grid<-probMap
      cell_grid[which(!is.na(cell_grid[]))]<-1:length(which(!is.na(cell_grid[])))
      centroids <- coordinates(STzVill[missedVill,])
      missedVillCells <- raster::extract(cell_grid,centroids)
      
      ## If some missed villages haven't got a cell as they're on the coast, assign these to the closest cell
      if(length(which(is.na(missedVillCells)))>0){
        for (i in which(is.na(missedVillCells))){
          missedVillCells[i] <- 
            apply(X = centroids[i,], MARGIN = 1, 
                  FUN = function(xy) cell_grid[which.min(replace(distanceFromPoints(cell_grid, xy), is.na(probMap), NA))])
        } 
      }
      
    } 
  }
  
    
  ## for each village
  for(i in 1:nrow(STzVill@data)){
    
    if(is.element(i,missedVill)){
      STzUTM$popMap[missedVillCells[which(missedVill==i)]] <- STzUTM$popMap[missedVillCells[which(missedVill==i)]] + villPops[i]
      
    }else {
      
      ## cells and individuals in village
      n_inds<-villPops[i]
      if(sign(n_inds)==-1){
        cells<-which(villageIDs==i & STzUTM$popMap>0)
        probs<-rep(1,length(cells))
      }else{
        cells<-which(villageIDs==i)
        probs <- probMap[which(!is.na(probMap[]))[cells]]
        if(sum(probs)==0){probs<-probs+1}
      }
      
      popChanges <- rep(0,length(cells))
      if(sign(n_inds)==-1){
        popChangesTable <- table(sample(rep(1:length(cells),times=STzUTM$popMap[cells]),abs(n_inds)))
        popChanges[as.numeric(names(popChangesTable))] <- as.numeric(popChangesTable)
        
      }else if(sign(n_inds)==1){
        popChanges <- rmultinom(1,abs(n_inds),probs)
      }    
      
      
      ##distribute among available cells
      STzUTM$popMap[cells] <- STzUTM$popMap[cells] + sign(n_inds)*popChanges
      
    }
    
  }
  
  return(STzUTM$popMap)
  
}


