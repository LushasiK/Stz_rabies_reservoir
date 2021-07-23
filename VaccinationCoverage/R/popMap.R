#
## Function to distribute individuals to cells based on a probability map
#________________________________________


PopMap<- function(cell_data,cellGrid,vill_map,init=0,probMap,villPops,missedVill=NA,missedVillCells=NA){
  
  ## Set up column to hold initial population map
  cell_data$popMap <- init
  
  
  if(!is.null(missedVill)){
    if(is.na(missedVill)|is.na(missedVillCells)){
      
      ##Find villages that don't have an assigned cell
      missedVill <- which(!is.element(1:length(villPops),unique(cell_data$VillageID)))
      
      ##Find which grid cell each of these has the greatest degree of overlap with
      centroids <- coordinates(vill_map[missedVill,])
      missedVillCells <- extract(cellGrid,centroids)
      
      ## If some missed villages haven't got a cell as they're on the coast, assign these to the closest cell
      if(length(which(is.na(missedVillCells)))>0){
        for (i in which(is.na(missedVillCells))){
          missedVillCells[i] <- which.min(gDistance(SpatialPoints(centroids[i,,drop=F],proj4string=cellGrid@crs), 
                                                    SpatialPoints(coordinates(cellGrid)[which(!is.na(cellGrid[])),],proj4string=cellGrid@crs), byid=TRUE))
        } 
      }
      
    } 
  }
  
    
  ## for each village
  if(length(missedVill>0)){
    for(i in missedVill){
      cell_data$popMap[missedVillCells[which(missedVill==i)]] <- cell_data$popMap[missedVillCells[which(missedVill==i)]] + villPops[i]
    }
  }
  for(i in (1:nrow(vill_map@data))[which(!is.element(1:nrow(vill_map@data),missedVill))]){
    
    ## cells and individuals in village
    n_inds<-villPops[i]
    if(sign(n_inds)==-1){
      cells<-which(cell_data$VillageID==i & cell_data$popMap>0)
      probs<-rep(1,length(cells))
    }else{
      cells<-which(cell_data$VillageID==i)
      probs <- probMap[which(!is.na(probMap[]))[cells]]
      if(sum(probs)==0){probs<-probs+1}
    }
    
    popChanges <- rep(0,length(cells))
    if(sign(n_inds)==-1){
      popChangesTable <- table(sample(rep(1:length(cells),times=cell_data$popMap[cells]),abs(n_inds)))
      popChanges[as.numeric(names(popChangesTable))] <- as.numeric(popChangesTable)
      
    }else if(sign(n_inds)==1){
      popChanges <- rmultinom(1,abs(n_inds),probs)
    }    
    
    
    ##distribute among available cells
    cell_data$popMap[cells] <- cell_data$popMap[cells] + sign(n_inds)*popChanges
    
  }
  
  
  
  return(cell_data$popMap)
  
}


