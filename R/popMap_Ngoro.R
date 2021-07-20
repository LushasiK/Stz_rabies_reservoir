#
## Function to distribute individuals to cells based on a probability map
#________________________________________


PopMap<- function(NgoroUTM,Ngorovill,init=0,probMap,villPops,missedVill=NA,missedVillCells=NA,villageIDs){
  
  ## Set up column to hold initial population map
  NgoroUTM$popMap <- init  # sets initial population to 0 as defined above
  
  ## for each village
  for(i in 1:nrow(Ngorovill@data)){
    # first part is if in missed_vill set to 0 - but we don't have any missedvill so skin to line 20
  if(is.element(i,missedVill)){
      NgoroUTM$popMap[missedVillCells[which(missedVill==i)]] <- NgoroUTM$popMap[missedVillCells[which(missedVill==i)]] + villPops[i]

    }else {
   
      ## cells and individuals in village
      n_inds<-villPops[i]  ## n_inds is the population for the village i 
      if(sign(n_inds)==-1){   # sign assigns -1 to negative values, 0 to zero and +1 to positive values
        ##  we don't have any negative values so the 2 lines below won't do anything
        ## so skip to line 29
       cells<-which(villageIDs==i & NgoroUTM$popMap>0)  ## I think this is saying that 'cells' are the
       # cells that match the current village [i] and have a pop of >0
       probs<-rep(1,length(cells))
      
       }else{  
        cells<-which(villageIDs==i) ## VillageIDs are defined as the village in NgoroUTM$VillageID - 
        # which has 3882 lines and each of these has one of the village IDS 1-40
        # cells is the vector containing the row numbers of all the rows that match the villageID. 
        probs <- probMap[which(!is.na(probMap[]))[cells]]  # probmap is NgoroPop12. Has the population
        ## from world pop by 4km2 area, 
        ## Extracts the values that match those locations  produced in 'cells'. 
        ## So this is the population extracted from the raster cells
        if(sum(probs)==0){probs<-probs+1}
      }
      
      popChanges <- rep(0,length(cells))
      ## again this first bit isn't used as the signs are all >0 so jump to line 45
      if(sign(n_inds)==-1){
        popChangesTable <- table(sample(rep(1:length(cells),times=NgoroUTM$popMap[cells]),abs(n_inds)))
        popChanges[as.numeric(names(popChangesTable))] <- as.numeric(popChangesTable)
      ## so it's this bit that will get used.   
      }else if(sign(n_inds)==1){
        popChanges <- rmultinom(1,abs(n_inds),probs) # uses multinomial distribution to 
        ## produce a number - 
      }    
      
      
      ##distribute among available cells
      NgoroUTM$popMap[cells] <- NgoroUTM$popMap[cells] + sign(n_inds)*popChanges
      
    }
    
  }
  
  return(NgoroUTM$popMap)
  
}


