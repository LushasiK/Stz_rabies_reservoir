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
library(lme4)
library(lubridate)

options(stringsAsFactors=F) 


cell_size <- 2 #km



## Read in data 
#--------------

## Cell grid & data
cell_data <- read.csv(paste("Output/LMCellData_",cell_size^2,"kmsq.csv",sep=""),sep=",")
cellGrid <- raster(paste("Output/GIS/LMCellGrid",cell_size^2,"kmsq.grd",sep=""))
cellGrid[which(cellGrid[]==0)]<-NA

## Village shapefile
LMVill <- readOGR(paste("data/GIS/LM_Vill_NBS2012",sep=""), paste("LM_Vill_NBS2012",sep=""))

## Ward shapefile
LMWard <- readOGR(paste("data/GIS/LM_Ward_NBS2012",sep=""), paste("LM_Ward_NBS2012",sep=""))
LMWard$DW <- paste(LMWard$District_N,LMWard$matchVill,sep="_")

## District shapefile
LMDist <- readOGR(paste("data/GIS/LM_Dist_NBS2012",sep=""), paste("LM_Dist_NBS2012",sep=""))

## Study area grid
LMGrid <- raster(paste("Output/GIS/LMGrid",cell_size^2,"kmsq.grd",sep=""))

## District dog population estimates for calculating human:dog ratios
HDR <- read.csv("data/EstimatedDogs2015.csv") # estimates are from Sambo et al. 2018, "Estimating the Size of Dog Populations in Tanzania to Inform Rabies Control", Veterinary Sciences, doi:10.3390/vetsci5030077
HDR$District[which(HDR$District=="masasi urban")]<-"masasi township authority"
HDR$District[which(HDR$District=="morogoro rural")]<-"morogoro"
HDR$District[which(HDR$District=="kibaha rural")]<-"kibaha"
HDR <- HDR[match(tolower(unique(LMVill$District_N)),HDR$District),]

##Transect data matched to vax data
transectsVill_vax <- read.csv("Output/transects_vax_village.csv") #village-level
transectsWard_vax <- read.csv("Output/transects_vax_ward.csv") #ward-level

## Vaccination data (has had the aggregated data removed)
vaxDogsMonthVill <- read.csv("Output/vaxDogsMonthVillage.csv")
vaxDogsMonthWard <- read.csv("Output/vaxDogsMonthWard.csv")

## maximum vaccination coverage
## Assuming that pups are not vaccinated, and relative pup/adult numbers are as recorded from the Serengeti district dog census (described again in Sambo et al. 2018)
PAR <- 12850/50155 #pup/adult ratio
propPups <- 12850/(12850+50155) #proportion of dogs that are pups
maxVax <- 1-propPups

## minimum sightings of dogs required for us not to throw out transect as
## unreliable for estimating coverage
minN <- 5

## Time period of interest
years <- 2010:2020
months <- 12*length(years)
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2020-12-31")

## Human population growth rate
growth_rates <- read.csv("data/human_pop_growth.csv")

## human population by cell/village and month
popMat <- as.matrix(read.csv(paste("Output/HumanPopMat_CellByMonth_",startDate,"_to_",endDate,".csv",sep=""),header=F))
popVillMat <- as.matrix(read.csv(paste("Output/HumanPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""),header=F))

## census happened in August 2012
census_month <- 12*2 + 8 

## Function for distributing individuals in cells for each village 
source("R/popMap.R")

## Villages missed by the grid and cellIDs they should be assigned
missedVill <- read.csv(paste("Output/VillagesMissedByGrid_",cell_size,"kmRes.csv",sep=""))
missedVillCells <- missedVill[,2]
missedVill <- missedVill[,1]






## Calculate HDRs and obtain dog population from human
#--------------

## Human:dog ratios for each district
humanDistPop12 <- rowsum(popVillMat[,census_month],(LMVill$District_N))
humanDistPop15 <- as.numeric(humanDistPop12)*(1+growth_rates$growth_rate[match(rownames(humanDistPop12),growth_rates$District)]/100)^3
HDR$HDR <- humanDistPop15[match(HDR$District,tolower(rownames(humanDistPop12)))]/HDR$est_dogs15
HDR_cells <- HDR$HDR[match(tolower(cell_data$District),HDR$District)] # for each cell
HDR_villages <- HDR$HDR[match(tolower(LMVill$District_N),HDR$District)] # for each village

## Dog Village Population matrix based on district-level HDRs
dogPopVillMat <- round(popVillMat/HDR_villages)
# Using the same approach to get cell-level estimates (i.e. by dogPopMat <-
# round(popMat/HDR_cells)) doesn't work well, since in low-density areas dog
# numbers often come out at <0.5, leading to lots of rounding down to zero, and
# underestimation of total numbers.  Instead use the following approach to
# distribute village dogs.

## Use village matrix to distribute dogs at the cell level for the first month
popGrid1<-LMGrid
popGrid1[which(popGrid1[]==0)]<-NA
popGrid1[which(!is.na(popGrid1[]))]<-popMat[,1]
dogPopMat<-matrix(0,ncol=ncol(popMat),nrow=nrow(popMat))
set.seed(0)
dogPopMat[,1]<- PopMap(cell_data, cellGrid, LMVill, probMap=popGrid1, villPops=dogPopVillMat[,1],
                       missedVill=missedVill, missedVillCells=missedVillCells)

## Fill in for rest of months
diff <- t(diff(t(dogPopVillMat)))
for(i in 2:ncol(dogPopMat)){
  popGrid<-LMGrid
  popGrid[which(popGrid[]==0)]<-NA
  popGrid[which(!is.na(popGrid[]))]<-popMat[,i]
  dogPopMat[,i] <- PopMap(cell_data, cellGrid, LMVill, init=dogPopMat[,i-1], probMap=popGrid, villPops=diff[,i-1],
                          missedVill=missedVill, missedVillCells=missedVillCells)
}

sum(dogPopMat[,months])
sum(dogPopVillMat[,months]) 

## Plot
# tiff(filename = "Figures/Dog&HumanMap_districtHDR.tiff",
#      width = 300, height = 100, units = "mm", pointsize = 12,
#      res=150)

par(mfrow=c(1,2),mar=c(0,0,0,6))
colours <- colorRampPalette(c(brewer.pal(8,"YlOrRd")[1:8]))(100)
popGrid<-LMGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-popMat[,census_month]
plot(LMDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Humans/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.75,0.76, .25,.75))

#par(mfrow=c(1,1),mar=c(0,0,0,6))
popGrid<-LMGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-dogPopMat[,census_month]
plot(LMDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Dogs/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.75,0.76, .25,.75))


# dev.off()





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
length(which(transectsVill_vax$cov==1))/nrow(transectsVill_vax) ## 20.6% of coverages are 100%...
par(mfrow=c(1,1))
par(mar=c(5,4,2,2))
hist(transectsVill_vax$cov,main="")
totalDogs <- transectsVill_vax$Dogs.with.collars+transectsVill_vax$Dogs.without.collars
hist(totalDogs,breaks="fd")
hist(totalDogs[which(transectsVill_vax$cov==1)])
length(which(totalDogs<5))/length(totalDogs) # 25.7% of transects involved <5 dogs
length(which(totalDogs>10))/length(totalDogs) # only 32.5% of transects counted more than 10 dogs

## Throw out transects with less than the minimum number of dogs (minN)
transectsVill_vax <- transectsVill_vax[which(totalDogs>=minN),]
length(which(transectsVill_vax$cov==1))/nrow(transectsVill_vax) # now 10.2% of coverages ==1

## Estimate population size from transect data
transectsVill_vax$pop <- round((transectsVill_vax$Dogs_vax/transectsVill_vax$cov)*(1+PAR)) # accounting for pups too with pup:adult ratio

## Get coverage that accounts for pups
transectsVill_vax$cov_pupAdjust <- transectsVill_vax$Dogs_vax/transectsVill_vax$pop
par(mfrow=c(1,1))
par(mar=c(5,4,2,2))
hist(transectsVill_vax$cov_pupAdjust,main="")
max(transectsVill_vax$cov_pupAdjust)
length(which(transectsVill_vax$cov_pupAdjust>maxVax)) # some slightly >maxVax - just a result of rounding the estimated pop above



## Ward-level
#------

## estimate vaccination coverage at the ward level
transectsWard_vax <- transectsWard_vax[which(!is.na(transectsWard_vax$Dogs_vax) & transectsWard_vax$Dogs.with.collars>0),]
transectsWard_vax$cov <- transectsWard_vax$Dogs.with.collars/(transectsWard_vax$Dogs.with.collars+transectsWard_vax$Dogs.without.collars)
hist(transectsWard_vax$cov,breaks="fd")
length(which(transectsWard_vax$cov==1))/nrow(transectsWard_vax) #6.5% of transects 100% coverage
totalDogs <- transectsWard_vax$Dogs.with.collars+transectsWard_vax$Dogs.without.collars
hist(totalDogs,breaks="fd")
hist(totalDogs[which(transectsWard_vax$cov==1)])
length(which(totalDogs<5))/length(totalDogs) # 6.1% of transects involved <5 dogs
length(which(totalDogs>10))/length(totalDogs) # 77.2% of transects counted more than 10 dogs
## considerably better than for villages...

## Throw out transects with less than the minimum number of dogs (minN)
transectsWard_vax <- transectsWard_vax[which(totalDogs>=minN),]
length(which(transectsWard_vax$cov==1))/nrow(transectsWard_vax) # now 4.3% of coverages ==1
hist(transectsWard_vax$cov,breaks="fd")

## Get population size from transect data
transectsWard_vax$pop <- round((transectsWard_vax$Dogs_vax/transectsWard_vax$cov)*(1+PAR)) # accounting for pups too with pup:adult ratio
hist(transectsWard_vax$pop,breaks="fd")

## Get coverage that accounts for pups
transectsWard_vax$cov_pupAdjust <- transectsWard_vax$Dogs_vax/transectsWard_vax$pop
hist(transectsWard_vax$cov_pupAdjust)



## Prep vaccination matrix
#--------------

## shapefile names to match to vax and transect data
LMVill$matchVill<-paste(
  tolower(gsub("[^[:alpha:]]", "", LMVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", LMVill$Ward_Name)),
  tolower(gsub("[^[:alpha:]]", "", LMVill$Vil_Mtaa_N)),
  sep = "_")
LMVill$matchWard<-paste(
  tolower(gsub("[^[:alpha:]]", "", LMVill$District_N)),
  tolower(gsub("[^[:alpha:]]", "", LMVill$Ward_Name)),
  sep = "_")
LMWard$matchWard<-paste(
  tolower(gsub("[^[:alpha:]]", "", LMWard$District_N)),
  tolower(gsub("[^[:alpha:]]", "", LMWard$matchVill)),
  sep = "_")

## Match vax data to vills and wards
vaxDogsMonthVill$matchVill <- match(vaxDogsMonthVill$Correct_Village,LMVill$matchVill)
which(is.na(vaxDogsMonthVill$matchVill)) 
vaxDogsMonthVill$matchWard <- match(vaxDogsMonthVill$DW,LMVill$matchWard)
which(is.na(vaxDogsMonthVill$matchWard)) 
vaxDogsMonthWard$matchWard <- match(tolower(vaxDogsMonthWard$DW),tolower(LMWard$matchWard))
which(is.na(vaxDogsMonthWard$matchWard)) # all good!


## Create vaccination matrix for rural villages and add data
dogVaxVillMat <- matrix(0,nrow(dogPopVillMat),ncol=ncol(dogPopVillMat))
for(i in 1:nrow(dogVaxVillMat)){
  if(LMVill$Category[i]=="Rural"){
    vax_i <- vaxDogsMonthVill[which(vaxDogsMonthVill$matchVill==i),]
    if(nrow(vax_i)>0){
      dogVaxVillMat[i,vax_i$month] <- vax_i$Dogs
    }
  }
}

## Create vaccination matrix for urban wards and add data
dogVaxWardMat <- matrix(0,nrow(LMWard),ncol=ncol(dogPopVillMat))
UrbanWards <- unique(LMVill$matchWard[which(LMVill$Category=="Urban")])
for(i in 1:length(UrbanWards)){
   vax_i <- vaxDogsMonthWard[which(vaxDogsMonthWard$DW==UrbanWards[i]),]
  if(nrow(vax_i)>0){
    dogVaxWardMat[which(LMWard$matchWard==UrbanWards[i]),vax_i$month]<-vax_i$Dogs
  }
}

## dog population matrix for wards
dogPopWardMat <- matrix(0,nrow(LMWard),ncol=ncol(dogPopVillMat))
for(i in 1:nrow(dogPopWardMat)){
  dogPopWardMat[i, ] <- colSums(matrix(dogPopVillMat[which(tolower(LMVill$matchWard)==tolower(LMWard$matchWard[i])),],ncol=ncol(dogPopWardMat)))
}



## Create Population matrices from combo of HDR, transect and vaccination data
#--------------

# Vaccination units are taken to be villages in rural areas and wards in urban
# areas (i.e. in urban areas all the vaccination and transect data will be
# combined over the villages in a ward when estimating populations).  This is
# because not every village has a vaccination point in urban areas due to the
# relatively small size of urban villages

RuralVills <- which(LMVill$Category=="Rural")
UrbanVills <- which(LMVill$Category=="Urban")

# Only keep dog population estimates where we don't have transect data
dogPopVillMat[RuralVills[which(is.element(LMVill$matchVill[RuralVills],transectsVill_vax$DWV))],] <- 0
dogPopVillMat[UrbanVills[which(is.element(LMVill$matchWard[UrbanVills],transectsWard_vax$DW))],] <- 0
length(which(rowSums(dogPopVillMat)==0))/nrow(dogPopVillMat) # have transect info in 64% of cases

set.seed(0)
count_no_transects <- 0
count_units <- 0
for(i in 1:nrow(LMVill@data)){
  
  ## Find any transects relating to village i (at ward level if urban, village
  ## level if rural), and extract population estimates
  if(LMVill$Category[i]=="Rural"){
    transects_i <- transectsVill_vax[which(transectsVill_vax$DWV==LMVill$matchVill[i]),]
    dogPop_i <- dogPopVillMat[i,]; dogPop_i[transects_i$month_vax] <- transects_i$pop
    vills <- i

  }else if(LMVill$Category[i]=="Urban"){
    transects_i <- transectsWard_vax[which(transectsWard_vax$DW==LMVill$matchWard[i]),]
    vills <- which(LMVill$matchWard==LMVill$matchWard[i]) # Find other villages in the ward
    dogPop_i <- dogPopVillMat[i,]; dogPop_i[transects_i$month] <- transects_i$pop
    
  }
  
  ## If there were no transects in this village & it is rural or it is urban and
  ## the first village in the ward to appear
  if(sum(dogPopVillMat[i,])>0 & (LMVill$Category[i]=="Rural"|(LMVill$Category[i]=="Urban" & i==vills[1]))){
    
    count_no_transects<-count_no_transects+1
    count_units<-count_units+1
    
    ## Get coverage values
    if(LMVill$Category[i]=="Rural"){vax_i <- dogVaxVillMat[i,]
    }else if(LMVill$Category[i]=="Urban"){vax_i <- dogVaxWardMat[which(LMWard$matchWard==LMVill$matchWard[i]),]}
    vc_i <- vax_i/dogPop_i
    
    ## If coverages>maxVax
    if(length(which(vax_i>round(dogPop_i*maxVax)))!=0){
  
      ## What should the population be to get highest coverage <=maxVax
      pop_correct <- round(vax_i[which.max(vc_i)]/maxVax)
      HDR_correct <- sum(popVillMat[vills,which.max(vc_i)])/pop_correct
      
      if(LMVill$Category[i]=="Rural"){
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
      if(LMVill$Category[i]=="Rural"){vax_i <- dogVaxVillMat[i,]
      }else if(LMVill$Category[i]=="Urban"){vax_i <- dogVaxWardMat[which(LMWard$matchWard==LMVill$matchWard[i]),]}
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
    if(LMVill$Category[i]=="Rural"){
      dogPopVillMat[i,] <- dogPop_proj_i 
      
    
    ## If we're looking at an urban ward...   
    }else if(LMVill$Category[i]=="Urban"){
      
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




## Use village matrix to distribute dogs at the cell level for the first month
popGrid1<-LMGrid
popGrid1[which(popGrid1[]==0)]<-NA
popGrid1[which(!is.na(popGrid1[]))]<-popMat[,1]
dogPopMat[]<-0
set.seed(0)
dogPopMat[,1]<- PopMap(cell_data, cellGrid, LMVill, probMap=popGrid1, villPops=dogPopVillMat[,1],
                       missedVill=missedVill, missedVillCells=missedVillCells)

## Fill in for rest of months
diff <- t(diff(t(dogPopVillMat)))
for(i in 2:months){
  popGrid<-LMGrid
  popGrid[which(popGrid[]==0)]<-NA
  popGrid[which(!is.na(popGrid[]))]<-popMat[,i]
  dogPopMat[,i] <-  PopMap(cell_data, cellGrid, LMVill, init=dogPopMat[,i-1], probMap=popGrid, villPops=diff[,i-1],
                           missedVill=missedVill, missedVillCells=missedVillCells)
}





## Plot
# tiff(filename = "Figures/Dog&HumanMap_usingTransects.tiff",
#      width = 300, height = 100, units = "mm", pointsize = 12,
#      res=150)

par(mfrow=c(1,2),mar=c(0,0,0,6))
popGrid<-LMGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-popMat[,census_month]
plot(LMDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Humans/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.75,0.76, .25,.75))

#par(mfrow=c(1,1),mar=c(0,0,0,6))
popGrid<-LMGrid
popGrid[which(popGrid[]==0)]<-NA
popGrid[which(!is.na(popGrid[]))]<-dogPopMat[,census_month]
plot(LMDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Dogs/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.75,0.76, .25,.75))


# dev.off()


## Save dog population matrices
LMVill$matchDWV <- gsub("'|\"|`|/|,", "",paste(LMVill$District_N,tolower(LMVill$Ward_Name),gsub(" ","",tolower(LMVill$Vil_Mtaa_N)),sep="_"))
write.table(cbind(LMVill$matchDWV,dogPopVillMat),paste("Output/dogPopMat_VillageByMonth_withNames_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")
write.table(dogPopMat,paste("Output/dogPopMat_CellByMonth_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")



