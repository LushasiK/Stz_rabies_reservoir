#
## Get population in each village/cell in each month
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

## Gridded shapefile
NgoroUTM <- readOGR("data/gis/Ngoro_gridded4kmsq", "Ngoro_gridded4kmsq")
plot(NgoroUTM)

## Village shapefile
TzVill <- readOGR("data/gis/Tanzania_villages", "TZ_Village_2012_pop")
Ngorovill <- subset(TzVill, TzVill@data$District_N == "Ngorongoro")
plot(Ngorovill)

## District shapefile
TanzDist <- readOGR("data/gis/Tanzania_Districts", "Districts")
NgoroDist <- subset(TanzDist, TanzDist@data$District_N == "Ngorongoro")
plot(NgoroDist)


## NBS 2012 human population census
NBSpop <- read.csv("data/NBS2012_MasterVillagePop_shpmatched.csv")
Ngoro_pop <-subset(NBSpop, NBSpop$shpDistrict == "Ngorongoro")
duplicated(Ngoro_pop$shpVillage)

## bring in the Ngororaster
NgoroGrid <- raster("data/gis/NgoroCellGrid.grd")
plot(NgoroGrid)

## Bring i the Worldpop data which is also in a raster. 
WorldPop12 <- raster("data/tza_ppp_2012.tif")
sum(WorldPop12[], na.rm = T)

## Prepare WorldPop data
#--------------
WorldPop12[] <- WorldPop12[]*(4/area(WorldPop12)[])
sum(WorldPop12[], na.rm = T)
# make cells the same size as in the grid we created. 

# 
NgoroPop12 <- projectRaster(WorldPop12, NgoroGrid) # project the values of a Raster* object to a new Raster* object
#with another projection (crs), from WorldPop to NgoroGrid
sum(NgoroPop12[], na.rm = T)
NgoroPop12[which(is.na(NgoroGrid[]))] <- NA
NgoroPop12[which(is.na(NgoroPop12[]) & !is.na(NgoroGrid[]))] <- 0 

sum(NgoroPop12[], na.rm = T)
sum(Ngoro_pop$Population) 

## Set population in protected areas to areas 

NgoroPop12[which(!is.na(NgoroPop12[]))[which(NgoroUTM$PA==1)]] <- 0

sum(WorldPop12[], na.rm = T)
sum(NgoroPop12[], na.rm = T)

## Match human census data to village shapefile
#--------------
## make a ward village metric which is the same in both
Ngorovill$matchDWV <- gsub("'|\"|`|/|,", "",paste(Ngorovill$District_N,tolower(Ngorovill$Ward_Name),gsub(" ","",tolower(Ngorovill$Vil_Mtaa_N))))
Ngoro_pop$matchDWV <- gsub("'|\"|`|/|,", "",paste(Ngoro_pop$shpDistrict,tolower(Ngoro_pop$shpWard),gsub(" ","",tolower(Ngoro_pop$shpVillage))))
#duplicated(Ngorovill$matchDWV)
#duplicated(Ngoro_pop$matchDWV)
# No duplicates in these data

matchVill <- match(Ngoro_pop$matchDWV,Ngorovill$matchDWV)
Ngoro_pop$matchDWV[which(is.na(matchVill))]  
Ngorovill$matchDWV[which(is.na(match(Ngorovill$matchDWV,Ngoro_pop$matchDWV)))] 

# None that don't match - super!

Ngorovill$villPops<-0
Ngorovill$villPops[sort(unique(matchVill))] <- rowsum(Ngoro_pop$Population,matchVill) # sum up the
# population of the 40 villages and add them to the village population

## Get human population in each cell in 2012
##--------------

##Find villages that don't have an assigned cell

missedVill <- which(!is.element(1:nrow(Ngorovill),unique(NgoroUTM$VillageID))) # NONE! :) 

## Add humans to each cell
source("R/popMap_Ngoro.R")
villPops<-Ngorovill$villPops
set.seed(0)

NgoroUTM$popMap<- PopMap(NgoroUTM, Ngorovill, init=0, probMap=NgoroPop12, villPops=villPops,
                         missedVill=missedVill, missedVillCells=missedVillCells,villageIDs=NgoroUTM$VillageID)


popGrid<-NgoroGrid
popGrid[which(popGrid[]==0)]<-NA  #create a copy of the Ngorogrid (grid with each cell numbered)
## and then assigning any which have a 0 value as an NA
popGrid[which(!is.na(popGrid[]))]<-NgoroUTM$popMap # transfer the population values from the cells
## in popmap to the cells of the raster
plot(popGrid)
plot(NgoroDist)


sum(popGrid[],na.rm=T)
sum(Ngoro_pop$Population,na.rm=T) 

# checking that the populations in the popGrid raster are the same as the population 
# in the Ngoro_pop data which are the census data

## Set up population matrices (both cell and village)
popMat <- matrix(nrow=nrow(NgoroUTM), ncol = 1 ) #ncol=monthsLong) #create a matrix with the 
# same number of rows as cells
popVillMat <- matrix(nrow=nrow(Ngorovill),ncol= 1 )# monthsLong)
# matrix same number of rows as villages. 

popMat <- NgoroUTM$popMap # All the values from the cells
popVillMat <- villPops 
popMat

## Round to the nearest human
popMat <- round(popMat)
popVillMat <- round(popVillMat)
sum(popMat[])
#sum(popMat[,months])


## Save human population matrices for simulation 

#write.table(popMat,"output/NgoroHumanPopMat_Cell.csv",row.names=F,col.names=F,sep=",")

length(popMat)*4 # This correlates with the area of Ngorongoro according to google. 
length(popMat[which(popMat!= 0)])*4
# 

