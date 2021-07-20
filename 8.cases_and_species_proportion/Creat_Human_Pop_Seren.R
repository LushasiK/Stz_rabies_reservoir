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
library(dplyr)

options(stringsAsFactors=F) 

cell_size <- 2 #km

## Read in data 
#--------------

## Gridded shapefile
SerenUTM <- readOGR("8.cases_and_species_proportion/GIS/Seren_gridded4kmsq", "Seren_gridded4kmsq")
plot(SerenUTM)

## Village shapefile
TzVill <- readOGR("8.cases_and_species_proportion/GIS/Tanzania_villages", "TZ_Village_2012_pop")
Serenvill <- subset(TzVill, TzVill@data$District_N == "Serengeti")
plot(Serenvill)

## District shapefile
TanzDist <- readOGR("8.cases_and_species_proportion/GIS/Tanzania_Districts", "Districts")
SerenDist <- subset(TanzDist, TanzDist@data$District_N == "Serengeti")
plot(SerenDist)


## NBS 2012 human population census
NBSpop <- read.csv("data/NBS2012_MasterVillagePop_shpmatched.csv")
Seren_pop <-subset(NBSpop, NBSpop$shpDistrict == "Serengeti")

SerenGrid <- raster("8.cases_and_species_proportion/GIS/SerenCellGrid.grd")
plot(SerenGrid)

WorldPop12 <- raster("data/tza_ppp_2012.tif")
sum(WorldPop12[], na.rm = T)

## Prepare WorldPop data
#--------------
WorldPop12[] <- WorldPop12[]*(4/area(WorldPop12)[])
sum(WorldPop12[], na.rm = T)

# 
SerenPop12 <- projectRaster(WorldPop12, SerenGrid)
sum(SerenPop12[], na.rm = T)
SerenPop12[which(is.na(SerenGrid[]))] <- NA
SerenPop12[which(is.na(SerenPop12[]) & !is.na(SerenGrid[]))] <- 0 
plot(SerenPop12)

# Population according to our calculations with worldpop
sum(SerenPop12[], na.rm = T)
## Calculation according to census
sum(Seren_pop$Population) 

## Set population in protected areas to 0 (even if human pop isn't quite 0, dog pop should be)

SerenPop12[which(!is.na(SerenPop12[]))[which(SerenUTM$PA==1)]] <- 0

#sum(WorldPop12[], na.rm = T)
sum(SerenPop12[], na.rm = T)


## Match human census data to village shapefile
#--------------
Serenvill$matchDWV <- gsub("'|\"|`|/|,", "",paste(Serenvill$District_N,tolower(Serenvill$Ward_Name),gsub(" ","",tolower(Serenvill$Vil_Mtaa_N))))
Seren_pop$matchDWV <- gsub("'|\"|`|/|,", "",paste(Seren_pop$shpDistrict,tolower(Seren_pop$shpWard),gsub(" ","",tolower(Seren_pop$shpVillage))))
# check for duplicates
duplicated(Serenvill$matchDWV) # Two duplicated in here
duplicated(Seren_pop$matchDWV)

## Look at duplicates. 
Dup_villages <- Serenvill@data[which(duplicated(Serenvill$matchDWV)),"matchDWV"]
dup_vills  <- Serenvill[which(Serenvill$matchDWV %in% Dup_villages),]
plot(dup_vills)

# The shapefile believes these are two distinct areas but they have the same name. 

## Combine the shapefiles but keep the data the same
SV2 <- raster::aggregate(Serenvill, by = c("Loc_ID", "Region_Nam", "District_N", "Ward_Name", "Vil_Mtaa_N",
                                     "Loc_type",   "pop_2012", "matchDWV"))

plot(SV2)
## separate out the duplicated villages and see if they are combined
dv2  <- SV2[which(SV2$matchDWV %in% Dup_villages),]
plot(dv2)
summary(SV2)

dv2@data

# Now have checked it, convert back to previous name 

Serenvill <- SV2

matchVill <- match(Seren_pop$matchDWV,Serenvill$matchDWV)
Seren_pop$matchDWV[which(is.na(matchVill))]  # 1 census villages still unmatched
Serenvill$matchDWV[which(is.na(match(Serenvill$matchDWV,Seren_pop$matchDWV)))] # 2 unmatched

# Make a new column in the shapefile data called villpops 
# and fill with values from the seren_pop (census) data 
Serenvill$villPops<-0
Serenvill$villPops[sort(unique(matchVill))] <- rowsum(Seren_pop$Population,matchVill)

# natta natta and rungabure machochwe still have 0 values for villpops as not matched by census data

## Two small villages where we don't have an estimate for the population
plot(Serenvill)
plot(subset(Serenvill, Serenvill@data$matchDWV == "Serengeti rungabure machochwe"), 
     col = "red", add = T)
plot(subset(Serenvill, Serenvill@data$matchDWV == "Serengeti natta natta"), 
     col = "green", add = T)


# Use the raster to extract the population from world pop for these regions 
# 
# Missing_polys <- subset(Serenvill, Serenvill@data$matchDWV == "Serengeti rungabure machochwe" |
#                           Serenvill@data$matchDWV == "Serengeti natta natta")  
# plot(Missing_polys)
# 
# Missing_polys_pop <- raster::extract(WorldPop12, Missing_polys)

## Quick check to see that they all match up OK.

## Need the unaltered worldpop data
WorldPop12_orig <- raster("Data/tza_ppp_2012.tif")

WP_vills <- raster::extract(WorldPop12_orig, Serenvill)
Vill_tots_WP <- sapply(WP_vills, FUN = sum, na.rm = TRUE)
head(Vill_tots_WP)
Serenvill@data$World_pop_12 <- round(Vill_tots_WP)

sum(Vill_tots_WP)
# sum(Vill_tots_WP_2)


Serenvill@data$villPops[is.na(Serenvill@data$pop_2012)] <- 
  Serenvill@data$World_pop_12[is.na(Serenvill@data$pop_2012)]


## Get human population in each cell in 2012
##--------------

## Find villages that don't have an assigned cell

missedVill <- which(!is.element(1:nrow(Serenvill),unique(SerenUTM$VillageID)))

crs(SerenUTM)
crs(Serenvill) ## these are different projections. For this next part to work they need to be the same

SVcopy <- Serenvill
SVcopy <- spTransform(SVcopy, "+proj=utm +zone=37 +south +ellps=clrk80 +units=m +no_defs")
crs(SVcopy)
centroids <- SpatialPoints(coordinates(SVcopy[missedVill,]),proj4string=SerenUTM@proj4string)
head(centroids@coords)
head(SerenDist@bbox)
##Find which grid cell each of these has the greatest degree of overlap with
#centroids <- SpatialPoints(coordinates(Serenvill[missedVill,]),proj4string=SerenUTM@proj4string)
#head(centroids@coords)
plot(SerenDist)
plot(centroids, add = T)
#plot(Serenvill, add = T)

missedVillCells <- over(centroids,SerenUTM)$cellID


## Add humans to each cell
source("R/popMap_Seren.R")
villPops<-Serenvill$villPops ## if using villpops column
#villPops <- Serenvill$pop_2012  ## if using pop2012 colums.
set.seed(0)
SerenUTM$popMap<- PopMap(SerenUTM, Serenvill, init=0, probMap=SerenPop12, villPops=villPops,
                      missedVill=missedVill, missedVillCells=missedVillCells,villageIDs=SerenUTM$VillageID)


popGrid<-SerenGrid
plot(popGrid)
popGrid[which(popGrid[]==0)]<-NA  ## This removes the space around the district
plot(popGrid) # Can see this when compare the 2 maps. 
popGrid[which(!is.na(popGrid[]))]<-SerenUTM$popMap # assigns the population to this grid.
plot(popGrid)
#plot(SerenDist)



sum(popGrid[],na.rm=T)  ## compare the people on our raster to the people from the census
sum(Seren_pop$Population,na.rm=T)  # match pretty well

## Set up population matrices (both cell and village)
popMat <- matrix(nrow=nrow(SerenUTM), ncol = 1 ) #ncol=monthsLong)
popVillMat <- matrix(nrow=nrow(Serenvill),ncol= 1 )# monthsLong)

popMat <- SerenUTM$popMap
popVillMat <- villPops
popMat

## Round to the nearest human
popMat <- round(popMat)
popVillMat <- round(popVillMat)
sum(popMat[])
#sum(popMat[,months])


## Save human population matrices for simulation 

#write.table(popMat,"output/SerenHumanPopMat_Cell.csv",row.names=F,col.names=F,sep=",")
#write.table(popVillMat,"output/SerenHumanPopMat_VillageByMonth.csv",row.names=F,col.names=F,sep=",")

length(popMat)*4 # This correlates with the area of according to google. 
# Multiply by 4 as each grid is 4km2. So length popMap is the number of squares and each square is 4km"
length(popMat[which(popMat!= 0)])*4  ### And this is the inhabited area. 

