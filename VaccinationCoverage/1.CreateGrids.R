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

## Projection
crs37S <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")

## Load in administrative shapefiles 
LMVill <- readOGR("data/GIS/LM_Vill_NBS2012","LM_Vill_NBS2012")
LMWard <- readOGR("data/GIS/LM_Ward_NBS2012","LM_Ward_NBS2012")
LMDist <- readOGR("data/GIS/LM_Dist_NBS2012","LM_Dist_NBS2012")

## Protected areas
PAs <- readShapePoly("data/GIS/ProtectedAreas/TZprotected_areas.shp", proj4string = crs37S) # IN CORRECT PROJECTION



## Create LM grids
#--------------

## Create grid
grid <- raster(extent(LMVill),crs=LMVill@proj4string)
res(grid) <- cell_size*1000

##Create LM Grid
LMVill$studyArea<-1
gridPoints <- SpatialPoints(rasterToPoints(grid), proj4string = LMVill@proj4string)
values <- over(gridPoints,LMVill)[,"studyArea"]; values[which(values==0)]<-1
LMGrid <- grid
LMGrid[] <- as.numeric(values)
plot(LMGrid)

## Create 2012 village grid
LMVill@data$VillID <- 1:nrow(LMVill@data)
values <- over(gridPoints,LMVill)$VillID
villGrid <- grid
villGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(LMGrid[]))
plot(villGrid)

## Create 2012 district grid
LMDist$DistID<-1:nrow(LMDist)
values <- over(gridPoints,LMDist)$DistID
distGrid <- grid
distGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(distGrid[]))
distGrid[cellsToFill]<-NA
plot(distGrid)

## Create 2012 ward grid
LMWard$WardID<-1:nrow(LMWard)
values <- over(gridPoints,LMWard)$WardID
wardGrid <- grid
wardGrid[] <- values
wardGrid[cellsToFill]<-NA
plot(wardGrid)

##Protected areas grid
PAs@proj4string <- LMVill@proj4string
PAs$layer<-1
values <- over(gridPoints,PAs)$layer
PAGrid <- grid
PAGrid[] <- values
PAGrid[which(is.na(LMGrid[])&!is.na(PAGrid[]))] <- NA
PAGrid[which(is.na(PAGrid[])&!is.na(LMGrid[]))] <- 0
plot(PAGrid)


##Save grids
if(!dir.exists("Output/GIS/")){dir.create("Output/GIS/")}
writeRaster(LMGrid,file=paste("Output/GIS/LMGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(distGrid,file=paste("Output/GIS/distGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(villGrid,file=paste("Output/GIS/villGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(wardGrid,file=paste("Output/GIS/wardGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
writeRaster(PAGrid,file=paste("Output/GIS/PAGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)

## Convert LMGrid to polygons and crop to land 
LMUTM <- rasterToPolygons(LMGrid)
LMUTM <- LMUTM[which(LMUTM@data$layer==1),]

## Give ID to each cell
LMUTM$cellID<-1:nrow(LMUTM@data)
cellGrid<-LMGrid
cellGrid[]<-over(gridPoints,LMUTM)$cellID
cellGrid[which(is.na(cellGrid@data@values))]<-0
plot(cellGrid)

## Add additional information on cells to gridded polygon
LMUTM$District <- LMDist$District_N[(distGrid[which(!is.na(distGrid@data@values))])]
LMUTM$DistrictID <- distGrid[which(!is.na(distGrid@data@values))]
LMUTM$Ward <- LMWard$matchVill[(wardGrid[which(!is.na(wardGrid@data@values))])]
LMUTM$WardID <- wardGrid[which(!is.na(wardGrid@data@values))]
LMUTM$Village <- LMVill$Vil_Mtaa_N[(villGrid[which(!is.na(villGrid@data@values))])]
LMUTM$VillageID <- villGrid[which(!is.na(villGrid@data@values))]
LMUTM$PA <- PAGrid[which(!is.na(PAGrid@data@values))]
LMUTM$Urban <- LMVill$Category[LMUTM$VillageID]; LMUTM$Urban <- ifelse(LMUTM$Urban=="Urban",1,0)


# Save cell numbers and associated info
write.table(as.matrix(cellGrid),paste("Output/LM_matrix_",cell_size^2,"kmsq_cellID.csv",sep=""),row.names=F,col.names=F,sep=",")
writeRaster(cellGrid,file=paste("Output/GIS/LMCellGrid",cell_size^2,"kmsq.grd",sep=""),overwrite=T)
write.table(LMUTM@data,paste("Output/LMCellData_",cell_size^2,"kmsq.csv",sep=""),row.names=F,sep=",")

