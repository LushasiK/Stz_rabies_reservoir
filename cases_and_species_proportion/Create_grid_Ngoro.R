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
AllTzVill <- readOGR("data/gis/Tanzania_villages", "TZ_Village_2012_pop")
Ngoro_Vill <- subset(AllTzVill, AllTzVill@data$District_N == "Ngorongoro")
plot(Ngoro_Vill)

TanzDist <- readOGR("data/gis/Tanzania_Districts", "Districts")
plot(TanzDist)
NgoroDist <- subset(TanzDist, TanzDist@data$District_N == "Ngorongoro")
plot(NgoroDist)

area(NgoroDist)/1000000

## Protected areas

PAs <- readShapePoly("data/gis/Protected_areas/TZprotected_areas.shp", proj4string = crs37S)
PAs <- spTransform(PAs, CRS(" +proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0
                            +units=m +no_defs"))

NgoroDist <- spTransform(NgoroDist, CRS(" +proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0
                                        +units=m +no_defs"))
Ngoro_Vill <- spTransform(Ngoro_Vill, CRS(" +proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0
                                          +units=m +no_defs"))

crs(PAs)
crs(NgoroDist)
crs(Ngoro_Vill)

plot(NgoroDist)
plot(PAs, add = T)

## Create grids
#--------------

## Create grid
Ngorogrid <- raster(extent(NgoroDist),crs=NgoroDist@proj4string)
#Ngorogrid <- raster(extent(Ngoro_Vill), crs=NgoroDist@proj4string)
res(Ngorogrid) <- cell_size*1000


##Create Ngoro Grid
NgoroDist$studyArea<-1  ## add a new column to the data entitled study area

## raster to point conversion - creates matrix of coordinates and then SpatialPoints creetes a spatial points object.
NgorogridPoints <- SpatialPoints(rasterToPoints(Ngorogrid), proj4string = NgoroDist@proj4string)
Ngorovalues <- over(NgorogridPoints,NgoroDist)[,"studyArea"] # spatial overlay - so for this will retrieve
## the indexes of attributes from NgoroDist at the spatial locations of Ngorogridpoints. So we get value of 1
## for those that are in the study area and NA otherwise
Ngorovalues[which(Ngorovalues==0)]<-1 ## Also make any 0 values a 1. 
NgoroGrid <- Ngorogrid 
NgoroGrid[] <- as.numeric(Ngorovalues)
plot(NgoroGrid) ## so now we have a raster which is just the study area. 
plot(NgoroDist,add = T)

## Create 2012 village 
# Ngorovill is the shapefile of Ngoro with the smaller divisions outlined.
plot(Ngoro_Vill) # quick reminder what this is. 
Ngoro_Vill@data$VillID <- 1:nrow(Ngoro_Vill@data) ## adds an ID number for each village
values <- over(NgorogridPoints,Ngoro_Vill)$VillID  # retrieves the indexes of attrbutes from NgoroVill
# (in this case the Village ID numer) at the spatial locations in NgorogridPoints
villGrid <- Ngorogrid  ## make a raster same size/shape as the District one generated above
villGrid[] <- values  # add the values for the village ID as assigned in lines 85- 87
cellsToFill <- which(is.na(villGrid[])&!is.na(NgoroGrid[])) 
plot(villGrid) ## plot the village grid coloured by the assigned village IDs

# ## Create 2012 district grid
NgoroDist$DistID<-1:nrow(NgoroDist) ## assign a new column to the data of the district grid with the district ID
values <- over(NgorogridPoints,NgoroDist)$DistID
distGrid <- Ngorogrid
distGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(distGrid[]))
distGrid[cellsToFill]<-NA ## cells that don't have a value in the villGrid and DistGrid are assigned NA 
plot(distGrid)


## Protected areas grid
PAs@proj4string <- Ngoro_Vill@proj4string
PAs$layer<-1
values <- over(NgorogridPoints,PAs)$layer
PAGrid <- Ngorogrid
PAGrid[] <- values
PAGrid[which(is.na(NgoroGrid[])&!is.na(PAGrid[]))] <- NA
PAGrid[which(is.na(PAGrid[])&!is.na(NgoroGrid[]))] <- 0

PAs_Ngoro <- PAs 
PAs_Ngoro@proj4string <- NgoroDist@proj4string
PAs_Ngoro$layer<-1
values <- over(NgorogridPoints,PAs_Ngoro)$layer
PAGrid_Ngoro <- Ngorogrid
PAGrid_Ngoro[] <- values
PAGrid_Ngoro[which(is.na(NgoroGrid[])&!is.na(PAGrid_Ngoro[]))] <- NA
PAGrid_Ngoro[which(is.na(PAGrid_Ngoro[])&!is.na(NgoroGrid[]))] <- 0

# CHECK GRIDS:
par(mfrow=c(2,3))
plot(NgoroGrid)
plot(villGrid)
plot(distGrid)
plot(PAGrid)
plot(PAGrid_Ngoro)

##Save grids
#writeRaster(NgoroGrid, file = "data/gis/NgoroGrid4kmsq", overwrite = T)
#writeRaster(distGrid,file=paste("data/gis/NgorodistGrid4kmsq.grd"),overwrite=T)
#writeRaster(villGrid, file = "data/gis/Ngoro4kmsqGridVillGrid", overwrite = T)

## Convert Grid to polygons and crop to land 

NgoroUTM <- rasterToPolygons(NgoroGrid) 
NgoroUTM <- NgoroUTM[which(NgoroUTM@data$layer==1),]
## converting the gridded raster to a polygon and just keeping the study area

plot(NgoroUTM)

## Give ID to each cell

NgoroUTM$cellID<-1:nrow(NgoroUTM@data) # Assign an ID to every cell in the gridded polygon
NgorocellGrid<-NgoroGrid
NgorocellGrid[]<-over(NgorogridPoints,NgoroUTM)$cellID
# converts back to a raster but every cell now has a unique ID. 

length(NgorocellGrid@data@values[which(!is.na(NgorocellGrid@data@values))])

#NgorocellGrid[which(is.na(NgorocellGrid@data@values))]<-0
plot(NgorocellGrid)
plot(NgoroGrid)

## Add additional information on cells to gridded polygon
NgoroUTM$District <-  "Ngorongoro" #NgoroDist$District_N[(distGrid[which(!is.na(distGrid@data@values))])]
NgoroUTM$DistrictID <- 1 # distGrid[which(!is.na(distGrid@data@values))]

## Assigning the village ID to the UTM grid. Assigned based on the cellgrid - only those that are not NA


NgoroUTM$Village <- Ngoro_Vill$Vil_Mtaa_N[(villGrid[which(!is.na(NgorocellGrid@data@values))])]
NgoroUTM$VillageID <- villGrid[which(!is.na(NgorocellGrid@data@values))]


NgoroUTM$PA <- PAGrid[which(!is.na(PAGrid@data@values))]

plot(NgoroUTM)

# Save cellGrid and STzUTM 
#write.table(as.matrix(NgorocellGrid),paste("output/Ngoro_matrix_4kmsq_cellID.csv"),row.names=F,col.names=F,sep=",")
#writeRaster(NgorocellGrid,file=paste("data/gis/Ngoro_CellGrid"),overwrite=T)
#writeOGR(NgoroUTM, dsn=paste("data/gis/Ngoro_gridded4kmsq"), ("Ngoro_gridded4kmsq"), driver="ESRI Shapefile", overwrite_layer=T, check_exists=T)
#write.table(NgoroUTM@data,paste("output/Ngoro_CellData4kmsq.csv",sep=""),row.names=F,sep=",")

