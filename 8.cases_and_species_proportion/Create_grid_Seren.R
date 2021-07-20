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


## Read in data 
#--------------
## Projections
crs37S <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
crs36S <- CRS("+proj=utm +zone=36 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
crsLL <- CRS("+proj=longlat")

## Shapefiles 
AllTzVill <- readOGR("8.cases_and_species_proportion/GIS/Tanzania_villages", "TZ_Village_2012_pop") ## Administrative areas
Seren_Vill <- subset(AllTzVill, AllTzVill@data$District_N == "Serengeti")
TanzDist <- readOGR("8.cases_and_species_proportion/GIS/Tanzania_Districts", "Districts")
SerenDist <- subset(TanzDist, TanzDist@data$District_N == "Serengeti")
PAs <- readShapePoly("8.cases_and_species_proportion/GIS/Protected_areas/TZprotected_areas.shp", proj4string = crs37S) ## Protected areas
SerenDist <- spTransform(SerenDist, crs37S)
Seren_Vill <- spTransform(Seren_Vill, crs37S)
area(SerenDist)/1000000 # 11157 km2

crs(PAs)
crs(SerenDist)
crs(Seren_Vill)

plot(SerenDist)
plot(PAs, add = T)

## Create grids
cell_size <- 2 # km  (MOVED THIS HERE SO NOT FORGOTTEN!!!)
Serengrid <- raster(extent(SerenDist), crs=SerenDist@proj4string)
res(Serengrid) <- cell_size*1000 # these are 2km gridcells!! (cell_size is defined above!)

## Create Seren Grid
SerenDist$studyArea <-1 # Make Serengeti district shapefile have variable "studyarea" equal to 1
SerengridPoints <- SpatialPoints(rasterToPoints(Serengrid), proj4string = SerenDist@proj4string)
Serenvalues <- over(SerengridPoints, SerenDist)[,"studyArea"] # Make Serengeti district cells on grid = 1
Serenvalues[which(Serenvalues==0)] <- 1 
SerenGrid <- Serengrid # rename
SerenGrid[] <- as.numeric(Serenvalues)

## Create 2012 village grid
Seren_Vill@data$VillID <- 1:nrow(Seren_Vill@data) # give each unique village a value (village ID)
values <- over(SerengridPoints,Seren_Vill)$VillID
villGrid <- Serengrid
villGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(SerenGrid[])) # 

## Create 2012 district grid - 
SerenDist$DistID <-1:nrow(SerenDist)
values <- over(SerengridPoints,SerenDist)$DistID # assigns which cells are in the district
distGrid <- Serengrid
distGrid[] <- values
cellsToFill <- which(is.na(villGrid[])&!is.na(distGrid[]))
distGrid[cellsToFill] <- NA

## Protected areas grid
PAs@proj4string <- Seren_Vill@proj4string
PAs$layer<-1
values <- over(SerengridPoints,PAs)$layer
PAGrid <- Serengrid
PAGrid[] <- values
PAGrid[which(is.na(SerenGrid[])&!is.na(PAGrid[]))] <- NA
PAGrid[which(is.na(PAGrid[])&!is.na(SerenGrid[]))] <- 0

PAs_Seren <- PAs 
PAs_Seren@proj4string <- SerenDist@proj4string
PAs_Seren$layer<-1
values <- over(SerengridPoints,PAs_Seren)$layer
PAGrid_Seren <- Serengrid
PAGrid_Seren[] <- values
PAGrid_Seren[which(is.na(SerenGrid[])&!is.na(PAGrid_Seren[]))] <- NA
PAGrid_Seren[which(is.na(PAGrid_Seren[])&!is.na(SerenGrid[]))] <- 0

# CHECK GRIDS:
par(mfrow=c(2,3))
plot(SerenGrid)
plot(villGrid)
plot(distGrid)
plot(PAGrid)
plot(PAGrid_Seren)

## Save grids - CUT OUT THE UNNECESSARY CODE FOR PAs and dist grid? 
#writeRaster(SerenGrid, file = "8.cases_and_species_proportion/GIS/SerenGrid4kmsq", overwrite = T)
#writeRaster(distGrid, file=paste("8.cases_and_species_proportion/GIS/SerendistGrid4kmsq.grd"), overwrite=T)
#writeRaster(villGrid, file = "8.cases_and_species_proportion/GIS/Seren4kmsqGridVillGrid", overwrite = T)

## Convert STzGrid to polygons and crop to land 
SerenUTM <- rasterToPolygons(SerenGrid)
SerenUTM <- SerenUTM[which(SerenUTM@data$layer==1), ]

# 
SerenUTM$cellID <- 1:nrow(SerenUTM@data)
SerencellGrid <- SerenGrid
SerencellGrid[] <- over(SerengridPoints,SerenUTM)$cellID
SerencellGrid[which(is.na(SerencellGrid@data@values))] <- 0

# Check:
par(mfrow=c(1,2))
plot(SerenUTM)
plot(SerencellGrid)

## Add additional information on cells to gridded polygon
SerenUTM$District <- SerenDist$District_N[(distGrid[which(!is.na(distGrid@data@values))])] # District name
SerenUTM$DistrictID <- distGrid[which(!is.na(distGrid@data@values))] # District 1/ 0
SerenUTM$Village <- Seren_Vill$Vil_Mtaa_N[(villGrid[which(!is.na(villGrid@data@values))])] # Village name
SerenUTM$VillageID <- villGrid[which(!is.na(villGrid@data@values))] # Village number
SerenUTM$PA <- PAGrid[which(!is.na(PAGrid@data@values))] # Protected area 1/0

# Save cellGrid
write.table(as.matrix(SerencellGrid), paste("output/Seren_matrix_4kmsq_cellID.csv"), row.names=F, col.names=F, sep=",")
writeRaster(SerencellGrid,file=paste("8.cases_and_species_proportion/GIS/Seren_CellGrid"), overwrite=T)
writeOGR(SerenUTM, dsn=paste("8.cases_and_species_proportion/GIS/Seren_gridded4kmsq"), ("Seren_gridded4kmsq"), driver="ESRI Shapefile", overwrite_layer=T, check_exists=T)
write.table(SerenUTM@data, paste("output/Seren_CellData4kmsq.csv", sep=""), row.names=F, sep=",")

