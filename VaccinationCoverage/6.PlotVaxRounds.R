
rm(list=ls())

library(maptools)
library(raster)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(scales)

options(stringsAsFactors=F) 

if(!dir.exists("Figures")){dir.create("Figures")}



## Load data of interest
#-----------------

## District shapefile
LMDist <- readOGR(paste("data/GIS/LM_Dist_NBS2012",sep=""), paste("LM_Dist_NBS2012",sep=""))

## Village shapefile
LMVill <- readOGR(paste("data/GIS/LM_Vill_NBS2012",sep=""), paste("LM_Vill_NBS2012",sep=""))

## Protected Areas
crs37S <- CRS("+proj=utm +zone=37 +south +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
PAs <- readShapePoly(paste("data/GIS/ProtectedAreas/TZprotected_areas.shp",sep=""), proj4string = crs37S) 
PAs <- gIntersection(PAs,LMDist)

## Study area grid
LMGrid <- raster(paste("Output/GIS/LMGrid4kmsq.grd",sep=""))

## Information about cells
cellData <- read.csv("Output/LMCellData_4kmsq.csv",stringsAsFactors = F)

## Vaccination coverage by village by round
vcMatRound <- as.matrix(read.csv(paste("Output/vcMat_Vill_Round_2010-01-01_to_2020-12-31.csv",sep=""),row.names = 1,header=F))




##  Plot vaccination coverage achieved in Lindi/Mtwara in each round
#-----------------


## Set up colours
breaks=seq(0,1,length.out=100)
colours=colorRampPalette(c("white",brewer.pal(8,"YlGn")[2:8],"black"))(length(breaks)-1)

## Open figure
tiff(filename = paste("Figures/VaccRoundsLindiMtwara.tiff",sep=""),
     width = 100, height = 180, units = "mm", pointsize = 12,
     res=400)

par(mfrow=c(3,2),mar=c(0,0,0,0))

## Run through each round
for(i in 1:ncol(vcMatRound)){
  
  ##  Plot vaccination map
  plot(LMVill,col=colours[findInterval(as.vector(as.character(vcMatRound[,i])),breaks,all.inside=T)],border=F)
  plot(PAs,col="grey70",add=T,border=F)
  plot(LMDist,add=T)
  text(510000,9130000,paste("(",letters[i],") Round ",i,sep=""),font=1,cex=1.1)
  
}

## Plot colour scale bar
plot.new()
plot(LMGrid, breaks=seq(0,1,length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text="Vaccination Coverage", side=4, line=3.8, cex=0.9),
     axis.args=list(at=seq(0,1,0.2)),
     smallplot=c(0.45,0.48, .25,.75))

## Close figure
dev.off()



