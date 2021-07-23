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

options(stringsAsFactors=F) 

cell_size <- 2 #km




## Read in data 
#--------------

## Village shapefile
LMVill <- readOGR(paste("data/GIS/LM_Vill_NBS2012",sep=""), paste("LM_Vill_NBS2012",sep=""))

## District shapefile
LMDist <- readOGR(paste("data/GIS/LM_Dist_NBS2012",sep=""), paste("LM_Dist_NBS2012",sep=""))

## NBS 2012 human population census
NBSpop <- read.csv("data/NBS2012_VillagePop_matched.csv")
NBSpop_unmatched <- read.csv("data/unmatchedShapefileVill.csv")
# 8 villages in the shapefile were not matchable to villages in the human census
# data.  These unmatched villages were formed by breaking off from other
# villages that are present in census data.  In analyses (see below), the
# population recorded for these progenitor villages is assumed to include the
# population of those villages plus the population of the village that broke
# off.  The progenitor village for each of the unmatched shapefile villages is
# recorded in NBSpop_unmatched


## Cell grid & data
cell_data <- read.csv(paste("Output/LMCellData_",cell_size^2,"kmsq.csv",sep=""),sep=",")
cellGrid <- raster(paste("Output/GIS/LMCellGrid",cell_size^2,"kmsq.grd",sep=""))
cellGrid[which(cellGrid[]==0)]<-NA

## Population rasters derived from WorldPop data
LMPop15 <- raster("data/GIS/Worldpop_reproject_2015.grd")
LMPop10 <- raster("data/GIS/Worldpop_reproject_2010.grd")
# These rasters were obtained by reprojecting data from WorldPop (see following
# citation) onto the Lindi/Mtwara spatial grid.  
# Citation: WorldPop (www.worldpop.org - School of Geography and Environmental
# Science, University of Southampton). 2013. United Republic of Tanzania 100m
# Population. Alpha version 2010 and 2015 estimates of numbers of people per
# grid square, with national totals adjusted to match UN population division
# estimates (http://esa.un.org/wpp/) and remaining unadjusted. DOI:
# 10.5258/SOTON/WP00288


## Human population growth rates by district (based on growth between censuses
## in 2002 and  2012)
growth_rates <- read.csv("data/human_pop_growth.csv")




## Remove population in protected areas
#--------------

## Set population in protected areas to 0 (even if human pop isn't quite 0, dog pop should be)
LMPop10[which(!is.na(LMPop10[]))][which(cell_data$PA==1)] <-0
LMPop15[which(!is.na(LMPop15[]))][which(cell_data$PA==1)] <-0


##Plot 
# tiff(filename = "Figures/worldPop_PAs.tiff",
#      width = 300, height = 100, units = "mm", pointsize = 12,
#      res=150)

par(mfrow=c(1,2))
colours <- colorRampPalette(c(brewer.pal(8,"YlOrRd")[1:8]))(100)

par(mar=c(0,6,0,0))
plot(LMDist)
plot(log10(LMPop10),add=T,col=colours,breaks=seq(0,max(log10(LMPop15[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
text(500000,9315869,"2010")

par(mar=c(0,0,0,6))
plot(LMDist)
plot(log10(LMPop15),add=T,col=colours,breaks=seq(0,max(log10(LMPop15[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(LMPop15), breaks=seq(0,max(log10(LMPop15[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Humans/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.80,0.81, .25,.75))
text(500000,9315869,"2015")

# dev.off()



## Exponential human pop growth rate in each grid cell based on Worldpop
#--------------

par(mar=c(4,4,1,3),mfrow=c(1,1))
LMPopGrowth <- (LMPop15/LMPop10)^(1/5)-1
LMPopGrowth[which(is.na(LMPopGrowth[])&!is.na(cellGrid[]))]<-0
plot(LMPopGrowth) 



##Project a population distribution for 2012 using Worldpop
#--------------

LMPop12 <- LMPop10*(1+LMPopGrowth)^2
plot(LMPop12)



## Match human census data to village shapefile
#--------------

## Match census data to shapefile
LMVill$matchDWV <- gsub("'|\"|`|/|,", "",paste(LMVill$District_N,tolower(LMVill$Ward_Name),gsub(" ","",tolower(LMVill$Vil_Mtaa_N))))
NBSpop$matchDWV <- gsub("'|\"|`|/|,", "",paste(NBSpop$matchDistrict,tolower(NBSpop$matchWard),gsub(" ","",tolower(NBSpop$matchVillage))))
matchVill <- match(NBSpop$matchDWV,LMVill$matchDWV)
NBSpop$matchDWV[which(is.na(matchVill))]  # no census villages still unmatched
LMVill$matchDWV[which(is.na(match(LMVill$matchDWV,NBSpop$matchDWV)))] # 8 unmatched

## Add matched population data to shapefile
LMVill$villPops<-0
LMVill$villPops[sort(unique(matchVill))] <- rowsum(NBSpop$Population,matchVill)


##Estimate the number of people in unmatched villages using the worldPop data
##and the population count for the village from which the unmatched village
##split
NBSpop_unmatched$DWV<-gsub("'|\"|`|/|,", "",paste(NBSpop_unmatched$District_N,tolower(NBSpop_unmatched$Ward_Name),gsub(" ","",tolower(NBSpop_unmatched$Vil_Mtaa_N))))
NBSpop_unmatched$DWVmatch<-gsub("'|\"|`|/|,", "",paste(NBSpop_unmatched$District_N,tolower(NBSpop_unmatched$Ward_Name),gsub(" ","",tolower(NBSpop_unmatched$progenitor_vill))))
for(i in 1:nrow(NBSpop_unmatched)){
  
  ## Find cells containing both the unmatched village and the village it split from
  vill_a<-which(LMVill$matchDWV==NBSpop_unmatched$DWV[i]) # unmatched village
  vill_b<-which(LMVill$matchDWV==NBSpop_unmatched$DWVmatch[i]) # village it split from
  cells_a<-which(cell_data$VillageID==vill_a) # which cells are assigned to village a
  cells_b<-which(cell_data$VillageID==vill_b) 
  if(length(cells_a)==0){ # if no cells (village too small), find the cell containing the village centroid
    cells_a <- extract(cellGrid,coordinates(LMVill[vill_a,]))}
  if(length(cells_b)==0){
    cells_b <- extract(cellGrid,coordinates(LMVill[vill_b,]))}
  
  ## mean density in cells containing the village
  mean_dens_a <- mean(LMPop12[which(!is.na(LMPop12[]))[cells_a]])  
  mean_dens_b <- mean(LMPop12[which(!is.na(LMPop12[]))[cells_b]])
  
  ## Proportion of people to go to each village based on the difference in WorldPop densities
  if(mean_dens_a==0|mean_dens_b==0){ # doesn't make a lot of sense, so just split based on village area
    prop_a <- LMVill$Area_kmsq[vill_a]/(LMVill$Area_kmsq[vill_b] + LMVill$Area_kmsq[vill_a])
  }else{
    prop_a <- (LMVill$Area_kmsq[vill_a]*mean_dens_a)/(LMVill$Area_kmsq[vill_b]*mean_dens_b + LMVill$Area_kmsq[vill_a]*mean_dens_a)
  }
  
  ## People going to each village
  LMVill$villPops[vill_a] <- round(prop_a*LMVill$villPops[vill_b])
  LMVill$villPops[vill_b] <- LMVill$villPops[vill_b] - LMVill$villPops[vill_a]
}



## Get human population in each cell in 2012
##--------------

##Find villages that don't have an assigned cell (can happen for small villages)
missedVill <- which(!is.element(1:nrow(LMVill),unique(cell_data$VillageID)))

##Find which grid cell each of these has the greatest degree of overlap with
centroids <- coordinates(LMVill[missedVill,])
missedVillCells <- extract(cellGrid,centroids)

## If some missed villages still haven't got a cell as they're on the coast,
## assign these to the cell closest to their centroid
if(length(which(is.na(missedVillCells)))>0){
  for (i in which(is.na(missedVillCells))){
    missedVillCells[i] <- which.min(gDistance(SpatialPoints(centroids[i,,drop=F],proj4string=cellGrid@crs), 
                                              SpatialPoints(coordinates(cellGrid)[which(!is.na(cellGrid[])),],proj4string=cellGrid@crs), byid=TRUE))
  } 
}

## Save for later
write.table(cbind(missedVill,missedVillCells),paste("Output/VillagesMissedByGrid_",cell_size,"kmRes.csv",sep=""),row.names = F,col.names = c("villID","cellID"),sep=",")

## Add humans to each cell
source("R/popMap.R")
set.seed(0)
cell_data$popMap<- PopMap(cell_data, cellGrid, LMVill, init=0, probMap=LMPop12, villPops=LMVill$villPops,
                       missedVill=missedVill, missedVillCells=missedVillCells)

## Plot
# tiff(filename = "Figures/HumanMap.tiff",
#      width = 150, height = 100, units = "mm", pointsize = 12,
#      res=150)

par(mfrow=c(1,1),mar=c(0,0,0,6))
popGrid<-cellGrid
popGrid[which(!is.na(popGrid[]))]<-cell_data$popMap
plot(LMDist)
plot(log10(popGrid),add=T,col=colours,breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),legend=F)
plot(LMDist,add=T)
plot(log10(popGrid), breaks=seq(0,max(log10(popGrid[]),na.rm=T),length.out=100),
     legend.only=T, add=T,col=colours,
     legend.args=list(text=expression(paste("Humans/4km"^2)), side=4, font=2, line=3.8, cex=1.2),
     axis.args=list(at=c(log10(c(1,10,100,1000,10000,100000))),labels=c("1","10","100",expression("1x10"^3),expression("1x10"^4),expression("1x10"^5))),cex.axis=0.8,
     smallplot=c(0.80,0.81, .25,.75))

# dev.off()

sum(popGrid[],na.rm=T)
sum(NBSpop$Population,na.rm=T) 



## Use district-level human population growth rates to project human population forwards and back  
##--------------

## Time period of interest
years <- 2010:2020
months <- 12*length(years)
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2020-12-31")

## Set up population matrices (both cell and village)
popMat <- matrix(nrow=nrow(cell_data),ncol=months)
popVillMat <- matrix(nrow=nrow(LMVill),ncol=months)
census_month <- 12*2 + 8 # census happened in August 2012
popMat[,census_month] <- cell_data$popMap
popVillMat[,census_month] <- LMVill$villPops

## exponential anuual growth rate for each cell/village
growth_cells <- growth_rates$growth_rate[match(cell_data$District,growth_rates$District)]/100
growth_villages <- growth_rates$growth_rate[match(LMVill$District_N,growth_rates$District)]/100

## Project forward
forward <- (census_month+1):months
popMat[,forward] <- rep(popMat[,census_month],length(forward))* 
  (1+rep(growth_cells,length(forward)))^rep((1:length(forward))/12,each=nrow(cell_data))
popVillMat[,forward] <- rep(popVillMat[,census_month],length(forward))* 
  (1+rep(growth_villages,length(forward)))^rep((1:length(forward))/12,each=nrow(LMVill))

## Project backwards
backward <- (census_month-1):1
popMat[,backward] <- rep(popMat[,census_month],length(backward))* 
  (1+rep(growth_cells,length(backward)))^rep(-(1:length(backward))/12,each=nrow(cell_data))
popVillMat[,backward] <- rep(popVillMat[,census_month],length(backward))* 
  (1+rep(growth_villages,length(backward)))^rep(-(1:length(backward))/12,each=nrow(LMVill))

## Round to the nearest human
popMat <- round(popMat)
popVillMat <- round(popVillMat)

## Save human population matrices 
write.table(popMat,paste("Output/HumanPopMat_CellByMonth_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")
write.table(popVillMat,paste("Output/HumanPopMat_VillageByMonth_",startDate,"_to_",endDate,".csv",sep=""),row.names=F,col.names=F,sep=",")

