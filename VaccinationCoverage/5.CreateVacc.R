#
## Get dog vaccination coverage in each ward/cell in each month
# paired with CreatePop5.R
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
startDate <- as.Date("2010-01-01")
endDate <- as.Date("2020-12-31")

## maximum vaccination coverage
## Assuming that pups are not vaccinated, and relative pup/adult numbers are as recorded from the Serengeti district dog census (described again in Sambo et al. 2018)
PAR <- 12850/50155 #pup/adult ratio
propPups <- 12850/(12850+50155) #proportion of dogs that are pups
maxVax <- 1-propPups

set.seed(0)


## Read in data 
#--------------

## Cell data
cell_data <- read.csv(paste("Output/LMCellData_",cell_size^2,"kmsq.csv",sep=""),sep=",")

## Village shapefile
LMVill <- readOGR(paste("data/GIS/LM_Vill_NBS2012",sep=""), paste("LM_Vill_NBS2012",sep=""))

## Ward shapefile
LMWard <- readOGR(paste("data/GIS/LM_Ward_NBS2012",sep=""), paste("LM_Ward_NBS2012",sep=""))
LMWard$DW <- paste(LMWard$District_N,LMWard$matchVill,sep="_")

## District shapefile
LMDist <- readOGR(paste("data/GIS/LM_Dist_NBS2012",sep=""), paste("LM_Dist_NBS2012",sep=""))

## Dog populations in villages by month
dogPopVillMat <- as.matrix(read.csv(paste("Output/dogPopMat_VillageByMonth_withNames_",startDate,"_to_",endDate,".csv",sep=""), header=F,row.names = 1))

## Vaccination data
vaxDogsMonthVill <- read.csv("Output/vaxDogsMonthVillage.csv")
vaxDogsMonthWard <- read.csv("Output/vaxDogsMonthWard.csv")
vaxDogsMonthVill$District <- word(vaxDogsMonthVill$Correct_Village,1,sep="_")
vaxDogsMonthWard$District <- word(vaxDogsMonthWard$DW,1,sep="_")
vax <- read.csv(paste("data/vaccinated_cleaned.csv",sep=""))
vax_agg <- vax[which(vax$Dogs>1000),] # single record for Masasi in 2011
vax_agg$dateVaccination <- as.Date(vax_agg$dateVaccination)

##Vaccination rounds (start and end year of each round in each district)
vacc_rounds <- read.csv("data/Vaccinationrounds.csv")
vacc_rounds <- vacc_rounds[which(vacc_rounds$Region%in%c("Lindi","Mtwara")),]




## Match vaccination data to village and ward shapefiles
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

vaxDogsMonthVill$matchVill <- match(vaxDogsMonthVill$Correct_Village,LMVill$matchVill)
which(is.na(vaxDogsMonthVill$matchVill)) 

vaxDogsMonthVill$matchWard <- match(vaxDogsMonthVill$DW,LMWard$matchWard)
which(is.na(vaxDogsMonthVill$matchWard)) 

vaxDogsMonthWard$matchWard <- match(vaxDogsMonthWard$DW,LMWard$matchWard)
which(is.na(vaxDogsMonthWard$matchWard)) # all good!



## Create matrix of dogs vaccinated in each village in each month
#--------------

## Create vaccination matrix and add data
dogVaxVillMat <- matrix(0,nrow(dogPopVillMat),ncol=ncol(dogPopVillMat))
for(i in 1:nrow(dogVaxVillMat)){
  if(LMVill$Category[i]=="Rural"){
    vax_i <- vaxDogsMonthVill[which(vaxDogsMonthVill$matchVill==i),]
    if(nrow(vax_i)>0){
      dogVaxVillMat[i,vax_i$month] <- vax_i$Dogs
    }
  }
}
UrbanWards <- unique(LMVill$matchWard[which(LMVill$Category=="Urban")])
for(i in 1:length(UrbanWards)){
  
  ## find vaccination events and villages in ward
  vax_i <- vaxDogsMonthWard[which(vaxDogsMonthWard$DW==UrbanWards[i]),]
  if(nrow(vax_i)>0){
    vills <- which(LMVill$matchWard==UrbanWards[i])
    
    ## For each vaccination event...
    for(j in 1:nrow(vax_i)){
      
      ##...Select dogs to vaccinate from each village
      Dogs_ij <- vax_i$Dogs[j]
      dogs_ij <- rep(0,length(vills))
      dogs_ij_table <- table(sample(rep(1:length(vills),times=dogPopVillMat[vills,vax_i$month[j]]),Dogs_ij))
      dogs_ij[as.numeric(names(dogs_ij_table))] <- as.numeric(dogs_ij_table)
      
      ## Add to matrix
      dogVaxVillMat[vills,vax_i$month[j]]<-dogs_ij
      
    }
  }
}
## For cases where we don't have village/ward-level resolution, distribute dogs between villages
## use rounds 4 & 5 to draw vaccination weights that give realistic levels of within district heterogeneity (since no aggregation happened in these rounds)
## note the villages in an urban ward only count as one vaccination unit
vcVillRound <- matrix(0,nrow=nrow(LMVill),ncol=5)
for(i in 1:ncol(vcVillRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    vcVillRound[which(LMVill$District_N==vacc_rounds$District[j]),i]<-rowSums((dogVaxVillMat/dogPopVillMat)[which(LMVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-year(startDate))])
  }
}
vax_draw <- c(c(vcVillRound[which(LMVill$Category=="Rural"),4:5]),c(vcVillRound[match(unique(LMVill$matchWard[which(LMVill$Category=="Urban")]),LMVill$matchWard),4:5]))
for(i in 1:nrow(vax_agg)){

  ## Villages in the district that we don't already have vaccination info for this year
  year <- year(vax_agg$dateVaccination[i])
  month <- month(vax_agg$dateVaccination[i]) + (year(vax_agg$dateVaccination[i])-year(startDate))*12
  vaxVillYear <- rowSums(dogVaxVillMat[,(1:12)+12*(year-year(startDate))])
  vills <- which(LMVill$District_N==vax_agg$Correct_District[i] & vaxVillYear==0)

  ## Is the district urban or rural
  category_i <- unique(LMVill$Category[vills])

  ## Total number of vaccination units
  if(is.element("Rural",category_i)){
    units_i <- length(vills)
  }else{
    units_i <- length(unique(LMVill$matchWard[vills]))
  }

  ## Draw vaccination weight for each unit
  weights <- sample(vax_draw,units_i,replace=T)
  weights[which(weights==0)]<-1e-12 # means dogs in these units can still be selected if we run out of dogs to vaccinate in non-zero weight units

  ## Distribute dogs among vaccination units based on weights (and dog populations)
  if(category_i=="Rural"){dogPops_i <- dogPopVillMat[vills,month]
    }else{dogPops_i <- rowsum(dogPopVillMat[vills,month], LMVill$matchWard[vills])}
  dogs_i <- rep(1:units_i,round(dogPops_i*maxVax))
  probs <- rep(weights,round(dogPops_i*maxVax))
  draw_dogs_i <- sample(dogs_i, vax_agg$Dogs[i],prob = probs)
  if(category_i=="Rural"){
    dogs_vill_i <- table(draw_dogs_i)
    dogVaxVillMat[vills[as.numeric(names(dogs_vill_i))],month]<-dogs_vill_i

  }else{
    dogs_ward_i <- table(draw_dogs_i)
    for(ward in 1:length(dogs_ward_i)){
      vills_j <- which(LMVill$matchWard==sort(unique(LMVill$matchWard[vills]))[as.numeric(names(dogs_ward_i))[ward]])
      dogs_vills_j <- table(sample(rep(1:length(vills_j),times=dogPopVillMat[vills_j,month]),dogs_ward_i[ward]))
      dogVaxVillMat[vills_j[as.numeric(names(dogs_vills_j))],month] <- as.numeric(dogs_vills_j)

    }
  }
}



## Get vaccination coverage estimates
#--------------

## Get vaccination coverages by village by month
vcVill <- dogVaxVillMat/dogPopVillMat
length(which(vcVill>0))
length(which(vcVill>1))/length(which(vcVill>0))
hist(vcVill[which(vcVill>0)],breaks="fd")
max(vcVill[which(dogPopVillMat!=0)],na.rm=T)
vcVill[which(is.nan(vcVill))] <- 0 #a few nans caused by dividing by 0 dogs

## Get vaccination coverage by village by round
vacc_rounds <- vacc_rounds[which(vacc_rounds$Region!="Pemba"),]
vcVillRound <- matrix(0,nrow=nrow(LMVill),ncol=5)
for(i in 1:ncol(vcVillRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    vcVillRound[which(LMVill$District_N==vacc_rounds$District[j]),i]<-rowSums(vcVill[which(LMVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-year(startDate))])
  }
}
write.table(vcVillRound,file=paste("Output/vcMat_Vill_Round_",startDate,"_to_",endDate,".csv",sep=""),row.names = LMVill$matchVill,col.names = F, sep=",")

## Get vaccination coverage by district by round
vcDistRound <- matrix(0,nrow=nrow(LMDist),ncol=5)
for(i in 1:ncol(vcDistRound)){
  for(j in 1:nrow(vacc_rounds)){
    years_round <- vacc_rounds[j,(2+1+(i-1)*2)]:vacc_rounds[j,(2+2+(i-1)*2)]
    months_round <- ((1:(12*length(years_round)))+12*(years_round[1]-2010))[which(vcVill[which(LMVill$District_N==vacc_rounds$District[j]),(1:(12*length(years_round)))+12*(years_round[1]-2010)]!=0,arr.ind = T)[,2]]
    vcDistRound[j,i]<-sum(vcVill[cbind(which(LMVill$District_N==vacc_rounds$District[j]),months_round)]*dogPopVillMat[cbind(which(LMVill$District_N==vacc_rounds$District[j]),months_round)])
    vcDistRound[j,i]<-vcDistRound[j,i]/sum(dogPopVillMat[cbind(which(LMVill$District_N==vacc_rounds$District[j]),months_round)])
  }
}
write.table(vcDistRound,file=paste("Output/vcMat_Dist_Round_",startDate,"_to_",endDate,".csv",sep=""),row.names = vacc_rounds$District,col.names = F, sep=",")




