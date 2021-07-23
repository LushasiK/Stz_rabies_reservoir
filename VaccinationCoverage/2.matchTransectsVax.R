# Match transect records to the corresponding records of total dogs vaccinated

library(dplyr)
library(lubridate)
library(plyr)
library(rgdal)
library(stringr)

rm(list=ls())



## Load data
#-------------------

## Transect data
transects <- read.csv("data/transects_cleaned.csv")

## Vaccination data
vax <- read.csv(paste("data/vaccinated_cleaned.csv",sep=""))



## Add vaccination round information
#--------------------

## dates
vax$dateVaccination <- as.Date(vax$dateVaccination)
vax$month <- month(vax$dateVaccination) + (year(vax$dateVaccination)-2010)*12
vax$year <- year(vax$dateVaccination)
transects$Date <- as.Date(transects$Date)

## Add vaccination round to vax records
vax$round <- NA
vax$round[which(vax$month<=19)] <- 1
vax$round[which(vax$month>=21 & vax$month<=34)] <- 2
vax$round[which(vax$month>=38 & vax$month<=57)] <- 3
vax$round[which(vax$month>=60 & vax$month<=70)] <- 4
vax$round[which(vax$month>=78)] <- 5
which(is.na(vax$round))

## Add vaccination round to transect records
transects$round <- NA
transects$round[which(transects$month<=19)] <- 1
transects$round[which(transects$month>=21 & transects$month<=34)] <- 2
transects$round[which(transects$month>=38 & transects$month<=57)] <- 3
transects$round[which(transects$month>=60 & transects$month<=70)] <- 4
transects$round[which(transects$month>=78)] <- 5
which(is.na(transects$round))



## Aggregate vaccination data by location (village/ward) and month
#-------------------

##now match with vaccinated data
matchVax <- match(unique(tolower(transects$DWV)),tolower(vax$Correct_Village))
unique(tolower(transects$DWV))[which(is.na(matchVax))] 
length(unique(tolower(transects$DWV))[which(is.na(matchVax))] )
# 69 not matching up - may correspond to villages that were vaccinated through
# central points in neighbouring villages


## Aggregate dogs vaccinated in each village/ward in each month
vax <- vax[order(vax$Correct_Village,vax$round),]
vax$DW <- paste(word(vax$Correct_Village,1,sep="_"),word(vax$Correct_Village,2,sep="_"),sep="_")
vaxDogsMonth<-aggregate(Dogs~Correct_Village+DW+month+year+round,vax,sum)
vaxDogsMonthWard<-aggregate(Dogs~DW+month+year+round,vax,sum)
which(duplicated(vaxDogsMonth[,c("Correct_Village","round")])) # no multiple vaccinations in the same round
which(duplicated(vaxDogsMonthWard[,c("DW","round")]))

## In Masasi in 2011, many of the vaccinations were not submitted individually
## at the village level and are instead aggregated into the following record:
vax[which(vax$Dogs>1000),]

## So repeat aggregation excluding this record
vaxDogsMonth<-aggregate(Dogs~Correct_Village+DW+month+year+round,vax[which(vax$Dogs<1000),],sum)
vaxDogsMonthWard<-aggregate(Dogs~DW+month+year+round,vax[which(vax$Dogs<1000),],sum)

## Save aggregated versions of data
write.csv(vaxDogsMonth,paste("Output/vaxDogsMonthVillage.csv",sep=""),row.names = F)
write.csv(vaxDogsMonthWard,paste("Output/vaxDogsMonthWard.csv",sep=""),row.names = F)



## Add vaccinated info to transect data
#-------------------

## Group transects carried out in same village in same round
transects_round <- aggregate(cbind(Dogs.with.collars,Dogs.without.collars)~DWV+DW+round,transects,sum)


## Match vaccination data to transects at the village level
transects_vax <- merge(transects_round, vaxDogsMonth, by.x=c("DWV","DW","round"), by.y=c("Correct_Village","DW","round"), all.x=T, all.y=F)
names(transects_vax)[(ncol(transects_vax)-2):ncol(transects_vax)] <- c("month_vax","year_vax","Dogs_vax")
write.csv(transects_vax,paste("Output/transects_vax_village.csv",sep=""),row.names = F)


## Group transects by ward
transects_round_ward <- aggregate(cbind(Dogs.with.collars,Dogs.without.collars)~DW+round,transects,sum)


## Match vaccination data to transects at the ward level
transectsWard_vax <- merge(transects_round_ward, vaxDogsMonthWard, by.x=c("DW","round"), by.y=c("DW","round"), all.x=T, all.y=F)
names(transectsWard_vax)[(ncol(transectsWard_vax)-2):ncol(transectsWard_vax)] <- c("month_vax","year_vax","Dogs_vax")
write.csv(transectsWard_vax,paste("Output/transects_vax_ward.csv",sep=""),row.names = F)



