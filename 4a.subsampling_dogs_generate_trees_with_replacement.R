## In this code we are generating transmission trees based on subsampling a proportion
## of the dogs in the data to look at how reporting rates might affect the results. 

## WIll use the 50m cutoff values for the parameters, include geographic uncertainty 
## and use 2 convolutions for unknown dogs and wildlife

# RUN EPIDEMIC TREE ALGORITHM
rm(list=ls()) # removes all objects from the current workspace (R memory)

options(stringsAsFactors = FALSE) 
#library(maptools)
#library(animation)
#library(maps)
library(RColorBrewer)
library(rgdal)
library(geosphere)
library(codetools)
library(data.table)
library(dplyr)

set.seed(4)

# SERIAL INTERVAL PARAMETERS
# Best fit for this is a log normal
SIparams <- read.csv("output/SIparams.csv")
SImean <- SIparams$SImean
SIsd <- SIparams$SIsd

# SPATIAL INFECTION KERNEL
# Using parameters generated from the single distribution but incorporate interval censoring
# and with an upper cut off of 50m for the interval. Best fit for this is a gamma
Distparams <- read.csv("output/Distparams.csv")
distshape <- Distparams$dist_shape
distscale <- Distparams$dist_scale

source("R/Prep_trees_data_SE.R")

#_____________________________________________________________________

# replace species detail with generic categories - dog, wildlife, livestock, cat, unknown, other

table(SE_Tanz_2011$Species)
SE_Tanz_2011$Species[grep("Wildlife", SE_Tanz_2011$Species)] <- "Wildlife"
SE_Tanz_2011$Species[grep("Domestic dog", SE_Tanz_2011$Species)] <- "Dog"
table(SE_Tanz_2011$Species)


## Need to add a time difference as a numeric on to the data set for use within the function
start <- as.Date("2011-01-01")  
SE_Tanz_2011$Time_diff <- as.numeric(SE_Tanz_2011$Date_started - start)

##Check if there are cases in this with NA for the animal ID. 
SE_Tanz_2011[(!complete.cases(SE_Tanz_2011$ID)),]


## Below is the version of the function that we need to combine this code with.
source("R/TT_function_uncertainty_50m.R")

## To accommodate uncertainty in the geographic data we need to generate a new 
## distmatrix for each run so this appears within the for loop

n = 1000 ## number of times to run
## make empty matrices to fill

df<- SE_Tanz_2011

## set percentage want to subsample
Sample_percent <- 0.6
To_remove <- round(nrow(df[which(df$Species == "Dog"),])*(1- Sample_percent))
To_keep <- nrow(df[which(df$Species == "Dog"),]) - To_remove

IDs = sources = probs = ll = distprobs = SIprobs = matrix(nrow = nrow(SE_Tanz_2011)-To_remove, ncol=n) 

dim(SIprobs)

#nearby = which(df$Uncertainty == "nearby") 
#on_farm = which(df$Uncertainty == "on the farm") 
#far = which(df$Uncertainty == "far away")


system.time(  # ONLY RUN IF NEEDED
  
  #INFORMED GUESSES - remove cows/livestock
  for (i in 1:n){
    df <- SE_Tanz_2011
    
    
    df_cat <- df[which(df$Species == "Cat"),] # make cat df
    df_dog <- df[which(df$Species == "Dog"),] ## and dog df
    df_wild <- df[which(df$Species == "Wildlife"),]
    dog_156 <- df[which(df$ID == 156),] ## dog 156 as needs to be in all df as first cases and loop won't work without it
    wild_433 <- df[which(df$ID == 433),]
   
    ss_dogrows <- sample_n(df_dog, To_keep - 1, replace = T) # sample all but one of the dog rows using replacement
    ss_wild <- sample_n(df_wild, 235, replace = T)
    ss_cat <- sample_n(df_cat, 10, replace = T)
    #ss_dogrows$ID
    #table(ss_dogrows$ID)
    
    df <- rbind(ss_dogrows, dog_156, ss_cat, ss_wild, wild_433)
    #sort(df1$ID)
   #table(df1$ID)  
   IDs[,i] <- df[,"ID"]
    
    nearby = which(df$Uncertainty == "nearby") 
    on_farm = which(df$Uncertainty == "on the farm") 
    far = which(df$Uncertainty == "far away")
    
    df["New_long"] <- df["Longitude"]
    df["New_lat"] <- df["Latitude"]
    df["degrees"] <- runif(nrow(df),1,360)
    degrees <- runif(nrow(df), 1,360)
    
    ##distance needs to be in metres to use Destpoint
     df["distance"] <- 0 
    df[nearby, "distance"] <- runif(df[nearby, "distance"], 0, 2000)
    df[on_farm, "distance"] <- runif(df[on_farm, "distance"], 2000, 5000)
    df[far, "distance"] <- runif(df[far, "distance"], 5000, 10000)
    
    new_coords <- vector(mode = "list")
    new_coords <- apply(df[,c("Longitude", "Latitude", "degrees", "distance")], 1, function(x) destPoint(x[1:2], x[3], x[4]))
    new_coords <- t(new_coords)
    
    df[,"New_long"] <- new_coords[,1]
    df[,"New_lat"] <- new_coords[,2]
    
    distmatrix <- distm(df[c("New_long","New_lat" )], df[c("New_long","New_lat")])
    
    ss = df$Time_diff
    
    # add uncertainty in dates #ss=sypmtoms started (since we don't have exact dates!)
    guesses = unlist(lapply(1:nrow(df), guess,
                            possdates=ss,
                            possIDs=df$ID, 
                            uncert = df$SI_accuracy, # Run algorithm
                            knownsource=as.list(rep(0, nrow(df))),
                            species=df$Species,
                            owner_status = df$Owner,
                            distmatrix = distmatrix))
    
    sources[,i] = guesses[seq(1, length(guesses), 5)]
    probs[,i] = guesses[seq(2, length(guesses), 5)]
    ll[,i] = guesses [seq(3, length(guesses), 5)]
    SIprobs[,i] = guesses [seq(4, length(guesses), 5)]
    distprobs[,i] = guesses [seq(5, length(guesses), 5)]
    print(i)
  }
  
)

##Suspect only - 50m cutoff - all dogs but sampled with replacement
#write.csv(sources, file="Output/sources_subsampling100_50m_WR.csv", row.names=FALSE) ## modified function. Include uncertainty. PGs <= date of case
#write.csv(probs, file="Output/probs_subsampling100_50m_WR.csv", row.names=FALSE)
#write.csv(ll, file="Output/ll_subsampling100_50m_WR.csv", row.names=FALSE)
#write.csv(SIprobs, file="Output/SIprobs_subsampling100_50m_WR.csv", row.names=FALSE)
#write.csv(distprobs, file="Output/distprobs_subsampling100_50m_WR.csv", row.names=FALSE)

#write.csv(IDs, file = "Output/IDs_subsampling100_50m_WR.csv", row.names= FALSE)


##Suspect only - 50m cutoff - 90% cutoff
#write.csv(sources, file="Output/sources_subsampling90_50m_WR.csv", row.names=FALSE) ## modified function. Include uncertainty. PGs <= date of case
#write.csv(probs, file="Output/probs_subsampling90_50m_WR.csv", row.names=FALSE)
#write.csv(ll, file="Output/ll_subsampling90_50m_WR.csv", row.names=FALSE)
#write.csv(SIprobs, file="Output/SIprobs_subsampling90_50m_WR.csv", row.names=FALSE)
#write.csv(distprobs, file="Output/distprobs_subsampling90_50m_WR.csv", row.names=FALSE)

#write.csv(IDs, file = "Output/IDs_subsampling90_50m_WR.csv", row.names= FALSE)

## 50m cutoff - 75% subsampling proportion
#write.csv(sources, file="Output/sources_subsampling75_50m_WR.csv", row.names=FALSE) ## modified function. Include uncertainty. PGs <= date of case
#write.csv(probs, file="Output/probs_subsampling75_50m_WR.csv", row.names=FALSE)
#write.csv(ll, file="Output/ll_subsampling75_50m_WR.csv", row.names=FALSE)
#write.csv(SIprobs, file="Output/SIprobs_subsampling75_50m_WR.csv", row.names=FALSE)
#write.csv(distprobs, file="Output/distprobs_subsampling75_50m_WR.csv", row.names=FALSE)

#write.csv(IDs, file = "Output/IDs_subsampling75_50m_WR.csv", row.names= FALSE)


## 50m cutoff - 60% subsampling proportion
#write.csv(sources, file="Output/sources_subsampling60_50m_WR.csv", row.names=FALSE) ## modified function. Include uncertainty. PGs <= date of case
#write.csv(probs, file="Output/probs_subsampling60_50m_WR.csv", row.names=FALSE)
#write.csv(ll, file="Output/ll_subsampling60_50m_WR.csv", row.names=FALSE)
#write.csv(SIprobs, file="Output/SIprobs_subsampling60_50m_WR.csv", row.names=FALSE)
#write.csv(distprobs, file="Output/distprobs_subsampling60_50m_WR.csv", row.names=FALSE)

#write.csv(IDs, file = "Output/IDs_subsampling60_50m_WR.csv", row.names= FALSE)

####################


### Now run follow up codes as needed depending on what we are looking at. 
### May be found in other scripts. 

## Subsampling_analysis_with_replacement is next script

###########  SWITCH TO OTHER CODES HERE!!!   ###################

