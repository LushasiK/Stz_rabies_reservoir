## This code has the following properties:

## Incorporates uncertainty
## Specifies dates of possible progenitors MUST be before the dates of cases
## 50m as upper limit of interval censoring for distance

rm(list = ls())

library(maptools)
#library(animation)
library(maps)
library(RColorBrewer)
library(rgdal)
library(codetools)
library(igraph)
library(prettymapr)
library(geosphere)

set.seed(4)

## Read in the relevant csv file. 
## code below is using files with a 50m cut off for distance and incorporating uncertainty
## This defines date as of progenitors as before those of the cases 
## Using the modified function to truncate the SI at 0 and incorporate geographical uncertainty

trees = data.frame(read.csv( file="output/sources_geo_uncert_50m.csv",sep=",", header = T))

LL = read.csv(file="output/ll_geo_uncert_50m.csv", sep=",", header = T)
distprobs = read.csv(file="output/distprobs_geo_uncert_50m.csv", sep=",", header = T)

### Read in the rabid data set

source("R/Prep_trees_data_trans_trees.R")

### Specify the parameter distributions from the best fitting distribution that
### was used in the algorithm. 
## Serial Interval

SIparams <- read.csv("output/SIparams.csv")
SImean <- SIparams$SImean
SIsd <- SIparams$SIsd


# Distance parameters calculated in Contact_tracing_distance distribution
# Using 50m as cutoff for interval censoring
Distparams <- read.csv("output/Distparams.csv")

distshape <- Distparams$dist_shape
distscale <- Distparams$dist_scale

# Examine where tree building algorithm failed:
nas = which(is.na(apply(trees, 1, sum))) #  number of cases where no progenitors identified for 50000 runs...********
NAS = rep(NA, length(nas))
for(i in 1:length(nas)){
  NAS[i] <- length(which(!is.na(trees[nas[i],])))  
}  ##For each row that has NAs it goes through and removes these nas
## so the NAS vector shows you how many of the runs for each of these lines don't have NAs. If we print NAS we see
## the number that are all NAS (0 non NAS)

rabid$ID[nas[which(NAS == 0)]] # The IDs that have no ancestor 

No_prog <- rabid[nas[which(NAS == 0)],]

## ones with 
rabid_cc <- rabid[complete.cases(rabid$ID),] ## this line removes any of the rabid dataframe which are 
## not complete cases for the ID
sum(is.na(rabid$ID)) ## is zero because removed in earlier code. 

#Find most likely progenitors for each case (and their bootstrap support):
cases <- nrow(rabid)
runs <- ncol(trees)
biter <- bootstrap <- progll <- numeric(cases)
for(i in 1:cases){ # For every case:
  if(length(which(is.na(trees[i,])))!=runs){   # If not all results are NA, assign most likely progenitor
    biters = tabulate(t(trees[i,]))
    biter[i] <- which(biters == max(biters))[1] ## This works out which one of the possible progenitors has the most ''votes'
    bootstrap[i] <- max(biters)
    print(i)
  }
}

length(which(bootstrap!=0))/cases   # % of assigned progenitors 

rabid$biter <- biter
## support for each assigned biter
rabid["biter_support"] <- (bootstrap/runs)*100
range(rabid$biter_support)

dev.off()
hist(rabid$biter_support, breaks = seq(0,100, by = 5), freq = TRUE, 
     ylab = "Number of cases", xlab = "percentage support for assigned progenitor", 
     col = "turquoise", main = "Histogram of level of support for assigned progenitor")


## Make a smaller df so can look at the columns more easily
rabid_small <- rabid[,c("ID", "Biter.ID", "biter", "Species")]

##calculate what proportion of cases that have a known ID are agreed on by our system.
rabid_small_na <-rabid_small[which(rabid_small$Biter.ID!=0), ]
matching <- rabid_small_na[which(rabid_small_na$Biter.ID == rabid_small_na$biter),]

prop_match <- (nrow(matching)/nrow(rabid_small_na))*100
prop_match

## Will write the knwon biters table in case want to see which matches
#write.csv(rabid_small_na, "Output/Knownbiter_50m_geo_uncert.csv")

### Extract the SIs and distance travelled for each of the most likely progenitors. 
### Won't work for those where the progenitor hasn't been suggested.
### So need to remove these 

rabid_reduced <- rabid[which(rabid$biter !=0),]
biter_reduced <- biter[which(biter != 0)]

## Also don't want to include 156 and 433 as these are the first cases so they
## are just assigned each other. 
## So remove these for further analysis

rabid_reduced <- rabid_reduced[which(rabid_reduced$ID != 156),]
rabid_reduced <- rabid_reduced[which(rabid_reduced$ID != 433),]


SI_res <- data.frame(matrix(ncol = 13, nrow = nrow(rabid_reduced)))
colnames(SI_res) <- c("ID", "biter_ID", "ID_species", "Biter_species", 
                      "ID_Time_diff", "Biter_Time_diff", "ID_lat", "ID_long", 
                      "Biter_lat", "Biter_long", "Year", "ID_accuracy", "Biter_accuracy")


for(i in 1:nrow(rabid_reduced)){
  SI_res[i,1] <- rabid_reduced$ID[i]
  SI_res[i,2] <- rabid[which(rabid$ID == rabid_reduced$biter[i]),"ID"]
  SI_res[i,3] <- rabid_reduced$Species[i]
  SI_res[i,4] <- rabid[which(rabid$ID == rabid_reduced$biter[i]), "Species"]
  SI_res[i,5] <- rabid_reduced$Time_diff[i]
  SI_res[i,6] <- rabid[which(rabid$ID == rabid_reduced$biter[i]), "Time_diff"]
  SI_res[i,7] <- rabid_reduced$Latitude[i]
  SI_res[i,8] <- rabid_reduced$Longitude[i]
  SI_res[i,9] <- rabid[which(rabid$ID == rabid_reduced$biter[i]),"Latitude"]
  SI_res[i,10] <- rabid[which(rabid$ID == rabid_reduced$biter[i]), "Longitude"]
  SI_res[i,11] <- rabid[which(rabid$ID == rabid_reduced$biter[i]), "year"]
  SI_res[i,12] <- rabid_reduced$SI_accuracy[i]
  SI_res[i,13] <- rabid[which(rabid$ID == rabid_reduced$biter[i]), "SI_accuracy"]
  print(i)
}


SI_res["Serial_Int"] <- SI_res["ID_Time_diff"] - SI_res["Biter_Time_diff"]


for(i in 1:nrow(SI_res)){
  SI_res[i,"Bite_dist"] <- distVincentyEllipsoid(c(SI_res[i,"ID_lat"], SI_res[i,"ID_long"]),
                                                 c(SI_res[i,"Biter_lat"], SI_res[i,"Biter_long"]))
}


## The process of making the table has converted the species to numbers 1-4. 
## look at the number of each

table(SI_res$ID_species, SI_res$Biter_species)

levels(rabid$Species)

table(rabid_reduced$Species)
table(SI_res$ID_species)

## This allows is to see that dog is coded as 1, wildlife is coded as 2 and cat is coded as 4. 
## Use the chunk that fits with the way the data have been loaded. 

# SI_res["Species_trans"] <- NA
# SI_res[which(SI_res$ID_species == 1 & SI_res$Biter_species == 1),"Species_trans"] <- "DD"
# SI_res[which(SI_res$ID_species == 2 & SI_res$Biter_species == 2),"Species_trans"] <- "WW"
# SI_res[which(SI_res$ID_species == 4 & SI_res$Biter_species == 4),"Species_trans"] <- "CC"
# SI_res[which(SI_res$ID_species == 1 & SI_res$Biter_species == 2),"Species_trans"] <- "WD"
# SI_res[which(SI_res$ID_species == 2 & SI_res$Biter_species == 1),"Species_trans"] <- "DW"
# SI_res[which(SI_res$ID_species == 1 & SI_res$Biter_species == 4),"Species_trans"] <- "CD"
# SI_res[which(SI_res$ID_species == 4 & SI_res$Biter_species == 1),"Species_trans"] <- "Dc"
# SI_res[which(SI_res$ID_species == 2 & SI_res$Biter_species == 4),"Species_trans"] <- "CW"
# SI_res[which(SI_res$ID_species == 4 & SI_res$Biter_species == 2),"Species_trans"] <- "WC"

SI_res["Species_trans"] <- NA
SI_res[which(SI_res$ID_species == "Dog" & SI_res$Biter_species == "Dog"),"Species_trans"] <- "DD"
SI_res[which(SI_res$ID_species == "Wildlife" & SI_res$Biter_species == "Wildlife"),"Species_trans"] <- "WW"
SI_res[which(SI_res$ID_species == "Cat" & SI_res$Biter_species == "Cat"),"Species_trans"] <- "CC"
SI_res[which(SI_res$ID_species == "Dog" & SI_res$Biter_species == "Wildlife"),"Species_trans"] <- "WD"
SI_res[which(SI_res$ID_species == "Wildlife" & SI_res$Biter_species == "Dog"),"Species_trans"] <- "DW"
SI_res[which(SI_res$ID_species == "Dog" & SI_res$Biter_species == "Cat"),"Species_trans"] <- "CD"
SI_res[which(SI_res$ID_species == "Cat" & SI_res$Biter_species == "Dog"),"Species_trans"] <- "Dc"
SI_res[which(SI_res$ID_species == "Wildlife" & SI_res$Biter_species == "Cat"),"Species_trans"] <- "CW"
SI_res[which(SI_res$ID_species == "Cat" & SI_res$Biter_species == "Wildlife"),"Species_trans"] <- "WC"


#write.csv(SI_res, "output/SI_res_most_likely_PG_50m.csv")

##########################################################
### Switch to markdown



## Also look if there is a difference between DD and JJ transmissions
## Possibly not worth looking at these for the all transmissions part? 
DD_trans <- SI_res[which(SI_res$Species_trans == "DD"),]
WW_trans <- SI_res[which(SI_res$Species_trans == "WW"),]
DW_trans <- SI_res[which(SI_res$Species_trans == "DW"),]
WD_trans <- SI_res[which(SI_res$Species_trans == "WD"),]


range(SI_res$Serial_Int)
mean(SI_res$Serial_Int)
sd(SI_res$Serial_Int)
median(SI_res$Serial_Int)

range(DD_trans$Serial_Int)
mean(DD_trans$Serial_Int)
sd(DD_trans$Serial_Int)
median(DD_trans$Serial_Int)

range(WW_trans$Serial_Int)
mean(WW_trans$Serial_Int)
sd(WW_trans$Serial_Int)
median(WW_trans$Serial_Int)

range(DW_trans$Serial_Int)
mean(DW_trans$Serial_Int)
sd(DW_trans$Serial_Int)
median(DW_trans$Serial_Int)

range(WD_trans$Serial_Int)
mean(WD_trans$Serial_Int)
sd(WD_trans$Serial_Int)
median(WD_trans$Serial_Int)


### Bite distances

range(SI_res$Bite_dist)
mean(SI_res$Bite_dist)
sd(SI_res$Bite_dist)
median(SI_res$Bite_dist)

range(DD_trans$Bite_dist)
mean(DD_trans$Bite_dist)
sd(DD_trans$Bite_dist)
median(DD_trans$Bite_dist)

range(WW_trans$Bite_dist)
mean(WW_trans$Bite_dist)
sd(WW_trans$Bite_dist)
median(WW_trans$Bite_dist)

range(DW_trans$Bite_dist)
mean(DW_trans$Bite_dist)
sd(DW_trans$Bite_dist)
median(DW_trans$Bite_dist)

range(WD_trans$Bite_dist)
mean(WD_trans$Bite_dist)
sd(WD_trans$Bite_dist)
median(WD_trans$Bite_dist)



###########################################################################
###########################################################################
#####   LOOK AT ALL THE POSSIBLE PROGENITORS FOR EACH CASE ################
###########################################################################
###########################################################################

#install.packages("tidyverse")
library(tidyverse)
library(reshape2)

## We want to see how many progenitors are considered possible for each ID
df <- trees
ID <- as.data.frame(rabid[,"ID"])


## This way produces a list  of lists. Each list corresponds to an ID and shows the 
## progenitors for each ID in the list and how many 
All_progenitors <- apply(df,MARGIN=1,table)

## another way to do it is
#levels=unique(do.call(c,df)) #all unique values in df
#All_prog_2 <- sapply(levels,function(x)rowSums(df==x)) #count occurrences of x in each row
#colnames(All_prog_2) <- levels

## Code below converts the list to a data frame and then works out the proportion
## of each 
## Make this in to a function
## With all the possible progenitors and uncertainty included we don't have any NAs so the
## input for the functions are the same as the original data frames/lists
rabid_prog <- rabid
Prog_no_NA <- All_progenitors

list_func_2 <- function(x){
  df1 <- as.data.frame(x)
  df2 <-  cbind(rabid_prog[i,"ID"], df1)
  colnames(df2) <- c("ID", "Biter_ID", "Freq")
  df2["prop"] <- (df2["Freq"]/50000*100) ## calculate the percentage support
  df2
}

for (i in 1:length(Prog_no_NA)) {
  nam <- paste("DFP", i, sep = "")
  assign(nam, list_func_2(Prog_no_NA[[i]]))
  print(i)
}



#### now want to try and combine all the dataframes

## make sure using the correct data frames depending on whether or not incorporating uncertainty

All_prog_DF <- rbind(DFP1, DFP2, DFP3, DFP4, DFP5, DFP6, DFP7, DFP8, DFP9, DFP10, 
                     DFP11, DFP12, DFP13, DFP14, DFP15, DFP16, DFP17, DFP18, DFP19, DFP20, 
                     DFP21, DFP22, DFP23, DFP24, DFP25, DFP26, DFP27, DFP28, DFP29, DFP30, 
                     DFP31, DFP32, DFP33, DFP34, DFP35, DFP36, DFP37, DFP38, DFP39, DFP40, 
                     DFP41, DFP42, DFP43, DFP44, DFP45, DFP46, DFP47, DFP48, DFP49, DFP50, 
                     DFP51, DFP52, DFP53, DFP54, DFP55, DFP56, DFP57, DFP58, DFP59, DFP60, 
                     DFP61, DFP62, DFP63, DFP64, DFP65, DFP66, DFP67, DFP68, DFP69, DFP70, 
                     DFP71, DFP72, DFP73, DFP74, DFP75, DFP76, DFP77, DFP78, DFP79, DFP80, 
                     DFP81, DFP82, DFP83, DFP84, DFP85, DFP86, DFP87, DFP88, DFP89, DFP90, 
                     DFP91, DFP92, DFP93, DFP94, DFP95, DFP96, DFP97, DFP98, DFP99, DFP100,
                     DFP101, DFP102, DFP103, DFP104, DFP105, DFP106, DFP107, DFP108, DFP109, DFP110, 
                     DFP111, DFP112, DFP113, DFP114, DFP115, DFP116, DFP117, DFP118, DFP119, DFP120, 
                     DFP121, DFP122, DFP123, DFP124, DFP125, DFP126, DFP127, DFP128, DFP129, DFP130, 
                     DFP131, DFP132, DFP133, DFP134, DFP135, DFP136, DFP137, DFP138, DFP139, DFP140, 
                     DFP141, DFP142, DFP143, DFP144, DFP145, DFP146, DFP147, DFP148, DFP149, DFP150, 
                     DFP151, DFP152, DFP153, DFP154, DFP155, DFP156, DFP157, DFP158, DFP159, DFP160, 
                     DFP161, DFP162, DFP163, DFP164, DFP165, DFP166, DFP167, DFP168, DFP169, DFP170, 
                     DFP171, DFP172, DFP173, DFP174, DFP175, DFP176, DFP177, DFP178, DFP179, DFP180, 
                     DFP181, DFP182, DFP183, DFP184, DFP185, DFP186, DFP187, DFP188, DFP189, DFP190, 
                     DFP191, DFP192, DFP193, DFP194, DFP195, DFP196, DFP197, DFP198, DFP199, DFP200,
                     DFP201, DFP202, DFP203, DFP204, DFP205, DFP206, DFP207, DFP208, DFP209, DFP210, 
                     DFP211, DFP212, DFP213, DFP214, DFP215, DFP216, DFP217, DFP218, DFP219, DFP220, 
                     DFP221, DFP222, DFP223, DFP224, DFP225, DFP226, DFP227, DFP228, DFP229, DFP230, 
                     DFP231, DFP232, DFP233, DFP234, DFP235, DFP236, DFP237, DFP238, DFP239, DFP240, 
                     DFP241, DFP242, DFP243, DFP244, DFP245, DFP246, DFP247, DFP248, DFP249, DFP250, 
                     DFP251, DFP252, DFP253, DFP254, DFP255, DFP256, DFP257, DFP258, DFP259, DFP260, 
                     DFP261, DFP262, DFP263, DFP264, DFP265, DFP266, DFP267, DFP268, DFP269, DFP270, 
                     DFP271, DFP272, DFP273, DFP274, DFP275, DFP276, DFP277, DFP278, DFP279, DFP280, 
                     DFP281, DFP282, DFP283, DFP284, DFP285, DFP286, DFP287, DFP288, DFP289, DFP290, 
                     DFP291, DFP292, DFP293, DFP294, DFP295, DFP296, DFP297, DFP298, DFP299, DFP300,
                     DFP301, DFP302, DFP303, DFP304, DFP305, DFP306, DFP307, DFP308, DFP309, DFP310, 
                     DFP311, DFP312, DFP313, DFP314, DFP315, DFP316, DFP317, DFP318, DFP319, DFP320, 
                     DFP321, DFP322, DFP323, DFP324, DFP325, DFP326, DFP327, DFP328, DFP329, DFP330, 
                     DFP331, DFP332, DFP333, DFP334, DFP335, DFP336, DFP337, DFP338, DFP339, DFP340, 
                     DFP341, DFP342, DFP343, DFP344, DFP345, DFP346, DFP347, DFP348, DFP349, DFP350, 
                     DFP351, DFP352, DFP353, DFP354, DFP355, DFP356, DFP357, DFP358, DFP359, DFP360, 
                     DFP361, DFP362, DFP363, DFP364, DFP365, DFP366, DFP367, DFP368, DFP369, DFP370, 
                     DFP371, DFP372, DFP373, DFP374, DFP375, DFP376, DFP377, DFP378, DFP379, DFP380, 
                     DFP381, DFP382, DFP383, DFP384, DFP385, DFP386, DFP387, DFP388, DFP389, DFP390, 
                     DFP391, DFP392, DFP393, DFP394, DFP395, DFP396, DFP397, DFP398, DFP399, DFP400,
                     DFP401, DFP402, DFP403, DFP404, DFP405, DFP406, DFP407, DFP408, DFP409, DFP410, 
                     DFP411, DFP412, DFP413, DFP414, DFP415, DFP416, DFP417, DFP418, DFP419, DFP420, 
                     DFP421, DFP422, DFP423, DFP424, DFP425, DFP426, DFP427, DFP428, DFP429, DFP430, 
                     DFP431, DFP432, DFP433, DFP434, DFP435, DFP436, DFP437, DFP438, DFP439, DFP440, 
                     DFP441, DFP442, DFP443, DFP444, DFP445, DFP446, DFP447, DFP448, DFP449, DFP450, 
                     DFP451, DFP452, DFP453, DFP454, DFP455, DFP456, DFP457, DFP458, DFP459, DFP460, 
                     DFP461, DFP462, DFP463, DFP464, DFP465, DFP466, DFP467, DFP468, DFP469, DFP470, 
                     DFP471, DFP472, DFP473, DFP474, DFP475, DFP476, DFP477, DFP478, DFP479, DFP480, 
                     DFP481, DFP482, DFP483, DFP484, DFP485, DFP486, DFP487, DFP488, DFP489, DFP490, 
                     DFP491, DFP492, DFP493, DFP494, DFP495, DFP496, DFP497, DFP498, DFP499, DFP500,
                     DFP501, DFP502, DFP503, DFP504, DFP505, DFP506, DFP507, DFP508, DFP509, DFP510,
                     DFP511, DFP512, DFP513, DFP514, DFP515, DFP516, DFP517, DFP518, DFP519, DFP520,
                     DFP521, DFP522, DFP523, DFP524, DFP525, DFP526, DFP527, DFP528, DFP529, DFP530,
                     DFP531, DFP532, DFP533, DFP534, DFP535, DFP536, DFP537, DFP538, DFP539, DFP540,
                     DFP541, DFP542, DFP543, DFP544, DFP545, DFP546, DFP547, DFP548, DFP549)




All_prog_DF <- All_prog_DF[which(All_prog_DF$ID != "156"),]
All_prog_DF <- All_prog_DF[which(All_prog_DF$ID != "433"),]

AP_res <- data.frame(matrix(ncol = 11, nrow = nrow(All_prog_DF)))

colnames(AP_res) <- c("ID", "biter_ID", "ID_species", "Biter_species", 
                      "ID_Time_diff", "Biter_Time_diff", "ID_lat", "ID_long", 
                      "Biter_lat", "Biter_long", "Year")


for(i in 1:nrow(All_prog_DF)){
  AP_res[i,1] <- All_prog_DF$ID[i]
  AP_res[i,2] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]),"ID"]
  AP_res[i,3] <- rabid[which(rabid$ID == All_prog_DF$ID[i]), "Species"]
  AP_res[i,4] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]), "Species"]
  AP_res[i,5] <- rabid[which(rabid$ID == All_prog_DF$ID[i]), "Time_diff"]
  AP_res[i,6] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]), "Time_diff"]
  AP_res[i,7] <- rabid[which(rabid$ID == All_prog_DF$ID[i]), "Latitude"]
  AP_res[i,8] <- rabid[which(rabid$ID == All_prog_DF$ID[i]), "Longitude"]
  AP_res[i,9] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]),"Latitude"]
  AP_res[i,10] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]), "Longitude"]
  AP_res[i,11] <- rabid[which(rabid$ID == All_prog_DF$Biter_ID[i]), "year"]
  print(i)
}


AP_res["Serial_Int"] <- AP_res["ID_Time_diff"] - AP_res["Biter_Time_diff"]


for(i in 1:nrow(AP_res)){
  AP_res[i,"Bite_dist"] <- distVincentyEllipsoid(c(AP_res[i,"ID_lat"], AP_res[i,"ID_long"]),
                                                 c(AP_res[i,"Biter_lat"], AP_res[i,"Biter_long"]))
}


##Add a column to AP res that includes the biter details from the original dataset
AP_res["known_biter"] <- "NA"
for ( k in 1:(nrow(AP_res))){
  AP_res$known_biter[k] <- rabid[which(rabid$ID == AP_res$ID[k]), "Biter.ID"]
  print(k)
}

## extract those where the biter is known. 
AP_match <- AP_res[which(AP_res$known_biter !=0),]
## and then remove the ones where this is 
matching_AP <- AP_match[which(AP_match$biter_ID == AP_match$known_biter),]
## find the percentage that match. 
nrow(matching_AP)/nrow(rabid[which(rabid$Biter.ID != 0 ),])*100

Not_matched <- AP_match[which(AP_match$biter_ID != AP_match$known_biter), ]

##what proportion of the overall support does each match get.

Match_support <- matching_AP
Match_support["prop_support"] <- NA
for(g in 1:nrow(Match_support)){
  Match_support$prop_support[g] <- All_prog_DF[which(All_prog_DF$ID == Match_support$ID[g] &
                                                       # All_prog_DF[which(
                                                       All_prog_DF$Biter_ID == Match_support$biter_ID[g]), "prop"]
}

### summary stats for the serial intervals for the

range(AP_res$Serial_Int)
mean(AP_res$Serial_Int)
sd(AP_res$Serial_Int)
median(AP_res$Serial_Int)

### Bite distances

range(AP_res$Bite_dist)
mean(AP_res$Bite_dist)
sd(AP_res$Bite_dist)
median(AP_res$Bite_dist)


####################################################


## The process of making the table has converted the species to numbers 1-4. 
## look at the number of each

table(AP_res$ID_species, AP_res$Biter_species)

# AP_res["Species_trans"] <- NA
# AP_res[which(AP_res$ID_species == 1 & AP_res$Biter_species == 1),"Species_trans"] <- "DD"
# AP_res[which(AP_res$ID_species == 2 & AP_res$Biter_species == 2),"Species_trans"] <- "WW"
# AP_res[which(AP_res$ID_species == 4 & AP_res$Biter_species == 4),"Species_trans"] <- "CC"
# AP_res[which(AP_res$ID_species == 1 & AP_res$Biter_species == 2),"Species_trans"] <- "WD"
# AP_res[which(AP_res$ID_species == 2 & AP_res$Biter_species == 1),"Species_trans"] <- "DW"
# AP_res[which(AP_res$ID_species == 1 & AP_res$Biter_species == 4),"Species_trans"] <- "CD"
# AP_res[which(AP_res$ID_species == 4 & AP_res$Biter_species == 1),"Species_trans"] <- "DC"
# AP_res[which(AP_res$ID_species == 2 & AP_res$Biter_species == 4),"Species_trans"] <- "CW"
# AP_res[which(AP_res$ID_species == 4 & AP_res$Biter_species == 2),"Species_trans"] <- "WC"

AP_res["Species_trans"] <- NA
AP_res[which(AP_res$ID_species == "Dog" & AP_res$Biter_species == "Dog"),"Species_trans"] <- "DD"
AP_res[which(AP_res$ID_species == "Wildlife" & AP_res$Biter_species == "Wildlife"),"Species_trans"] <- "WW"
AP_res[which(AP_res$ID_species == "Cat" & AP_res$Biter_species == "Cat"),"Species_trans"] <- "CC"
AP_res[which(AP_res$ID_species == "Dog" & AP_res$Biter_species == "Wildlife"),"Species_trans"] <- "WD"
AP_res[which(AP_res$ID_species == "Wildlife" & AP_res$Biter_species == "Dog"),"Species_trans"] <- "DW"
AP_res[which(AP_res$ID_species == "Dog" & AP_res$Biter_species == "Cat"),"Species_trans"] <- "CD"
AP_res[which(AP_res$ID_species == "Cat" & AP_res$Biter_species == "Dog"),"Species_trans"] <- "Dc"
AP_res[which(AP_res$ID_species == "Wildlife" & AP_res$Biter_species == "Cat"),"Species_trans"] <- "CW"
AP_res[which(AP_res$ID_species == "Cat" & AP_res$Biter_species == "Wildlife"),"Species_trans"] <- "WC"


#write.csv(AP_res, "output/AP_res_allPGs_50m.csv")
#write.csv(All_prog_DF, "output/All_prog_combined_df_50m.csv")


