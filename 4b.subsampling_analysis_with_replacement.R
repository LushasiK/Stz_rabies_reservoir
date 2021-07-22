rm(list = ls())

library(geosphere)

## Bring in the IDs for each of the subsampled tables
IDs <- read.csv("Output/IDs_subsampling100_50m_WR.csv")
#IDs <- read.csv("Output/IDs_subsampling90_50m_WR.csv")
#IDs <- read.csv("Output/IDs_subsampling75_50m_WR.csv")
#IDs <- read.csv("Output/IDs_subsampling60_50m_WR.csv")


## Then bring in the source IDs for each of the tables
SourceIDs <- read.csv("Output/sources_subsampling100_50m_WR.csv")
#SourceIDs <- read.csv("Output/sources_subsampling90_50m_WR.csv")
#SourceIDs <- read.csv("Output/sources_subsampling75_50m_WR.csv")
#SourceIDs <- read.csv("Output/sources_subsampling60_50m_WR.csv")


IDs[1:5, 1:5]
SourceIDs[1:5, 1:5]

source("R/Prep_trees_data_trans_trees.R")

table(SE_Tanz_2011$Species)

# Try writing a function that works for one column and then apply this over the whole matrix

source("R/Species_function.R")

# Create a matrix of Species of IDs
ID_Sp_mat <- apply(IDs, 2, Species_function)
Source_Sp_mat <- apply(SourceIDs, 2, Species_function)

ID_Sp_mat[1:5, 1:5]
Source_Sp_mat[1:5, 1:5]

## write a function to assign the type of transmission
#start looking at one col each from the data. 
#OneID <- ID_Sp_mat[,1]
#OneSource <- Source_Sp_mat[,1]

### Function currently not working correctly
#source("R/SpSpFunction.R")

#SPSP_function(OneID, OneSource)

#SPTrans_mat <- apply(Source_Sp_mat, 2, SPSP_function, ID_Sp_mat)
#sapply(1:ncol(dat1), function(i) all(rowSums(sapply(dat1[,i], grepl, dat2[,i]))>0)) 

## Use this until function working
## if data loads species as number need to transform back to species.
## Check correct numbers/species match before do that. 

ID_Sp_mat[which(ID_Sp_mat == 1)] <- "Dog"
ID_Sp_mat[which(ID_Sp_mat == 2)] <- "Wildlife"
ID_Sp_mat[which(ID_Sp_mat == 4)] <- "Cat"

Source_Sp_mat[which(Source_Sp_mat == 1)] <- "Dog"
Source_Sp_mat[which(Source_Sp_mat == 2)] <- "Wildlife"
Source_Sp_mat[which(Source_Sp_mat == 4)] <- "Cat"

source("Assigning_transmission.R")
SpSpTrans[1:5, 1:5]


## Now try and look at proportions of each transmission in each column

round(table(SpSpTrans[,1])/nrow(SpSpTrans)*100,1)

Mat_Res <- matrix(nrow = 9, ncol = ncol(IDs))
for (i in 1:ncol(SpSpTrans)) {
  Mat_Res[1,i] <- sum(as.numeric(SpSpTrans[,i] == "CC"))
  Mat_Res[2,i] <- sum(as.numeric(SpSpTrans[,i] == "CD"))
  Mat_Res[3,i] <- sum(as.numeric(SpSpTrans[,i] == "CW"))
  Mat_Res[4,i] <- sum(as.numeric(SpSpTrans[,i] == "DC"))
  Mat_Res[5,i] <- sum(as.numeric(SpSpTrans[,i] == "DD"))
  Mat_Res[6,i] <- sum(as.numeric(SpSpTrans[,i] == "DW"))
  Mat_Res[7,i] <- sum(as.numeric(SpSpTrans[,i] == "WC"))
  Mat_Res[8,i] <- sum(as.numeric(SpSpTrans[,i] == "WD"))
  Mat_Res[9,i] <- sum(as.numeric(SpSpTrans[,i] == "WW"))
  Mat_Res
}

Mat_Res[1:9, 1:10]

Sum_res <- as.data.frame(matrix(nrow = 9, ncol = 5))
colnames(Sum_res) <- c("Transmission", "Mean", "Median", "Low_quant", "High_quant")
Sum_res[,1] <- c("CC", "CD", "CW", "DC", "DD", "DW", "WC", "WD", "WW")

for (j in 1:nrow(Sum_res)) {
  Sum_res[j,2:5] <- c(mean(Mat_Res[j,]), median(Mat_Res[j,]), quantile(Mat_Res[j,], 0.025),
                      quantile(Mat_Res[j,], 0.975))
}

## Now to work out the percentages
Sum_res["percent_median"] <- round((Sum_res[, "Median"]/sum(Sum_res$Median))*100, 1)
Sum_res["percent_low"] <- round((Sum_res[, "Low_quant"]/sum(Sum_res$Median))*100, 1)
Sum_res["percent_high"] <- round((Sum_res[, "High_quant"]/sum(Sum_res$Median))*100, 1)

#write.csv(Sum_res, "Output/subsampling_results_100_50_WR.csv")
#write.csv(Sum_res, "Output/subsampling_results_90_50_WR.csv")
#write.csv(Sum_res, "Output/subsampling_results_75_50_WR.csv")
#write.csv(Sum_res, "Output/subsampling_results_60_50_WR.csv")

################################################################################

### If we want to look at only those transmissions within 99th percentile
### then also need to work out the SI and distance between the assigned transmissions

# Function to extract times is in the script with "Species_function"

ID_TD_mat <- apply(IDs, 2, Onset_function)
Source_TD_mat <- apply(SourceIDs, 2, Onset_function)

ID_TD_mat[1:5, 1:5]
Source_TD_mat[1:5, 1:5]

#Five_ID <- ID_TD_mat[1:5, 1:5]
#Five_source <- Source_TD_mat[1:5, 1:5]

## As they are in matrices can just subtract

Time_diff_mat <- ID_TD_mat - Source_TD_mat
Time_diff_mat[1:5, 1:5]

## Also need the distances 

ID_lat_mat <- apply(IDs, 2, Lat_function)
ID_long_mat <- apply(IDs, 2, Long_function)
ID_lat_mat[1:5, 1:5]
ID_long_mat[1:5, 1:5]

Source_lat_mat <- apply(SourceIDs, 2, Lat_function)
Source_long_mat <- apply(SourceIDs, 2, Long_function)
Source_lat_mat[1:5, 1:5]
Source_long_mat[1:5, 1:5]


Distances <- matrix(nrow = nrow(ID_lat_mat), ncol =  ncol(ID_lat_mat))

system.time(
for(i in 1:ncol(ID_lat_mat)){
  ID_vect <- cbind(ID_long_mat[,i],ID_lat_mat[,i])
  Source_vect <- cbind(Source_long_mat[,i], Source_lat_mat[,i])
  Distances[,i] <- distVincentyEllipsoid(ID_vect, Source_vect)
  Distances
}
)

Distances[1:5, 1:5]

##Set the cutoffs for the 99th percentile
SIparams <- read.csv("output/SIparams.csv")
SImean <- SIparams$SImean
SIsd <- SIparams$SIsd

# Distance parameters calculated in Contact_tracing_distance distribution
# Using 50m as cutoff for interval censoring
Distparams <- read.csv("output/Distparams.csv")
distshape <- Distparams$dist_shape
distscale <- Distparams$dist_scale

SI_cutoff <- qlnorm(0.99, meanlog = SImean, sdlog = SIsd)
BD_cutoff <- qgamma(0.99, shape = distshape*2, scale = distscale)


## Make a copy whilst working out the code

Copy_dist <- Distances

#Copy_dist[which(Copy_dist > BD_cutoff)] <- NA
#Copy_dist[1:10, 1:10]

Copy_spsp <- SpSpTrans

Copy_spsp[Copy_dist > BD_cutoff] <-NA 
Copy_spsp[1:5, 1:5]
Copy_spsp[100:110, 100:110]

Copy_Time <- Time_diff_mat
Time_diff_mat[1:5, 1:5]

Copy_spsp2 <- Copy_spsp
Copy_spsp2[Copy_Time > SI_cutoff] <- NA
Copy_spsp2[100:110, 100:110]
Copy_spsp2[1:5, 1:5]

Copy_spsp[150:160, 150:160]
Copy_spsp2[150:160, 150:160]

#write.csv(Copy_spsp2, "Subsampling_trans_within_99.csv")

Mat_Res_99 <- matrix(nrow = 9, ncol = ncol(IDs))
for (i in 1:ncol(Copy_spsp2)) {
  Mat_Res_99[1,i] <- sum(as.numeric(Copy_spsp2[,i] == "CC"), na.rm = T)
  Mat_Res_99[2,i] <- sum(as.numeric(Copy_spsp2[,i] == "CD"), na.rm = T)
  Mat_Res_99[3,i] <- sum(as.numeric(Copy_spsp2[,i] == "CW"), na.rm = T)
  Mat_Res_99[4,i] <- sum(as.numeric(Copy_spsp2[,i] == "DC"), na.rm = T)
  Mat_Res_99[5,i] <- sum(as.numeric(Copy_spsp2[,i] == "DD"), na.rm = T)
  Mat_Res_99[6,i] <- sum(as.numeric(Copy_spsp2[,i] == "DW"), na.rm = T)
  Mat_Res_99[7,i] <- sum(as.numeric(Copy_spsp2[,i] == "WC"), na.rm = T)
  Mat_Res_99[8,i] <- sum(as.numeric(Copy_spsp2[,i] == "WD"), na.rm = T)
  Mat_Res_99[9,i] <- sum(as.numeric(Copy_spsp2[,i] == "WW"), na.rm = T)
  Mat_Res_99
}


Sum_res_99 <- as.data.frame(matrix(nrow = 9, ncol = 5))
colnames(Sum_res_99) <- c("Transmission", "Mean", "Median", "Low_quant", "High_quant")
Sum_res_99[,1] <- c("CC", "CD", "CW", "DC", "DD", "DW", "WC", "WD", "WW")

for (j in 1:nrow(Sum_res_99)) {
  Sum_res_99[j,2:5] <- c(mean(Mat_Res_99[j,]), median(Mat_Res_99[j,]), quantile(Mat_Res_99[j,], 0.025),
                      quantile(Mat_Res_99[j,], 0.975))
}

## Now to work out the percentages
Sum_res_99["percent_median"] <- round((Sum_res_99[, "Median"]/sum(Sum_res_99$Median))*100, 1)
Sum_res_99["percent_low"] <- round((Sum_res_99[, "Low_quant"]/sum(Sum_res_99$Median))*100, 1)
Sum_res_99["percent_high"] <- round((Sum_res_99[, "High_quant"]/sum(Sum_res_99$Median))*100, 1)


#write.csv(Sum_res_99, "Output/subsampling_results_90_50_WR_below99.csv")
#write.csv(Sum_res_99, "Output/subsampling_results_75_50_WR_below99.csv")
#write.csv(Sum_res_99, "Output/subsampling_results_60_50_WR_below99.csv")
#write.csv(Sum_res_99, "Output/subsampling_results_100_50_WR_below99.csv")
