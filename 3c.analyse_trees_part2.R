# This code has the following properties:

# Incorporates uncertainty
# Specifies dates of possible progenitors MUST be before the dates of cases
# Uses 50m cut off for interval censoring of distance

rm(list = ls())

library(maps)
library(RColorBrewer)
library(rgdal)
library(codetools)
library(igraph)
library(prettymapr)
library(fitdistrplus)
library(tidyverse)
library(reshape2)
library(tictoc)

set.seed(4)

## Read in the relevant csv file. 
SI_res <- read.csv("output/SI_res_most_likely_PG_50m.csv")
source("Prep_trees_data_trans_trees.R")

# Bring in the data. 

## Specify the parameter distributions from the best fitting distribution that was used in the algorithm. 

## Serial Interval

SIparams <- read.csv("output/SIparams.csv")
SImean <- SIparams$SImean
SIsd <- SIparams$SIsd


# Distance parameters calculated in Contact_tracing_distance distribution
# Using 50m as cutoff for interval censoring
Distparams <- read.csv("output/Distparams.csv")

distshape <- Distparams$dist_shape
distscale <- Distparams$dist_scale

# Look at the distribution of the transmissions



#   LOOK AT ALL THE POSSIBLE PROGENITORS FOR EACH CASE 

AP_res <- read.csv("output/AP_res_allPGs_50m.csv")
All_prog_DF <- read.csv("output/All_prog_combined_df_50m.csv")

APDF <- All_prog_DF ## make a copy of APDF so not altering this inadvertantly. 
colnames(APDF) <- c("X", "ID", "biter_ID", "count", "prop") ## label the column names so they amtch those in AP-res
AP_FE <- merge(AP_res, APDF, by = c("ID", "biter_ID")) ## merge them
AP_FE$prop <- AP_FE$prop/100 ## make it decimal ather than percentage



## First save multiple tables
## I need the data in slightly different formats to undertake the tests after this bit
## So as an output I have MR which is the tables as tables. And RT which has the tables of transmission 
## as dataframes. 


# All progenitors but using a cut off for the SIs and BDs based on 95th and 99th percentile values from reference interval

# First use 95th percentile

SI_cutoff <- qlnorm(0.95, meanlog = SImean, sdlog = SIsd)
BD_cutoff <- qgamma(0.95, shape = distshape*2, scale = distscale)
SI_cutoff
BD_cutoff

## Remove from SI-res all those results with either SIs or BDs above the cutoffs

Cut_res_AP <- AP_res[which(AP_res$Serial_Int <= SI_cutoff),] 
Cut_res_AP2 <- Cut_res_AP[which(Cut_res_AP$Bite_dist <= BD_cutoff),]

## Use df to use the appropriate data set
df <- Cut_res_AP2

length(unique(df$ID))

# As previously some IDs still have multiple entries so use tables with one biter for each case

## Will use bootstrapping to incorporate the uncertainty. 
## Select cases with replacement
## Select a biter for each case weighted by the proportion of times it was selected by the algorithm.
## Do this 1000 times and find the median and CI from these estimates

Cut_APDF <- All_prog_DF ## make a copy of APDF so not altering this inadvertantly. 
Cut_APDF2 <- Cut_APDF[which(APDF$ID %in% Cut_res_AP2$ID),] 
colnames(Cut_APDF2) <- c("X", "ID", "biter_ID", "count", "prop") ## label the column names so they amtch those in AP-res
Cut_AP_FE <- merge(Cut_res_AP2, Cut_APDF2, by = c("ID", "biter_ID")) ## merge them
Cut_AP_FE$prop <- Cut_AP_FE$prop/100 ## make it decimal ather than percentage

case_numbers <- unique(Cut_AP_FE$ID)

# set up a dataframe to use within the loop 
full_df <- as.data.frame(matrix(ncol = 2, nrow = 9))
colnames(full_df) <- c("Var1", "Var2")
full_df[,1] <- c(1,2,4,1,2,4,1,2,4)
full_df[,2] <- c(1,1,1,2,2,2,4,4,4)
full_df$Var1 <-as.factor(full_df$Var1)
full_df$Var2 <- as.factor(full_df$Var2)

MR_AP <- list()
RT_AP <- list()
tic()
for (k in 1:1000) {
 nums <- sample(case_numbers, replace = T)
 new_df <- as.data.frame(matrix(nrow = length(nums), ncol = ncol(Cut_AP_FE)))
 colnames(new_df) <- colnames(Cut_AP_FE)
 for (i in 1:length(nums)) {
   new_df[i,] <- Cut_AP_FE[which(Cut_AP_FE$ID == nums[i]),] %>% group_by(ID) %>% slice_sample(n = 1, weight_by = prop, replace = T)
  Tab <- table(new_df$Biter_species, new_df$ID_species) # with a table - x becomes rows and y columns. So biter is row and case is column
  Tabdf <- as.data.frame(Tab)  # x becomes column 1 (Var1) and y column 2 (Var2)
  Tdf <- merge(full_df, Tabdf, all.x = T) # merge with the pre-prepared df so all transmission types are present
  Tdf[which(is.na(Tdf$Freq)), "Freq"] = 0  # add 0s if any NAs.
 }
  MR_AP[[k]] <- Tab   ### Saving each into a list 
  RT_AP[[k]] <- Tdf
  Out <- c(MR_AP, RT_AP)
}
toc()


### then just apply the fisher test function across all the tables. 
Pvals <- lapply(MR_AP, fisher.test)
## And select just the p-value from the results of the Fisher test
Only_pvals <- sapply(Pvals, function(x) x["p.value"]) ## Results in a list
Pvals_vector <- unlist(sapply(Pvals, function(x) x["p.value"])) ## Results in a vector
mean(Pvals_vector)
range(Pvals_vector)

# Find mean of all the different types of transmission


Row_trans <- sapply(RT_AP, '[[', 3) 

Row_trans[,1]
RT_AP[[1]] # use this to double check the order that the transmissions occur in. 1 = dog, 2 = wildlife, 4 = cat
rownames(Row_trans) <- c("DD", "DW", "DC", "WD", "WW", "WC", "CD", "CW", "CC") # This is the order of the transmissions if we look at 

Mean_tab_95 <- as.data.frame(matrix(ncol = 2, nrow = 9))
colnames(Mean_tab_95) <- c("trans", "freq")
Mean_tab_95[,"trans"] <- c("DD", "WD", "CD", "DW", "WW", "CW", "DC", "WC", "CC")
Mean_tab_95$trans <- as.factor(Mean_tab_95$trans)

RTtrans <- t(Row_trans) ## transpose row trans to make use of column names

Mean_tab_95[which(Mean_tab_95$trans == "DD"), "freq"] <- mean(RTtrans[,"DD"])
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "freq"] <- mean(RTtrans[,"WD"])
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "freq"] <- mean(RTtrans[,"CD"])
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "freq"] <- mean(RTtrans[,"DW"])
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "freq"] <- mean(RTtrans[,"WW"])
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "freq"] <- mean(RTtrans[,"CW"])
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "freq"] <- mean(RTtrans[,"DC"])
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "freq"] <- mean(RTtrans[,"WC"])
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "freq"] <- mean(RTtrans[,"CC"])

sum(Mean_tab_95[,"freq"])
Mean_tab_95["prop"] <- Mean_tab_95[,"freq"]/(sum(Mean_tab_95[,"freq"]))*100
sum(Mean_tab_95[,"prop"])

Mean_tab_95["median"] <- NA
Mean_tab_95[which(Mean_tab_95$trans == "DD"), "median"] <- median(RTtrans[,"DD"])
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "median"] <- median(RTtrans[,"WD"])
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "median"] <- median(RTtrans[,"CD"])
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "median"] <- median(RTtrans[,"DW"])
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "median"] <- median(RTtrans[,"WW"])
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "median"] <- median(RTtrans[,"CW"])
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "median"] <- median(RTtrans[,"DC"])
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "median"] <- median(RTtrans[,"WC"])
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "median"] <- median(RTtrans[,"CC"])


Mean_tab_95[which(Mean_tab_95$trans == "DD"), "lower_95CI"] <- quantile(RTtrans[,"DD"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "lower_95CI"] <- quantile(RTtrans[,"WD"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "lower_95CI"] <- quantile(RTtrans[,"CD"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "lower_95CI"] <- quantile(RTtrans[,"DW"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "lower_95CI"] <- quantile(RTtrans[,"WW"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "lower_95CI"] <- quantile(RTtrans[,"CW"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "lower_95CI"] <- quantile(RTtrans[,"DC"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "lower_95CI"] <- quantile(RTtrans[,"WC"], 0.025)
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "lower_95CI"] <- quantile(RTtrans[,"CC"], 0.025)


Mean_tab_95[which(Mean_tab_95$trans == "DD"), "upper_95CI"] <- quantile(RTtrans[,"DD"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "upper_95CI"] <- quantile(RTtrans[,"WD"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "upper_95CI"] <- quantile(RTtrans[,"CD"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "upper_95CI"] <- quantile(RTtrans[,"DW"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "upper_95CI"] <- quantile(RTtrans[,"WW"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "upper_95CI"] <- quantile(RTtrans[,"CW"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "upper_95CI"] <- quantile(RTtrans[,"DC"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "upper_95CI"] <- quantile(RTtrans[,"WC"], 0.975)
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "upper_95CI"] <- quantile(RTtrans[,"CC"], 0.975)

## As another way of getting a CI I need to order the percentages and then pick ## the 25th and 975th estimate
CI_fun_AP <- function(k){
  Output <- c()
  for (i in 1:length(MR_AP)) {
    Output[i] <- RT_AP[[i]][k,3]
  }
  Output <- sort(Output)
  Output
}

DD_order <- CI_fun_AP(1)
DW_order <- CI_fun_AP(2)
DC_order <- CI_fun_AP(3)
WD_order <- CI_fun_AP(4)
WW_order <- CI_fun_AP(5)
WC_order <- CI_fun_AP(6)
CD_order <- CI_fun_AP(7)
CW_order <- CI_fun_AP(8)
CC_order <- CI_fun_AP(9)

## Now select the 25th and 975th from each. 
range(DD_order)
DD_order <- sort(DD_order)

Find_95CI <- function(DF){
  sorted <- sort(DF)
  res <- sorted[c(25,975)]
  res
}
Mean_tab_95[which(Mean_tab_95$trans == "DD"), "ordered_25"] <- DD_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "ordered_25"] <- WD_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "ordered_25"] <- CD_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "ordered_25"] <- DW_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "ordered_25"] <- WW_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "ordered_25"] <- CW_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "ordered_25"] <- DC_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "ordered_25"] <- WC_order[25]
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "ordered_25"] <- CC_order[25]

Mean_tab_95[which(Mean_tab_95$trans == "DD"), "ordered_975"] <- DD_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "WD"), "ordered_975"] <- WD_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "CD"), "ordered_975"] <- CD_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "DW"), "ordered_975"] <- DW_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "WW"), "ordered_975"] <- WW_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "CW"), "ordered_975"] <- CW_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "DC"), "ordered_975"] <- DC_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "WC"), "ordered_975"] <- WC_order[975]
Mean_tab_95[which(Mean_tab_95$trans == "CC"), "ordered_975"] <- CC_order[975]
Mean_tab_95["Percent"] <- Mean_tab_95[,"freq"]/(sum(Mean_tab_95[,"freq"]))*100
Mean_tab_95["Percent_median"] <-
  Mean_tab_95[,"median"]/(sum(Mean_tab_95[,"freq"]))*100
Mean_tab_95["Percent_lower"] <-
  Mean_tab_95[,"lower_95CI"]/(sum(Mean_tab_95[,"freq"]))*100
Mean_tab_95["Percent_upper"] <-
  Mean_tab_95[,"upper_95CI"]/(sum(Mean_tab_95[,"freq"]))*100
Mean_tab_95["Order_lower"] <-
  Mean_tab_95[,"ordered_25"]/(sum(Mean_tab_95[,"freq"]))*100
Mean_tab_95["Order_upper"] <-
  Mean_tab_95[,"ordered_975"]/(sum(Mean_tab_95[,"freq"]))*100


# Next looks at the fisher test for these results

Mean_tab_952 <- as.data.frame(matrix(ncol = 3, nrow = 3))

colnames(Mean_tab_952) <- c("D", "W", "C")
row.names(Mean_tab_952) <- c("D", "W", "C")

Mean_tab_952["D","D"] <- mean(RTtrans[,"DD"])
Mean_tab_952["W","D"] <- mean(RTtrans[,"WD"])
Mean_tab_952["C","D"] <- mean(RTtrans[,"CD"])
Mean_tab_952["D","W"] <- mean(RTtrans[,"DW"])
Mean_tab_952["W","W"] <- mean(RTtrans[,"WW"])
Mean_tab_952["C","W"] <- mean(RTtrans[,"CW"])
Mean_tab_952["D","C"] <- mean(RTtrans[,"DC"])
Mean_tab_952["W","C"] <- mean(RTtrans[,"WC"])
Mean_tab_952["C","C"] <- mean(RTtrans[,"CC"])
fisher.test(Mean_tab_952)

#################################################################
### Then use 99th percentile


SI_cutoff <- qlnorm(0.99, meanlog = SImean, sdlog = SIsd)                
BD_cutoff <- qgamma(0.99, shape = distshape*2, scale = distscale)
SI_cutoff
BD_cutoff

### Remove from SI-res all those results with either SIs or BDs above the cutoffs

Cut_res_AP <- AP_res[which(AP_res$Serial_Int <= SI_cutoff),] 
Cut_res_AP2 <- Cut_res_AP[which(Cut_res_AP$Bite_dist <= BD_cutoff),]

## Use df to use the appropriate data set
df <- Cut_res_AP2

#As previously some IDs still have multiple entries so use tables with one biter for each case


Cut_APDF <- All_prog_DF ## make a copy of APDF so not altering this inadvertently. 
Cut_APDF2 <- Cut_APDF[which(APDF$ID %in% Cut_res_AP2$ID),] 
colnames(Cut_APDF2) <- c("X", "ID", "biter_ID", "count", "prop") ## label the column names so they match those in AP-res
Cut_AP_FE <- merge(Cut_res_AP2, Cut_APDF2, by = c("ID", "biter_ID")) ## merge them
Cut_AP_FE$prop <- Cut_AP_FE$prop/100 ## make it decimal ather than percentage

case_numbers <- unique(Cut_AP_FE$ID)

# set up a dataframe to use within the loop 
full_df <- as.data.frame(matrix(ncol = 2, nrow = 9))
colnames(full_df) <- c("Var1", "Var2")
full_df[,1] <- c(1,2,4,1,2,4,1,2,4)
full_df[,2] <- c(1,1,1,2,2,2,4,4,4)
full_df$Var1 <-as.factor(full_df$Var1)
full_df$Var2 <- as.factor(full_df$Var2)

MR_AP <- list()
RT_AP <- list()
tic()
for (k in 1:1000) {
  nums <- sample(case_numbers, replace = T)
  new_df <- as.data.frame(matrix(nrow = length(nums), ncol = ncol(Cut_AP_FE)))
  colnames(new_df) <- colnames(Cut_AP_FE)
  for (i in 1:length(nums)) {
    new_df[i,] <- Cut_AP_FE[which(Cut_AP_FE$ID == nums[i]),] %>% group_by(ID) %>% slice_sample(n = 1, weight_by = prop, replace = T)
    Tab <- table(new_df$Biter_species, new_df$ID_species) # with a table - x becomes rows and y columns. So biter is row and case is column
    Tabdf <- as.data.frame(Tab)  # x becomes column 1 (Var1) and y column 2 (Var2)
    Tdf <- merge(full_df, Tabdf, all.x = T) # merge with the pre-prepared df so all transmission types are present
    Tdf[which(is.na(Tdf$Freq)), "Freq"] = 0  # add 0s if any NAs.
  }
  MR_AP[[k]] <- Tab   ### Saving each into a list 
  RT_AP[[k]] <- Tdf
  Out <- c(MR_AP, RT_AP)
}
toc()



### then just apply the fisher test function across all the tables. 
Pvals <- lapply(MR_AP, fisher.test)
## And select just the p-value from the results of the Fisher test
Only_pvals <- sapply(Pvals, function(x) x["p.value"]) ## Results in a list
Pvals_vector <- unlist(sapply(Pvals, function(x) x["p.value"])) ## Results in a vector
mean(Pvals_vector)
range(Pvals_vector)

# Find mean of all the different types of transmission
Row_trans <- sapply(RT_AP, '[[', 3) 

Row_trans[,1]
RT_AP[[1]] # use this to double check the order that the transmissions occur in. 1 = dog, 2 = wildlife, 4 = cat
rownames(Row_trans) <- c("DD", "DW", "DC", "WD", "WW", "WC", "CD", "CW", "CC") # This is the order of the transmissions if we look at 

Mean_tab_99 <- as.data.frame(matrix(ncol = 2, nrow = 9))
colnames(Mean_tab_99) <- c("trans", "freq")
Mean_tab_99[,"trans"] <- c("DD", "WD", "CD", "DW", "WW", "CW", "DC", "WC", "CC")
Mean_tab_99$trans <- as.factor(Mean_tab_99$trans)


RTtrans <- t(Row_trans) ## transpose row trans to make use of column names
Mean_tab_99[which(Mean_tab_99$trans == "DD"), "freq"] <- mean(RTtrans[,"DD"])
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "freq"] <- mean(RTtrans[,"WD"])
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "freq"] <- mean(RTtrans[,"CD"])
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "freq"] <- mean(RTtrans[,"DW"])
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "freq"] <- mean(RTtrans[,"WW"])
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "freq"] <- mean(RTtrans[,"CW"])
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "freq"] <- mean(RTtrans[,"DC"])
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "freq"] <- mean(RTtrans[,"WC"])
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "freq"] <- mean(RTtrans[,"CC"])

sum(Mean_tab_99[,"freq"])
Mean_tab_99["prop"] <- Mean_tab_99[,"freq"]/(sum(Mean_tab_99[,"freq"]))*100
sum(Mean_tab_99[,"prop"])
Mean_tab_99["median"] <- NA
Mean_tab_99[which(Mean_tab_99$trans == "DD"), "median"] <- median(RTtrans[,"DD"])
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "median"] <- median(RTtrans[,"WD"])
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "median"] <- median(RTtrans[,"CD"])
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "median"] <- median(RTtrans[,"DW"])
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "median"] <- median(RTtrans[,"WW"])
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "median"] <- median(RTtrans[,"CW"])
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "median"] <- median(RTtrans[,"DC"])
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "median"] <- median(RTtrans[,"WC"])
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "median"] <- median(RTtrans[,"CC"])

Mean_tab_99[which(Mean_tab_99$trans == "DD"), "lower_95CI"] <- quantile(RTtrans[,"DD"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "lower_95CI"] <- quantile(RTtrans[,"WD"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "lower_95CI"] <- quantile(RTtrans[,"CD"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "lower_95CI"] <- quantile(RTtrans[,"DW"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "lower_95CI"] <- quantile(RTtrans[,"WW"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "lower_95CI"] <- quantile(RTtrans[,"CW"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "lower_95CI"] <- quantile(RTtrans[,"DC"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "lower_95CI"] <- quantile(RTtrans[,"WC"], 0.025)
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "lower_95CI"] <- quantile(RTtrans[,"CC"], 0.025)


Mean_tab_99[which(Mean_tab_99$trans == "DD"), "upper_95CI"] <- quantile(RTtrans[,"DD"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "upper_95CI"] <- quantile(RTtrans[,"WD"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "upper_95CI"] <- quantile(RTtrans[,"CD"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "upper_95CI"] <- quantile(RTtrans[,"DW"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "upper_95CI"] <- quantile(RTtrans[,"WW"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "upper_95CI"] <- quantile(RTtrans[,"CW"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "upper_95CI"] <- quantile(RTtrans[,"DC"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "upper_95CI"] <- quantile(RTtrans[,"WC"], 0.975)
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "upper_95CI"] <- quantile(RTtrans[,"CC"], 0.975)                                                                       
## As another way of getting a CI I need to order the percentages and then pick ## the 25th and 975th estimate
CI_fun_AP <- function(k){
  Output <- c()
  for (i in 1:length(MR_AP)) {
    Output[i] <- RT_AP[[i]][k,3]
  }
  Output <- sort(Output)
  Output
}


DD_order <- CI_fun_AP(1)
DW_order <- CI_fun_AP(2)
DC_order <- CI_fun_AP(3)
WD_order <- CI_fun_AP(4)
WW_order <- CI_fun_AP(5)
WC_order <- CI_fun_AP(6)
CD_order <- CI_fun_AP(7)
CW_order <- CI_fun_AP(8)
CC_order <- CI_fun_AP(9)


## Now select the 25th and 975th from each. 
range(DD_order)
DD_order <- sort(DD_order)

Find_95CI <- function(DF){
  sorted <- sort(DF)
  res <- sorted[c(25,975)]
  res
}
Mean_tab_99[which(Mean_tab_99$trans == "DD"), "ordered_25"] <- DD_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "ordered_25"] <- WD_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "ordered_25"] <- CD_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "ordered_25"] <- DW_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "ordered_25"] <- WW_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "ordered_25"] <- CW_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "ordered_25"] <- DC_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "ordered_25"] <- WC_order[25]
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "ordered_25"] <- CC_order[25]

Mean_tab_99[which(Mean_tab_99$trans == "DD"), "ordered_975"] <- DD_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "WD"), "ordered_975"] <- WD_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "CD"), "ordered_975"] <- CD_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "DW"), "ordered_975"] <- DW_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "WW"), "ordered_975"] <- WW_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "CW"), "ordered_975"] <- CW_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "DC"), "ordered_975"] <- DC_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "WC"), "ordered_975"] <- WC_order[975]
Mean_tab_99[which(Mean_tab_99$trans == "CC"), "ordered_975"] <- CC_order[975]
Mean_tab_99["Percent"] <- Mean_tab_99[,"freq"]/(sum(Mean_tab_99[,"freq"]))*100
Mean_tab_99["Percent_median"] <-
  Mean_tab_99[,"median"]/(sum(Mean_tab_99[,"freq"]))*100
Mean_tab_99["Percent_lower"] <-
  Mean_tab_99[,"lower_95CI"]/(sum(Mean_tab_99[,"freq"]))*100
Mean_tab_99["Percent_upper"] <-
  Mean_tab_99[,"upper_95CI"]/(sum(Mean_tab_99[,"freq"]))*100
Mean_tab_99["Order_lower"] <-
  Mean_tab_99[,"ordered_25"]/(sum(Mean_tab_99[,"freq"]))*100
Mean_tab_99["Order_upper"] <-
  Mean_tab_99[,"ordered_975"]/(sum(Mean_tab_99[,"freq"]))*100
                              

# Fishers exact test

Mean_tab_992 <- as.data.frame(matrix(ncol = 3, nrow = 3))
colnames(Mean_tab_992) <- c("D", "W", "C")
row.names(Mean_tab_992) <- c("D", "W", "C")
Mean_tab_992["D","D"] <- mean(RTtrans[,"DD"])
Mean_tab_992["W","D"] <- mean(RTtrans[,"WD"])
Mean_tab_992["C","D"] <- mean(RTtrans[,"CD"])
Mean_tab_992["D","W"] <- mean(RTtrans[,"DW"])
Mean_tab_992["W","W"] <- mean(RTtrans[,"WW"])
Mean_tab_992["C","W"] <- mean(RTtrans[,"CW"])
Mean_tab_992["D","C"] <- mean(RTtrans[,"DC"])
Mean_tab_992["W","C"] <- mean(RTtrans[,"WC"])
Mean_tab_992["C","C"] <- mean(RTtrans[,"CC"])
fisher.test(Mean_tab_992)


