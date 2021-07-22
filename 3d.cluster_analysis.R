
## This code is providing information regarding the cluster sizes
## that are generated and the composition of those clusters. 

rm(list = ls())

library(maptools)
library(maps)
library(RColorBrewer)
library(rgdal)
library(codetools)
library(igraph)
library(prettymapr)
library(fitdistrplus)
library(dplyr)
library(ggplot2)
library(grDevices)

#dev.off()
set.seed(4)

## Read in the relevant csv file. 
SI_res <- read.csv("output/SI_res_most_likely_PG_50m.csv")
source("R/Prep_trees_data_trans_trees.R")

##group the data
table(rabid$Species)
rabid$Species[grep("Wildlife", rabid$Species)] <- "Wildlife"
rabid$Species[grep("Domestic dog", rabid$Species)] <- "Dog"
table(rabid$Species)

## change the species to a number
rabid[which(rabid$Species == "Dog"), "Species"] <- 1
rabid[which(rabid$Species == "Wildlife"), "Species"] <- 2
rabid[which(rabid$Species == "Cat"), "Species"] <- 4

## Bring in the parameters
SIparams <- read.csv("output/SIparams.csv")
SImean <- SIparams$SImean
SIsd <- SIparams$SIsd

# Distance parameters calculated in Contact_tracing_distance distribution
# Using 50m as cutoff for interval censoring
Distparams <- read.csv("output/Distparams.csv")

distshape <- Distparams$dist_shape
distscale <- Distparams$dist_scale



##########################################################
## Use igraph as a tool for identifying the clusters.

# Examine only transmissions with SIs and BDs below the value of 95th and 99th  percentile from the reference distributions from the Serengeti data. Currently using the cutoff for distance as two convolutions

# First lets use 95th percentile

SI_cutoff_95 <- qlnorm(0.95, meanlog = SImean, sdlog = SIsd)
BD_cutoff_95 <- qgamma(0.95, shape = distshape*2, scale = distscale)

### Remove from SI-res all those results with either SIs or BDs above the cutoffs

Cut_res <- SI_res[which(SI_res$Serial_Int <= SI_cutoff_95),] 
Cut_res2 <- Cut_res[which(Cut_res$Bite_dist <= BD_cutoff_95),]

# Prep the data for igraph

# Make a smaller version of rabid which just has the IDs and biter IDs from our reduced DF
Cut_rabid <- rabid[which(rabid$ID %in% Cut_res2$ID | rabid$ID %in% Cut_res2$biter_ID),]

## The set up the Rabnet object  
Cont_list <- Cut_res2[, c("ID", "biter_ID")]
Cont_list$ID <- as.character(Cont_list$ID)
Cont_list$biter_ID <- as.character(Cont_list$biter_ID)
colnames(Cont_list) <- c("to", "from")
Line_list <- Cut_rabid
Line_list$ID <- as.character(Line_list$ID)
Line_list <- Line_list[,c("ID", "Species","Latitude", "Longitude")]
Line_list$Species <- as.numeric(Line_list$Species)

Rabnet <- graph_from_data_frame(d= Cont_list, vertices=Line_list, directed=T) 

colrs <- c("red", "blue", "black", "gold") ## Our animals are 1,2 and 4. Livestock is 3 but there
V(Rabnet)$color <- colrs[V(Rabnet)$Species]
V(Rabnet)$size <- 4
V(Rabnet)$label.color <- "black"
V(Rabnet)$label <- NA
E(Rabnet)$arrow.size <- .2
E(Rabnet)$edge.color <- "black"
E(Rabnet)$arrow.mode <- 1 ## this sets the direction of the arrows. ## 1 is backward arrow

par(mar = c(6,3,3,3))
plot(Rabnet)
## Tweak the legend to miss out livestock as we don't actually have any.
legend(x=-1.1, y=-1.1, c("Dog","Wildlife", "Cat"), pch=21,
       col="#777777", pt.bg=colrs[c(1,2,4)], pt.cex=1.5, bty="n", ncol=1, cex = 0.6)

## Identify the clusters
Clust_no <- clusters(Rabnet)$membership
table(Clust_no) # shows how may entries are in each cluster


IG_res <- SI_res # make a copy of the dataframe - this still includes all the results
IG_res <- SI_res[which(SI_res$ID %in% Cut_res2$ID | SI_res$ID %in% Cut_res2$biter_ID),]
# remove the ones that don't appear in cut_res

IG_res["Cluster_No"] <- Clust_no   #add a column for cluster number

## Cut down to those within the cut-offs
IG_res_95 <- IG_res[which(IG_res$Serial_Int <= SI_cutoff_95 & 
                            IG_res$Bite_dist <= BD_cutoff_95),]


### NOW EXTRACT THE INDIVIDUAL CLUSTERS AND WORK OUT HOW MANY ARE MADE UP OF ALL DOG; 
### ALL WILDLIFE OR MIXTURE

ID_try_small <- IG_res_95[,c("Species_trans", "Cluster_No")]

SPSP_Cluster <- as.data.frame(table(ID_try_small$Cluster_No, ID_try_small$Species_trans))
SPSP_Cluster <- reshape(SPSP_Cluster, idvar = "Var1", timevar = "Var2", direction = "wide")

SPSP_Cluster[,"Total"] <- rowSums(SPSP_Cluster[,2:7])

sum(SPSP_Cluster[,"Total"])

## Still have 165 entries as expected

SPSP_Cluster<- SPSP_Cluster[, -1]

## choose the clusters that only contain dogs or wildlife
## do this by choosing the ones where that single type of transmission adds up to the total
DD_only <- SPSP_Cluster[which(SPSP_Cluster$Freq.DD == SPSP_Cluster$Total),]
WW_only <- SPSP_Cluster[which(SPSP_Cluster$Freq.WW == SPSP_Cluster$Total),]

## And then those that are mixed - DW and WD and WC 

Mixed <- SPSP_Cluster[which(SPSP_Cluster$Freq.DD != SPSP_Cluster$Total &
                              SPSP_Cluster$Freq.WW!= SPSP_Cluster$Total),]

## Check we have got the number we expect
nrow(DD_only)
nrow(WW_only)
nrow(Mixed)

nrow(DD_only) + nrow(WW_only) + nrow(Mixed)
nrow(SPSP_Cluster)

sum(DD_only[,"Total"])
sum(WW_only[,"Total"])
sum(Mixed[,"Total"])

##Then see how many of each cluster size 
dfDD <- DD_only %>%
  group_by(Total) %>%
  summarise(counts = n())
dfDD

dfWW <- WW_only %>%
  group_by(Total) %>%
  summarise(counts = n())
dfWW

dfmixed <- Mixed %>%
  group_by(Total) %>%
  summarise(counts = n())
dfmixed


## Add a column for the type of transmission and then combine to form a data frame
## containing all the results
dfDD["Trans"] <- "DD"
DD_only["Trans"] <- "DD"
dfWW["Trans"] <- "WW"
WW_only["Trans"] <- "WW"
dfDDWW <- rbind(dfDD, dfWW)
dfDDWW

DDWW <- rbind((DD_only[,c("Total", "Trans")]), (WW_only[,c("Total", "Trans")]))

## Put this info into a dataframe

######
BPtableDD <- as.data.frame(matrix(ncol = 1, nrow = 7))
colnames(BPtableDD) <- c("Total")#, "counts")
BPtableDD$Total <- c(1,2,3,4,5,6,7)
BPDD <-merge(BPtableDD, dfDD, by = "Total", all.x= TRUE)
BPDD$Trans <- "DD"

BPtableWW <- as.data.frame(matrix(ncol = 1, nrow = 7))
colnames(BPtableWW) <- c("Total")#, "counts")
BPtableWW$Total <- c(1,2,3,4,5,6,7)
BPWW <-merge(BPtableWW, dfWW, by = "Total", all.x= TRUE)
BPWW$Trans <- "WW"

BPtablemixed <- as.data.frame(matrix(ncol = 1, nrow = 7))
colnames(BPtablemixed) <- c("Total")#, "counts")
BPtablemixed$Total <- c(1,2,3,4,5,6,7)
BPmixed <-merge(BPtablemixed, dfmixed, by = "Total", all.x= TRUE)
BPmixed["Trans"] <- "mixed"

### Now try and do a grouped bar plot

Group_trans <- rbind(BPDD, BPWW, BPmixed)
Group_trans[is.na(Group_trans)]<-0
## The numbers in total refer to the transmissions. This is one fewer than the number
## of animals in the cluster. i.e - a single transmission requires 2 animals. 
## So add one to the totals columns
Group_trans$Total <- Group_trans$Total +1


own_cols <- c("steelblue1", "steelblue3", "steelblue4")

ggplot(Group_trans, aes(factor(Total), counts, fill = Trans)) + 
  geom_bar(stat="identity", position = "dodge", colour = "black") + 
  scale_fill_manual(values = own_cols, name = "Type of transmission",
                    #guide = guide_legend(reverse=TRUE), 
                    labels = c("Dog-dog", "Mixture of all species", "Wildlife-wildlife")) +
  theme_classic() +   theme(axis.title.x = element_text(size=14, face="bold"), 
                            axis.title.y = element_text(size=14, face="bold"), 
                            axis.text.x  = element_text(size = 12, face = "bold"), 
                            axis.text.y = element_text(size = 12, face = "bold"),
                            legend.text = element_text(size = 12, face = "bold"), 
                            legend.title = element_text(size = 12, face = "bold"),
                            legend.position = "right" ) + #c(0.8,0.7)) +
  labs(title = "", 
       y = "Frequency", x = "Cluster size") + 
  scale_x_discrete(labels = c("2", "3", "4", "5", "6", "7", "8"))

##########################################################################
##########################################################################

# Now lets use 99th percentile

SI_cutoff_99 <- qlnorm(0.99, meanlog = SImean, sdlog = SIsd)
BD_cutoff_99 <- qgamma(0.99, shape = distshape*2, scale = distscale)

### Remove from SI-res all those results with either SIs or BDs above the cutoffs

## Use one of the below depending on which data set decide to use
Cut_res <- SI_res[which(SI_res$Serial_Int <= SI_cutoff_99),] ## Goes from 518 to 324 in this case

Cut_res2 <- Cut_res[which(Cut_res$Bite_dist <= BD_cutoff_99),]


# Plot the links on a map using igraph

# Make a smaller version of rabid which just has the IDs and biter IDs from our reduced DF
Cut_rabid <- rabid[which(rabid$ID %in% Cut_res2$ID | rabid$ID %in% Cut_res2$biter_ID),]

## The set up the Rabnet object again 
Cont_list <- Cut_res2[, c("ID", "biter_ID")]
Cont_list$ID <- as.character(Cont_list$ID)
Cont_list$biter_ID <- as.character(Cont_list$biter_ID)
colnames(Cont_list) <- c("to", "from")
Line_list <- Cut_rabid
Line_list$ID <- as.character(Line_list$ID)
Line_list <- Line_list[,c("ID", "Species","Latitude", "Longitude")]
Line_list$Species <- as.numeric(Line_list$Species)

Rabnet <- graph_from_data_frame(d= Cont_list, vertices=Line_list, directed=T) 

colrs <- c("red", "blue", "black", "gold") ## Our animals are 1,2 and 4. Livestock is 3 but there
V(Rabnet)$color <- colrs[V(Rabnet)$Species]
V(Rabnet)$size <- 4
V(Rabnet)$label.color <- "black"
V(Rabnet)$label <- NA
E(Rabnet)$arrow.size <- .2
E(Rabnet)$edge.color <- "black"
E(Rabnet)$arrow.mode <- 1 ## this sets the direction of the arrows. 

par(mar = c(6,3,3,3))
plot(Rabnet)
legend(x=-1.1, y=-1.1, c("Dog","Wildlife", "Cat"), pch=21,
       col="#777777", pt.bg=colrs[c(1,2,4)], pt.cex=1.5, bty="n", ncol=1, cex = 0.6)
source("R/Fig_label.R")
fig_label("A", region="figure", pos="topleft", cex=1.2, font.main = 4) 


#########################################################################
#########################################################################

## Identify the clusters
Clust_no <- clusters(Rabnet)$membership
table(Clust_no) # shows how may entries are in each cluster

IG_res <- rabid[which(rabid$ID %in% Cut_res2$ID | rabid$ID %in% Cut_res2$biter_ID),]

IG_res["Cluster_No"] <- Clust_no   #add a column for cluster number

## Cut down to those within the cut-offs

## Next we want the SI_res results which have IDs within the IG_res
Next_Step <- merge(IG_res, SI_res, by = "ID")

IG_res_99 <- Next_Step[which(Next_Step$Serial_Int <= SI_cutoff_99 & 
                               Next_Step$Bite_dist <= BD_cutoff_99),]


### NOW I NEED TO EXTRACT THE INDIVIDUAL CLUSTERS AND WORK OUT HOW MANY ARE MADE UP OF ALL DOG; 
### ALL WILDLIFE OR MIXTURE

ID_try_small <- IG_res_99[,c("Species_trans", "Cluster_No")]

SPSP_Cluster <- as.data.frame(table(ID_try_small$Cluster_No, ID_try_small$Species_trans))
SPSP_Cluster <- reshape(SPSP_Cluster, idvar = "Var1", timevar = "Var2", direction = "wide")

SPSP_Cluster[,"Total"] <- rowSums(SPSP_Cluster[,2:9])

#SPSP_Cluster<- SPSP_Cluster[, -1]

DD_only <- SPSP_Cluster[which(SPSP_Cluster$Freq.DD == SPSP_Cluster$Total),]
WW_only <- SPSP_Cluster[which(SPSP_Cluster$Freq.WW == SPSP_Cluster$Total),]

## And then those that are mixed - DW and WD and WC 

Mixed <- SPSP_Cluster[which(SPSP_Cluster$Freq.DD != SPSP_Cluster$Total &
                              SPSP_Cluster$Freq.WW!= SPSP_Cluster$Total),]

## Check we have got the number we expect
nrow(DD_only)
nrow(WW_only)
nrow(Mixed)
nrow(DD_only) + nrow(WW_only) + nrow(Mixed) 
nrow(SPSP_Cluster)


dfDD <- DD_only %>%
  group_by(Total) %>%
  summarise(counts = n())
dfDD

dfWW <- WW_only %>%
  group_by(Total) %>%
  summarise(counts = n())
dfWW

dfmixed <- Mixed %>%
  group_by(Total) %>%
  summarise(counts = n())
dfmixed


dfDD["Trans"] <- "DD"
DD_only["Trans"] <- "DD"
dfWW["Trans"] <- "WW"
WW_only["Trans"] <- "WW"
dfDDWW <- rbind(dfDD, dfWW)
dfDDWW

DDWW <- rbind((DD_only[,c("Total", "Trans")]), (WW_only[,c("Total", "Trans")]))
#DD_only


######
BPtableDD <- as.data.frame(matrix(ncol = 1, nrow = 12))
colnames(BPtableDD) <- c("Total")#, "counts")
BPtableDD$Total <- c(1,2,3,4,5,6,7,8,9,10,11,12)
BPDD <-merge(BPtableDD, dfDD, by = "Total", all.x= TRUE)
BPDD$Trans <- "DD"

BPtableWW <- as.data.frame(matrix(ncol = 1, nrow = 12))
colnames(BPtableWW) <- c("Total")#, "counts")
BPtableWW$Total <- c(1,2,3,4,5,6,7,8,9,10,11,12)
BPWW <-merge(BPtableWW, dfWW, by = "Total", all.x= TRUE)
BPWW$Trans <- "WW"

BPtablemixed <- as.data.frame(matrix(ncol = 1, nrow = 12))
colnames(BPtablemixed) <- c("Total")#, "counts")
BPtablemixed$Total <- c(1,2,3,4,5,6,7,8,9,10,11,12)
BPmixed <-merge(BPtablemixed, dfmixed, by = "Total", all.x= TRUE)
BPmixed["Trans"] <- "mixed"

### Now do a grouped bar plot

Group_trans <- rbind(BPDD, BPWW, BPmixed)
Group_trans[is.na(Group_trans)]<-0

Group_trans$Total <- Group_trans$Total+1

own_cols <- c("red", "mediumorchid", "blue")
own_cols <- c("firebrick2", "palevioletred1", "dodgerblue2")

#source("R/Fig_label.R")

cluster_plot <- ggplot(Group_trans, aes(factor(Total), counts, fill = Trans)) + 
  geom_bar(stat="identity", position = "dodge", colour = "black") + labs(tag = "B") +
  scale_fill_manual(values = own_cols, name = "Type of transmission",
                    #guide = guide_legend(reverse=TRUE), 
                    labels = c("Dog-dog", "Mixture of all species", "Wildlife-wildlife")) +
  theme_classic() +   theme(axis.title.x = element_text(size=14, face="bold"), 
                            axis.title.y = element_text(size=14, face="bold"), 
                            axis.text.x  = element_text(size = 12, face = "bold"), 
                            axis.text.y = element_text(size = 12, face = "bold"),
                            legend.text = element_text(size = 12, face = "bold"), 
                            legend.title = element_text(size = 12, face = "bold"),
                            legend.position = c(0.7,0.8)) +
  #legend.position = "right" ) + #c(0.8,0.7)) +
  labs(title = "", 
       y = "Frequency", x = "Cluster size") + 
  scale_x_discrete(labels = c("2", "3", "4", "5", "6", "7", "8", "9", "10",
                              "11", "12", "13") )


cluster_plot

###########################################################################################
## Weighted linear regression

library(dplyr)
library(tidyverse)
library(MASS)

IGR <- IG_res

# Looking at how the average chain/unit length differs over the study period.

IGR2 <- IGR %>% 
  dplyr::select(Region, District, ID, Species, Symptoms.started, Cluster_No, Time_diff)


cluster_animals <- unique(c(IGR$ID, IGR$Biter.ID)) ## animals that have been assigned to a cluster
Solos <- rabid %>% filter(!ID %in% cluster_animals) ## solo animals without a cluster

## For the cluster animals we want to assign a column with the number of animals within that cluster

IGR2 <- add_count(IGR2, Cluster_No)

## Add the time period to each of the data frames
# set the day number depending on what period we want to look at. 

day_num <- 186 # for when looking at 6 months. 
#day_num <- 365 # for looking at years
#day_num <- 92  # looking at 3 month periods


IGR2[,"period"] <- ceiling(IGR2[,"Time_diff"]/day_num)
Solos[,"period"] <- ceiling(Solos[,"Time_diff"]/day_num)

table(IGR2$period)
table(Solos$period)

## If using year need to remove the last group as this is not a full year period. 
#IGR2 <-  IGR2[which(IGR2$period != 9),]
#Solos <- Solos[which(Solos$period !=9),]


## Now we want to find only the first entry in each cluster in the IGR2 

clust_start <- IGR2 %>%  # clust start just contains data on clusters of >=2.Doesn't contain singles
  group_by(Cluster_No) %>% 
  slice(which.min(Time_diff)) 

table(clust_start$period)
table(Solos$period)


max(Solos$Time_diff); min(Solos$Time_diff)
max(clust_start$Time_diff); min(clust_start$Time_diff)


## Also want to know the total number of individuals within clusters per period
number_in_clust_per_period <- clust_start %>%
  group_by(period) %>%
  summarise(Frequency = sum(n))
colnames(number_in_clust_per_period) <- c("period", "total_ind_in_clusters")
number_in_clust_per_period$period <- as.character(number_in_clust_per_period$period)

## calculate the number of clusters per period
clust_per_period <- as.data.frame(table(clust_start$period))
colnames(clust_per_period) <- c("period", "count_clusters")

## and the solos per period
singles_per_period <- as.data.frame(table(Solos$period))
colnames(singles_per_period) <- c("period", "count_singles")

## for 3 months need to build df as not clusters observed in all periods.
#counts_per_period <- as.data.frame(matrix(ncol = 1, nrow = 34))
#colnames(counts_per_period) <- "period"
#counts_per_period$period <- seq(1,34,1)
#counts_per_period$period <- as.factor(counts_per_period$period)
#counts_per_period <- left_join(counts_per_period, singles_per_period)
#counts_per_period <- left_join(counts_per_period, clust_per_period)

## join the two together
counts_per_period <- left_join(singles_per_period, clust_per_period)
counts_per_period[which(is.na(counts_per_period$count_clusters)), "count_clusters"] <- 0 
counts_per_period[which(is.na(counts_per_period$count_singles)), "count_singles"] <- 0 

counts_per_period[,"count_total"] <- rowSums(counts_per_period[,2:3]) # this is the number of clusters plus 
# number of solos

## And now add the number of individuals that are within the clusters within each period

counts_per_period <- left_join(counts_per_period, number_in_clust_per_period)
counts_per_period[which(is.na(counts_per_period$total_ind_in_clusters)), "total_ind_in_clusters"] <- 0 


## calculate a mean unit size that takes into account the cluster sizes and the singletons-------
counts_per_period[,"mean_unit_size"] <- (counts_per_period[,"count_singles"] + 
                                           counts_per_period[,"total_ind_in_clusters"])/counts_per_period[,"count_total"]
# count singles is the singlesm total ind in clusters is the number of animals in all the clusters and counts per period is total number of clusters including singles. 
# counts_per_period[is.na(counts_per_period$mean_unit_size), "mean_unit_size"] <-0


cpp_num <- counts_per_period
cpp_num$period <- as.numeric(cpp_num$period)
cpp_num$new_period <- cpp_num$period - 1
cpp_num$new_period_sq <- cpp_num$new_period^2

#plot of mean unit size
par(mar = c(5,5,3,3))

year_labs <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") # for 6-months and 3 months
#year_labs <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018") # for the year value

pos_labs <- c(0,2,4,6,8,10,12,14,16) # for 6 months
#pos_labs <- c(0,1,2,3,4,5,6,7) # for year
#pos_labs <- c(0,4,8,12,16,20,24,28,32) #for 3 months

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "orange", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n")
axis(1, labels= year_labs, at = pos_labs)

###### 

# count_total = no of data points used to calculate mean
# mean_unit_size is the mean
# period is the time period

wts <- cpp_num$count_total

## create predicted output - for 6 month period
newx <- seq(0,16, by = 0.1)
newx_sq <- seq(0, 16, by = 0.1)^2

# for year
#newx <- seq(0,7, by = 0.1)
#newx_sq <- seq(0,7, by = 0.1)^2

# for 3 month period
#newx <- seq(0,33, by = 0.1)
##newx_sq <- seq(0,32, by = 0.1)^2

## Quadratic term included

wt_quad <- lm(mean_unit_size ~ new_period + new_period_sq, data = cpp_num, weights = wts )
summary(wt_quad)

predictedcounts <- predict(wt_quad, list(new_period = newx, new_period_sq = newx_sq))

conf_interval <- predict(wt_quad, list(new_period = newx, new_period_sq = newx_sq),
                         interval="confidence",
                         level = 0.95)

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "orange", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4))
axis(1, labels= year_labs, at = pos_labs)
lines(newx, predictedcounts, col = "darkgreen", lwd = 2)
matlines(newx, conf_interval[,2:3], col = "dark green", lty=2, lwd = 2)

AIC(wt_quad)
###

### With spline

# for 6 month model - place knot at the 12th 6-month period as this visually looks like where there is a change
cpp_num$spline_bit <- ifelse(cpp_num$new_period - 12 < 0,0, cpp_num$new_period - 12)
newx_spline <- if_else(newx -12 <0, 0, newx-12)

## For year model
#cpp_num$spline_bit <- ifelse(cpp_num$new_period - 6 < 0,0, cpp_num$new_period - 6)
#newx_spline <- if_else(newx -6 <0, 0, newx-6)

# For 3 month model

#cpp_num$spline_bit <- ifelse(cpp_num$new_period - 26 < 0,0, cpp_num$new_period - 26)
#newx_spline <- if_else(newx -26 <0, 0, newx-26)

spliney <- lm(mean_unit_size ~ new_period + spline_bit, data = cpp_num, weights = wts)
summary(spliney)
AIC(spliney)

predictedcounts_spline <- predict(spliney, list(new_period = newx, spline_bit = newx_spline))
conf_interval_spline <- predict(spliney, list(new_period = newx, spline_bit = newx_spline),
                                interval="confidence",
                                level = 0.95)

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "red", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4))
axis(1, labels= year_labs, at = pos_labs)
lines(newx, predictedcounts_spline, col = "blue", lwd = 2)
matlines(newx, conf_interval_spline[,2:3], col = "blue", lty=2, lwd = 2)

AIC(spliney)

### With break at 13 for the 6 month period

cpp_num$spline_bit_2 <- ifelse(cpp_num$new_period - 13 < 0,0, cpp_num$new_period - 13)
newx_spline_2 <- if_else(newx -13 <0, 0, newx-13)

spliney_2 <- lm(mean_unit_size ~ new_period + spline_bit_2, data = cpp_num, weights = wts)
summary(spliney_2)

predictedcounts_spline_2 <- predict(spliney_2, list(new_period = newx, spline_bit_2 = newx_spline_2))
conf_interval_spline_2 <- predict(spliney_2, list(new_period = newx, spline_bit_2 = newx_spline_2),
                                  interval="confidence",
                                  level = 0.95)

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "turquoise", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4))
axis(1, labels= year_labs, at = pos_labs)
lines(newx, predictedcounts_spline_2, col = "pink", lwd = 2)
matlines(newx, conf_interval_spline_2[,2:3], col = "pink", lty=2, lwd = 2)

# Figure plot
plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, cex = 1.4, col = "orange", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4),cex.axis = 1.4, cex.lab = 1.4)
axis(1, labels= year_labs, at = pos_labs)
lines(newx, predictedcounts_spline_2, col = "dark green", lwd = 2)
polygon(c(newx, rev(newx)), c(conf_interval_spline_2[,2], rev(conf_interval_spline_2[,3])),
        col = "light grey", border = "light grey")
lines(newx, predictedcounts_spline_2, col = "dark green", lwd = 2)
lines(newx, conf_interval_spline_2[,3], col = "dark green", lty = 2, lwd = 2)
lines(newx, conf_interval_spline_2[,2], col = "dark green", lty = 2, lwd = 2)
points(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, cex = 1.4, col = "dark orange")
dev.off()



#### Test whether the positive slope is significant. 
#### Knot is occurring at 13 (when labelled 0-16)

df_slope_check <- cpp_num

# for 6-month model
df_slope_check$pos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,3)
df_slope_check$neg <- c(-13,-12,-11,-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 0, 0, 0)

# for the year model
#df_slope_check$pos <- c(0,0,0,0,0,0,0,1)
#df_slope_check$neg <- c(-6, -5, -4, -3, -2, -1, 0,0)

# For 3-month model
#df_slope_check$pos <- c(rep(0, 27), seq(1,7,1))
#df_slope_check$neg <- c(seq(-26, 0, 1), rep(0,7))


slope_mod <- lm(mean_unit_size ~ neg + pos, data = df_slope_check, weights = wts)
summary(slope_mod)


se_neg <- summary(slope_mod)$coefficients["neg",2]
se_pos <- summary(slope_mod)$coefficients["pos",2]
est_neg <- summary(slope_mod)$coefficients["neg",1]
est_pos <- summary(slope_mod)$coefficients["pos",1]

# now use these to calulate the CI. 

# initial negative slope
upper_CI_neg <- est_neg + (1.96*se_neg)
lower_CI_neg <- est_neg - (1.96*se_neg)

# and positive slope after the spline
upper_CI_pos <- est_pos + (1.96*se_pos)
lower_CI_pos <- est_pos - (1.96*se_pos)

lower_CI_neg ; upper_CI_neg
lower_CI_pos; upper_CI_pos

predictedcounts_slope <- predict(slope_mod, list(pos = df_slope_check$pos, neg = df_slope_check$neg))
conf_interval_slope <- predict(slope_mod, list(pos = df_slope_check$pos,  neg = df_slope_check$neg),
                               interval="confidence",
                               level = 0.95)

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "turquoise", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4))
axis(1, labels= year_labs, at = pos_labs)
lines(cpp_num$new_period, predictedcounts_slope, col = "orange", lwd = 2)
matlines(cpp_num$new_period, conf_interval_slope[,2:3], col = "pink", lty=2, lwd = 2)


## also try a linear model for the year data. 

###------------------------------------------------------------------------------


lin_mod <- lm(mean_unit_size ~ new_period, data = cpp_num, weights = wts)
summary(lin_mod)
AIC(lin_mod)

predictedcounts_line <- predict(lin_mod, list(new_period = newx))
conf_interval_line <- predict(lin_mod, list(new_period = newx),
                              interval="confidence",
                              level = 0.95)

plot(cpp_num$new_period, cpp_num$mean_unit_size, pch = 16, col = "red", xlab = "Date",
     ylab = "Mean cluster size", xaxt = "n", ylim = c(0,4))
axis(1, labels= year_labs, at = pos_labs)
lines(newx, predictedcounts_line, col = "blue", lwd = 2)
matlines(newx, conf_interval_line[,2:3], col = "blue", lty=2, lwd = 2)


## export data for chains_figure

colnames(IG_res)

chains_data <- IG_res %>%
  dplyr::select(Region, District, ID, Species, Symptoms.started, Cluster_No)

chains_data$date_onset <- as.Date(chains_data$Symptoms.started, format = "%d/%m/%Y")
head(chains_data)

#chains_data$date_ionset_2 <- as.POSIXct(chains_data$Symptoms.started, format = "%d/%m/%Y")

chains_data$days_in <- as.numeric(chains_data$date_onset - start)
#head(chains_data)

chains_data$Species <- as.numeric(chains_data$Species)

write.csv(chains_data, "data/cluster_assignment.csv")