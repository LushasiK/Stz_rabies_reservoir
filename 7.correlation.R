rm(list = ls())

library(lubridate)
library(dplyr)
library(tidyverse)

data <- read.csv("output/animal_cases.csv")

SE_Tanz_2011 <- data
SE_Tanz_2011$Date_started <- as.Date(SE_Tanz_2011$Symptoms.started, format = "%d/%m/%Y")

# Just have cases that have entries for the symptoms started and the long/lat
SE_Tanz_2011 <-SE_Tanz_2011[complete.cases(SE_Tanz_2011$Symptoms.started),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Latitude),]
SE_Tanz_2011 <- SE_Tanz_2011[complete.cases(SE_Tanz_2011$Longitude),] # down to 569!

# Take only suspect = Yes
SE_Tanz_2011 <- SE_Tanz_2011[which(SE_Tanz_2011$Suspect == "Yes"),]

## Need to add a column for the month since the start of data collection
SE_Tanz_2011$month <- as.numeric(format(SE_Tanz_2011$Date_started,  "%m"))
SE_Tanz_2011$year <- as.numeric(format(SE_Tanz_2011$Date_started, "%y"))


months = 1: 103 ## 8.5 years of data - I have added the one as I get NA for the first year
n_months <- 103

table(SE_Tanz_2011$months_since_start)


rabies_ts = hist(SE_Tanz_2011$months_since_start, breaks=0:(n_months), plot=FALSE)$counts #modif 12*7
plot(months, rabies_ts, type="l")
dev.off()

rabies_ts

dogs <- subset(SE_Tanz_2011, SE_Tanz_2011$Species == "Domestic dog")
jackals <- subset(SE_Tanz_2011, SE_Tanz_2011$Species == "Wildlife: Jackal")

dogs_ts <- hist(dogs$months_since_start, breaks=0:n_months, plot=FALSE)$counts
plot(months, dogs_ts, type="l")
jackals_ts <- hist(jackals$months_since_start, breaks=0:n_months, plot=FALSE)$counts
plot(months, jackals_ts, type="l")


## Let's bind the time series altogether so we can look at correlations 

Monthly_Totals <- as.data.frame(cbind(rabies_ts, dogs_ts, jackals_ts))
colnames(Monthly_Totals) <- c("All_Totals", "Dog_Totals", "Jackal_Totals")



## Look at correlations between the groups as lagged 
## Can do it individually like this
cor.test(Monthly_Totals$Dog_Totals, Monthly_Totals$Jackal_Totals)
cor.test(Monthly_Totals$Dog_Totals[1:102], Monthly_Totals$Jackal_Totals[2:103])
cor.test(Monthly_Totals$Dog_Totals[1:101], Monthly_Totals$Jackal_Totals[3:103])
cor.test(Monthly_Totals$Dog_Totals[1:100], Monthly_Totals$Jackal_Totals[4:103])
cor.test(Monthly_Totals$Dog_Totals[1:99], Monthly_Totals$Jackal_Totals[5:103])
cor.test(Monthly_Totals$Dog_Totals[1:98], Monthly_Totals$Jackal_Totals[6:103])
cor.test(Monthly_Totals$Dog_Totals[1:97], Monthly_Totals$Jackal_Totals[7:103])
cor.test(Monthly_Totals$Dog_Totals[1:96], Monthly_Totals$Jackal_Totals[8:103])
cor.test(Monthly_Totals$Dog_Totals[1:95], Monthly_Totals$Jackal_Totals[9:103])
cor.test(Monthly_Totals$Dog_Totals[1:94], Monthly_Totals$Jackal_Totals[10:103])
cor.test(Monthly_Totals$Dog_Totals[1:93], Monthly_Totals$Jackal_Totals[11:103])
cor.test(Monthly_Totals$Dog_Totals[1:92], Monthly_Totals$Jackal_Totals[12:103])

abd <- cor.test(Monthly_Totals$Dog_Totals, Monthly_Totals$Jackal_Totals)
abd$p.value
abd$estimate[[1]]
## Or can use the function like this. 

source("R/corr_fun.R")

Dogs_defore_jackals <- corr_fun(Monthly_Totals, Monthly_Totals, "Dog_Totals", "Jackal_Totals", "pearson") 
Dogs_defore_jackals$cor_pvals <- round(Dogs_defore_jackals$cor_pvals, 3)
Dogs_defore_jackals$cor_coeffs
## Then do it lagged the other way

Jackals_before_dogs <- corr_fun(Monthly_Totals, Monthly_Totals, "Jackal_Totals", "Dog_Totals", "pearson") 
Jackals_before_dogs$cor_pvals <- round(Jackals_before_dogs$cor_pvals, 3)


#write.csv(Dogs_defore_jackals, "output/Dogs_before_jackals_corr_all_areas.csv")
#write.csv(Jackals_before_dogs, "output/Jackals_before_dogs_corr_all_areas.csv")

