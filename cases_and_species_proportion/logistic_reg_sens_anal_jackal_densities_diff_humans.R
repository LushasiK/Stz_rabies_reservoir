## Looking at the alternative scenarios that are included in the sensitivity analysis for the logistic regressin, 

rm(list = ls())

library(reshape2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(Hmisc)

Cases <- read.csv("data/Case_numbers_all_areas.csv", stringsAsFactors = F)
## This is a table with yearly counts for each species for each district. 

colnames(Cases)
## Don't think we need the Year 
Cases <- Cases[,-c(1,2)]
colnames(Cases)


Cases_wide <- dcast(Cases, District ~ Species , value.var = "Count", fun.aggregate = sum)
#Transforms it so each species has a column of its own, but we still have annual values for each district.

#Replace the NAs in the animal counts with 0 

Cases_wide[which(is.na(Cases_wide$Cat)), "Cat"] <-0
Cases_wide[which(is.na(Cases_wide$Livestock)), "Livestock"] <- 0

#Now do some sums on the columns. 

#Cases_wide["Total_cases"] <- rowSums(Cases_wide[,c("Domestic dog", "Livestock", "Cat", "Wildlife", "Jackal")])
Cases_wide["Total_cases"] <- rowSums(Cases_wide[,c("Domestic dog", "Jackal")])
## Add a total cases column which is the sum of domestic dogs and jackals only. 


## Now using dog numbers and CIs from Maganaga
Dog_numbers <- read.csv("output/Number_of_dogs24.csv")

# Combine the Pemba districts
Pemba <- Dog_numbers %>%
  dplyr::select(District, Numdogs, Numdogs.lwr, Numdogs.upr) %>%
  filter(District %in% c( "wete", "micheweni", "chakechake", "mkoani")) 


Pemba_totes <- colSums(Pemba[,2:4])

Pemba <- Pemba[1,]
Pemba[1,1] <- "Pemba"
Pemba[1,2:4] <- Pemba_totes

Dog_numbers <- Dog_numbers %>%
  dplyr::select(District, Numdogs, Numdogs.lwr, Numdogs.upr) %>%
  filter(District %in% c("kilwa", "lindi rural", "lindi urban", "liwale", "masasi", "masasi urban", "mtwara rural", 
                         "mtwara urban", "nachingwea", "nanyumbu", "newala", "ruangwa", "tandahimba",
                         "ngorongoro", "serengeti"))

Dog_numbers[Dog_numbers$District =="masasi urban", "District"] <- "Masasi Township Authority"
Dog_numbers[,"District"] <- str_to_title(Dog_numbers[,"District"])
Dog_numbers <- rbind(Dog_numbers, Pemba)

colnames(Dog_numbers) <- c("District","Dog_no_estimate", "Dog_no_lower", "Dog_no_upper")

## And the vacc coverage
V <- read.csv("data/Vacc_coverage_summary_STzdists.csv")
V <- V[,-1]
##Add the vaccine data to the dog numbers
Dog_numbers_vacc <- merge(Dog_numbers, V, by = "District", all.x = T)


## Use the jackal estimates when applying the jackals across the area 
## with >20 and <2000 people per cell (4kmsq)
Jackal_numbers <- read.csv("output/Jackal_number_est_above_20_below_2000_percell.csv", stringsAsFactors = F)

# Will just use the higher dog density to start with
Jackal_numbers_05 <- Jackal_numbers[,c(2,3,4)]

table(Cases_wide$District)
##Need to amend MTA in wide tab to match in the other tables
Cases_wide[which(Cases_wide$District == "Masasi  Township Authority"), "District"] <-
  "Masasi Township Authority"

##merge the various bits of info
Combined_numbers <- merge(Cases_wide, Dog_numbers_vacc)

Combined_numbers <- merge(Combined_numbers, Jackal_numbers)

## Need to make sure we still have the 16 districts once all merged
## Remove the column named X
Combined_numbers <- dplyr::select(Combined_numbers,-X)

##Create a column for the total number of susceptible animals which combines jackals and unvaccinated dogs
Combined_numbers["Total_animals_none_vacc"] <- 
  Combined_numbers[, "Dog_no_estimate"] + Combined_numbers[, "Jackals_using_density_0.05"]

Combined_numbers["Total_susc_animals_median"] <-
  Combined_numbers[,"Dog_no_estimate" ]*(1- Combined_numbers[, "row_median"]) +
  Combined_numbers[, "Jackals_using_density_0.05"] 

Combined_numbers["Total_susc_animals_min"] <- 
  Combined_numbers[, "Dog_no_estimate"]*(1-Combined_numbers[, "max_vacc"]) + 
  Combined_numbers[, "Jackals_using_density_0.05"]

## and now work out the proportions of the susceptible animals that are jackals
Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_no_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.05"]/Combined_numbers[,"Total_animals_none_vacc"]

Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_median_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.05"]/Combined_numbers[,"Total_susc_animals_median"]

Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_max_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.05"]/Combined_numbers[,"Total_susc_animals_min"]


Combined_numbers["Jackals_as_prop_of_cases"] <-
  Combined_numbers[, "Jackal"]/Combined_numbers[,"Total_cases"]

## Calculate a CI around the proportion of jackals positive
## Use bin conf and exact method due to low numbers

# First the CIS for the proportion of cases

Temp_df <- dplyr::select(Combined_numbers, c(Jackal, Total_cases))

binconf(3,64, alpha = 0.05, method = "exact")

Temp_df[,"jackal_prop_cases_point_est"] <- binconf(Temp_df[,"Jackal"], Temp_df[, "Total_cases"], alpha = 0.05, 
                                                   method = "exact")[,1]
Temp_df[,"jackal_prop_cases_lower"] <- binconf(Temp_df[,"Jackal"], Temp_df[, "Total_cases"], alpha = 0.05, 
                                               method = "exact")[,2]
Temp_df[,"jackal_prop_cases_upper"] <- binconf(Temp_df[,"Jackal"], Temp_df[, "Total_cases"], alpha = 0.05, 
                                               method = "exact")[,3]

Combined_numbers <- cbind(Combined_numbers, Temp_df[,c("jackal_prop_cases_lower", "jackal_prop_cases_upper")] )

# Also need to estimate a CI for jackals as proportion of susceptibles
# For this we will use the median vaccination coverage but will use the CI 
# for the dog numbers to estimate the total susceptibles 

Combined_numbers["Lower_est_susc_animals_median"] <-
  Combined_numbers[,"Dog_no_lower" ]*(1- Combined_numbers[, "row_median"]) +
  Combined_numbers[, "Jackals_using_density_0.05"] 

Combined_numbers["Upper_est_susc_animals_median"] <-
  Combined_numbers[,"Dog_no_upper" ]*(1- Combined_numbers[, "row_median"]) +
  Combined_numbers[, "Jackals_using_density_0.05"] 

Combined_numbers[,"Jack_as_prop_of_of_susc_med_vacc_lower"] <- Combined_numbers[, "Jackals_using_density_0.05"]/
  Combined_numbers[,"Upper_est_susc_animals_median"] ## use upper est of susp animals to estimate lower estimate of prop as 
## upper estimate of susceptibles gives more dogs so lower est of prop of pop that are jackals

Combined_numbers[,"Jack_as_prop_of_of_susc_med_vacc_upper"] <- Combined_numbers[, "Jackals_using_density_0.05"]/
  Combined_numbers[,"Lower_est_susc_animals_median"]

## make a data frame suitable for logistic regression

## For this I have converted the dataframe to one where each cases has a row. 
# It contains the species (y) and the proportion of animals that are jackals (x) 

logistic_df <- Combined_numbers %>% 
  dplyr::select(c(`Domestic dog`, Jackal, Jackals_as_prop_of_dogs_and_jackals_median_vacc)) %>%
  pivot_longer(-Jackals_as_prop_of_dogs_and_jackals_median_vacc, names_to = "species") %>%
  uncount(value)


str(logistic_df)
logistic_df <- as.data.frame(logistic_df)
logistic_df$species <- as.factor(logistic_df$species)

# transform to a percentage instead of proportion for easier interpretation of output
# logistic_df$Jackals_as_prop_of_dogs_and_jackals_median_vacc <-
# logistic_df$Jackals_as_prop_of_dogs_and_jackals_median_vacc*100

logist <- glm(species ~ Jackals_as_prop_of_dogs_and_jackals_median_vacc, data = logistic_df, 
              family = "binomial")

summary(logist)
exp(logist$coefficients)

##and now plot the points and the logistic regression

# calculate points for the line using the regression model

xweight <- seq(0,1,0.01)
yweight <- predict(logist, list(Jackals_as_prop_of_dogs_and_jackals_median_vacc = xweight),
                   type="response", se.fit = T)

line_med <- exp(yweight$fit)/(1+exp(yweight$fit))
#confidence interval 
exp(yweight$fit-1.96*yweight$se.fit)/(1+exp(yweight$fit-1.96*yweight$se.fit))
exp(yweight$fit+1.96*yweight$se.fit)/(1+exp(yweight$fit+1.96*yweight$se.fit))

#Now we plot.

### Or with the area shaded. 
ci1 <- yweight$fit - 1.96*yweight$se.fit
ci2 <- yweight$fit + 1.96*yweight$se.fit

#png("Figures/log_reg_jackals_5_500_j005.png", height = 380, width = 540)
plot(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc, #*100,
     Combined_numbers$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of susceptible animal population", ylab = "Jackals as proportion of  probable cases",
     col = "purple", pch = 19, cex= log10(Combined_numbers$Total_cases), xlim = c(0,1), ylim = c(0,1))
polygon(c(xweight, rev(xweight)), c(ci1, rev(ci2)), col = "pink", lty = 2)
lines(xweight, yweight$fit, lwd = 1.5)
arrows(x0=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc,
       x1=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc,
       y0=Combined_numbers$jackal_prop_cases_lower, 
       y1=Combined_numbers$jackal_prop_cases_upper,
       code=3, angle=90, length=0.1, col = "grey")

arrows(x0=Combined_numbers$Jack_as_prop_of_of_susc_med_vacc_lower,
       x1=Combined_numbers$Jack_as_prop_of_of_susc_med_vacc_upper,
       y0=Combined_numbers$Jackals_as_prop_of_cases, 
       y1=Combined_numbers$Jackals_as_prop_of_cases,
       code=3, angle=90, length=0.1, col = "grey")
points(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc, # replot points to cover arrows
       Combined_numbers$Jackals_as_prop_of_cases,
       col = "purple", pch = 19, cex= log10(Combined_numbers$Total_cases))

dev.off()

######## 
## Now with > 0 but less than 2000 per 4km2

Jackal_numbers_0_20 <- read.csv("output/Jackal_number_est_above_0_below_2000_percell.csv", stringsAsFactors = F)

# Will just use the higher dog density to start with
Jackal_numbers_0_20 <- Jackal_numbers_0_20[,c(2,3,4)]


##merge the various bits of info
CN_0_20 <- merge(Cases_wide, Dog_numbers_vacc)

CN_0_20 <- merge(CN_0_20, Jackal_numbers_0_20)

## Need to make sure we still have the 16 districts once all merged
## Remove the column named X
CN_0_20 <- dplyr::select(CN_0_20,-X)

##Create a column for the total number of susceptible animals which combines jackals and unvaccinated dogs
CN_0_20["Total_animals_none_vacc"] <- 
  CN_0_20[, "Dog_no_estimate"] + CN_0_20[, "Jackals_using_density_0.05"]

CN_0_20["Total_susc_animals_median"] <-
  CN_0_20[,"Dog_no_estimate" ]*(1- CN_0_20[, "row_median"]) +
  CN_0_20[, "Jackals_using_density_0.05"] 

CN_0_20["Total_susc_animals_min"] <- 
  CN_0_20[, "Dog_no_estimate"]*(1-CN_0_20[, "max_vacc"]) + 
  CN_0_20[, "Jackals_using_density_0.05"]

## and now work out the proportions of the susceptible animals that are jackals
CN_0_20["Jackals_as_prop_of_dogs_and_jackals_no_vacc"] <- 
  CN_0_20[, "Jackals_using_density_0.05"]/CN_0_20[,"Total_animals_none_vacc"]

CN_0_20["Jackals_as_prop_of_dogs_and_jackals_median_vacc"] <- 
  CN_0_20[, "Jackals_using_density_0.05"]/CN_0_20[,"Total_susc_animals_median"]

CN_0_20["Jackals_as_prop_of_dogs_and_jackals_max_vacc"] <- 
  CN_0_20[, "Jackals_using_density_0.05"]/CN_0_20[,"Total_susc_animals_min"]


CN_0_20["Jackals_as_prop_of_cases"] <-
  CN_0_20[, "Jackal"]/CN_0_20[,"Total_cases"]

## Calculate a CI around the proportion of jackals positive
## Use bin conf and exact method due to low numbers

# Doing it in steps so I don't get confused! 
# First the CIS for the proportion of cases

Temp_0_20_df <- dplyr::select(CN_0_20, c(Jackal, Total_cases))

Temp_0_20_df[,"jackal_prop_cases_point_est"] <- binconf(Temp_0_20_df[,"Jackal"], Temp_0_20_df[, "Total_cases"], alpha = 0.05, 
                                                        method = "exact")[,1]
Temp_0_20_df[,"jackal_prop_cases_lower"] <- binconf(Temp_0_20_df[,"Jackal"], Temp_0_20_df[, "Total_cases"], alpha = 0.05, 
                                                    method = "exact")[,2]
Temp_0_20_df[,"jackal_prop_cases_upper"] <- binconf(Temp_0_20_df[,"Jackal"], Temp_0_20_df[, "Total_cases"], alpha = 0.05, 
                                                    method = "exact")[,3]

CN_0_20 <- cbind(CN_0_20, Temp_0_20_df[,c("jackal_prop_cases_lower", "jackal_prop_cases_upper")] )

# Also need to estimate a CI for jackals as proportion of susceptibles
# For this we will use the median vaccination coverage but will use the CI 
# for the dog numbers to estimate the total susceptibles 

CN_0_20["Lower_est_susc_animals_median"] <-
  CN_0_20[,"Dog_no_lower" ]*(1- CN_0_20[, "row_median"]) +
  CN_0_20[, "Jackals_using_density_0.05"] 

CN_0_20["Upper_est_susc_animals_median"] <-
  CN_0_20[,"Dog_no_upper" ]*(1- CN_0_20[, "row_median"]) +
  CN_0_20[, "Jackals_using_density_0.05"] 

CN_0_20[,"Jack_as_prop_of_of_susc_med_vacc_lower"] <- CN_0_20[, "Jackals_using_density_0.05"]/
  CN_0_20[,"Upper_est_susc_animals_median"] ## use upper est of susp animals to estimate lower estimate of prop as 
## upper estimate of susceptibles gives more dogs so lower est of prop of pop that are jackals

CN_0_20[,"Jack_as_prop_of_of_susc_med_vacc_upper"] <- CN_0_20[, "Jackals_using_density_0.05"]/
  CN_0_20[,"Lower_est_susc_animals_median"]

## make a data frame suitable for logistic regression

## For this I have converted the dataframe to one where each cases has a row. 
# It contains the species (y) and the proportion of animals that are jackals (x) 

logistic_0_20_df <- CN_0_20 %>% 
  dplyr::select(c(`Domestic dog`, Jackal, Jackals_as_prop_of_dogs_and_jackals_median_vacc)) %>%
  pivot_longer(-Jackals_as_prop_of_dogs_and_jackals_median_vacc, names_to = "species") %>%
  uncount(value)


str(logistic_0_20_df)
logistic_0_20_df <- as.data.frame(logistic_0_20_df)
logistic_0_20_df$species <- as.factor(logistic_0_20_df$species)

logist_0_20_glm <- glm(species ~ Jackals_as_prop_of_dogs_and_jackals_median_vacc, data = logistic_0_20_df, 
                       family = "binomial")

summary(logist_0_20_glm)
exp(logist_0_20_glm$coefficients)

##and now plot the points and the logistic_0_20 regression

# calculate points for the line using the regression model

xweight <- seq(0,1,0.01)
yweight <- predict(logist_0_20_glm, list(Jackals_as_prop_of_dogs_and_jackals_median_vacc = xweight),
                   type="response", se.fit = T)

line_med <- exp(yweight$fit)/(1+exp(yweight$fit))
#confidence interval 
exp(yweight$fit-1.96*yweight$se.fit)/(1+exp(yweight$fit-1.96*yweight$se.fit))
exp(yweight$fit+1.96*yweight$se.fit)/(1+exp(yweight$fit+1.96*yweight$se.fit))

#Now we plot.

### Or with the area shaded. 
ci1 <- yweight$fit - 1.96*yweight$se.fit
ci2 <- yweight$fit + 1.96*yweight$se.fit

#png("Figures/log_reg_jackals_0_500_j005.png", height = 380, width = 540)
plot(CN_0_20$Jackals_as_prop_of_dogs_and_jackals_median_vacc, #*100,
     CN_0_20$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of susceptible animal population", ylab = "Jackals as proportion of  probable cases",
     col = "purple", pch = 19, cex= log10(CN_0_20$Total_cases), xlim = c(0,1), ylim = c(0,1))
polygon(c(xweight, rev(xweight)), c(ci1, rev(ci2)), col = "pink", lty = 2)
lines(xweight, yweight$fit, lwd = 1.5)
arrows(x0=CN_0_20$Jackals_as_prop_of_dogs_and_jackals_median_vacc,
       x1=CN_0_20$Jackals_as_prop_of_dogs_and_jackals_median_vacc,
       y0=CN_0_20$jackal_prop_cases_lower, 
       y1=CN_0_20$jackal_prop_cases_upper,
       code=3, angle=90, length=0.1, col = "grey")

arrows(x0=CN_0_20$Jack_as_prop_of_of_susc_med_vacc_lower,
       x1=CN_0_20$Jack_as_prop_of_of_susc_med_vacc_upper,
       y0=CN_0_20$Jackals_as_prop_of_cases, 
       y1=CN_0_20$Jackals_as_prop_of_cases,
       code=3, angle=90, length=0.1, col = "grey")
points(CN_0_20$Jackals_as_prop_of_dogs_and_jackals_median_vacc, # replot points to cover arrows
       CN_0_20$Jackals_as_prop_of_cases,
       col = "purple", pch = 19, cex= log10(CN_0_20$Total_cases))

dev.off()





