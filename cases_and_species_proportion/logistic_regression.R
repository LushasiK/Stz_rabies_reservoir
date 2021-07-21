
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
## with >0 and <2000 people per cell (4kmsq)
Jackal_numbers <- read.csv("output/Jackal_number_est_above_10_below_2000_percell.csv", stringsAsFactors = F)

# Will just use the higher dog density to start with
Jackal_numbers_30 <- Jackal_numbers[,c(2,3,6)]

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
  Combined_numbers[, "Dog_no_estimate"] + Combined_numbers[, "Jackals_using_density_0.30"]

Combined_numbers["Total_susc_animals_median"] <-
  Combined_numbers[,"Dog_no_estimate" ]*(1- Combined_numbers[, "row_median"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers["Total_susc_animals_min"] <- 
  Combined_numbers[, "Dog_no_estimate"]*(1-Combined_numbers[, "max_vacc"]) + 
  Combined_numbers[, "Jackals_using_density_0.30"]

## and now work out the proportions of the susceptible animals that are jackals
Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_no_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.30"]/Combined_numbers[,"Total_animals_none_vacc"]

Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_median_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.30"]/Combined_numbers[,"Total_susc_animals_median"]

Combined_numbers["Jackals_as_prop_of_dogs_and_jackals_max_vacc"] <- 
  Combined_numbers[, "Jackals_using_density_0.30"]/Combined_numbers[,"Total_susc_animals_min"]


Combined_numbers["Jackals_as_prop_of_cases"] <-
  Combined_numbers[, "Jackal"]/Combined_numbers[,"Total_cases"]

## Calculate a CI around the proportion of jackals positive
## Use bin conf and exact method due to low numbers

# Doing it in steps so I don't get confused! 
# First the CIS for the proportion of cases

Temp_df <- dplyr::select(Combined_numbers, c(Jackal, Total_cases))

#binconf(3,64, alpha = 0.05, method = "exact")

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
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers["Upper_est_susc_animals_median"] <-
  Combined_numbers[,"Dog_no_upper" ]*(1- Combined_numbers[, "row_median"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers[,"Jack_as_prop_of_of_susc_med_vacc_lower"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Upper_est_susc_animals_median"] 

Combined_numbers[,"Jack_as_prop_of_of_susc_med_vacc_upper"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Lower_est_susc_animals_median"]


#Now plot these numbers 
dev.off()
plot(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc,
     Combined_numbers$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of animals", ylab = "Jackals as proportion of cases", col = "blue", pch = 19, cex= log10(Combined_numbers$Total_cases))

# Also look at using a logistic regression

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
#xweight <- seq(0, 100, 0.1)
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

dot_size <- c(1,2,3)
cases <- c(10,100,1000)

#png("Figures/log_reg_jackals_2.5_500.png", height = 450, width = 540)
#png("Figures/log_reg_jackals_2.5_500_with_line.png", height = 450, width = 540)


par(mfrow = c(1,1), oma=c(5,0,0,0),xpd=NA, cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)

plot(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_median_vacc, #*100,
     Combined_numbers$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of susceptible animal population", ylab = "Jackals as proportion of probable cases",
     col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases), xlim = c(0,1), ylim = c(0,1))
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
       col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases))

legend(0.25, -0.4, ncol = 3, legend=cases, pt.cex=dot_size, cex = 1.2, pch=19, col =  "black", title = "Number of cases")

## add 1:1 line to plot
xline <- seq(0,1, by = 0.1)
yline <- seq(0,1,by = 0.1)
lines(xline, yline, lty = 2, lwd = 2)
dev.off()

############################################################################################
### Also look at the min and max vaccination coverage

## First min vacc coverage

# need to estimate a CI for jackals as proportion of susceptibles using minimum vacc coverage
# For this we will use the median vaccination coverage but will use the CI 
# for the dog numbers to estimate the total susceptibles 

Combined_numbers["Lower_est_susc_animals_min_vacc"] <-
  Combined_numbers[,"Dog_no_lower" ]*(1- Combined_numbers[, "min_vacc"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers["Upper_est_susc_animals_min_vacc"] <-
  Combined_numbers[,"Dog_no_upper" ]*(1- Combined_numbers[, "min_vacc"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers[,"Jack_as_prop_of_of_susc_min_vacc_lower"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Upper_est_susc_animals_min_vacc"] ## use upper est of susp animals to estimate lower estimate of prop as 
## upper estimate of susceptibles gives more dogs so lower est of prop of pop that are jackals

Combined_numbers[,"Jack_as_prop_of_of_susc_min_vacc_upper"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Lower_est_susc_animals_min_vacc"]

## make a data frame suitable for logistic regression

logistic_df_min_vacc <- Combined_numbers %>% 
  dplyr::select(c(`Domestic dog`, Jackal, Jackals_as_prop_of_dogs_and_jackals_no_vacc)) %>%
  pivot_longer(-Jackals_as_prop_of_dogs_and_jackals_no_vacc, names_to = "species") %>%
  uncount(value)


str(logistic_df_min_vacc)
logistic_df_min_vacc <- as.data.frame(logistic_df_min_vacc)
logistic_df_min_vacc$species <- as.factor(logistic_df_min_vacc$species)

logist_min_vacc <- glm(species ~ Jackals_as_prop_of_dogs_and_jackals_no_vacc, data = logistic_df_min_vacc, 
              family = "binomial")

summary(logist_min_vacc)
exp(logist_min_vacc$coefficients)

##and now plot the points and the logistic regression

# calculate points for the line using the regression model
#xweight <- seq(0, 100, 0.1)
xweight <- seq(0,1,0.01)
yweight <- predict(logist_min_vacc, list(Jackals_as_prop_of_dogs_and_jackals_no_vacc = xweight),
                   type="response", se.fit = T)

line_med <- exp(yweight$fit)/(1+exp(yweight$fit))
#confidence interval 
exp(yweight$fit-1.96*yweight$se.fit)/(1+exp(yweight$fit-1.96*yweight$se.fit))
exp(yweight$fit+1.96*yweight$se.fit)/(1+exp(yweight$fit+1.96*yweight$se.fit))

#Now we plot with the area shaded. 
ci1 <- yweight$fit - 1.96*yweight$se.fit
ci2 <- yweight$fit + 1.96*yweight$se.fit

#png("Figures/log_reg_jackals_2.5_500_no_vacc.png", height = 380, width = 540)
plot(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_no_vacc, #*100,
     Combined_numbers$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of susceptible animal population", ylab = "Jackals as proportion of probable cases",
     col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases), xlim = c(0,1), ylim = c(0,1))
polygon(c(xweight, rev(xweight)), c(ci1, rev(ci2)), col = "turquoise", lty = 2)
lines(xweight, yweight$fit, lwd = 1.5)
arrows(x0=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_no_vacc,
       x1=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_no_vacc,
       y0=Combined_numbers$jackal_prop_cases_lower, 
       y1=Combined_numbers$jackal_prop_cases_upper,
       code=3, angle=90, length=0.1, col = "grey")

arrows(x0=Combined_numbers$Jack_as_prop_of_of_susc_min_vacc_lower,
       x1=Combined_numbers$Jack_as_prop_of_of_susc_min_vacc_upper,
       y0=Combined_numbers$Jackals_as_prop_of_cases, 
       y1=Combined_numbers$Jackals_as_prop_of_cases,
       code=3, angle=90, length=0.1, col = "grey")
points(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_no_vacc, # replot points to cover arrows
       Combined_numbers$Jackals_as_prop_of_cases,
       col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases))

dev.off()

############################################################################

### And now with max vacc coverage

Combined_numbers["Lower_est_susc_animals_max_vacc"] <-
  Combined_numbers[,"Dog_no_lower" ]*(1- Combined_numbers[, "max_vacc"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers["Upper_est_susc_animals_max_vacc"] <-
  Combined_numbers[,"Dog_no_upper" ]*(1- Combined_numbers[, "max_vacc"]) +
  Combined_numbers[, "Jackals_using_density_0.30"] 

Combined_numbers[,"Jack_as_prop_of_of_susc_max_vacc_lower"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Upper_est_susc_animals_max_vacc"] ## use upper est of susp animals to estimate lower estimate of prop as 
## upper estimate of susceptibles gives more dogs so lower est of prop of pop that are jackals

Combined_numbers[,"Jack_as_prop_of_of_susc_max_vacc_upper"] <- Combined_numbers[, "Jackals_using_density_0.30"]/
  Combined_numbers[,"Lower_est_susc_animals_max_vacc"]

## make a data frame suitable for logistic regression

## For this I have converted the dataframe to one where each cases has a row. 
# It contains the species (y) and the proportion of animals that are jackals (x) 

logistic_df_max_vacc <- Combined_numbers %>% 
  dplyr::select(c(`Domestic dog`, Jackal, Jackals_as_prop_of_dogs_and_jackals_max_vacc)) %>%
  pivot_longer(-Jackals_as_prop_of_dogs_and_jackals_max_vacc, names_to = "species") %>%
  uncount(value)


str(logistic_df_max_vacc)
logistic_df_max_vacc <- as.data.frame(logistic_df_max_vacc)
logistic_df_max_vacc$species <- as.factor(logistic_df_max_vacc$species)

logist_max_vacc <- glm(species ~ Jackals_as_prop_of_dogs_and_jackals_max_vacc, data = logistic_df_max_vacc, 
                       family = "binomial")

summary(logist_max_vacc)
exp(logist_max_vacc$coefficients)

##and now plot the points and the logistic regression

# calculate points for the line using the regression model
#xweight <- seq(0, 100, 0.1)
xweight <- seq(0,1,0.01)
yweight <- predict(logist_max_vacc, list(Jackals_as_prop_of_dogs_and_jackals_max_vacc = xweight),
                   type="response", se.fit = T)

line_med <- exp(yweight$fit)/(1+exp(yweight$fit))
#confidence interval 
exp(yweight$fit-1.96*yweight$se.fit)/(1+exp(yweight$fit-1.96*yweight$se.fit))
exp(yweight$fit+1.96*yweight$se.fit)/(1+exp(yweight$fit+1.96*yweight$se.fit))

#Now we plot with the area shaded. 
ci1 <- yweight$fit - 1.96*yweight$se.fit
ci2 <- yweight$fit + 1.96*yweight$se.fit

#png("Figures/log_reg_jackals_2.5_500_max_vacc.png", height = 380, width = 540)
plot(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_max_vacc, #*100,
     Combined_numbers$Jackals_as_prop_of_cases,
     xlab = "Jackals as proportion of susceptible animal population", ylab = "Jackals as proportion of probable cases",
     col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases), xlim = c(0,1), ylim = c(0,1))
polygon(c(xweight, rev(xweight)), c(ci1, rev(ci2)), col = "turquoise", lty = 2)
lines(xweight, yweight$fit, lwd = 1.5)
arrows(x0=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_max_vacc,
       x1=Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_max_vacc,
       y0=Combined_numbers$jackal_prop_cases_lower, 
       y1=Combined_numbers$jackal_prop_cases_upper,
       code=3, angle=90, length=0.1, col = "grey")

arrows(x0=Combined_numbers$Jack_as_prop_of_of_susc_max_vacc_lower,
       x1=Combined_numbers$Jack_as_prop_of_of_susc_max_vacc_upper,
       y0=Combined_numbers$Jackals_as_prop_of_cases, 
       y1=Combined_numbers$Jackals_as_prop_of_cases,
       code=3, angle=90, length=0.1, col = "grey")
points(Combined_numbers$Jackals_as_prop_of_dogs_and_jackals_max_vacc, # replot points to cover arrows
       Combined_numbers$Jackals_as_prop_of_cases,
       col = "black", pch = 19, cex= log10(Combined_numbers$Total_cases))

dev.off()

