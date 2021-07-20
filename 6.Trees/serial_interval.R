#####################################################################################################
##
## Look at serial interval distributions for cases in the Serengeti where we know the biter. 
##
#####################################################################################################

rm(list = ls())
library(fitdistrplus)
library(extraDistr)

Seren <- readRDS(file = "output/clean_bite_data.rda")

set.seed(4)

# ###### SERENGETI REGION ONLY #############
# #Seren <- subset(AllTanz, AllTanz$District == "Serengeti")
# #Seren[] <- lapply(Seren, function(x) if(is.factor(x)) factor(x) else x)
# #Seren$Symptoms.started <- as.Date(Seren$Symptoms.started, format = "%d-%b-%y")
# 
# ##See how many cases for each species there are.
# #table(Seren$Species)
# 
# ###Check that it adds up to the number I am expecting
# #sum(table(Seren$Species)) # 9931
# 
# ## Just use cases from 2002 onwards
Seren <- subset(Seren, Seren$Symptoms.started >= "2002-01-01")
Seren_CC <- subset(Seren, Seren$Symptoms.started >= "2002-01-01")
# 
# ########################################################################################
# #########################################################################################
# 
 ## Remove livestock - select just domestic dogs as these are the only carnivores where info is known
 Known_bites <- subset(Seren_CC, Seren_CC$Species == "Domestic dog")
 Known_bites <- subset(Seren, Seren$Species == "Domestic dog")
 
### Look at suspect cases only
table(Known_bites$Suspect)
## Those labelled "To Do" and with symptoms.started entered include as rabid:
Known_bites <- subset(Known_bites, Known_bites$Suspect == "Yes" | # 3101
                         Known_bites$Suspect == "To Do") # 24
 
##Remove the ones without a date of symptoms.started
Known_bites <- Known_bites[!is.na(Known_bites$Symptoms.started),] 
 
# change date bitten into a date format
Known_bites$Date.bitten <- as.Date(Known_bites$Date.bitten, format = "%d-%b-%y")
Known_bites <- Known_bites[which(Known_bites$Biter.ID !=0),] # 1144


########################################################################################
######     SET UP A DATA FRAME WITH THE SERIAL INTERVALS   ######
########################################################################################

## ID 3075 was bitten by 2 dogs  

## nclude both "cases" as both were suspect rabid and 
## both bit so this is 2 possible SIs

## so alter the ID number but in such a way that it is still recognizable.
#range(Known_bites$ID)

Known_bites[which(Known_bites$ID == 3075 & Known_bites$Biter.ID == 3072), "ID"] <- 1003075
Known_bites[which(Known_bites$ID == 3075 & Known_bites$Biter.ID == 3073), "ID"] <- 2003075

Seren[which(Seren$ID == 3075 & Seren$Biter.ID == 3072), "ID"] <- 1003075
Seren[which(Seren$ID == 3075 & Seren$Biter.ID == 3073), "ID"] <- 2003075

## Also need to change the biter ID as otherwise the loop won't run. 
## As 1003075 and 2003075 are the same dog I can just select any of these
Known_bites[which(Known_bites$Biter.ID == 3075), "Biter.ID"] <- 1003075
Seren[which(Seren$ID == 3075), "Biter.ID"] <- 1003075

biter_known <- Known_bites
levels(biter_known$Symptoms.started.accuracy)

## Functions for uncertain dates/periods
extract_uncertainty = function(uncertainty){
  temp1 <- gregexpr("[0-9]+", uncertainty)  # Numbers with any number of digits
  as.numeric(unlist(regmatches(uncertainty, temp1))) # extract digits
}

biter_known$SI_accuracy <- extract_uncertainty(biter_known$Symptoms.started.accuracy)


########################################
## Also do for Seren data set. 
Seren_accuracy <- extract_uncertainty(Seren$Symptoms.started.accuracy)
Seren$SI_accuracy <- NA
Seren$SI_accuracy[which(!is.na(Seren$Symptoms.started.accuracy))] <- Seren_accuracy

SI_results <- data.frame(ID = biter_known$ID,
                         biter_ID = biter_known$Biter.ID,
                         SS = biter_known$Symptoms.started,
                         SS_uncert = as.numeric(biter_known$SI_accuracy),
                         biter_SS = Seren$Symptoms.started[match(biter_known$Biter.ID, Seren$ID)],
                         biter_SS_uncert = Seren$SI_accuracy[match(biter_known$Biter.ID, Seren$ID)])
SI_results$Serial_Interval <- as.numeric(SI_results$SS - SI_results$biter_SS)
range(SI_results$Serial_Interval, na.rm = TRUE)

## Identify the results that are NA or <0 and then remove
Subzeros <- SI_results[which(SI_results$Serial_Interval <0),] 
Unknowns <- SI_results[which(is.na(SI_results$Serial_Interval)),]
Subzeros 
Unknowns
SI_results <- SI_results[which(SI_results$Serial_Interval >= 0 | SI_results$Serial_Interval == "NA"),]

par(mar= c(5,5,3,3))

hist(SI_results$Serial_Interval, breaks = -1:405, xlab = "Serial Interval (days)", 
     main = "SI distribution for Serengeti where biter known",
     col = "red", cex.axis = 1.5, 
     cex.lab = 1.5, font.lab = 2, plot = T)$counts ## By adding the -1 we have 0 and 1 separately

## Add 0.5 to all the results that are 0
SI_results$Serial_Interval[which(SI_results$Serial_Interval == 0)] <- 0.5
nrow(SI_results[which(SI_results$Serial_Interval==0.5),])
nrow(SI_results[which(SI_results$Serial_Interval==0),])

mean(SI_results$Serial_Interval)  ## 27.8
sd(SI_results$Serial_Interval)    ## 36.9
range(SI_results$Serial_Interval) ## 0.5 - 405
median(SI_results$Serial_Interval) ## 19
quantile(SI_results$Serial_Interval, 0.95) ## 88.1
quantile(SI_results$Serial_Interval, 0.975) ## 128.55


#################################################################################################

# Maximum Likelihood Fitting of the SI Distribution

#########################################################################################
###########   INTERVAL CENSORING      ###################################################
#########################################################################################

###Now need to set up columns in the data set that can be used for the interval censoring. 
### Have left and right columns. These have the same value in if we have NO UNCERTAINTY. 
## If we are censoring we have the minimum and maximum vsluesin left and right. 
### https://r-forge.r-project.org/scm/viewvc.php/*checkout*/JSS/fitdistrplus/paper2JSS.pdf?revision=246&root=riskassessment&pathrev=250

SI_results["left"] <- SI_results["Serial_Interval"] - SI_results["biter_SS_uncert"] - SI_results["SS_uncert"]
SI_results["right"] <- SI_results["Serial_Interval"] + SI_results["biter_SS_uncert"] + SI_results["SS_uncert"]

### Some of the left values produced are negative
### As SI cannot be negative then set these to 0.5
### Upper limit still stays as the maximum - no need to change this
SI_results[which(SI_results$left <= 0), "left"] <- 0.5

## For dog formerly 3075 has 2 results in the table. 
## If we look at the longest and shortest periods for this SI it is 7 - 45 depending on which dog passed on the virus. 
## As we don't know which, remove the two entries and replace with one whose uncertainty 
## takes into account all the options. 

SI_results <- SI_results[-(which(SI_results$ID == 2003075)),] ## remove one of the entries
SI_results[which(SI_results$ID == 1003075),"left"] <- 7
Censored <- SI_results[,c("left", "right")]

#############################################################################################

source("R/means_from_distributions.R")

df <- SI_results

## Log normal dist. 
cens_ln <- fitdistcens(Censored, "lnorm")
summary(cens_ln)

LN_cens_1 <- diff(plnorm(seq(0,410, by = 5), meanlog = cens_ln$estimate[1], 
                         sdlog = cens_ln$estimate[2]))

lognorm_mean(cens_ln$estimate[1], cens_ln$estimate[2])

hist(df[,"Serial_Interval"], breaks = seq(0, 410, by = 5 ), xlim = c(0, 410), axes = T, ylab = "Frequency observed", xlab = "",
     main = "Best fitting log normal distribution using interval censoring", freq = TRUE)
lines(seq(2.5, 410, by = 5), LN_cens_1*nrow(SI_results), col = "dark green", lwd = 2)

## histogram for paper
par(mfrow=c(2,1))
hist(df[,"Serial_Interval"], breaks = seq(0, 410, by = 5 ), xlim = c(0, 410), axes = T, ylab = "Frequency observed", xlab = "Generation time (days)",
     main = "Log normal", freq = TRUE)
lines(seq(2.5, 410, by = 5), LN_cens_1*nrow(SI_results), col = "dark green", lwd = 2)


#####################################################################################
cens_wei <- fitdistcens(Censored, "weibull")
summary(cens_wei)

wei_cens_1 <- diff(pweibull(seq(0,410, by = 5), shape = cens_wei$estimate[1], 
                            scale = cens_wei$estimate[2]))
weibull_mean(cens_wei$estimate[1], cens_wei$estimate[2])

hist(df[,"Serial_Interval"], breaks = seq(0, 410, by = 5 ), xlim = c(0, 410), axes = T, ylab = "Frequency observed", xlab = "Generation time (days)",
     main = "Weibull", freq = TRUE)
lines(seq(2.5, 410, by = 5), wei_cens_1*nrow(SI_results), col = "blue", lwd = 2)
######################################################

cens_gamma <- fitdistcens(Censored, "gamma", start = list(shape = 0.5, scale= 500), 
                          lower = 0.00001)
summary(cens_gamma)
gamma_mean(cens_gamma$estimate[1], cens_gamma$estimate[2])

## and then simulate a distribution to cHeck and plot
gamm_cens_1 <- diff(pgamma(seq(0,410, by = 5), shape = cens_gamma$estimate[1], 
                           scale = cens_gamma$estimate[2]))

hist(df[,"Serial_Interval"], breaks = seq(0, 410, by = 5 ), xlim = c(0, 410), axes = T, 
     ylab = "Frequency observed", xlab = "Generation time (days)",
     main = "Gamma", freq = TRUE)
lines(seq(2.5, 410, by = 5), gamm_cens_1*nrow(SI_results), col = "red", lwd = 2)

###########################################################################
#########################################################################

summary(cens_ln)
summary(cens_wei)
summary(cens_gamma)
cens_ln$aic; cens_wei$aic; cens_gamma$aic

## Log normal has the best fit so use the parameters from this for trees etc
SIparams = data.frame(SImean = coef(cens_ln)["meanlog"], SIsd = coef(cens_ln)["sdlog"])
#write.csv(SIparams, "output/SIparams.csv", row.names=FALSE)




