#####################################################################################################
##
## Look at serial interval distributions for cases in the Serengeti where we know the biter. 
##
#####################################################################################################

rm(list = ls())
library(fitdistrplus)
library(extraDistr)

# read in SI info from Serengeti

SI_results <- read.csv("data/seren_si.csv")

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




