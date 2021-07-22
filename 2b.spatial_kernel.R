#####################################################################
# Extract data to estimate parameters for:
# Spatial infection kernel
# 
#####################################################################


rm(list=ls())

## load libraries
#library(dplyr)
library(bbmle)
library(fitdistrplus)
library(extraDistr)

## source functions
source("R/distribution_functions.R")


dists_trunc <- read.csv("data/seren_dists.csv")
dists_trunc <- dists_trunc[,1]

### Now look at various distributions using interval censoring
## Choose the threshold for the interval censoring

#threshold <- 100
threshold <- 50
### Now will look at a number of distributions using interval censoring
### As per the code above will censor all values below 50 as 0-50m 

Censored <- as.data.frame(dists_trunc)
colnames(Censored) <- "left"
Censored["right"] <- Censored["left"]
Censored[which(Censored$right <= threshold), "left"] <- 0
Censored[which(Censored$right <= threshold), "right"] <- threshold


########   Gamma 
cens_gamma <- fitdistcens(Censored, "gamma", start = list(shape = 0.285, scale= 6500), lower = 0.00001)
summary(cens_gamma)
gamma_mean(cens_gamma$estimate[1], cens_gamma$estimate[2])

## then simulate a distribution to check and plot
cens_gamma_checker <- rgamma(1000000,  shape = cens_gamma$estimate[1], scale = cens_gamma$estimate[2])
mean(cens_gamma_checker)
sd(cens_gamma_checker)

gamma_cens_dist <- diff(pgamma(seq(0, 21000, by = 50), shape =  cens_gamma$estimate[1],  
                               scale = cens_gamma$estimate[2]))
 dev.off()

hist(dists_trunc, breaks = seq(0, 22000, by = 50), xlim = c(0, 4000), axes = T, 
     ylab = "Frequency observed", xlab = "Distance between linked cases (metres)",
     main = "Gamma single", freq = TRUE, cex.axis = 1.4, cex.lab = 1.4)
lines(seq(25, 21000, by = 50), gamma_cens_dist*length(dists_trunc), col = "red", lwd = 2)


#### Log normal
cens_ln <- fitdistcens(Censored, "lnorm")
summary(cens_ln)
lognorm_mean(cens_ln$estimate[1], cens_ln$estimate[2])

## Simulate a distribution to cHeck and plot
cens_ln_checker <- rlnorm(1000000,  meanlog = cens_ln$estimate[1], sdlog = cens_ln$estimate[2])
mean(cens_ln_checker)
sd(cens_ln_checker)

LN_cens_dist <- diff(plnorm(seq(0, 21000, by = 50),  meanlog = cens_ln$estimate[1], sdlog = cens_ln$estimate[2]))

hist(dists_trunc, breaks = seq(0, 22000, by = 50 ), xlim = c(0, 4000), axes = T, 
     ylab = "Frequency observed", xlab = "Distance between linked cases (metres)",
     main = "Lognormal - single", freq = TRUE, cex.axis = 1.4, cex.lab = 1.4)
lines(seq(25, 21000, by = 50), LN_cens_dist*1079, col = "dark green", lwd = 2)


#########  Weibull
cens_wei <- fitdistcens(censdata =  Censored, distr = "weibull")
summary(cens_wei)

weibull_mean(cens_wei$estimate[1], cens_wei$estimate[2])

## and then simulate a distribution to cHeck and plot
cens_wei_checker <- rweibull(1000000,  shape =  cens_wei$estimate[1],  scale = cens_wei$estimate[2])
mean(cens_wei_checker)

wei_cens_dist <- diff(pweibull(seq(0, 21000, by = 50), shape =  cens_wei$estimate[1], scale = cens_wei$estimate[2]))

hist(dists_trunc, breaks = seq(0, 22000, by = 50 ), xlim = c(0, 4000), axes = T, 
     ylab = "Frequency observed", xlab = "Distance between linked cases (metres)",
     main = "Weibull single", freq = TRUE, cex.axis = 1.4, cex.lab = 1.4)
lines(seq(25, 21000, by = 50), wei_cens_dist*1079, col = "blue", lwd = 2)

######################################################

## Compare the results of fitting the different distributions
cens_gamma$aic ; cens_ln$aic; cens_wei$aic

### Best fit here is the gamma distribution

##############################################
#                                            #
#    MIXTURE DISTRIBUTIONS                   #
#                                            #
##############################################

## example taken from:
## http://blogs2.datall-analyse.nl/2016/02/18/rcode_mixture_distribution_censored/  

#function for fitting a mixture distribution to INTERVAL censored data
library(SPREDA)
library(LearnBayes)

set.seed(6)

y1 <- Censored[,1]
y2 <- Censored[,2]
# NOTE: this function employs the type="interval" coding scheme as used
# in the "Surv" function (from the survival package) for interval censored data

#lower bound of censoring interval
L <- y1
# upper bound of censoring interval
U <- y2
# indicator for type of censoring

Cens <-rep(1,length(Censored)) ## sets default code for the interval censoring to 1 - exact dates
# censoring in the interval [0,100] is identical to left censoring at t=100


## Then changes to code 2 for the entries that are interval censored.
#<<<<<<< HEAD

#Select based on the cutoff being used
Cens <- ifelse((L == 0 & U == threshold), 2, Cens) ## so here we are coding the interval censored data as left censored.

#for these left censored observations, set L to the threshold, and U to NA
L <- ifelse(Cens==2, U, L)
U <- ifelse(Cens==2, NA, U)

#censoring distribution
table(Cens)

source("R/mixcensoredInt_function.R")
source("R/mixcensoredInt_SHPN_ln.R")
source("R/mixcensoredInt_SHPN_gamma.R")
source("R/mixcensoredInt_SHPN_weibull.R")

#fit a lognormal mixture distribution to the interval censored data
mixmult_ln <- replicate(10, mixcensoredInt(y1=L, y2=U, d=Cens,
                                           dist="lognormal", n=2,
                                           classify="EM", maxiter=50000))

## Repeat using our function to see if get same output
mixmult_ln_PNSH <- replicate(10, mixcensoredInt_SHPN_ln(y1=L, y2=U, d=Cens,
                                                        dist="lognormal", n=2,
                                                        classify="EM", maxiter=50000))

## And now look at output - log-likelihood values for the different solutions
mixmult_ln[3,]
mixmult_ln_PNSH[3,]

#discard models that did not converge
mixmult_ln[3,] <- ifelse(mixmult_ln[3,]==0, NA, mixmult_ln[3,])
mixmult_ln_PNSH[3,] <- ifelse(mixmult_ln_PNSH[3,]==0, NA, mixmult_ln_PNSH[3,])
# number of discarded models
length(which(is.na(mixmult_ln[3,])))
length(which(is.na(mixmult_ln_PNSH[3,])))

#select the solution with the maximum value of the log-likelihood
mixmult_lnMLE <- mixmult_ln[, which.max(mixmult_ln[3,])]
mixmult_lnMLE[1:9]

mixmult_lnMLE_PNSH <- mixmult_ln_PNSH[, which.max(mixmult_ln_PNSH[3,])]
mixmult_lnMLE_PNSH[1:9]


## Now run with other distributions
set.seed(6)
mixmult_wei <- replicate(10, mixcensoredInt(y1=L, y2=U, d=Cens,
                                            dist="weibull", n=2,
                                            classify="EM", maxiter=500000))

set.seed(6)
mixmult_wei_PNSH <- replicate(10, mixcensoredInt_SHPN_weibull(y1=L, y2=U, d=Cens,
                                                              dist="weibull", 
                                                              n=2,
                                                              classify="EM", maxiter=500000))
## And now look at output
#log-likelihood values for the different solutions
mixmult_wei[3,]
mixmult_wei_PNSH[3,]

#discard models that did not converge
mixmult_wei[3,] <- ifelse(mixmult_wei[3,]==0, NA, mixmult_wei[3,])
mixmult_wei_PNSH[3,] <- ifelse(mixmult_wei_PNSH[3,]==0, NA, mixmult_wei_PNSH[3,])
#number of discarded models
length(which(is.na(mixmult_wei[3,])))
length(which(is.na(mixmult_wei_PNSH[3,])))

#select the solution with the maximum value of the log-likelihood
mixmult_weiMLE <- mixmult_wei[, which.max(mixmult_wei[3,])]
mixmult_weiMLE[1:9]

mixmult_weiMLE_PNSH <- mixmult_wei_PNSH[, which.max(mixmult_wei_PNSH[3,])]
mixmult_weiMLE_PNSH[1:9]

## Weibull is parameterised differently in all the different packages and functions

library(mixdist)
##use weibullpar to get the shape and scale params from the mu and sigma params generated
## This works on the original function as is 

## these are from the model using the original function
wei_params_1 <-  weibullpar(mixmult_weiMLE$components[[1,"mu"]], mixmult_weiMLE$components[[1,"sigma"]])
wei_params_2 <- weibullpar(mixmult_weiMLE$components[[2,"mu"]], mixmult_weiMLE$components[[2,"sigma"]])

wei_params_1
wei_params_2

## And these are from the PN/SH modified function
## However, the results from our own function are different again. 
## To change our values to the same as in the original function I need to log the first param
## and do 1/second param
parms_model <- c(log(mixmult_weiMLE_PNSH$components[1,"mu"]), 1/mixmult_weiMLE_PNSH$components[1, "sigma"])
parms_model_2 <- c(log(mixmult_weiMLE_PNSH$components[2,"mu"]), 1/mixmult_weiMLE_PNSH$components[2,"sigma"])
parms_model
parms_model_2

#These are now the same as in the original version mu and sigma as per the original function. 

### Gamma 
set.seed(6)
mixmult_gamma_PNSH <- replicate(10, mixcensoredInt_SHPN_gamma(y1=L, y2=U, d=Cens,
                                                              dist="gamma", 
                                                              n=2,
                                                              classify="EM", maxiter=50000))

## And now look at output - log-likelihood values for the different solutions
mixmult_gamma_PNSH[3,]

#discard models that did not converge
mixmult_gamma_PNSH[3,] <- ifelse(mixmult_gamma_PNSH[3,]==0, NA, mixmult_gamma_PNSH[3,])
#number of discarded models
length(which(is.na(mixmult_gamma_PNSH[3,])))

mixmult_gammaMLE_PNSH <- mixmult_gamma_PNSH[, which.max(mixmult_gamma_PNSH[3,])]
mixmult_gammaMLE_PNSH[1:9]

##In these outputs sigma is the shape param and scale is 1/mu
shape_1 = mixmult_gammaMLE_PNSH$components[1,"sigma"]
scale_1 = 1/mixmult_gammaMLE_PNSH$components[1,"mu"]

shape_2 = mixmult_gammaMLE_PNSH$components[2,"sigma"]
scale_2 = 1/mixmult_gammaMLE_PNSH$components[2,"mu"]

#########################################

# AIC for the different solutions
mixmult_lnMLE$AIC
mixmult_weiMLE$AIC
mixmult_gammaMLE_PNSH$AIC

## work out the densities to add to the histogram plot to look at fit
ln_dens_1 <- diff(plnorm(seq(0,20750, by =50), meanlog = mixmult_lnMLE$components[[1,"mu"]],
                         sdlog = mixmult_lnMLE$components[[1,"sigma"]]))
ln_dens_2 <- diff(plnorm(seq(0,20750, by =50), meanlog = mixmult_lnMLE$components[[2,"mu"]],
                         sdlog = mixmult_lnMLE$components[[2,"sigma"]]))
ln_dens_3 <- ln_dens_1*mixmult_lnMLE$prior[1] + ln_dens_2*mixmult_lnMLE$prior[2]
## density is a function in R that computes the probability density for each value in gamma_dist_checker

wei_dens_1 <- diff(pweibull(seq(0,20750, by = 50), shape = wei_params_1$shape, scale = wei_params_1$scale))
wei_dens_2 <- diff(pweibull(seq(0,20750, by = 50), shape = wei_params_2$shape, scale = wei_params_2$scale))
wei_dens_3 <- wei_dens_1*mixmult_weiMLE$prior[1] + wei_dens_2*mixmult_weiMLE$prior[2]


##need to do 1/mu to get the scale
gamma_dens_1 <- diff(pgamma(seq(0,20750, by = 50), shape = mixmult_gammaMLE_PNSH$components[1,"sigma"],
                            scale = 1/mixmult_gammaMLE_PNSH$components[1,"mu"]))
gamma_dens_2 <- diff(pgamma(seq(0,20750, by = 50), shape = mixmult_gammaMLE_PNSH$components[2,"sigma"],
                            scale = 1/mixmult_gammaMLE_PNSH$components[2,"mu"]))
gamma_dens_3 <- gamma_dens_1*mixmult_gammaMLE_PNSH$prior[1] + gamma_dens_2*mixmult_gammaMLE_PNSH$prior[2]

gamma_single <- diff(pgamma(seq(0,20750, by =50), shape = 0.170, scale = 3018))

dev.off()

hist(dists_trunc, breaks = seq(0,22000, by = threshold), xlim = c(0, 5000), axes = T,
     ylab = "Frequency observed", xlab = "Bite distance", cex.axis = 1.4, cex.lab = 1.4,
     main = "Best fitting interval censored lognormal mixture distribution for bite distance", freq = TRUE)
lines(seq(25, 20750, by = threshold), ln_dens_3*length(dists_trunc), col = "dark green", lwd = 2)
#lines(seq(50, 22000, by = threshold), ln_dens_3*length(dists_trunc), col = "dark green", lwd = 2)


##Truncated version for paper
hist(dists_trunc, breaks = seq(0,22000, by = threshold), xlim = c(0, 4000), axes = T,
     ylab = "Frequency observed", xlab = "Distance between linked cases", cex.axis = 1.4, cex.lab = 1.4,
     main = "Lognormal - mixture", freq = TRUE)
lines(seq(25, 20750, by = threshold), ln_dens_3*length(dists_trunc), col = "dark green", lwd = 2)

### Gamma
hist(dists_trunc, breaks = seq(0, 22000, by = threshold ), xlim = c(0, 5000), axes = T, 
     ylab = "Frequency observed", xlab = "Bite distance",
     main = "Best fitting interval censored gamma mixture distribution for bite distance", freq = TRUE)
lines(seq(25, 20750, by = threshold), gamma_dens_3*length(dists_trunc), col = "red", lwd = 2)

## Truncated x axis for paper.
hist(dists_trunc, breaks = seq(0, 22000, by = threshold ), xlim = c(0, 4000), axes = T, 
     ylab = "Frequency observed", xlab = "Distance between linked cases (metres)", cex.axis = 1.4, cex.lab = 1.4,
     main = "Gamma - mixture", freq = TRUE)
lines(seq(25, 20750, by = threshold), gamma_dens_3*length(dists_trunc), col = "red", lwd = 2)

## Weibull
hist(dists_trunc, breaks = seq(0, 22000, by = threshold ), xlim = c(0, 4000), axes = T, 
     ylab = "Frequency observed", xlab = "Distance between linked cases (metres)", cex.axis = 1.4, cex.lab = 1.4,
     main = "Weibull - mixture", freq = TRUE)
lines(seq(25, 20750, by = threshold), wei_dens_3*length(dists_trunc), col = "blue", lwd = 2)

## The lowest AIC is seen with the single gamma distribution

# Write parameters for transmission tree inference
distparams = data.frame(dist_shape = coef(cens_gamma)["shape"], dist_scale = coef(cens_gamma)["scale"])
#write.csv(distparams, "output/distparams.csv", row.names=FALSE)

## And with the 100m cutoff the single gamma still performs best
distparams_100m = data.frame(dist_shape = coef(cens_gamma)["shape"], dist_scale = coef(cens_gamma)["scale"])
#write.csv(distparams_100m, "output/distparams_100m.csv", row.names=FALSE)

